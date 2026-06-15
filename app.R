library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(sf)
library(lubridate)
library(DT)
library(shinyjs)
library(shinyWidgets)

# -------------------------------------------------------------------------
# GLOBAL CONFIGURATION & DATA LOADING
# -------------------------------------------------------------------------

# Disable scientific notation globally
options(scipen = 999)

# Correction factor to convert straight-line distance to real walking network distance
DETOUR_INDEX <- 1.3

# Load the district boundaries (GeoJSON)
districts_geo <- st_read("krakow-districts.geojson", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

# Month abbreviations lookup for the price-trend timeline chart
month_lookup <- c(
  "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
  "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
  "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"
)

# Load and prepare the main listings dataset
df_master <- read_csv("krakow_master_dataset.csv", show_col_types = FALSE) %>%
  # Guard against missing coordinates and invalid prices (price must be > 0 for log-transform)
  filter(!is.na(lon), !is.na(lat), !is.na(price_per_m2), price_per_m2 > 0) %>%
  mutate(
    id = row_number(),
    listing_date = as.Date(listing_date),
    # Convert binary text indicators into standard dummy variables
    elevator_num = if_else(hasElevator == "yes", 1, 0),
    parking_num = if_else(hasParkingSpace == "yes", 1, 0),
    balcony_num = if_else(hasBalcony == "yes", 1, 0),
    storage_num = if_else(hasStorageRoom == "yes", 1, 0),
    security_num = if_else(hasSecurity == "yes", 1, 0),
    # Impute missing architectural baseline data with zeros or typical constants
    floor = if_else(is.na(floor), 0, as.numeric(floor)),
    buildYear = if_else(is.na(buildYear), 2000, as.numeric(buildYear)),
    
    # Adjust geometric distances to reflect actual pedestrian network routing 
    schoolDist_net = schoolDistance * DETOUR_INDEX,
    clinicDist_net = clinicDistance * DETOUR_INDEX,
    pharmacyDist_net = pharmacyDistance * DETOUR_INDEX,
    restaurantDist_net = restaurantDistance * DETOUR_INDEX,
    postOfficeDist_net = postOfficeDistance * DETOUR_INDEX,
    kindergartenDist_net = kindergartenDistance * DETOUR_INDEX
  )

# --- ADVANCED ANALYTICAL ENGINE ---

# 1. K-Fold CV (Includes Variance Correction for log-level models)
calculate_cv_metrics <- function(data, formula, k = 5) {
  set.seed(42) # Ensure reproducibility of the CV splits
  folds <- sample(1:k, nrow(data), replace = TRUE)
  
  cv_rmse_errors <- numeric(k)
  cv_mape_errors <- numeric(k)
  
  for(i in 1:k) {
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    # Guard against extremely small fold samples that break the model
    if(nrow(train_data) < 10) next 
    
    tryCatch({
      fit <- lm(formula, data = train_data)
      preds_log <- predict(fit, newdata = test_data)
      
      # Smearing / Variance Correction applied during Cross-Validation
      # This fixes Jensen's Inequality for log-transformed dependent variables
      correction <- exp((sigma(fit)^2) / 2)
      preds_real <- exp(preds_log) * correction
      
      actuals <- test_data$price_per_m2
      
      cv_rmse_errors[i] <- mean((actuals - preds_real)^2, na.rm = TRUE)
      cv_mape_errors[i] <- mean(abs((actuals - preds_real) / actuals), na.rm = TRUE) * 100
    }, error = function(e) { NULL })
  }
  
  return(list(
    rmse = sqrt(mean(cv_rmse_errors[cv_rmse_errors > 0], na.rm = TRUE)),
    mape = mean(cv_mape_errors[cv_mape_errors > 0], na.rm = TRUE)
  ))
}

# 2. Build the model ONCE per district (Fixes Data Leakage & improves performance)
build_valuation_model <- function(district_data, global_data) {
  form <- as.formula(log(price_per_m2) ~ squareMeters + floor + centreDistance + buildYear + elevator_num + parking_num + balcony_num)
  
  local_model_data <- district_data %>% drop_na(squareMeters, floor, centreDistance, buildYear, elevator_num, parking_num, balcony_num)
  global_model_data <- global_data %>% drop_na(squareMeters, floor, centreDistance, buildYear, elevator_num, parking_num, balcony_num)
  
  # Strict safety threshold to prevent OLS overfitting on small local clusters
  if(nrow(local_model_data) > 30) {
    model <- lm(form, data = local_model_data)
    metrics <- calculate_cv_metrics(local_model_data, form)
    return(list(model = model, type = "Dedicated Local Model (Log-Level OLS)", n_obs = nrow(local_model_data), metrics = metrics))
  } else {
    model <- lm(form, data = global_model_data)
    metrics <- calculate_cv_metrics(global_model_data, form)
    return(list(model = model, type = "Citywide Model (Log-Level OLS)", n_obs = nrow(global_model_data), metrics = metrics))
  }
}

# 3. Predict Prices Safely (Applying Jensen's Inequality Correction)
apply_valuation <- function(model_obj, apt_data) {
  preds_log <- predict(model_obj$model, newdata = apt_data)
  
  # The Smearing / Variance Correction
  correction <- exp((sigma(model_obj$model)^2) / 2)
  return(exp(as.numeric(preds_log)) * correction)
}


# -------------------------------------------------------------------------
# UI ARCHITECTURE
# -------------------------------------------------------------------------

ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "lumen", primary = "#2563eb", "card-border-radius" = "12px"),
  
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; background-color: #f8fafc; font-family: 'Inter', -apple-system, sans-serif; }
      
      /* STICKY ONE-LINE FILTER BAR */
      .light-filter-bar {
        background-color: #ffffff; padding: 12px 25px; border-bottom: 1px solid #e2e8f0;
        width: 100%; box-shadow: 0 4px 15px rgba(0,0,0,0.04); position: sticky; top: 0; z-index: 1000;
        display: flex; flex-wrap: nowrap; align-items: flex-end; gap: 20px;
      }
      
      .filter-item { flex: 0 1 auto; min-width: 140px; }
      .filter-item-small { flex: 0 1 auto; min-width: 120px; }
      .filter-item-large { flex: 1 1 auto; min-width: 180px; max-width: 250px; }
      .filter-item-more { flex: 0 0 auto; padding-bottom: 2px; }
      
      .form-group { margin-bottom: 0 !important; }
      .control-label { display: block !important; font-size: 10.5px !important; font-weight: 700 !important; color: #475569 !important; text-transform: uppercase; margin-bottom: 5px; white-space: nowrap; }
      .irs { margin-top: -5px !important; }
      
      input[type=number]::-webkit-inner-spin-button, input[type=number]::-webkit-outer-spin-button { -webkit-appearance: none; margin: 0; }
      input[type=number] { -moz-appearance: textfield; }
      
      .bootstrap-select .dropdown-toggle, .btn-light { border: 1px solid #ced4da !important; border-radius: 6px !important; padding: 6px 12px !important; font-size: 13px !important; color: #334155 !important; background-color: #fff !important; font-weight: normal !important; }
      .bootstrap-select .dropdown-toggle:focus, .btn-light:focus { outline: none !important; box-shadow: 0 0 0 0.2rem rgba(37,99,235,.25) !important; border-color: #2563eb !important; }
      .sw-dropdown-content { padding: 20px !important; border-radius: 12px !important; box-shadow: 0 10px 25px rgba(0,0,0,0.1) !important; border: 1px solid #e2e8f0 !important; min-width: 280px !important;}
      
      .counter-badge { flex: 0 0 auto; background: #eff6ff; padding: 6px 20px; border-radius: 8px; border: 1px solid #bfdbfe; text-align: center; box-shadow: inset 0 2px 4px rgba(0,0,0,0.02); margin-left: auto; }
      
      .hint-box { font-size: 0.9rem; color: #2563eb; font-weight: 600; margin-bottom: 15px; background-color: #eff6ff; padding: 10px 15px; border-radius: 8px; border-left: 4px solid #3b82f6; display: inline-block; }
      .story-section { min-height: calc(100vh - 80px); padding: 35px 50px; display: flex; flex-direction: column; justify-content: space-between; box-sizing: border-box; }
      .card { box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); border: none; }
      .card-header { background-color: #ffffff; border-bottom: 1px solid #f1f5f9; font-weight: 600; color: #1e293b; }
      
      /* EMPTY-STATE STYLING */
      .placeholder-msg { color: #64748b; font-style: italic; text-align: center; font-size: 18px; padding: 80px 40px; background-color: #ffffff; border-radius: 12px; border: 2px dashed #cbd5e1; margin: 15px 0; }
      .placeholder-msg-act1 { color: #64748b; text-align: center; padding: 60px 40px; background-color: #ffffff; border-radius: 12px; border: 2px dashed #cbd5e1; margin: 15px 0; }
      
      .act-badge { background-color: #2563eb; color: white; padding: 5px 14px; border-radius: 20px; font-weight: 700; font-size: 12px; margin-right: 12px; }
      .nav-btn-container { display: flex; justify-content: space-between; margin-top: 20px; padding-top: 15px; border-top: 1px solid #e2e8f0; }
    "))
  ),
  
  div(class = "light-filter-bar",
      
      div(class = "filter-item-small", numericInput("budget", "Max Price (PLN)", value = 1000000, min = 100000, step = 50000)),
      div(class = "filter-item-large", sliderInput("area", "Area (m²)", min = 15, max = 300, value = c(35, 80))),
      
      div(class = "filter-item", 
          pickerInput(inputId = "rooms", label = "Rooms", choices = c("1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6+"=6), selected = c(2,3), multiple = TRUE, 
                      options = list(`selected-text-format` = "count > 2", `none-selected-text` = "Any", container = "body"))
      ),
      
      div(class = "filter-item",
          pickerInput(inputId = "tech", label = "Amenities", choices = c("Elevator"="elevator", "Balcony/Terrace"="balcony", "Parking"="parking", "Storage Room"="storage", "Security"="security", "15-min City"="15min"), multiple = TRUE, 
                      options = list(`selected-text-format` = "count > 1", `none-selected-text` = "No requirements", container = "body"))
      ),
      
      div(class = "filter-item-more",
          dropdownButton(
            inputId = "more_filters", label = "More filters", icon = icon("sliders-h"), status = "light", circle = FALSE, right = FALSE,
            tags$h6("Location & Building", style="font-weight:700; color:#1e293b; margin-bottom:15px;"),
            sliderInput("dist", "Distance to center (max km)", min = 0, max = 15, value = 8, step = 0.5),
            sliderInput("floor_range", "Floor range", min = 0, max = 10, value = c(0, 4), step = 1),
            numericInput("year_from", "Built after year", value = 1990, min = 1950, max = 2026, step = 5)
          )
      ),
      
      div(class = "counter-badge",
          div(style="font-size:10px; color:#64748b; font-weight:700; text-transform:uppercase; line-height: 1;", "Listings"),
          div(textOutput("count_text", inline=TRUE), style="font-weight:900; color:#2563eb; font-size:18px; line-height: 1.2; margin-top: 2px;")
      )
  ),
  
  div(id = "story_container",
      
      div(id = "act_1", class = "story-section",
          div(
            h4(span(class = "act-badge", "ACT I"), "Smart Location Recommendation System", style="font-weight:800; color:#0f172a;"),
            p(style = "font-size: 1.05rem; color: #475569; margin-bottom: 20px;", "Based on your filters, the system identified the optimal areas. ", span("Gold bars highlight the top districts that combine an affordable price with high listing availability", style="color:#d97706; font-weight:700;"), ". Click any bar to proceed."),
            uiOutput("act1_content")
          ),
          div(class = "nav-btn-container", div(), actionButton("go_to_act2", "Go to district map ➔", class = "btn-primary", style="font-weight:bold; padding: 10px 24px; border-radius: 8px;"))
      ),
      
      div(id = "act_2", class = "story-section",
          div(
            h4(span(class = "act-badge", "ACT II"), "Local Map: Price Anomaly Detection", style="font-weight:800; color:#0f172a;"),
            p(style = "font-size: 1.05rem; color: #475569; margin-bottom: 10px;", "The system re-priced every listing with a robust log-level OLS model. ", span("Green marks underpriced units (good deals)", style="color:#10b981; font-weight:700;"), ", while ", span("red marks overpriced units", style="color:#ef4444; font-weight:700;"), ". Model errors are rigorously validated out-of-sample (CV)."),
            div(class = "hint-box", icon("lightbulb"), " Note: The model does not factor in renovation condition. Some 'deals' might require major refurbishment. Tip: click the legend to toggle map layers."),
            uiOutput("map_or_placeholder")
          ),
          div(class = "nav-btn-container", actionButton("back_to_act1", "« Back to recommendations", class = "btn-light", style="font-weight:bold; padding: 10px 24px; border-radius: 8px; color: #475569;"), actionButton("go_to_act3", "Go to comparison panel ➔", class = "btn-primary", style="font-weight:bold; padding: 10px 24px; border-radius: 8px;"))
      ),
      
      div(id = "act_3", class = "story-section",
          div(
            h4(span(class = "act-badge", "ACT III"), "Advanced Listing Audit & Direct Competition", style="font-weight:800; color:#0f172a;"),
            p(style = "font-size: 1.05rem; color: #475569; margin-bottom: 20px;", "Verify the market valuation. The table below shows the closest alternatives based on multi-dimensional KNN (K-Nearest Neighbors). ", tags$b("You can click any unit in the table to switch the analytics view.")),
            uiOutput("detail_ui")
          ),
          div(class = "nav-btn-container", actionButton("back_to_act2", "« Back to map", class = "btn-light", style="font-weight:bold; padding: 10px 24px; border-radius: 8px; color: #475569;"), actionButton("go_to_act4", "Check infrastructure (15-minute city) ➔", class = "btn-primary", style="font-weight:bold; color:white; padding: 10px 24px; border-radius: 8px;"))
      ),
      
      div(id = "act_4", class = "story-section",
          div(
            h4(span(class = "act-badge", "ACT IV"), "PropTech Index - Walkability Score", style="font-weight:800; color:#0f172a;"),
            p(style = "font-size: 1.05rem; color: #475569; margin-bottom: 10px;", "Check whether the bargain price comes at the cost of being cut off from urban infrastructure. The radar chart shows whether the unit meets the ", tags$b("15-minute city"), " concept."),
            p(style = "font-size: 0.85rem; color: #94a3b8; margin-bottom: 10px; margin-top: -6px;", "Important: Distances have been multiplied by a Detour Index of 1.3 to simulate real pedestrian network distance, not just a straight line."),
            # ADDED: The interactive UX hint box for Act IV
            div(class = "hint-box", icon("mouse-pointer"), " Tip: hover over the chart to see exact distances in kilometers. Click the legend below to toggle layers."),
            uiOutput("radar_ui")
          ),
          div(class = "nav-btn-container", actionButton("back_to_act3", "« Back to audit", class = "btn-light", style="font-weight:bold; padding: 10px 24px; border-radius: 8px; color: #475569;"), actionButton("reset_all", "Start over ↺", class = "btn-dark", style="font-weight:bold; color:white; padding: 10px 24px; border-radius: 8px;"))
      )
  )
)


# -------------------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  
  clicks_store <- reactiveValues(district = NULL, apartment = NULL)
  
  # Reset the workflow if main filters are modified
  observeEvent(list(input$budget, input$area, input$rooms, input$dist, input$year_from, input$floor_range, input$tech), {
    clicks_store$district <- NULL
    clicks_store$apartment <- NULL
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  }, ignoreInit = TRUE)
  
  # Navigation Observers
  observeEvent(input$go_to_act2, { if(!is.null(clicks_store$district)) shinyjs::runjs("document.getElementById('act_2').scrollIntoView({behavior: 'smooth'});") else showNotification("First select a district from the Act I chart!", type = "warning") })
  observeEvent(input$back_to_act1, { shinyjs::runjs("document.getElementById('act_1').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$go_to_act3, { if(!is.null(clicks_store$apartment)) shinyjs::runjs("document.getElementById('act_3').scrollIntoView({behavior: 'smooth'});") else showNotification("First select an apartment from the map in Act II!", type = "warning") })
  observeEvent(input$back_to_act2, { clicks_store$apartment <- NULL; shinyjs::runjs("document.getElementById('act_2').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$go_to_act4, { if(!is.null(clicks_store$apartment)) shinyjs::runjs("document.getElementById('act_4').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$back_to_act3, { shinyjs::runjs("document.getElementById('act_3').scrollIntoView({behavior: 'smooth'});") })
  observeEvent(input$reset_all, { clicks_store$district <- NULL; clicks_store$apartment <- NULL; shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});") })
  
  # JS to Shiny listener for the KNN table button click
  observeEvent(input$current_knn_id, {
    req(input$current_knn_id)
    clicks_store$apartment <- as.numeric(input$current_knn_id)
  })
  
  filtered_data <- reactive({
    # Ensure inputs are populated before executing filtering to prevent startup crashes
    req(input$budget, input$area, input$floor_range)
    
    selected_rooms <- if(is.null(input$rooms)) 1:6 else as.numeric(input$rooms)
    
    res <- df_master %>%
      filter(
        price <= input$budget,
        squareMeters >= input$area[1],
        squareMeters <= input$area[2],
        ((rooms %in% selected_rooms) | (6 %in% selected_rooms & rooms >= 6)),
        centreDistance <= input$dist,
        floor >= input$floor_range[1],
        floor <= input$floor_range[2],
        buildYear >= input$year_from
      )
    
    tech_opts <- input$tech
    if("elevator" %in% tech_opts) res <- res %>% filter(elevator_num == 1)
    if("balcony" %in% tech_opts)  res <- res %>% filter(balcony_num == 1)
    if("parking" %in% tech_opts) res <- res %>% filter(parking_num == 1)
    if("storage" %in% tech_opts) res <- res %>% filter(storage_num == 1)
    if("security" %in% tech_opts) res <- res %>% filter(security_num == 1)
    
    # Apply 15-min city spatial index using adjusted Network Distances
    if("15min" %in% tech_opts) {
      res <- res %>% filter(schoolDist_net <= 1.5, clinicDist_net <= 1.5, pharmacyDist_net <= 1.5, restaurantDist_net <= 1.5, postOfficeDist_net <= 1.5)
    }
    
    return(res)
  })
  
  output$count_text <- renderText({ paste(format(nrow(filtered_data()), big.mark=" ")) })
  
  output$act1_content <- renderUI({
    if (nrow(filtered_data()) == 0) {
      div(class = "placeholder-msg-act1",
          icon("search-minus", style = "font-size: 3.5rem; color: #94a3b8; margin-bottom: 15px;"), br(),
          span(style = "font-weight: 800; color: #334155; font-size: 22px;", "No apartments match your criteria"), br(),
          p(style = "font-size: 15px; color: #64748b; margin-top: 10px;", "Your filters are too restrictive. Try increasing the budget, widening the area range, or relaxing some amenity requirements.")
      )
    } else {
      layout_columns(
        col_widths = c(7, 5),
        card(card_header(icon("chart-bar"), " Average listing prices (Gold = Optimal choice)"), plotlyOutput("macroRank", height = "380px")),
        card(card_header(icon("chart-line"), " Timeline: dataset price stability"), plotlyOutput("trendPlot", height = "380px"))
      )
    }
  })
  
  # Safe Range Scaling for KNN (prevents dividing by zero if variance is 0)
  knn_neighbors <- reactive({
    req(clicks_store$apartment)
    target <- df_master %>% filter(id == clicks_store$apartment)
    
    # Filter out the target itself AND REMOVE DUPLICATE LISTINGS (agency clones)
    pool <- filtered_data() %>% 
      filter(name == target$name, id != target$id) %>%
      distinct(price_per_m2, squareMeters, .keep_all = TRUE)
    
    if(nrow(pool) == 0) return(pool)
    
    reference <- filtered_data() %>% filter(name == target$name)
    
    # Helper to calculate the robust range (Max - Min). If 0, fallback to 1e-5.
    safe_rng <- function(x) {
      rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
      if (is.na(rng) || rng == 0) return(1e-5)
      return(rng)
    }
    
    rng_sq <- safe_rng(reference$squareMeters)
    rng_cd <- safe_rng(reference$centreDistance)
    rng_fl <- safe_rng(reference$floor)
    rng_by <- safe_rng(reference$buildYear)
    rng_el <- safe_rng(reference$elevator_num)
    rng_pa <- safe_rng(reference$parking_num)
    
    # Euclidean distance over Min-Max Range Scaled dimensions
    pool %>%
      mutate(dist_val = sqrt(
        ((squareMeters - target$squareMeters) / rng_sq)^2 +
          ((centreDistance - target$centreDistance) / rng_cd)^2 +
          ((floor - target$floor) / rng_fl)^2 +
          ((buildYear - target$buildYear) / rng_by)^2 +
          ((elevator_num - target$elevator_num) / rng_el)^2 +
          ((parking_num - target$parking_num) / rng_pa)^2
      )) %>%
      arrange(dist_val) %>% head(5) %>% select(-dist_val)
  })
  
  # Calculate generalized deal potential considering the log(price) transformation
  top_recommended <- reactive({
    req(nrow(filtered_data()) > 0)
    pool <- filtered_data()
    valid_data <- pool %>% drop_na(squareMeters, floor, centreDistance, buildYear)
    
    tryCatch({
      if(nrow(valid_data) < 30) stop()
      city_model <- lm(log(price_per_m2) ~ squareMeters + floor + centreDistance + buildYear, data = valid_data)
      
      # Apply Variance Correction
      correction <- exp((sigma(city_model)^2) / 2)
      pool_with_residual <- valid_data %>% 
        mutate(
          pred_price = exp(predict(city_model, newdata = valid_data)) * correction,
          # Negative residual = cheaper than predicted by the regression line
          residual = price_per_m2 - pred_price 
        )
      pool_with_residual %>% group_by(name) %>% summarise(deal_potential = mean(residual, na.rm = TRUE), n = n()) %>% filter(n >= 2) %>% arrange(deal_potential) %>% head(3) %>% pull(name)
    }, error = function(e) {
      # Fallback ranking if data is insufficient for regression
      pool %>% group_by(name) %>% summarise(avg_p = mean(price_per_m2, na.rm=TRUE)) %>% arrange(avg_p) %>% head(3) %>% pull(name)
    })
  })
  
  output$macroRank <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    district_avg <- filtered_data() %>% group_by(name) %>% summarise(avg_p = mean(price_per_m2, na.rm = TRUE)) %>% arrange(avg_p)
    top3 <- top_recommended()
    selected <- clicks_store$district
    
    district_avg <- district_avg %>% mutate(color = case_when(name %in% selected ~ "#2563eb", name %in% top3 ~ "#d97706", TRUE ~ "#94a3b8"))
    
    plot_ly(district_avg, y = ~name, x = ~avg_p, type = 'bar', orientation = 'h', source = "macro_click", customdata = ~name,
            marker = list(color = ~color, line = list(color = "white", width = 1)), textposition = "none", text = ~paste0("<b>", name, "</b><br>Average: ", format(round(avg_p), big.mark = " "), " PLN/m²"), hoverinfo = "text", name = "") %>%
      layout(xaxis = list(title = "", ticksuffix = " PLN"), yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = ~name, dtick = 1), margin = list(l = 140, r = 15, t = 10, b = 10))
  })
  
  output$trendPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    dt <- filtered_data() %>% mutate(m = floor_date(listing_date, "month")) %>% group_by(m) %>% summarise(avg_p = mean(price_per_m2, na.rm = TRUE)) %>% drop_na(m) %>% mutate(lbl = paste(month_lookup[as.character(month(m))], year(m)))
    
    plot_ly(dt, x = ~m, y = ~avg_p, type = 'scatter', mode = 'lines+markers', line = list(color = '#cbd5e1', width = 2.5), marker = list(size = 8, color = "#475569"),
            text = ~paste0("<b>Period:</b> ", lbl, "<br><b>Average:</b> ", format(round(avg_p), big.mark = " "), " PLN/m²"), hoverinfo = "text", name = "") %>%
      layout(xaxis = list(title = "", ticktext = ~lbl, tickvals = ~m, tickangle = -45), yaxis = list(title = "", ticksuffix = " PLN"), margin = list(t=10, b=40))
  })
  
  # Precompute the CV model ONCE per district to prevent memory leaks and UI stuttering
  district_model_precomputed <- reactive({
    req(clicks_store$district)
    local_points <- filtered_data() %>% filter(name == clicks_store$district)
    req(nrow(local_points) > 0)
    
    # Passing filtered data to ensure fallback model matches user constraints
    build_valuation_model(local_points, filtered_data())
  })
  
  output$map_or_placeholder <- renderUI({
    if (is.null(clicks_store$district)) return(div(class = "placeholder-msg", icon("map-marked-alt"), "Requires completing ACT I: click a district bar to load the map."))
    
    val_model <- district_model_precomputed()
    
    # Render UI caption with out-of-sample error estimates (OOS-CV RMSE)
    fit_caption <- sprintf("%s \u00b7 Cross-Validated RMSE \u2248 %s PLN/m\u00b2 \u00b7 n = %d listings used",
                           val_model$type, format(round(val_model$metrics$rmse), big.mark = " "), val_model$n_obs)
    return(
      card(
        card_header(icon("map"), paste(" Price anomaly map:", clicks_store$district)),
        plotlyOutput("microMap", height = "460px"),
        div(style = "font-size: 11px; color: #94a3b8; text-align: center; padding: 6px 0 0;", fit_caption)
      )
    )
  })
  
  output$microMap <- renderPlotly({
    req(clicks_store$district)
    selected <- clicks_store$district
    local_points <- filtered_data() %>% filter(name == selected)
    local_geo <- districts_geo %>% filter(name == selected)
    if (nrow(local_points) == 0) return(NULL)
    
    val_model <- district_model_precomputed()
    local_points$pred <- apply_valuation(val_model, local_points)
    local_points$residual <- local_points$price_per_m2 - local_points$pred
    
    apt_id <- clicks_store$apartment
    
    p <- plot_ly(source = "map_click") %>% add_sf(data = local_geo, type = "scatter", mode = "lines", line = list(color = "#e2e8f0", width = 1.5), showlegend = FALSE, hoverinfo = "none")
    
    if (is.null(apt_id)) {
      deals <- local_points %>% filter(residual <= 0)
      overpriced <- local_points %>% filter(residual > 0)
      if(nrow(deals) > 0) p <- p %>% add_markers(data = deals, x = ~lon, y = ~lat, customdata = ~id, name = "Attractive price (Deal)", marker = list(size = 9, color = "#10b981", opacity = 0.85, line = list(color = "white", width = 0.5)), text = ~paste0("<b>Status:</b> Attractive price<br><b>Price:</b> ", format(round(price_per_m2), big.mark = " "), " PLN/m²<br><b>Underpriced by:</b> ", format(round(abs(residual)), big.mark = " "), " PLN/m²"), hoverinfo = "text")
      if(nrow(overpriced) > 0) p <- p %>% add_markers(data = overpriced, x = ~lon, y = ~lat, customdata = ~id, name = "Overpriced", marker = list(size = 9, color = "#ef4444", opacity = 0.85, line = list(color = "white", width = 0.5)), text = ~paste0("<b>Status:</b> Overpriced<br><b>Price:</b> ", format(round(price_per_m2), big.mark = " "), " PLN/m²<br><b>Overpriced by:</b> +", format(round(residual), big.mark = " "), " PLN/m²"), hoverinfo = "text")
    } else {
      neighbors <- knn_neighbors()
      selected_target <- local_points %>% filter(id == apt_id)
      
      if(nrow(neighbors) > 0) { 
        neighbors$pred <- apply_valuation(val_model, neighbors)
        neighbors$neighbor_residual <- neighbors$price_per_m2 - neighbors$pred 
      }
      
      others <- local_points %>% filter(id != apt_id, !(id %in% neighbors$id))
      p <- p %>% add_markers(data = others, x = ~lon, y = ~lat, marker = list(size = 6, color = "#f8fafc", opacity = 0.4), hoverinfo = "none", showlegend = FALSE)
      
      if(nrow(neighbors) > 0) {
        neighbor_deals <- neighbors %>% filter(neighbor_residual <= 0)
        neighbor_overpriced <- neighbors %>% filter(neighbor_residual > 0)
        if(nrow(neighbor_deals) > 0) p <- p %>% add_markers(data = neighbor_deals, x = ~lon, y = ~lat, customdata = ~id, marker = list(size = 13, color = "#10b981", symbol = "square", line = list(color="white", width=0.8)), text = ~paste0("<b>Alternative (Deal)</b><br>Area: ", sprintf("%.2f m²", squareMeters)), hoverinfo = "text", name = "Neighbors: Attractive price")
        if(nrow(neighbor_overpriced) > 0) p <- p %>% add_markers(data = neighbor_overpriced, x = ~lon, y = ~lat, customdata = ~id, marker = list(size = 13, color = "#ef4444", symbol = "square", line = list(color="white", width=0.8)), text = ~paste0("<b>Alternative (Overpriced)</b><br>Area: ", sprintf("%.2f m²", squareMeters)), hoverinfo = "text", name = "Neighbors: Overpriced")
      }
      target_color <- if_else(selected_target$residual <= 0, "#10b981", "#ef4444")
      target_label <- if_else(selected_target$residual <= 0, "Target: Deal", "Target: Overpriced")
      p <- p %>% add_markers(data = selected_target, x = ~lon, y = ~lat, customdata = ~id, marker = list(size = 22, color = target_color, symbol = "star", line = list(color="white", width=2)), text = ~paste0("<b>Audited unit ID: ", id, "</b>"), hoverinfo = "text", name = target_label)
    }
    p %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), margin = list(l=0, r=0, t=0, b=0), legend = list(orientation = "h", x = 0.02, y = 1.05)) %>% hide_colorbar()
  })
  
  observeEvent(event_data("plotly_click", source = "macro_click"), {
    click <- event_data("plotly_click", source = "macro_click")
    if (!is.null(click) && !is.null(click$y)) {
      val <- as.character(click$y) 
      if (val %in% isolate(filtered_data())$name) { clicks_store$district <- val; clicks_store$apartment <- NULL; shinyjs::runjs("document.getElementById('act_2').scrollIntoView({behavior: 'smooth'});") }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "map_click"), {
    map_click <- event_data("plotly_click", source = "map_click")
    tryCatch({
      if (!is.null(map_click) && !is.null(map_click$customdata)) {
        apt_id <- as.numeric(map_click$customdata[1])
        if (nrow(isolate(filtered_data())) > 0 && apt_id %in% isolate(filtered_data())$id) { clicks_store$apartment <- apt_id; shinyjs::runjs("document.getElementById('act_3').scrollIntoView({behavior: 'smooth'});") }
      }
    }, error = function(e) { return(NULL) })
  })
  
  output$detail_ui <- renderUI({
    apt_id <- clicks_store$apartment
    if (is.null(apt_id)) return(div(class = "placeholder-msg", icon("hand-pointer"), "Requires completing ACT II: click a marker on the map to generate the audit."))
    
    apt <- df_master %>% filter(id == apt_id)
    val_model <- district_model_precomputed()
    est_value <- apply_valuation(val_model, apt)
    
    # Compute deviations in PLN and relative percentages
    residual <- apt$price_per_m2 - est_value
    pct_diff <- (residual / est_value) * 100
    
    is_deal <- residual <= 0
    color <- if_else(is_deal, "#10b981", "#ef4444")
    badge_bg <- if_else(is_deal, "#d1fae5", "#fee2e2")
    badge_text <- if_else(is_deal, "#065f46", "#991b1b")
    
    # Generate dynamic alerts based on deal status
    alert_text <- if_else(
      is_deal, 
      sprintf("Market price is LOWER than the algorithmic valuation by %s PLN/m²", format(round(abs(residual)), big.mark = " ")), 
      sprintf("Market price is HIGHER than the algorithmic valuation by %s PLN/m²", format(round(residual), big.mark = " "))
    )
    
    div(
      layout_columns(
        col_widths = c(4, 8),
        card(card_header(icon("file-contract"), " Property technical sheet"),
             tags$table(class = "table table-sm table-borderless", style = "font-size: 13px; margin-top:5px; color:#334155;",
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "District:")), tags$td(tags$b(apt$name))),
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "Price per m²:")), tags$td(tags$b(paste0(format(round(apt$price_per_m2), big.mark = " "), " PLN")))),
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "Total price:")), tags$td(paste0(format(round(apt$price), big.mark = " "), " PLN"))),
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "Specification:")), tags$td(sprintf("%.2f m² / %d rooms", apt$squareMeters, apt$rooms))),
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "Distance to center:")), tags$td(paste0(apt$centreDistance, " km"))),
                        tags$tr(tags$td(tags$span(style="color:#94a3b8;", "Estimated Value:")), tags$td(tags$span(style="color:#2563eb; font-weight:700;", paste0(format(round(est_value), big.mark = " "), " PLN/m²"))))
             ),
             
             # Main tile displaying the final verdict and percentage deviation
             div(style = paste0("border: 2px solid ", color, "; background-color: ", badge_bg, "; padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 10px;"),
                 div(style = paste0("font-size: 24px; font-weight: 900; color: ", badge_text, "; line-height: 1;"), 
                     sprintf("%s%.1f %%", if_else(is_deal, "-", "+"), abs(pct_diff))),
                 div(style = paste0("font-size: 12px; font-weight: 700; color: ", color, "; margin-top: 5px;"), alert_text)
             ),
             
             # Display model reliability metrics including District MAPE
             p(style = "font-size: 11px; color: #64748b; text-align: center; margin-bottom: 2px;", tags$b(paste("Algorithm:", val_model$type))),
             p(style = "font-size: 10px; color: #94a3b8; text-align: center; margin-top: 0;",
               sprintf("District Typical Error (MAPE): %.1f%% \u00b7 Avg Volatility: \u00b1%s PLN/m\u00b2",
                       val_model$metrics$mape, format(round(val_model$metrics$rmse), big.mark = " ")))
        ),
        card(card_header(icon("bullseye"), " Listing position vs. market"), layout_columns(col_widths = c(5, 7), plotlyOutput("densityPlot", height = "240px"), div(p(style="font-size:11px; color:#64748b; font-style:italic; margin-bottom:5px;", "Pick a similar listing from the table to inspect it:"), DTOutput("knnTable"))))
      ),
      
      # --- KNOWN LIMITATIONS WARNING BOX ---
      div(style = "background-color: #fffbeb; border: 1px solid #fde68a; border-left: 4px solid #f59e0b; color: #92400e; padding: 12px 18px; border-radius: 8px; font-size: 0.9rem; margin-top: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
          icon("exclamation-triangle", style = "color: #d97706; margin-right: 5px;"), 
          tags$b("Known Limitations - Subjective Quality Bias: ", style = "color: #b45309;"),
          tags$span(style = "color: #92400e;", "The model relies on tabulated metrics (area, floor, amenities) but does not employ Computer Vision to analyze listing photos. A property flagged as a 'Bargain' (e.g., -20% deviation) might require major renovation, while an 'Overpriced' unit might feature a premium interior design. The typical district MAPE often reflects this uncaptured interior quality and standard negotiation margins.")
      )
    )
  })
  
  output$densityPlot <- renderPlotly({
    req(clicks_store$apartment)
    apt <- df_master %>% filter(id == clicks_store$apartment)
    district_data <- filtered_data() %>% filter(name == apt$name)
    plot_ly() %>% add_histogram(data = district_data, x = ~price_per_m2, marker = list(color = "#cbd5e1", line = list(color = "white", width = 1)), name = "Market", hoverinfo = "x+y") %>%
      layout(shapes = list(list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = apt$price_per_m2, x1 = apt$price_per_m2, line = list(color = "#2563eb", width = 3, dash = "dash"))), xaxis = list(title = "Price per m²"), yaxis = list(title = "Number of listings"), showlegend = FALSE, margin = list(t=10, b=10))
  })
  
  output$knnTable <- renderDT({
    req(clicks_store$apartment)
    raw_neighbors <- knn_neighbors()
    req(nrow(raw_neighbors) > 0)
    actions <- sprintf('<button class="btn btn-xs btn-primary" style="font-size:10px; padding:4px 8px; background-color:#2563eb; border:none; border-radius:4px;" onclick="Shiny.setInputValue(\'current_knn_id\', %d, {priority: \'event\'})">Inspect</button>', raw_neighbors$id)
    
    # Formatted output for presentation
    neighbors_table <- raw_neighbors %>% 
      mutate(
        ` ` = actions, 
        `Price/m²` = paste0(format(round(price_per_m2), big.mark=" "), " PLN"), 
        `Area` = sprintf("%.2f m²", squareMeters), 
        `Distance` = sprintf("%.2f km", centreDistance)
      ) %>% 
      select(` `, `Price/m²`, `Area`, `Rooms` = rooms, `Distance`)
    
    datatable(neighbors_table, escape = FALSE, options = list(dom = 't', ordering = FALSE, pageLength = 5), rownames = FALSE, selection = 'none', class = 'cell-border stripe tight')
  })
  
  # Radar Chart visualizer utilizing Network Distances (_net)
  output$radar_ui <- renderUI({
    apt_id <- clicks_store$apartment
    if (is.null(apt_id)) return(div(class = "placeholder-msg", icon("map-signs"), "Requires completing ACT II: select a property from the map to load the spatial index."))
    
    apt <- df_master %>% filter(id == apt_id)
    max_dist <- max(c(apt$schoolDist_net, apt$clinicDist_net, apt$postOfficeDist_net, apt$kindergartenDist_net, apt$restaurantDist_net, apt$pharmacyDist_net), na.rm = TRUE)
    
    verdict_color <- if_else(max_dist <= 1.5, "#10b981", "#f59e0b")
    verdict_icon <- if_else(max_dist <= 1.5, "walking", "car")
    verdict_title <- if_else(max_dist <= 1.5, "Certified: 15-minute city!", "Note: commute required")
    verdict_text <- if_else(max_dist <= 1.5, "All key amenities are within a realistic 1.5 km walking network radius. You're car-independent!", "This unit may be cheaper, but some infrastructure points lie beyond the optimal real-walking radius (over 1.5 km over the street network).")
    
    layout_columns(
      col_widths = c(7, 5),
      card(card_header(icon("compass"), " Walking Network Distance (km)"), plotlyOutput("radarChart", height = "320px")),
      card(card_header(icon("gavel"), " Algorithm verdict"), div(style = "display: flex; flex-direction: column; justify-content: center; height: 100%; padding: 20px; text-align: center;", div(style = paste0("font-size: 45px; color: ", verdict_color, "; margin-bottom: 15px;"), icon(verdict_icon)), h4(verdict_title, style = paste0("color: ", verdict_color, "; font-weight: 800;")), p(verdict_text, style = "color: #475569; font-size: 15px; margin-top: 10px; line-height: 1.6;")))
    )
  })
  
  output$radarChart <- renderPlotly({
    req(clicks_store$apartment)
    apt <- df_master %>% filter(id == clicks_store$apartment)
    district_data <- filtered_data() %>% filter(name == apt$name)
    
    district_averages <- district_data %>% summarise(school = mean(schoolDist_net, na.rm=T), clinic = mean(clinicDist_net, na.rm=T), post_office = mean(postOfficeDist_net, na.rm=T), kindergarten = mean(kindergartenDist_net, na.rm=T), restaurant = mean(restaurantDist_net, na.rm=T), pharmacy = mean(pharmacyDist_net, na.rm=T)) %>% as.numeric()
    apartment_values <- c(apt$schoolDist_net, apt$clinicDist_net, apt$postOfficeDist_net, apt$kindergartenDist_net, apt$restaurantDist_net, apt$pharmacyDist_net)
    categories <- c('School', 'Clinic', 'Post Office', 'Kindergarten', 'Restaurant', 'Pharmacy')
    
    max_scale <- max(c(district_averages, apartment_values), na.rm=TRUE) + 0.2
    
    plot_ly(type = 'scatterpolar') %>%
      add_trace(
        r = c(district_averages, district_averages[1]), theta = c(categories, categories[1]),
        name = 'District Avg (Network)', fill = 'none', line = list(color = "#ef4444", width = 2.5), marker = list(color = "#ef4444", size=6),
        hovertemplate = "District average: <b>%{r:.2f} km</b><extra></extra>"
      ) %>%
      add_trace(
        r = c(apartment_values, apartment_values[1]), theta = c(categories, categories[1]),
        name = 'Your Selection', fill = 'toself', fillcolor = 'rgba(37, 99, 235, 0.4)', line = list(color = "#2563eb", width = 3), marker = list(color = "#2563eb", size=8),
        hovertemplate = "%{theta}: <b>%{r:.2f} km</b><extra></extra>"
      ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, showticklabels = FALSE, range = c(0, max_scale))),
        margin = list(t=30, b=20, l=40, r=40), legend = list(orientation = "h", x = 0.05, y = -0.15)
      )
  })
}

# -------------------------------------------------------------------------
# RUN APPLICATION
# -------------------------------------------------------------------------
shinyApp(ui = ui, server = server)