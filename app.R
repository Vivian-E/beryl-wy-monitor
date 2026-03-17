# Libraries
library(rsconnect)
library(pdftools)
library(rvest)
library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(gbfs)
library(leaflet)
library(DT)
library(plotly)
library(geojsonsf)
library(sf)

# --- 1. CONFIG & TARGET DATA ---

get_targets <- function() {
  targets <- tibble(
    city = c("Leeds", "Bradford"),
    target_bikes = c(655, 200),
    target_stations = c(116, 50)
  )
  try({
    # Bradford Scrape
    url_brads <- "https://zagdaily.com/trends/lner-beryl-bikes-expand-to-bradford/"
    html_b <- read_html(url_brads) %>% html_text()
    targets$target_bikes[targets$city == "Bradford"] <- as.numeric(str_extract(html_b, "(\\d+)(?=\\s+e-bikes)"))
    targets$target_stations[targets$city == "Bradford"] <- as.numeric(str_extract(html_b, "(\\d+)(?=\\s+docking stations)"))
    
    # Leeds Scrape
    pdf_url <- "https://westyorkshire.moderngov.co.uk/documents/s39064/IP5%20Leeds%20City%20Bikes.pdf"
    pdf_text <- suppressMessages(pdftools::pdf_text(pdf_url)) %>% paste(collapse = " ")
    targets$target_bikes[targets$city == "Leeds"] <- as.numeric(str_extract(pdf_text, "(\\d+)(?=\\s+e-bikes)"))
    targets$target_stations[targets$city == "Leeds"] <- as.numeric(str_extract(pdf_text, "(\\d+)(?=\\s+docking stations)"))
  }, silent = TRUE)
  return(targets)
}

TARGET_DATA <- get_targets()

legend_palette <- c(
  "Empty" = "#d73027",
  "Low (1-4)" = "#fc8d59",
  "Good (5+)" = "#1a9850",
  "Inactive" = "#999999"
)

# --- 2. UI DESIGN ---
ui <- page_navbar(
  title = "Beryl West Yorkshire Monitor",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  sidebar = sidebar(
    title = "Dashboard Controls",
    checkboxGroupInput("city_select", "Regions", 
                       choices = c("Leeds", "Bradford"), 
                       selected = c("Leeds", "Bradford")),
    hr(),
    actionButton("refresh", "Refresh Live Data", icon = icon("sync"), class = "btn-sm btn-primary"),
    
    span(style="font-size: 0.8rem; color: #444; font-weight: bold;", 
         textOutput("last_updated_txt")),
    
    hr(),
    tags$div(style="font-size: 0.75rem; color: #666;",
             tags$b("Reference Sources for Targets:"),
             tags$br(), tags$a(href="https://zagdaily.com/trends/lner-beryl-bikes-expand-to-bradford/", "Bradford", target="_blank"),
             tags$br(), tags$a(href="https://westyorkshire.moderngov.co.uk/documents/s39064/IP5%20Leeds%20City%20Bikes.pdf", "Leeds", target="_blank"),
             tags$p(style="margin-top:10px;", "Zone boundaries shown as dashed outlines.")
    )
  ),
  
  nav_panel("Live Map",
            layout_column_wrap(
              width = 1/3,
              value_box(
                title = "Active Bikes Available",
                value = textOutput("total_bikes"),
                showcase = bs_icon("bicycle"),
                theme = "primary",
                p(textOutput("bike_breakdown"))
              ),
              value_box(
                title = "Operational Stations",
                value = textOutput("active_stations"),
                showcase = bs_icon("geo-alt-fill"),
                theme = "info",
                p(textOutput("station_breakdown"))
              ),
              value_box(
                title = "Functional Rollout Progress",
                value = textOutput("target_pct"),
                showcase = bs_icon("rocket-takeoff"),
                theme = "success",
                p("Vs planned infrastructure targets")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("Fleet Distribution & Service Zones"),
              leafletOutput("map")
            )
  ),
  
  nav_panel("Bikes Analysis",
            layout_columns(
              card(card_header("Bike Density"), plotlyOutput("density_plot")),
              card(card_header("Current Stock vs Targets"), plotlyOutput("stock_bar"))
            ),
            card(
              card_header("Station Statistics (Sorted by Bikes Available)"), 
              DTOutput("analysis_table")
            )
  ),
  
  nav_panel("Historical Trends",
            layout_columns(
              card(
                card_header("Total Bike Availability Over Time"),
                plotlyOutput("history_plot")
              ),
              card(
                card_header("Rollout Progress (Active Stations)"),
                plotlyOutput("station_history_plot")
              )
            )
  )
) 

# --- 3. SERVER LOGIC ---

server <- function(input, output, session) {
  
  # Reactive Data Fetching
  live_data_bundle <- reactivePoll(120000, session,
                                   checkFunc = function() { Sys.time() },
                                   valueFunc = function() {
                                     fetch_city_all <- function(slug, label) {
                                       tryCatch({
                                         base <- paste0("https://beryl-gbfs-production.web.app/v2_2/", slug, "/")
                                         info <- suppressMessages(gbfs::get_station_information(paste0(base, "station_information.json")))
                                         status <- suppressMessages(gbfs::get_station_status(paste0(base, "station_status.json")))
                                         
                                         stations <- info %>%
                                           left_join(status %>% select(station_id, num_bikes_available, num_docks_available, is_renting), 
                                                     by = "station_id") %>%
                                           filter(!str_detect(toupper(name), "DEPOT|TEMP|TEMPORARY")) %>%
                                           mutate(city = label, 
                                                  is_active = as.logical(is_renting),
                                                  bike_color = case_when(
                                                    !is_active ~ "#999999",
                                                    num_bikes_available == 0 ~ "#d73027",
                                                    num_bikes_available < 5 ~ "#fc8d59",
                                                    TRUE ~ "#1a9850"
                                                  ))
                                         
                                         geo_url <- paste0(base, "geofencing_zones.json")
                                         geo_res <- jsonlite::fromJSON(geo_url)
                                         geo_sf <- geojsonsf::geojson_sf(jsonlite::toJSON(geo_res$data$geofencing_zones, auto_unbox = TRUE)) %>%
                                           mutate(city = label)
                                         
                                         list(stations = stations, geofences = geo_sf)
                                       }, error = function(e) return(NULL))
                                     }
                                     
                                     l_data <- fetch_city_all("Leeds", "Leeds")
                                     b_data <- fetch_city_all("Bradford", "Bradford")
                                     
                                     list(
                                       stations = bind_rows(l_data$stations, b_data$stations),
                                       geofences = bind_rows(l_data$geofences, b_data$geofences),
                                       time = Sys.time()
                                     )
                                   }
  )
  
  
  # Load the history file from GitHub (so it stays updated on the web)
  history_df <- reactive({
    # Replace YOUR_USERNAME and YOUR_REPO with your actual GitHub details
    url <- "https://raw.githubusercontent.com/Vivian-E/beryl-wy-monitor/main/data/beryl_history.csv"
    
    # Use tryCatch so the app doesn't crash if it can't reach GitHub
    tryCatch({
      read_csv(url)
    }, error = function(e) {
      # Fallback to local file if URL fails
      if(file.exists("data/beryl_history.csv")) read_csv("data/beryl_history.csv") else NULL
    })
  })
  
  # Reactives
  filtered_df <- reactive({ 
    req(live_data_bundle())
    live_data_bundle()$stations %>% filter(city %in% input$city_select) 
  })
  
  filtered_geo <- reactive({
    req(live_data_bundle())
    live_data_bundle()$geofences %>% filter(city %in% input$city_select)
  })
  
  output$last_updated_txt <- renderText({
    paste("Last updated:", format(live_data_bundle()$time, "%H:%M:%S"))
  })
  
  # KPIs
  output$total_bikes <- renderText({ sum(filtered_df()$num_bikes_available, na.rm=T) })
  output$bike_breakdown <- renderText({
    d <- filtered_df()
    paste0("Leeds: ", sum(d$num_bikes_available[d$city=="Leeds"], na.rm=T), " | Bradford: ", sum(d$num_bikes_available[d$city=="Bradford"], na.rm=T))
  })
  output$active_stations <- renderText({ sum(filtered_df()$is_active, na.rm=T) })
  output$station_breakdown <- renderText({
    d <- filtered_df()
    paste0("Leeds: ", sum(d$is_active & d$city=="Leeds"), " | Bradford: ", sum(d$is_active & d$city=="Bradford"))
  })
  output$target_pct <- renderText({
    targets <- TARGET_DATA %>% filter(city %in% input$city_select)
    total_planned <- sum(targets$target_stations)
    if(total_planned == 0) return("0%")
    paste0(round((sum(filtered_df()$is_active) / total_planned) * 100, 1), "%")
  })
  
  output$history_plot <- renderPlotly({
    req(history_df())
    p <- history_df() %>%
      filter(city %in% input$city_select) %>%
      ggplot(aes(x = date, y = total_bikes, color = city)) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_brewer(palette = "Set2") +
      labs(y = "Bikes Available", x = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$station_history_plot <- renderPlotly({
    req(history_df())
    p <- history_df() %>%
      filter(city %in% input$city_select) %>%
      ggplot(aes(x = date, y = active_stations, color = city)) +
      geom_step(size = 1) + # Use a step plot to show infrastructure changes
      scale_color_brewer(palette = "Set1") +
      labs(y = "Active Stations", x = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # --- Map ---
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(-1.68, 53.8, 11) %>%
      addLegend(position = "topright", colors = as.character(legend_palette), labels = names(legend_palette), title = "Station Status", opacity = 0.8)
  })
  
  observe({
    stations <- filtered_df(); geos <- filtered_geo()
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    if(nrow(geos) > 0) {
      proxy %>% addPolygons(data = geos, color = "#555", weight = 1.5, dashArray = "5, 10", fill = FALSE, opacity = 0.4)
    }
    active <- stations %>% filter(is_active); inactive <- stations %>% filter(!is_active)
    if(nrow(active) > 0) {
      proxy %>% addCircleMarkers(data = active, lng = ~lon, lat = ~lat, radius = ~sqrt(num_bikes_available + 5) * 2.5, 
                                 fillColor = ~bike_color, color = "white", weight = 1, fillOpacity = 0.8,
                                 label = ~paste0(name, ": ", num_bikes_available), popup = ~paste0("<b>", name, "</b>"))
    }
    if(nrow(inactive) > 0) {
      proxy %>% addCircleMarkers(data = inactive, lng = ~lon, lat = ~lat, radius = 6, fillColor = "#666", color = "white", weight = 1, fillOpacity = 0.5) %>%
        addLabelOnlyMarkers(data = inactive, lng = ~lon, lat = ~lat, label = "✕", labelOptions = labelOptions(noHide = T, direction = "center", textOnly = T, style = list("color" = "red", "font-weight" = "bold")))
    }
  })
  
  # --- Analysis Outputs ---
  output$density_plot <- renderPlotly({
    p <- filtered_df() %>% filter(is_active) %>%
      ggplot(aes(x = city, y = num_bikes_available, fill = city)) +
      geom_boxplot(alpha = 0.6) + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$stock_bar <- renderPlotly({
    targets <- TARGET_DATA %>% filter(city %in% input$city_select)
    curr <- filtered_df() %>% group_by(city) %>% summarise(Live = sum(num_bikes_available, na.rm=T)) %>%
      left_join(targets %>% select(city, Target = target_bikes), by = "city") %>% pivot_longer(cols = c(Live, Target))
    p <- ggplot(curr, aes(x = city, y = value, fill = name)) + geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = c("Live" = "#2ecc71", "Target" = "#bdc3c7")) + theme_minimal()
    ggplotly(p)
  })
  
  output$analysis_table <- renderDT({
    req(nrow(filtered_df()) > 0)
    
    # 1. Prepare the data using the direct 'capacity' column
    table_df <- filtered_df() %>%
      mutate(
        # Numeric decimal for sorting
        Utilization_val = ifelse(capacity > 0, num_bikes_available / capacity, 0)
      ) %>%
      select(
        City = city, 
        Name = name, 
        Bikes = num_bikes_available, 
        Capacity = capacity, 
        Renting = is_renting, 
        Utilisation = Utilization_val
      )
    
    # 2. Render the table
    datatable(
      table_df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        # Default sort: 3rd column (Bikes), index 2, descending
        order = list(list(2, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = 2:5)
        )
      )
    ) %>%
      # 3. Format 'Utilisation' as a percentage (numeric sorting remains intact)
      formatPercentage('Utilisation', 0)
  })
}

shinyApp(ui, server)