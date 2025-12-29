library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(DT)
library(tidyr)
library(purrr)
library(leaflet)
library(lubridate)

# wspolrzedne geograficzne dzielnic Poznania
quarter_coords <- tribble(
  ~quarter, ~lat, ~lng,
  "Centrum", 52.4080, 16.9320,
  "Stare Miasto", 52.4130, 16.9500,
  "Stary Rynek", 52.4065, 16.9380,
  "Garbary", 52.4180, 16.9520,
  "Chwaliszewo", 52.4040, 16.9450,
  "Ostrów Tumski", 52.4145, 16.9550,
  "Śródka", 52.4110, 16.9650,
  "Grunwald", 52.3980, 16.8750,
  "Łazarz", 52.3920, 16.8950,
  "Rynek Łazarski", 52.3900, 16.8900,
  "Górczyn", 52.3800, 16.8800,
  "Junikowo", 52.3720, 16.8550,
  "Marcelin", 52.3900, 16.8500,
  "Ławica", 52.4100, 16.8250,
  "Bonin", 52.4000, 16.8400,
  "Raszyn", 52.3850, 16.8650,
  "Jeżyce", 52.4220, 16.8950,
  "Rynek Jeżycki", 52.4200, 16.8900,
  "Ogrody", 52.4150, 16.8850,
  "Sołacz", 52.4320, 16.8950,
  "Winiary", 52.4400, 16.9000,
  "Podolany", 52.4280, 16.8700,
  "Strzeszyn", 52.4420, 16.8550,
  "Smochowice", 52.4520, 16.8650,
  "Wola", 52.4380, 16.8450,
  "Piątkowo", 52.4500, 16.9100,
  "Winogrady", 52.4380, 16.9350,
  "Naramowice", 52.4600, 16.9400,
  "Umultowo", 52.4700, 16.9300,
  "Morasko", 52.4800, 16.9150,
  "Rataje", 52.4000, 16.9800,
  "Chartowo", 52.4150, 17.0000,
  "Żegrze", 52.4250, 17.0200,
  "Malta", 52.4050, 17.0100,
  "Nowe Miasto", 52.4200, 16.9900,
  "Warszawskie", 52.4300, 16.9750,
  "Główna", 52.4350, 16.9650,
  "Antoninek", 52.3900, 17.0100,
  "Wilda", 52.3880, 16.9250,
  "Rynek Wildecki", 52.3860, 16.9200,
  "Górna Wilda", 52.3820, 16.9350,
  "Dolna Wilda", 52.3920, 16.9150,
  "Dębiec", 52.3720, 16.9100,
  "Świerczewo", 52.3650, 16.9050,
  "Starołęka", 52.3650, 16.9600,
  "Zieliniec", 52.3700, 16.9400,
  "Wilczak", 52.4260, 16.9200,
  "Zawady", 52.4060, 16.9700,
  "Komandoria", 52.4020, 16.9620,
  "Fabianowo", 52.3600, 16.8450
)

# wczytaj dane
df <- read_excel("rent-poznan.xlsx")


df <- df %>%
  mutate(
    date_activ = as.Date(date_activ),
    year_month = floor_date(date_activ, "month"),
    price_per_m2 = round(price / flat_area, 2),
    rooms_cat = factor(
      case_when(flat_rooms == 1 ~ "1 room", flat_rooms == 2 ~ "2 rooms",
                flat_rooms == 3 ~ "3 rooms", TRUE ~ "4+ rooms"),
      levels = c("1 room", "2 rooms", "3 rooms", "4+ rooms")
    )
  )

quarters_sorted <- df %>% count(quarter, sort = TRUE) %>% pull(quarter)

amenity_cols <- c("flat_furnished", "flat_balcony", "flat_utility_room", "flat_garage",
                  "flat_basement", "flat_garden", "flat_tarrace", "flat_lift",
                  "flat_kitchen_sep", "flat_air_cond", "flat_washmachine", "flat_dishwasher",
                  "flat_fridge", "flat_cooker", "flat_internet", "flat_intercom")
amenity_labels <- c("Furnished", "Balcony", "Utility Room", "Garage",
                    "Basement", "Garden", "Terrace", "Elevator",
                    "Separate Kitchen", "Air Conditioning", "Washing Machine", "Dishwasher",
                    "Fridge", "Cooker", "Internet", "Intercom")

# kolory
uep <- list(
  primary = "#006633",
  secondary = "#008844",
  accent = "#4CAF50",
  light = "#E8F5E9",
  dark = "#004D26",
  text = "#2C3E50",
  muted = "#7F8C8D",
  white = "#FFFFFF",
  border = "#DEE2E6"
)

theme_app <- bs_theme(
  version = 5,
  bg = uep$white,
  fg = uep$text,
  primary = uep$primary,
  secondary = uep$secondary,
  success = uep$accent,
  base_font = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro"),
  "border-radius" = "4px",
  "card-border-radius" = "6px"
)

css <- paste0('
  .navbar { background: linear-gradient(90deg, ', uep$primary, ' 0%, ', uep$dark, ' 100%) !important; }
  .navbar-brand { font-weight: 600; font-size: 1.15rem; }
  
  .bslib-sidebar-layout > .sidebar {
    background-color: ', uep$light, ';
    border-right: 3px solid ', uep$primary, ';
  }
  .sidebar-title {
    color: ', uep$primary, ';
    font-weight: 700;
    font-size: 1rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    padding-bottom: 10px;
    margin-bottom: 15px;
    border-bottom: 2px solid ', uep$primary, ';
  }
  
  .card {
    border: 1px solid ', uep$border, ';
    box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    margin-bottom: 20px;
  }
  .card-header {
    background: ', uep$light, ';
    color: ', uep$primary, ';
    font-weight: 600;
    font-size: 0.95rem;
    border-bottom: 2px solid ', uep$primary, ';
    padding: 12px 16px;
  }
  .card-body { padding: 16px; }
  
  .stat-card {
    background: linear-gradient(135deg, ', uep$primary, ' 0%, ', uep$secondary, ' 100%);
    color: white;
    border-radius: 6px;
    padding: 20px;
    text-align: center;
    height: 100%;
  }
  .stat-value { font-size: 1.75rem; font-weight: 700; margin-bottom: 4px; }
  .stat-label { font-size: 0.8rem; text-transform: uppercase; letter-spacing: 1px; opacity: 0.9; }
  
  .nav-pills .nav-link {
    color: ', uep$primary, ';
    font-weight: 500;
    border-radius: 0;
    padding: 10px 18px;
    border-bottom: 3px solid transparent;
    margin-right: 5px;
  }
  .nav-pills .nav-link:hover { background: ', uep$light, '; border-bottom-color: ', uep$accent, '; }
  .nav-pills .nav-link.active { background: ', uep$primary, '; color: white; }
  
  .btn-primary { background: ', uep$primary, '; border-color: ', uep$primary, '; }
  .btn-primary:hover { background: ', uep$dark, '; border-color: ', uep$dark, '; }
  .btn-outline-secondary { color: ', uep$primary, '; border-color: ', uep$primary, '; }
  .btn-outline-secondary:hover { background: ', uep$primary, '; color: white; }
  
  .leaflet-container { border-radius: 6px; }
  .dataTables_wrapper { font-size: 0.9rem; }
  
  .tab-content { padding: 20px 0; }
  .bslib-page-navbar .tab-pane { padding-bottom: 40px; }
  .card { margin-bottom: 24px; }
  .card-body { padding: 20px; }
  
  .control-section {
    background: #f8f9fa;
    border-radius: 6px;
    padding: 12px;
    margin-bottom: 12px;
  }
  .control-section-title {
    font-size: 0.75rem;
    font-weight: 600;
    color: ', uep$muted, ';
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 8px;
    display: flex;
    align-items: center;
    gap: 6px;
  }
  .control-hint {
    font-size: 0.75rem;
    color: ', uep$muted, ';
    font-style: italic;
    margin-top: 4px;
  }
  
  .map-subtitle {
    font-size: 0.85rem;
    color: ', uep$muted, ';
    margin-bottom: 10px;
    padding: 8px 12px;
    background: #f8f9fa;
    border-radius: 4px;
    border-left: 3px solid ', uep$accent, ';
  }
  
  .table-highlight {
    background-color: ', uep$light, ' !important;
    font-weight: 600;
  }
  
  .chart-explanation {
    font-size: 0.8rem;
    color: ', uep$muted, ';
    padding: 8px;
    background: #fafafa;
    border-radius: 4px;
    margin-top: 8px;
  }
')




ui <- page_navbar(
  title = span(icon("building"), "Poznań Rental Market"),
  theme = theme_app,
  bg = uep$primary,
  fillable = FALSE,  # FIXED: Allow vertical scrolling instead of squeezing
  
  tags$head(tags$style(HTML(css))),
  
  sidebar = sidebar(
    width = 280,
    div(class = "sidebar-title", icon("filter"), " Filters"),
    
    sliderInput("price_range", "Price (PLN)", min = 0, max = 10000, value = c(0, 6000), step = 100),
    sliderInput("area_range", "Area (m²)", min = 0, max = 200, value = c(0, 150), step = 5),
    
    checkboxGroupInput("rooms_filter", "Rooms",
                       choices = c("1 room", "2 rooms", "3 rooms", "4+ rooms"),
                       selected = c("1 room", "2 rooms", "3 rooms", "4+ rooms")),
    
    selectInput("quarter_filter", "Quarter", choices = c("All", quarters_sorted), selected = "All"),
    
    actionButton("reset_btn", "Reset Filters", icon = icon("undo"), class = "btn-outline-secondary w-100 mt-3"),
    
    hr(),
    div(style = "text-align: center; color: #666; font-size: 0.85rem;",
        textOutput("filter_info"))
  ),
  
  # Overview
  nav_panel(title = "Overview", icon = icon("chart-line"),
            layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
                           div(class = "stat-card", div(class = "stat-value", textOutput("s_count")), div(class = "stat-label", "Listings")),
                           div(class = "stat-card", div(class = "stat-value", textOutput("s_price")), div(class = "stat-label", "Avg Price")),
                           div(class = "stat-card", div(class = "stat-value", textOutput("s_area")), div(class = "stat-label", "Avg Area")),
                           div(class = "stat-card", div(class = "stat-value", textOutput("s_m2")), div(class = "stat-label", "Price/m²"))
            ),
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Price Distribution"), card_body(plotlyOutput("hist_price", height = "320px"))),
                           card(card_header("Monthly Trend"), card_body(plotlyOutput("trend_monthly", height = "320px")))
            ),
            layout_columns(col_widths = c(4, 8),
                           card(card_header("Room Distribution"), card_body(plotlyOutput("pie_rooms", height = "320px"))),
                           card(card_header("Top Quarters"), card_body(plotlyOutput("bar_quarters", height = "320px")))
            )
  ),
  
  # Price Analysis
  nav_panel(title = "Price Analysis", icon = icon("money-bill"),
            layout_columns(col_widths = c(8, 4),
                           card(card_header("Price vs Area"), card_body(plotlyOutput("scatter_main", height = "450px"))),
                           card(card_header("Settings"), card_body(
                             checkboxInput("show_trend", "Show trend lines", TRUE),
                             sliderInput("point_alpha", "Transparency", 0.1, 1, 0.4, 0.1),
                             hr(),
                             h6("Summary"),
                             verbatimTextOutput("summary_stats")
                           ))
            ),
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Price by Rooms"), card_body(plotlyOutput("box_price", height = "320px"))),
                           card(card_header("Price/m² by Rooms"), card_body(plotlyOutput("box_m2", height = "320px")))
            )
  ),
  
  # Location Tab
  nav_panel(title = "Location", icon = icon("map-marker-alt"),
            # Map Controls - IMPROVED with grouped sections and hints
            card(
              card_header(icon("sliders-h"), " Map Controls"),
              card_body(
                layout_columns(col_widths = c(4, 4, 4),
                               # Visual Encoding section
                               div(class = "control-section",
                                   div(class = "control-section-title", icon("palette"), "Visual Encoding"),
                                   selectInput("map_metric", NULL,
                                               choices = c("Average Price (PLN)" = "avg_price", 
                                                           "Median Price (PLN)" = "med_price",
                                                           "Price per m² (PLN)" = "price_m2", 
                                                           "Number of Listings" = "count")),
                                   div(class = "control-hint", "Circle color reflects this metric")
                               ),
                               # Data section
                               div(class = "control-section",
                                   div(class = "control-section-title", icon("database"), "Data Selection"),
                                   sliderInput("map_n", "Quarters to display", 15, 50, 35, step = 5),
                                   div(class = "control-hint", "Top N quarters by listing count")
                               ),
                               # Display section
                               div(class = "control-section",
                                   div(class = "control-section-title", icon("layer-group"), "Map Display"),
                                   selectInput("map_tiles", NULL,
                                               choices = c("Light (recommended)" = "CartoDB.Positron", 
                                                           "Street Map" = "OpenStreetMap", 
                                                           "Dark Mode" = "CartoDB.DarkMatter")),
                                   checkboxInput("map_clustering", "Group nearby markers", FALSE),
                                   div(class = "control-hint", "Enable clustering for dense areas")
                               )
                )
              )
            ),
            
            # Map and Statistics
            layout_columns(col_widths = c(8, 4),
                           card(
                             card_header(icon("map"), " Poznań Neighborhoods"),
                             card_body(
                               # NEW: Map explanation subtitle
                               div(class = "map-subtitle",
                                   icon("info-circle"), 
                                   " Circle ", strong("size"), " = number of listings | Circle ", 
                                   strong("color"), " = selected metric | ", 
                                   em("Click markers for details")),
                               leafletOutput("map_main", height = "520px")
                             )
                           ),
                           card(
                             card_header(icon("table"), " Quarter Statistics"),
                             card_body(
                               # NEW: Table header explanation
                               div(style = "font-size: 0.8rem; color: #666; margin-bottom: 10px;",
                                   strong("N"), " = listings | ", strong("Avg"), " = mean price (PLN) | ",
                                   strong("PLN/m²"), " = price per square meter"),
                               DTOutput("tbl_quarters", height = "450px"),
                               div(class = "control-hint", style = "margin-top: 8px;",
                                   icon("mouse-pointer"), " Click a row to highlight on map")
                             )
                           )
            ),
            
            # Price by Quarter Chart - IMPROVED
            card(
              card_header(
                div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                    span(icon("chart-bar"), " Price Distribution by Quarter"),
                    # NEW: Chart type toggle
                    div(style = "display: flex; gap: 8px; align-items: center;",
                        span(style = "font-size: 0.8rem; color: #666;", "Chart type:"),
                        radioButtons("chart_type", NULL, 
                                     choices = c("Boxplot" = "box", "Violin" = "violin", "Bar (Median)" = "bar"),
                                     selected = "box", inline = TRUE)
                    )
                )
              ),
              card_body(
                plotlyOutput("chart_quarters", height = "380px"),
                # NEW: Chart explanation
                div(class = "chart-explanation",
                    icon("question-circle"), " ",
                    uiOutput("chart_explanation_text"))
              )
            )
  ),
  
  # Amenities
  nav_panel(title = "Amenities", icon = icon("home"),
            layout_columns(col_widths = c(6, 6),
                           card(card_header("Amenity Prevalence"), card_body(plotlyOutput("bar_amenities", height = "480px"))),
                           card(card_header("Price Impact"), card_body(
                             selectInput("sel_amenity", "Select amenity", choices = setNames(amenity_cols, amenity_labels)),
                             plotlyOutput("box_amenity", height = "380px")
                           ))
            ),
            card(card_header("Price Premium (% difference)"), card_body(plotlyOutput("bar_premium", height = "420px")))
  ),
  
  # Data
  nav_panel(title = "Data Explorer", icon = icon("database"),
            card(card_header(
              div(style = "display: flex; justify-content: space-between; align-items: center;",
                  span(icon("table"), " Dataset"),
                  downloadButton("dl_btn", "Download CSV", class = "btn-primary btn-sm"))
            ), card_body(DTOutput("tbl_data")))
  )
)

# Server
server <- function(input, output, session) {
  
  selected_quarter <- reactiveVal(NULL)
  
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "price_range", value = c(0, 6000))
    updateSliderInput(session, "area_range", value = c(0, 150))
    updateCheckboxGroupInput(session, "rooms_filter", selected = c("1 room", "2 rooms", "3 rooms", "4+ rooms"))
    updateSelectInput(session, "quarter_filter", selected = "All")
    selected_quarter(NULL)
  })
  
  fdata <- reactive({
    d <- df %>%
      filter(price >= input$price_range[1], price <= input$price_range[2],
             flat_area >= input$area_range[1], flat_area <= input$area_range[2],
             rooms_cat %in% input$rooms_filter)
    if (input$quarter_filter != "All") d <- d %>% filter(quarter == input$quarter_filter)
    d
  })
  
  # Stats
  output$s_count <- renderText(format(nrow(fdata()), big.mark = ","))
  output$s_price <- renderText(paste0(format(round(mean(fdata()$price)), big.mark = ","), " zł"))
  output$s_area <- renderText(paste0(round(mean(fdata()$flat_area), 1), " m²"))
  output$s_m2 <- renderText(paste0(round(mean(fdata()$price_per_m2), 1), " zł"))
  output$filter_info <- renderText(paste(format(nrow(fdata()), big.mark = ","), "of", format(nrow(df), big.mark = ","), "listings"))
  
  # Overview plots
  output$hist_price <- renderPlotly({
    plot_ly(fdata(), x = ~price, type = "histogram", nbinsx = 50,
            marker = list(color = uep$primary, line = list(color = "white", width = 0.5))) %>%
      layout(xaxis = list(title = "Price (PLN)", range = c(0, 6000)), yaxis = list(title = "Count"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$trend_monthly <- renderPlotly({
    fdata() %>% count(year_month) %>% filter(!is.na(year_month)) %>%
      plot_ly(x = ~year_month, y = ~n, type = "scatter", mode = "lines+markers",
              line = list(color = uep$primary, width = 2), marker = list(color = uep$primary, size = 5)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Listings"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$pie_rooms <- renderPlotly({
    fdata() %>% count(rooms_cat) %>%
      plot_ly(labels = ~rooms_cat, values = ~n, type = "pie",
              marker = list(colors = c(uep$primary, uep$secondary, uep$accent, "#81C784")),
              textinfo = "label+percent") %>%
      layout(showlegend = FALSE, paper_bgcolor = "transparent")
  })
  
  output$bar_quarters <- renderPlotly({
    fdata() %>% count(quarter, sort = TRUE) %>% head(10) %>%
      mutate(quarter = factor(quarter, levels = rev(quarter))) %>%
      plot_ly(y = ~quarter, x = ~n, type = "bar", orientation = "h",
              marker = list(color = uep$primary)) %>%
      layout(xaxis = list(title = "Listings"), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  # Price Analysis
  output$scatter_main <- renderPlotly({
    samp <- fdata() %>% filter(flat_area <= 150, price <= 6000) %>% sample_n(min(nrow(.), 2500))
    cols <- c("1 room" = uep$primary, "2 rooms" = uep$secondary, "3 rooms" = uep$accent, "4+ rooms" = "#FFA000")
    
    p <- plot_ly(samp, x = ~flat_area, y = ~price, color = ~rooms_cat, colors = cols,
                 type = "scatter", mode = "markers", marker = list(opacity = input$point_alpha, size = 7),
                 text = ~paste("Area:", flat_area, "m²<br>Price:", price, "PLN"))
    
    if (input$show_trend) {
      for (rm in unique(samp$rooms_cat)) {
        sub <- samp %>% filter(rooms_cat == rm)
        if (nrow(sub) > 10) {
          fit <- lm(price ~ flat_area, data = sub)
          p <- p %>% add_lines(x = sub$flat_area, y = fitted(fit), line = list(color = cols[rm], width = 2),
                               showlegend = FALSE, inherit = FALSE)
        }
      }
    }
    p %>% layout(xaxis = list(title = "Area (m²)"), yaxis = list(title = "Price (PLN)"),
                 paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$summary_stats <- renderPrint({
    d <- fdata()
    cat("N:", format(nrow(d), big.mark = ","), "\n")
    cat("Price:", format(min(d$price), big.mark = ","), "-", format(max(d$price), big.mark = ","), "PLN\n")
    cat("Median:", format(median(d$price), big.mark = ","), "PLN\n")
    cat("r:", round(cor(d$price, d$flat_area), 3))
  })
  
  output$box_price <- renderPlotly({
    plot_ly(fdata() %>% filter(price <= 5000), x = ~rooms_cat, y = ~price, color = ~rooms_cat, type = "box",
            colors = c(uep$primary, uep$secondary, uep$accent, "#81C784")) %>%
      layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = "Price (PLN)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$box_m2 <- renderPlotly({
    plot_ly(fdata() %>% filter(price_per_m2 <= 80), x = ~rooms_cat, y = ~price_per_m2, color = ~rooms_cat, type = "box",
            colors = c(uep$primary, uep$secondary, uep$accent, "#81C784")) %>%
      layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = "Price/m² (PLN)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  
  # LOCATION TAB
  
  qstats <- reactive({
    fdata() %>%
      group_by(quarter) %>%
      summarise(
        count = n(), 
        avg_price = round(mean(price)), 
        med_price = round(median(price)),
        price_m2 = round(mean(price_per_m2), 1),
        min_price = min(price),
        max_price = max(price),
        .groups = "drop"
      ) %>%
      arrange(desc(count)) %>% 
      head(input$map_n) %>%
      left_join(quarter_coords, by = "quarter") %>% 
      filter(!is.na(lat))
  })
  
  # IMPROVED: Better contrast color palette
  output$map_main <- renderLeaflet({
    qs <- qstats()
    met <- input$map_metric
    
    # IMPROVED: Higher contrast color palette with darker mid-tones
    pal <- colorNumeric(
      palette = c("#C8E6C9", "#66BB6A", "#2E7D32", "#1B5E20", uep$dark),
      domain = qs[[met]]
    )
    
    lbl <- switch(met, 
                  "avg_price" = "Avg Price", 
                  "med_price" = "Median", 
                  "price_m2" = "PLN/m²", 
                  "count" = "Listings")
    
    map <- leaflet(qs) %>%
      addProviderTiles(input$map_tiles) %>%
      setView(lng = 16.925, lat = 52.41, zoom = 12)
    
    if (input$map_clustering) {
      map <- map %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = ~sqrt(count) * 2,
          color = ~pal(qs[[met]]),
          fillColor = ~pal(qs[[met]]),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          popup = ~paste0(
            "<div style='min-width: 180px;'>",
            "<h4 style='margin: 0 0 8px 0; color: ", uep$primary, ";'>", quarter, "</h4>",
            "<hr style='margin: 5px 0; border-color: #ddd;'>",
            "<table style='width: 100%; font-size: 13px;'>",
            "<tr><td><b>Listings:</b></td><td style='text-align: right;'>", format(count, big.mark = ","), "</td></tr>",
            "<tr><td><b>Avg Price:</b></td><td style='text-align: right;'>", format(avg_price, big.mark = ","), " PLN</td></tr>",
            "<tr><td><b>Median:</b></td><td style='text-align: right;'>", format(med_price, big.mark = ","), " PLN</td></tr>",
            "<tr><td><b>PLN/m²:</b></td><td style='text-align: right;'>", price_m2, "</td></tr>",
            "<tr><td><b>Range:</b></td><td style='text-align: right;'>", format(min_price, big.mark = ","), " - ", format(max_price, big.mark = ","), "</td></tr>",
            "</table></div>"
          ),
          label = ~paste(quarter, "-", lbl, ":", format(round(qs[[met]]), big.mark = ",")),
          layerId = ~quarter,
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS("
              function(cluster) {
                var count = cluster.getChildCount();
                var size = count < 10 ? 'small' : count < 20 ? 'medium' : 'large';
                return L.divIcon({
                  html: '<div style=\"background: #006633; color: white; border-radius: 50%; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center; font-weight: bold;\">' + count + '</div>',
                  className: 'marker-cluster',
                  iconSize: L.point(40, 40)
                });
              }
            ")
          )
        )
    } else {
      map <- map %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = ~sqrt(count) * 2,
          color = ~pal(qs[[met]]),
          fillColor = ~pal(qs[[met]]),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          popup = ~paste0(
            "<div style='min-width: 180px;'>",
            "<h4 style='margin: 0 0 8px 0; color: ", uep$primary, ";'>", quarter, "</h4>",
            "<hr style='margin: 5px 0; border-color: #ddd;'>",
            "<table style='width: 100%; font-size: 13px;'>",
            "<tr><td><b>Listings:</b></td><td style='text-align: right;'>", format(count, big.mark = ","), "</td></tr>",
            "<tr><td><b>Avg Price:</b></td><td style='text-align: right;'>", format(avg_price, big.mark = ","), " PLN</td></tr>",
            "<tr><td><b>Median:</b></td><td style='text-align: right;'>", format(med_price, big.mark = ","), " PLN</td></tr>",
            "<tr><td><b>PLN/m²:</b></td><td style='text-align: right;'>", price_m2, "</td></tr>",
            "<tr><td><b>Range:</b></td><td style='text-align: right;'>", format(min_price, big.mark = ","), " - ", format(max_price, big.mark = ","), "</td></tr>",
            "</table></div>"
          ),
          label = ~paste(quarter, "-", lbl, ":", format(round(qs[[met]]), big.mark = ",")),
          layerId = ~quarter
        )
    }
    
    map %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = qs[[met]], 
        title = lbl,
        opacity = 0.9,
        labFormat = labelFormat(big.mark = ",")
      )
  })
  
  observeEvent(input$map_main_marker_click, {
    click <- input$map_main_marker_click
    if (!is.null(click$id)) {
      selected_quarter(click$id)
    }
  })

  output$tbl_quarters <- renderDT({
    qs <- qstats() %>% 
      select(Quarter = quarter, N = count, Avg = avg_price, `PLN/m²` = price_m2)
    
    datatable(
      qs,
      selection = "single",
      options = list(
        pageLength = 12, 
        dom = "tp", 
        scrollY = "420px",
        columnDefs = list(
          list(className = 'dt-right', targets = 1:3)
        )
      ),
      rownames = FALSE,
      callback = JS("
        table.on('click', 'tr', function() {
          var data = table.row(this).data();
          if (data) {
            Shiny.setInputValue('table_quarter_click', data[0], {priority: 'event'});
          }
        });
      ")
    ) %>%
      formatStyle(
        'Avg',
        background = styleColorBar(range(qs$Avg), uep$light),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'PLN/m²',
        color = styleInterval(
          c(35, 40, 45),
          c(uep$accent, uep$secondary, uep$primary, uep$dark)
        ),
        fontWeight = 'bold'
      )
  })
  
  # poprawic mape
  observeEvent(input$table_quarter_click, {
    selected_quarter(input$table_quarter_click)
    qs <- qstats()
    sel <- qs %>% filter(quarter == input$table_quarter_click)
    if (nrow(sel) > 0) {
      leafletProxy("map_main") %>%
        setView(lng = sel$lng, lat = sel$lat, zoom = 14)
    }
  })
  
  # Price by Quarter chart with multiple types
  output$chart_quarters <- renderPlotly({
    topq <- fdata() %>% count(quarter, sort = TRUE) %>% head(10) %>% pull(quarter)
    plot_data <- fdata() %>% 
      filter(quarter %in% topq, price <= 5000) %>%
      mutate(quarter = factor(quarter, levels = topq))
    
    # Highlight selected quarter
    sel_q <- selected_quarter()
    colors <- ifelse(topq == sel_q, "#FFA000", uep$primary)
    
    if (input$chart_type == "box") {
      # Boxplot
      p <- plot_ly(plot_data, x = ~quarter, y = ~price, type = "box",
                   marker = list(color = uep$primary),
                   line = list(color = uep$primary),
                   fillcolor = "rgba(0,102,51,0.3)")
    } else if (input$chart_type == "violin") {
      # Violin plot
      p <- plot_ly(plot_data, x = ~quarter, y = ~price, type = "violin",
                   box = list(visible = TRUE),
                   meanline = list(visible = TRUE),
                   fillcolor = "rgba(0,102,51,0.3)",
                   line = list(color = uep$primary))
    } else {
      # Bar chart (median with error bars for IQR)
      summary_data <- plot_data %>%
        group_by(quarter) %>%
        summarise(
          median = median(price),
          q1 = quantile(price, 0.25),
          q3 = quantile(price, 0.75),
          .groups = "drop"
        )
      
      p <- plot_ly(summary_data, x = ~quarter, y = ~median, type = "bar",
                   marker = list(color = uep$primary),
                   error_y = list(
                     type = "data",
                     symmetric = FALSE,
                     array = ~q3 - median,
                     arrayminus = ~median - q1,
                     color = uep$dark
                   ))
    }
    
    p %>% layout(
      xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11)),
      yaxis = list(title = "Price (PLN)"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      margin = list(b = 100)
    )
  })
  
  # Dynamic chart explanation
  output$chart_explanation_text <- renderUI({
    if (input$chart_type == "box") {
      span(strong("Boxplot:"), " The box shows the middle 50% of prices (IQR). ",
           "The line inside is the median. Whiskers extend to 1.5× IQR. ",
           "Points beyond are outliers.")
    } else if (input$chart_type == "violin") {
      span(strong("Violin plot:"), " Width shows price distribution density. ",
           "Wider sections = more listings at that price. ",
           "The box inside shows median and IQR.")
    } else {
      span(strong("Bar chart:"), " Bar height = median price. ",
           "Error bars show interquartile range (25th to 75th percentile).")
    }
  })

  
  # AMENITIES TAB
  
  output$bar_amenities <- renderPlotly({
    fdata() %>%
      summarise(across(all_of(amenity_cols), ~ mean(., na.rm = TRUE) * 100)) %>%
      pivot_longer(everything(), names_to = "am", values_to = "pct") %>%
      mutate(label = amenity_labels[match(am, amenity_cols)]) %>%
      arrange(desc(pct)) %>% mutate(label = factor(label, levels = rev(label))) %>%
      plot_ly(y = ~label, x = ~pct, type = "bar", orientation = "h",
              marker = list(color = ~pct, colorscale = list(c(0, "#FFF8E1"), c(1, "#FF8F00")))) %>%
      layout(xaxis = list(title = "% of Listings"), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$box_amenity <- renderPlotly({
    am <- input$sel_amenity
    lab <- amenity_labels[match(am, amenity_cols)]
    fdata() %>% filter(price <= 5000) %>%
      mutate(has = ifelse(.data[[am]], paste("With", lab), paste("Without", lab))) %>%
      plot_ly(x = ~has, y = ~price, color = ~has, type = "box",
              colors = c("#FFCDD2", uep$primary)) %>%
      layout(showlegend = FALSE, xaxis = list(title = ""), yaxis = list(title = "Price (PLN)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })
  
  output$bar_premium <- renderPlotly({
    map_dfr(seq_along(amenity_cols), function(i) {
      with_am <- fdata() %>% filter(.data[[amenity_cols[i]]]) %>% pull(price) %>% median(na.rm = TRUE)
      without_am <- fdata() %>% filter(!.data[[amenity_cols[i]]]) %>% pull(price) %>% median(na.rm = TRUE)
      data.frame(am = amenity_labels[i], prem = round((with_am - without_am) / without_am * 100, 1))
    }) %>% arrange(desc(prem)) %>% mutate(am = factor(am, levels = rev(am)), col = prem >= 0) %>%
      plot_ly(y = ~am, x = ~prem, type = "bar", orientation = "h",
              marker = list(color = ifelse(.$col, uep$primary, "#E57373"))) %>%
      layout(xaxis = list(title = "Premium (%)", zeroline = TRUE), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = length(amenity_cols) - 0.5,
                                line = list(color = "gray", dash = "dash"))))
  })
  
  # Data
  output$tbl_data <- renderDT({
    fdata() %>%
      select(quarter, price, flat_area, flat_rooms, price_per_m2, flat_furnished, flat_balcony, flat_lift, flat_garage, date_activ) %>%
      rename(Quarter = quarter, Price = price, Area = flat_area, Rooms = flat_rooms, `PLN/m²` = price_per_m2,
             Furnished = flat_furnished, Balcony = flat_balcony, Elevator = flat_lift, Garage = flat_garage, Listed = date_activ) %>%
      datatable(filter = "top", options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$dl_btn <- downloadHandler(
    filename = function() paste0("poznan_rentals_", Sys.Date(), ".csv"),
    content = function(file) write.csv(fdata(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)