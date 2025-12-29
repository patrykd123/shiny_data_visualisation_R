library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(tidyr)
library(purrr)
library(leaflet)

df <- read_excel("rent-poznan.xlsx")

# Quarter coordinates (geocoded centroids)
quarter_coords <- tribble(
  ~quarter, ~lat, ~lng,
  "Grunwald", 52.4020, 16.8850,
  "Centrum", 52.4064, 16.9340,
  "Jeżyce", 52.4180, 16.9050,
  "Wilda", 52.3920, 16.9280,
  "Rataje", 52.4020, 16.9750,
  "Stare Miasto", 52.4100, 16.9450,
  "Piątkowo", 52.4450, 16.9150,
  "Winogrady", 52.4320, 16.9380,
  "Łazarz", 52.3980, 16.9050,
  "Naramowice", 52.4550, 16.9450,
  "Nowe Miasto", 52.4150, 16.9850,
  "Górczyn", 52.3850, 16.8900,
  "Sołacz", 52.4280, 16.9050,
  "Dębiec", 52.3780, 16.9150,
  "Ogrody", 52.4100, 16.8950,
  "Marcelin", 52.3950, 16.8650,
  "Malta", 52.4020, 17.0050,
  "Winiary", 52.4380, 16.9100,
  "Podolany", 52.4250, 16.8800,
  "Stary Rynek", 52.4085, 16.9340,
  "Górna Wilda", 52.3880, 16.9350,
  "Strzeszyn", 52.4380, 16.8650,
  "Garbary", 52.4150, 16.9500,
  "Chartowo", 52.4180, 16.9950,
  "Warszawskie", 52.4250, 16.9800,
  "Śródka", 52.4120, 16.9600,
  "Dolna Wilda", 52.3950, 16.9200,
  "Zawady", 52.4080, 16.9650,
  "Chwaliszewo", 52.4050, 16.9420,
  "Ławica", 52.4150, 16.8350
)

df <- df %>%
  mutate(
    price_per_m2 = round(price / flat_area, 2),
    rooms_cat = factor(
      case_when(flat_rooms == 1 ~ "1 room", flat_rooms == 2 ~ "2 rooms",
                flat_rooms == 3 ~ "3 rooms", TRUE ~ "4+ rooms"),
      levels = c("1 room", "2 rooms", "3 rooms", "4+ rooms")
    )
  )

quarters_sorted <- df %>% count(quarter, sort = TRUE) %>% pull(quarter)

amenity_cols <- c("flat_furnished", "flat_balcony", "flat_garage", "flat_lift",
                  "flat_basement", "flat_garden", "flat_tarrace", "flat_air_cond",
                  "flat_washmachine", "flat_dishwasher", "flat_fridge", "flat_internet")
amenity_labels <- c("Furnished", "Balcony", "Garage", "Elevator", "Basement", "Garden",
                    "Terrace", "Air Conditioning", "Washing Machine", "Dishwasher", "Fridge", "Internet")

colors <- c(primary = "#1B5E20", secondary = "#388E3C", accent = "#4CAF50", light = "#81C784")

ui <- fluidPage(
  titlePanel("Poznań Rental Market Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      sliderInput("price_range", "Price (PLN):", min = 0, max = 10000, value = c(0, 5000), step = 100),
      sliderInput("area_range", "Area (m²):", min = 0, max = 200, value = c(0, 150), step = 5),
      checkboxGroupInput("rooms_filter", "Rooms:",
                         choices = c("1 room", "2 rooms", "3 rooms", "4+ rooms"),
                         selected = c("1 room", "2 rooms", "3 rooms", "4+ rooms")),
      selectInput("quarter_filter", "Quarter:", choices = c("All", quarters_sorted), selected = "All"),
      hr(),
      textOutput("filter_summary")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Overview",
                 br(),
                 fluidRow(
                   column(3, wellPanel(h5("Listings"), h3(textOutput("stat_count")))),
                   column(3, wellPanel(h5("Avg Price"), h3(textOutput("stat_avg_price")))),
                   column(3, wellPanel(h5("Avg Area"), h3(textOutput("stat_avg_area")))),
                   column(3, wellPanel(h5("Price/m²"), h3(textOutput("stat_price_m2"))))
                 ),
                 fluidRow(
                   column(6, h4("Price Distribution"), plotOutput("price_histogram", height = "280px")),
                   column(6, h4("Room Distribution"), plotOutput("rooms_chart", height = "280px"))
                 )
        ),
        
        tabPanel("Price Analysis",
                 br(),
                 plotOutput("scatter_plot", height = "350px"),
                 hr(),
                 fluidRow(
                   column(6, plotOutput("price_by_rooms", height = "250px")),
                   column(6, plotOutput("price_m2_by_rooms", height = "250px"))
                 )
        ),
        
        # Updated Location Tab with Map
        tabPanel("Location",
                 br(),
                 fluidRow(
                   column(4, selectInput("map_metric", "Color markers by:",
                                         choices = c("Average Price" = "avg_price", "Price/m²" = "price_m2", "Listings" = "count"))),
                   column(4, sliderInput("map_top_n", "Show top N quarters:", min = 10, max = 30, value = 20))
                 ),
                 fluidRow(
                   column(8, 
                          h4("Interactive Map"),
                          leafletOutput("poznan_map", height = "450px")
                   ),
                   column(4,
                          h4("Quarter Statistics"),
                          DTOutput("quarter_stats")
                   )
                 ),
                 hr(),
                 h4("Price by Quarter"),
                 plotOutput("price_by_quarter_box", height = "280px")
        ),
        
        tabPanel("Amenities",
                 br(),
                 fluidRow(
                   column(6, h4("Amenity Prevalence"), plotOutput("amenity_prevalence", height = "350px")),
                   column(6, 
                          h4("Price Impact"),
                          selectInput("selected_amenity", "Amenity:", choices = setNames(amenity_cols, amenity_labels)),
                          plotOutput("amenity_impact", height = "280px"))
                 ),
                 hr(),
                 h4("Price Premium by Amenity"),
                 plotOutput("amenity_premium", height = "320px")
        ),
        
        tabPanel("Data",
                 br(),
                 downloadButton("download_btn", "Download CSV"),
                 br(), br(),
                 DTOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- df %>%
      filter(price >= input$price_range[1], price <= input$price_range[2],
             flat_area >= input$area_range[1], flat_area <= input$area_range[2],
             rooms_cat %in% input$rooms_filter)
    if (input$quarter_filter != "All") data <- data %>% filter(quarter == input$quarter_filter)
    data
  })
  
  output$stat_count <- renderText({ format(nrow(filtered_data()), big.mark = ",") })
  output$stat_avg_price <- renderText({ paste(format(round(mean(filtered_data()$price)), big.mark = ","), "PLN") })
  output$stat_avg_area <- renderText({ paste(round(mean(filtered_data()$flat_area), 1), "m²") })
  output$stat_price_m2 <- renderText({ paste(round(mean(filtered_data()$price_per_m2), 1), "PLN") })
  output$filter_summary <- renderText({ paste(nrow(filtered_data()), "listings") })
  
  # Overview plots
  output$price_histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = price)) +
      geom_histogram(bins = 40, fill = colors["primary"], color = "white") +
      scale_x_continuous(labels = comma_format(), limits = c(0, 6000)) +
      labs(x = "Price (PLN)", y = "Count") + theme_minimal()
  })
  
  output$rooms_chart <- renderPlot({
    filtered_data() %>% count(rooms_cat) %>%
      ggplot(aes(x = rooms_cat, y = n, fill = rooms_cat)) +
      geom_col() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], colors["accent"], colors["light"])) +
      labs(x = NULL, y = "Count") + theme_minimal() + theme(legend.position = "none")
  })
  
  # Price plots
  output$scatter_plot <- renderPlot({
    sample_data <- filtered_data() %>% filter(flat_area <= 150, price <= 6000) %>% sample_n(min(nrow(.), 2000))
    ggplot(sample_data, aes(x = flat_area, y = price, color = rooms_cat)) +
      geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = c(colors["primary"], colors["secondary"], colors["accent"], "#FF8F00")) +
      labs(x = "Area (m²)", y = "Price (PLN)") + theme_minimal() + theme(legend.position = "bottom")
  })
  
  output$price_by_rooms <- renderPlot({
    ggplot(filtered_data() %>% filter(price <= 5000), aes(x = rooms_cat, y = price, fill = rooms_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], colors["accent"], colors["light"])) +
      labs(x = NULL, y = "Price (PLN)") + theme_minimal() + theme(legend.position = "none")
  })
  
  output$price_m2_by_rooms <- renderPlot({
    ggplot(filtered_data() %>% filter(price_per_m2 <= 80), aes(x = rooms_cat, y = price_per_m2, fill = rooms_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], colors["accent"], colors["light"])) +
      labs(x = NULL, y = "Price/m²") + theme_minimal() + theme(legend.position = "none")
  })
  
  # Map
  quarter_stats <- reactive({
    filtered_data() %>%
      group_by(quarter) %>%
      summarise(count = n(), avg_price = round(mean(price)), price_m2 = round(mean(price_per_m2), 1), .groups = "drop") %>%
      arrange(desc(count)) %>%
      head(input$map_top_n) %>%
      left_join(quarter_coords, by = "quarter") %>%
      filter(!is.na(lat))
  })
  
  output$poznan_map <- renderLeaflet({
    stats <- quarter_stats()
    metric <- input$map_metric
    
    pal <- colorNumeric(palette = c("#C8E6C9", colors["primary"]), domain = stats[[metric]])
    
    leaflet(stats) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 16.925, lat = 52.41, zoom = 12) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(count) * 1.5,
        color = ~pal(stats[[metric]]),
        fillColor = ~pal(stats[[metric]]),
        fillOpacity = 0.7,
        stroke = TRUE, weight = 2,
        popup = ~paste0("<b>", quarter, "</b><br>",
                        "Listings: ", count, "<br>",
                        "Avg Price: ", format(avg_price, big.mark = ","), " PLN<br>",
                        "Price/m²: ", price_m2, " PLN")
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = stats[[metric]],
                title = switch(metric, "avg_price" = "Avg Price", "price_m2" = "PLN/m²", "count" = "Listings"))
  })
  
  output$quarter_stats <- renderDT({
    quarter_stats() %>%
      select(Quarter = quarter, N = count, `Avg` = avg_price, `PLN/m²` = price_m2) %>%
      datatable(options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  })
  
  output$price_by_quarter_box <- renderPlot({
    top_q <- filtered_data() %>% count(quarter, sort = TRUE) %>% head(8) %>% pull(quarter)
    filtered_data() %>% filter(quarter %in% top_q, price <= 5000) %>%
      mutate(quarter = factor(quarter, levels = top_q)) %>%
      ggplot(aes(x = quarter, y = price, fill = quarter)) +
      geom_boxplot() + scale_fill_brewer(palette = "Greens") +
      labs(x = NULL, y = "Price (PLN)") + theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # Amenities
  output$amenity_prevalence <- renderPlot({
    filtered_data() %>%
      summarise(across(all_of(amenity_cols), ~ mean(., na.rm = TRUE) * 100)) %>%
      pivot_longer(everything(), names_to = "amenity", values_to = "pct") %>%
      mutate(label = amenity_labels[match(amenity, amenity_cols)]) %>%
      arrange(desc(pct)) %>%
      mutate(label = factor(label, levels = rev(label))) %>%
      ggplot(aes(x = label, y = pct, fill = pct)) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low = "#FFF9C4", high = "#F57F17") +
      labs(x = NULL, y = "% of listings") + theme_minimal() + theme(legend.position = "none")
  })
  
  output$amenity_impact <- renderPlot({
    amenity <- input$selected_amenity
    label <- amenity_labels[match(amenity, amenity_cols)]
    filtered_data() %>% filter(price <= 5000) %>%
      mutate(has = ifelse(.data[[amenity]], paste("With", label), paste("Without", label))) %>%
      ggplot(aes(x = has, y = price, fill = has)) +
      geom_boxplot() + scale_fill_manual(values = c("#EF5350", colors["primary"])) +
      labs(x = NULL, y = "Price") + theme_minimal() + theme(legend.position = "none")
  })
  
  output$amenity_premium <- renderPlot({
    map_dfr(seq_along(amenity_cols), function(i) {
      with_am <- filtered_data() %>% filter(.data[[amenity_cols[i]]]) %>% pull(price) %>% median(na.rm = TRUE)
      without_am <- filtered_data() %>% filter(!.data[[amenity_cols[i]]]) %>% pull(price) %>% median(na.rm = TRUE)
      data.frame(amenity = amenity_labels[i], premium = round((with_am - without_am) / without_am * 100, 1))
    }) %>% arrange(desc(premium)) %>%
      mutate(amenity = factor(amenity, levels = rev(amenity)), color = premium >= 0) %>%
      ggplot(aes(x = amenity, y = premium, fill = color)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#EF5350", "TRUE" = colors["primary"])) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = NULL, y = "Premium (%)") + theme_minimal() + theme(legend.position = "none")
  })
  
  # Data
  output$data_table <- renderDT({
    filtered_data() %>% select(quarter, price, flat_area, flat_rooms, price_per_m2) %>%
      datatable(options = list(pageLength = 20), rownames = FALSE)
  })
  
  output$download_btn <- downloadHandler(
    filename = function() { paste0("poznan_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)