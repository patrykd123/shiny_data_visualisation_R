library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

df <- read_excel("rent-poznan.xlsx")

df <- df %>%
  mutate(
    price_per_m2 = round(price / flat_area, 2),
    rooms_cat = factor(
      case_when(
        flat_rooms == 1 ~ "1 room",
        flat_rooms == 2 ~ "2 rooms",
        flat_rooms == 3 ~ "3 rooms",
        TRUE ~ "4+ rooms"
      ),
      levels = c("1 room", "2 rooms", "3 rooms", "4+ rooms")
    )
  )

# Color palette (starting to think about UEP green theme)
colors <- c(
  primary = "#1B5E20",
  secondary = "#388E3C", 
  accent = "#4CAF50",
  light = "#81C784"
)

ui <- fluidPage(
  titlePanel("Poznań Rental Market Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      
      sliderInput("price_range", "Price (PLN):",
                  min = 0, max = 10000, 
                  value = c(0, 5000), step = 100),
      
      sliderInput("area_range", "Area (m²):",
                  min = 0, max = 200, 
                  value = c(0, 150), step = 5),
      
      checkboxGroupInput("rooms_filter", "Number of rooms:",
                         choices = c("1 room", "2 rooms", "3 rooms", "4+ rooms"),
                         selected = c("1 room", "2 rooms", "3 rooms", "4+ rooms")),
      
      hr(),
      textOutput("filter_summary")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        type = "tabs",
        
        # Overview Tab
        tabPanel("Overview",
                 br(),
                 fluidRow(
                   column(3, wellPanel(
                     h5("Total Listings"), h3(textOutput("stat_count"))
                   )),
                   column(3, wellPanel(
                     h5("Average Price"), h3(textOutput("stat_avg_price"))
                   )),
                   column(3, wellPanel(
                     h5("Average Area"), h3(textOutput("stat_avg_area"))
                   )),
                   column(3, wellPanel(
                     h5("Avg Price/m²"), h3(textOutput("stat_price_m2"))
                   ))
                 ),
                 fluidRow(
                   column(6, 
                          h4("Price Distribution"),
                          plotOutput("price_histogram", height = "280px")
                   ),
                   column(6,
                          h4("Room Distribution"),
                          plotOutput("rooms_chart", height = "280px")
                   )
                 )
        ),
        
        # Price Analysis Tab
        tabPanel("Price Analysis",
                 br(),
                 h4("Price vs Area"),
                 plotOutput("scatter_plot", height = "400px"),
                 hr(),
                 fluidRow(
                   column(6,
                          h4("Price by Room Count"),
                          plotOutput("price_by_rooms", height = "280px")
                   ),
                   column(6,
                          h4("Price per m² by Rooms"),
                          plotOutput("price_m2_by_rooms", height = "280px")
                   )
                 )
        ),
        
        # Data Tab
        tabPanel("Data",
                 br(),
                 tableOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>%
      filter(
        price >= input$price_range[1],
        price <= input$price_range[2],
        flat_area >= input$area_range[1],
        flat_area <= input$area_range[2],
        rooms_cat %in% input$rooms_filter
      )
  })
  
  # Statistics
  output$stat_count <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$stat_avg_price <- renderText({
    paste(format(round(mean(filtered_data()$price)), big.mark = ","), "PLN")
  })
  
  output$stat_avg_area <- renderText({
    paste(round(mean(filtered_data()$flat_area), 1), "m²")
  })
  
  output$stat_price_m2 <- renderText({
    paste(round(mean(filtered_data()$price_per_m2), 1), "PLN")
  })
  
  output$filter_summary <- renderText({
    paste("Showing", nrow(filtered_data()), "of", nrow(df), "listings")
  })
  
  # Histogram
  output$price_histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = price)) +
      geom_histogram(bins = 40, fill = colors["primary"], color = "white", alpha = 0.9) +
      scale_x_continuous(labels = comma_format(), limits = c(0, 6000)) +
      labs(x = "Price (PLN)", y = "Count") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
  })
  
  # Room distribution
  output$rooms_chart <- renderPlot({
    room_counts <- filtered_data() %>%
      count(rooms_cat)
    
    ggplot(room_counts, aes(x = rooms_cat, y = n, fill = rooms_cat)) +
      geom_col() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], 
                                   colors["accent"], colors["light"])) +
      labs(x = NULL, y = "Count") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    sample_data <- filtered_data() %>%
      filter(flat_area <= 150, price <= 6000) %>%
      sample_n(min(nrow(.), 2000))
    
    ggplot(sample_data, aes(x = flat_area, y = price, color = rooms_cat)) +
      geom_point(alpha = 0.5, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = c(colors["primary"], colors["secondary"], 
                                    colors["accent"], "#FF8F00")) +
      scale_y_continuous(labels = comma_format()) +
      labs(x = "Area (m²)", y = "Price (PLN)", color = "Rooms") +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
  })
  
  # Boxplots
  output$price_by_rooms <- renderPlot({
    ggplot(filtered_data() %>% filter(price <= 5000), 
           aes(x = rooms_cat, y = price, fill = rooms_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], 
                                   colors["accent"], colors["light"])) +
      scale_y_continuous(labels = comma_format()) +
      labs(x = NULL, y = "Price (PLN)") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })
  
  output$price_m2_by_rooms <- renderPlot({
    ggplot(filtered_data() %>% filter(price_per_m2 <= 80), 
           aes(x = rooms_cat, y = price_per_m2, fill = rooms_cat)) +
      geom_boxplot() +
      scale_fill_manual(values = c(colors["primary"], colors["secondary"], 
                                   colors["accent"], colors["light"])) +
      labs(x = NULL, y = "Price per m² (PLN)") +
      theme_minimal() +
      theme(legend.position = "none", panel.grid.minor = element_blank())
  })
  
  output$data_table <- renderTable({
    filtered_data() %>%
      select(quarter, price, flat_area, flat_rooms, price_per_m2) %>%
      head(25)
  })
  
}

shinyApp(ui = ui, server = server)