library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

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
      
      # Summary statistics row
      fluidRow(
        column(3, wellPanel(
          h5("Total Listings"),
          h3(textOutput("stat_count"))
        )),
        column(3, wellPanel(
          h5("Average Price"),
          h3(textOutput("stat_avg_price"))
        )),
        column(3, wellPanel(
          h5("Average Area"),
          h3(textOutput("stat_avg_area"))
        )),
        column(3, wellPanel(
          h5("Avg Price/m²"),
          h3(textOutput("stat_price_m2"))
        ))
      ),
      
      hr(),
      
      # Histogram
      h4("Price Distribution"),
      plotOutput("price_histogram", height = "300px"),
      
      hr(),
      
      # Data table
      h4("Sample Data"),
      tableOutput("data_table")
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
      geom_histogram(bins = 40, fill = "#2E7D32", color = "white", alpha = 0.8) +
      scale_x_continuous(labels = scales::comma_format(), limits = c(0, 6000)) +
      labs(x = "Price (PLN)", y = "Count") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
  })
  
  output$data_table <- renderTable({
    filtered_data() %>%
      select(quarter, price, flat_area, flat_rooms, price_per_m2) %>%
      head(10)
  })
  
}

shinyApp(ui = ui, server = server)
