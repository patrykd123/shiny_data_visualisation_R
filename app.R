library(shiny)
library(readxl)
library(dplyr)

df <- read_excel("rent-poznan.xlsx")

df <- df %>%
  mutate(
    price_per_m2 = round(price / flat_area, 2),
    rooms_cat = case_when(
      flat_rooms == 1 ~ "1 room",
      flat_rooms == 2 ~ "2 rooms",
      flat_rooms == 3 ~ "3 rooms",
      TRUE ~ "4+ rooms"
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
      h3("Filtered Data"),
      tableOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
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
  
  output$filter_summary <- renderText({
    paste("Showing", nrow(filtered_data()), "of", nrow(df), "listings")
  })
  
  output$data_table <- renderTable({
    filtered_data() %>%
      select(quarter, price, flat_area, flat_rooms, price_per_m2) %>%
      head(20)
  })
  
}

shinyApp(ui = ui, server = server)
