library(shiny)
library(readxl)
library(dplyr)

# Load data
df <- read_excel("rent-poznan.xlsx")

# Quick data prep
df <- df %>%
  mutate(
    price_per_m2 = round(price / flat_area, 2)
  )

ui <- fluidPage(
  titlePanel("Poznań Rental Market Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      p("diltry"),
      hr(),
      p(paste("Dataset loaded:", nrow(df), "listings"))
    ),
    
    mainPanel(
      h3("Data Preview"),
      p("Dane zaladowane."),
      tableOutput("preview_table")
    )
  )
)

server <- function(input, output, session) {
  
  output$preview_table <- renderTable({
    df %>%
      select(quarter, price, flat_area, flat_rooms, price_per_m2) %>%
      head(10)
  })
  
}

shinyApp(ui = ui, server = server)