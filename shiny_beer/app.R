library(shiny)


# Define UI for application
ui <- fluidPage(
   titlePanel("Beer Maps by ABV (% Alcohol By Volume)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("abv_select",
                     "Select Alcohol Level",
                     choices = c('Low', 'Medium', 'High', 'Very High'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Load the external file for some helper functions
  source("../beer_functions.R")
  map_list <- read_and_clean("../beers.csv", "../breweries.csv") %>% 
    get_abv_dfs %>% 
    make_maps
   
   output$distPlot <- renderPlot({
      # Draw the map with the specified ABV data
      if (input$abv_select == "Low") {
        map_list$lo
      } else if (input$abv_select == "Medium") {
        map_list$med
      } else if (input$abv_select == "High") {
        map_list$hi
      } else {
        map_list$vhi
      }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

