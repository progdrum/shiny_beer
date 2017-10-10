library(shiny)


# Define UI for application
ui <- fluidPage(
   titlePanel("Beer Maps by ABV (% Alcohol By Volume"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
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
  test <- read_and_clean("../beers.csv", "../breweries.csv") %>% 
    get_abv_dfs %>% 
    make_maps
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      ggplot(data = x) + geom_histogram(bins = bins)
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

