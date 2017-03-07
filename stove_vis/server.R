library(shiny)
library(tidyverse)
df <- readRDS("data/emissions.RDS")

# Define server logic required to draw a histogram
shinyServer(function(input, output){

 # plot one pollutant versus another
  output$stove_plot <- renderPlot({
    p <- ggplot(df, aes_string(input$x_var, input$y_var, colour = input$p_col)) +
      geom_line() +
      theme_minimal() +
      facet_wrap(as.character(input$p_facet), scales = "free", ncol = 1) +
      theme(legend.position = "right") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    print(p)
  })

 # plot 
  
  
})