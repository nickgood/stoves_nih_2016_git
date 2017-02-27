library(shiny)

# define UI
shinyUI(fluidPage(
 # title
  titlePanel("Plot stove emissions"),

 # sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput('x_var', 'x axis:', colnames(df)),
      selectInput('y_var', 'y axis:', colnames(df)),
      selectInput('p_col', 'color by:', colnames(df)),
      selectInput('p_facet', 'facet by:', colnames(df)),
      selectInput('p_filter', 'filter by:', colnames(df)),
      checkboxInput("p_bad","exclude bad data")
    ),
 # show plot
  mainPanel(
    plotOutput("stove_plot")
  )
  )
))