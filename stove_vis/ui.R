library(shiny)
library(shinythemes)

# define UI
shinyUI(fluidPage(

  navbarPage(theme = shinytheme("united"), "STOVES",

    tabPanel("Overview",
      fluidRow(
        column(width = 12,
        includeText("text/intro.txt"))),
      fluidRow(
        column(width = 12,
        includeText("text/intro.txt")))
    ),

    tabPanel("Compare Pollutants",
      sidebarLayout(
        sidebarPanel(
          selectInput('x_var', 'x axis:', colnames(df)),
          selectInput('y_var', 'y axis:', colnames(df)),
          selectInput('p_col', 'color by:', colnames(df)),
          selectInput('p_facet', 'facet by:', colnames(df)),
          selectInput('p_filter', 'filter by:', colnames(df)),
          checkboxInput("p_bad","exclude bad data")
        ),
        mainPanel(
          plotOutput("stove_plot")
       )
      )
    ),
    tabPanel("Stove info"),
    tabPanel("Fuel info"),
    tabPanel("About")
  )
))