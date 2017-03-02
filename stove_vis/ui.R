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
               align="center",
          tags$h2("Stoves"))),
      fluidRow(
        column(width = 12,
               align="center",
          tags$img(height = 180,
                  width = 700,
                  src = "stoves_stacked.png"))),
      fluidRow(
        column(width = 12,
               align="center",
          tags$h2("Fuels"))),
      fluidRow(
        column(width = 12,
               align="center",
          tags$img(height = 180,
                   width = 600,
                   src = "fuels_stacked.png"))),
      fluidRow(
        column(width = 12,
               align="center",
        tags$h2("Protocol"),
        includeText("text/protocol.txt"),
        tags$h4(""),
        tags$img(height = 150,
                 width = "90%",
                 src = "pic_all_stoves.png"),
        tags$h4(""))),
      fluidRow(
        column(width = 12,
               align="left",
               tags$footer(includeText("text/acknowledge.txt"),
                           style = "padding: 10px;")))
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