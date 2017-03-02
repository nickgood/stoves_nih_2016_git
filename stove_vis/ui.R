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
          wellPanel(
            tags$strong("left axis"),
            selectInput("y_metric", "metric:", unit_list),
            selectInput("y_inst", "instrument:", inst_list),
            selectInput('y_pol', 'pollutant:', choices=character(0))
          ),
          wellPanel(
            tags$strong("bottom axis"),
            selectInput("x_metric", "metric:", unit_list),
            selectInput("x_inst", "instrument:", inst_list),
            selectInput('x_pol', 'pollutant:', choices=character(0))
          ),
          wellPanel(
            tags$strong("appearance"),
            selectInput('p_col', 'color by:', colnames(df)),
            selectInput('p_facet', 'facet by:', colnames(df)),
            selectInput('p_filter', 'filter by:', colnames(df)),
            selectInput('p_group', 'group by:', colnames(df))
          ),
          wellPanel(
            actionButton("updateButton", "update")
          )
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