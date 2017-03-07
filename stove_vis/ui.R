library(shiny)
library(shinythemes)

# define UI
shinyUI(fluidPage(
#_______________________________________________________________________________
# naigation bar
  navbarPage(theme = shinytheme("united"), "STOVES",
#_______________________________________________________________________________

#_______________________________________________________________________________
# overview tab
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
          tags$img(height = 70,
                  width = 750,
                  src = "stoves.png"))),
      fluidRow(
        column(width = 12,
               align="center",
          tags$h2("Fuels"))),
      fluidRow(
        column(width = 12,
               align="center",
          tags$img(height = 60,
                   width = 600,
                   src = "fuels.png"))),
      fluidRow(
        column(width = 12,
               align="center",
        tags$h2("Protocol"),
        includeText("text/protocol.txt"),
        tags$h4(""),
        tags$img(height = 100,
                 width = "80%",
                 src = "pic_all_stoves.png"),
        tags$h4(""))),
      fluidRow(
        column(width = 12,
               align="left",
               tags$footer(includeText("text/acknowledge.txt"),
                           style = "padding: 10px;")))
    ),
# end overview tab
#_______________________________________________________________________________

#_______________________________________________________________________________
# interactive plot tab
    tabPanel("Compare pollutants",
      fluidRow(column(width = 3,
                      wellPanel(style = "padding: 5px;",
                      tags$strong("left axis"),
                      selectInput("y_metric", "metric:", unit_list),
                      selectInput("y_inst", "instrument:", inst_list),
                      selectInput('y_pol', 'pollutant:', choices=character(0))
                      )),
               column(width = 3,
                      wellPanel(style = "padding: 5px;",
                      tags$strong("bottom axis"),
                      selectInput("x_metric", "metric:", unit_list),
                      selectInput("x_inst", "instrument:", inst_list),
                      selectInput('x_pol', 'pollutant:', choices=character(0))
                      )),
               column(width = 3,
                      wellPanel(style = "padding: 5px;",
                      tags$strong("appearance"),
                      selectInput('p_col', 'color by:', by_color)
                      ))
               ),
      fluidRow(column(width = 12,
        plotOutput('stove_plot'))),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 200, left = 425, right = "auto", bottom = "auto",
                    width = "auto", height = "auto",
                    actionButton("updateButton", "update")

      )
    ),

# end interactive plot tab
#_______________________________________________________________________________

#_______________________________________________________________________________
# interactive plot tab 2
    tabPanel("Single pollutant",
      fluidRow(column(width = 3,
               wellPanel(style = "padding: 5px;",
               selectInput("sin_metric", "metric:", unit_list),
               selectInput("sin_inst", "instrument:", inst_list),
               selectInput('sin_pol', 'pollutant:', choices=character(0))
               )))),
#_______________________________________________________________________________

    tabPanel("Stove info"),
    tabPanel("Fuel info"),
    tabPanel("About")
  )
))