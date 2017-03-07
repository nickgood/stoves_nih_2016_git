# libararies
  library(shiny)
  library(tidyverse)

# read data
 df <- readRDS("data/emissions_long.RDS")

# Define server logic required to draw a histogram
  shinyServer(function(input, output, clientData, session){

#_______________________________________________________________________________
 # plot one pollutant versus another
  stove_plot <- eventReactive(input$updateButton, 
    {d_left <- dplyr::filter_(df,
                              ~pol == input$y_pol,
                              ~units ==  input$y_metric)

     d_bottom <- dplyr::filter_(df,
                                ~pol == input$x_pol,
                                ~units ==  input$x_metric)

     p_data <- dplyr::inner_join(d_left,
                                 d_bottom,
                                 by = "id")

  p <- ggplot(p_data, aes(x = value.y, y = value.x)) +
         theme_minimal() +
         ylab(paste0(input$y_pol, " (", input$y_metric, ")")) +
         xlab(paste0(input$x_pol, " (", input$x_metric, ")"))
  # color
  if(input$p_col != "none"){
    p <- p + geom_point(aes_string(color = input$p_col)) +
         theme(legend.position = "hide")
  }else{
    p <- p + geom_point()
  }
  # plot
  print(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# output plot
  output$stove_plot <- renderPlot({
    stove_plot()
  })
#_______________________________________________________________________________

  
#_______________________________________________________________________________
# watch for updates to menu items
  observe({
  #_______________________________________________________________________________
  # instrument selected
    inst_y <- input$y_inst
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update y axis pol menu based on instrument selection
    if(inst_y == "carbs"){
      pol_y <- pol_carb
    }else if(inst_y == "ecoc"){
      pol_y <- pol_ecoc
    }else if(inst_y == "fivegas"){
      pol_y <-  pol_fivegas
    }else if(inst_y == "grav"){
      pol_y <-  pol_grav  
    }else if(inst_y == "ions"){
      pol_y <-  pol_ions
    }else if(inst_y == "voc"){
      pol_y <-  pol_voc
    }else(pol_y <-  "select instrument")
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update y axis pollutant menu
    updateSelectInput(session,
                      "y_pol",
                       choices = pol_y)
  #_______________________________________________________________________________
}) # close observe expression
  
#_______________________________________________________________________________
# watch for updates to menu items
  observe({ 
  inst_x <- input$x_inst
  #_______________________________________________________________________________
  # update x axis pol menu based on instrument selection
    if(inst_x == "carbs"){
      pol_x <- pol_carb
    }else if(inst_x == "ecoc"){
      pol_x <- pol_ecoc
    }else if(inst_x == "fivegas"){
      pol_x <-  pol_fivegas
  }else if(inst_x == "grav"){
      pol_x <-  pol_grav
  }else if(inst_x == "ions"){
      pol_x <-  pol_ions
  }else if(inst_x == "voc"){
      pol_x <-  pol_voc
  }else(pol_x <-  "select instrument")
  #_______________________________________________________________________________

  #_______________________________________________________________________________
   # update x axis pollutant menu
    updateSelectInput(session,
                      "x_pol",
                      choices = pol_x)
  }) # close observe expression
}) 
#_______________________________________________________________________________