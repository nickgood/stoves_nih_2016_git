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

  p <- ggplot(p_data, aes(value.x, value.y)) +
         geom_point() +
         theme_minimal() +
         ylab(paste0(input$y_pol, " (", input$y_metric, ")")) +
         xlab(paste0(input$x_pol, " (", input$x_metric, ")"))
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
  # pollutant list to display - left axis
    inst_y <- input$y_inst
    inst_x <- input$x_inst
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update y axis pol menu based on instrument selection
    if(inst_y == "carbs"){
      inst_y <- pol_carb
    }else if(inst_y == "ecoc"){
      inst_y <- pol_ecoc
    }else if(inst_y == "fivegas"){
      inst_y <-  pol_fivegas
    }else if(inst_y == "grav"){
      inst_y <-  pol_grav  
    }else if(inst_y == "ions"){
      inst_y <-  pol_ions
    }else if(inst_y == "voc"){
      inst_y <-  pol_voc
    }else(inst_y <-  "select instrument")
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update y axis pollutant menu
    updateSelectInput(session,
                      "y_pol",
                       choices = inst_y)
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update x axis pol menu based on instrument selection
    if(inst_x == "carbs"){
      inst_x <- pol_carb
    }else if(inst_x == "ecoc"){
      inst_x <- pol_ecoc
    }else if(inst_x == "fivegas"){
      inst_x <-  pol_fivegas
  }else if(inst_x == "grav"){
      inst_x <-  pol_grav
  }else if(inst_x == "ions"){
      inst_x <-  pol_ions
  }else if(inst_x == "voc"){
      inst_x <-  pol_voc
    }else(inst_x <-  "select instrument")
  #_______________________________________________________________________________

  #_______________________________________________________________________________
   # update x axis pollutant menu
    updateSelectInput(session,
                      "x_pol",
                      choices = inst_x)
    })
}) # close obsere expression
#_______________________________________________________________________________