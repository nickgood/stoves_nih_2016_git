library(shiny)
library(tidyverse)
df <- readRDS("data/emissions.RDS")

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
         xlab(paste0(input$x_pol, " (", input$x_metric, ")")) +
         theme(text = element_text(size = 18))
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
  # plot one pollutant versus another
  single_stove_plot <- eventReactive(input$updateButton_sin, 
                         {p_data <- dplyr::filter_(df,
                                    ~pol == input$sin_pol,
                                    ~units ==  input$sin_metric)

                          p <- ggplot(p_data, aes(y = value)) +
                               theme_minimal()
                            # group
                             if(input$sin_group == "none" | input$sin_group == "id"){
                                 p <- p + geom_point(aes(x = id)) + 
                                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                      theme(text = element_text(size = 18)) +
                                      ylab(paste0(input$sin_pol, " (", input$sin_metric, ")"))
                                        
                             }else{
                             p <- p + geom_boxplot(aes_string(x = input$sin_group, fill = input$sin_group)) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                  ylab(paste0(input$sin_pol, " (", input$sin_metric, ")")) +
                                  theme(text = element_text(size = 18))
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
  # output plot
  output$single_stove_plot <- renderPlot({
    single_stove_plot()
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
#_______________________________________________________________________________
  
#_______________________________________________________________________________
  # watch for updates to menu items
  observe({
  #_______________________________________________________________________________
  # instrument selected
   sin_inst <- input$sin_inst
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update single pol menu based on instrument selection
   if(sin_inst == "carbs"){
     pol_sin <- pol_carb
   }else if(sin_inst == "ecoc"){
     pol_sin <- pol_ecoc
   }else if(sin_inst == "fivegas"){
     pol_sin <-  pol_fivegas
   }else if(sin_inst == "grav"){
     pol_sin <-  pol_grav  
   }else if(sin_inst == "ions"){
     pol_sin <-  pol_ions
   }else if(sin_inst == "voc"){
     pol_sin <-  pol_voc
   }else(sin_inst <- "select instrument")
  #_______________________________________________________________________________

  #_______________________________________________________________________________
  # update single pollutant menu
    updateSelectInput(session,
                      "sin_pol",
                      choices = pol_sin)
  #_______________________________________________________________________________
  }) # close observe expression

})