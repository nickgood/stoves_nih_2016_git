# libararies
  library(shiny)
  library(tidyverse)

# read data
 df <- readRDS("data/emissions_long.RDS")

# Define server logic required to draw a histogram
shinyServer(function(input, output, clientData, session){

  observe({
  # pollutant list to display - left axis
   inst_y <- input$y_inst
   inst_x <- input$x_inst

    # plot one pollutant versus another
      output$stove_plot <- eventReactive(input$updateButton, 
                           {renderPlot({
                           # get data
                           p <- ggplot(df, aes_string(input$x_var, input$y_var, colour = input$p_col)) +
                                  geom_line() +
                                  theme_minimal() +
                                  facet_wrap(as.character(input$p_facet), scales = "free", ncol = 1) +
                                  theme(legend.position = "right") +
                                  theme(axis.text.x = element_text(angle = 60, hjust = 1))
                            print(p)
                         })
      })
      
 # update y axis pol menu based on instrument selection
  if(inst_y == "carbs"){
  	inst_y <- pol_carb
  }else if(inst_y == "ecoc"){
  	inst_y <- pol_ecoc
  }else if(inst_y == "fivegas"){
  	inst_y <-  pol_fivegas
  }else if(inst_y == "ions"){
  	inst_y <-  pol_ions
  }else if(inst_y == "voc"){
  	inst_y <-  pol_voc
  }else(inst_y <-  "select instrument")

 # update y axis pollutant menu
  updateSelectInput(session,
                    "y_pol",
                     choices = inst_y)

  # update x axis pol menu based on instrument selection
  if(inst_x == "carbs"){
  	inst_x <- pol_carb
  }else if(inst_x == "ecoc"){
  	inst_x <- pol_ecoc
  }else if(inst_x == "fivegas"){
  	inst_x <-  pol_fivegas
  }else if(inst_x == "ions"){
  	inst_x <-  pol_ions
  }else if(inst_x == "voc"){
  	inst_x <-  pol_voc
  }else(inst_x <-  "select instrument")
  
 # update x axis pollutant menu
  updateSelectInput(session,
                    "x_pol",
                    choices = inst_x)

  })
})