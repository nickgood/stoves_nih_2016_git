#________________________________________________________
# libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# melts data preserving id and date
tidy.id.date <- function(df, regex, namesub){
  # select weight
    out <- select(df, id, date, matches(regex)) %>%
      filter(is.na(id)==FALSE) # remove NaNs
    
  # rename columns
    names(out) <- gsub(namesub, "",colnames(out))
    
  # melt data
    out <- gather(out, var, value, -id, -date)
    
  # classes
    out$var <- as.factor(out$var)
    
  # return
    return(out)
}
#________________________________________________________
  
#________________________________________________________
# melts data preserving date
tidy.date <- function(df, regex, namesub){
  
  # select weight
    out <- select(df, date, matches(regex)) %>%
      filter(is.na(date)==FALSE) # remove NaNs
  
  # rename columns
    names(out) <- gsub(namesub, "", colnames(out))
  
  # melt data
    out <- gather(out, var, value, -date)
    
  # classes
    out$var <- as.factor(out$var)
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# pax
split.pax.flows <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("flow.*", "", df$var)), 
                
                    loc=as.factor(ifelse(grepl("paxexit", df$var), "exit", "inlet")),
                
                    rep=as.factor(gsub("[^0-9]", "", df$var)))
  
  # drop original variable      
    out <- select(out, -var)
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# smps, iso, carb
split.flows <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("flow.*", "", df$var)), 
                
                rep=as.factor(gsub("[^0-9]", "", df$var)))
  
  # drop original variable      
    out <- select(out, -var)
  
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# carb
split.times <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("_.*", "", df$var)))
            
  # drop original variable      
    out <- select(out, -var)
  
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# co2
# split co2 calibration variable name into pollutant and type
split.co2.cal <- function(df){
  
  # split variable
    out <- mutate(df, pol=as.factor(sub("_.*", "", df$var)),
                
                    type=as.factor(sub(".*_", "", df$var)))
  
  # drop original variable      
    out <- select(out, -var)
  
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# filter flows
split.filter.flows <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("flow.*", "", df$var)), 
                
                      colour=as.factor(gsub("[^_]*_[^_]*_|_[^_]*$", "", df$var)),
                
                      rep=as.factor(gsub("[^0-9]", "", df$var)))
  
  # drop original variable      
    out <- select(out, -var)
  
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# filter times
split.filter.times <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("_.*", "", df$var)), 
                
                      colour_1=gsub("[^_]*_[^_]*_|_[^_]*$", "", df$var),
                  
                      colour_2=gsub("[^_]*_[^_]*_[^_]*_|_[^_]*$", "", df$var))
                
  # drop original variable and melt      
    out <- select(out, -var) 
    
    out <- gather(out, col_var, color, -date, -value, -type)
    
    out <- select(out, -col_var)
    
    out$color <- as.factor(out$color)
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# fivegas cal times
split.fivegas.cal.times <- function(df){
  
  # split variable
    out <- mutate(df, type=as.factor(sub("_.*", "", df$var)),
                
                pol=as.factor(sub(".*_", "", df$var)))
  
  # drop original variable      
    out <- select(out, -var)
  
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# Notes
split.notes <- function(){
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# Map id, based on date and time
map.id <- function(df, test_times){
  for(i in 1:nrow(test_times)){
    # extract data for time period
      tmp <- filter(df, (df$date == test_times$date[i] & df$time >= test_times$start[i] & df$time <= test_times$end[i]))
    # output in first instance
      if(exists("out", inherits = FALSE) == FALSE & nrow(tmp) > 0){
        tmp$id <- test_times$id[i]
        out <- tmp
      }
    # output
      if(exists("out", inherits = FALSE) == TRUE & nrow(tmp) > 0){
        tmp$id <- test_times$id[i]
        out <- rbind(out, tmp)
      }
    #
  }
  # return
    return(out)
}
#________________________________________________________




