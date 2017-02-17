#________________________________________________________
# libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# melts data preserving id and date
tidy_id_date <- function(df, regex, namesub){
 # select weight
  out <- dplyr::select(df, id, date, matches(regex)) %>%
         dplyr::filter(is.na(id)==FALSE) # remove NaNs

 # rename columns
  names(out) <- gsub(namesub, "",colnames(out))
 
 # melt data
   out <- tidyr::gather(out, var, value, -id, -date) %>%
          dplyr::mutate(var = as.factor(var))

 # return
  return(out)
}
#________________________________________________________
  
#________________________________________________________
# melts data preserving date
tidy_date <- function(df, regex, namesub){
 # select weight
  out <- dplyr::select(df, date, matches(regex)) %>%
         dplyr::filter(is.na(date)==FALSE) # remove NaNs
  
 # rename columns
  names(out) <- gsub(namesub, "", colnames(out))
  
 # melt data
  out <- tidyr::gather(out, var, value, -date) %>%
         dplyr::mutate(var = as.factor(var))

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# pax
split_pax_flows <- function(df){
 # split variable
  out <- dplyr::mutate(df, type=as.factor(sub("flow.*", "", df$var)), 
                           loc=as.factor(ifelse(grepl("paxexit", df$var), "exit", "inlet")),
                           rep=as.factor(gsub("[^0-9]", "", df$var)))

 # drop original variable      
  out <- dplyr::select(out, -var)
 
 # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# smps, iso, carb
split_flows <- function(df){
 # split variable
  out <- dplyr::mutate(df, type=as.factor(sub("flow.*", "", df$var)),
                           rep=as.factor(gsub("[^0-9]", "", df$var))) %>%
         dplyr::select(-var)

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# carb
split_times <- function(df){
  # split variable
    out <- dplyr::mutate(df, type = as.factor(sub("_.*", "", var))) %>%
           dplyr::select(-var)

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# smps pax times
split_times_smps_pax <- function(df){
 # split variable
  out <- dplyr::mutate(df, time_point = as.factor(sub("_.*", "", var)),
                           type = ifelse(grepl("_pre$", var), "bg_pre", NA),
                           type = ifelse(grepl("_post$", var), "bg_post", type),
                           type = ifelse(grepl("_pax$", var), "sample", type)) %>%
         dplyr::select(-var)

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# co2
# split co2 calibration variable name into pollutant and type
split_co2_cal <- function(df){
 # split variable
  out <- dplyr::mutate(df, pol=as.factor(sub("_.*", "", df$var)),
                           type=as.factor(sub(".*_", "", df$var))) %>%
         dplyr::select(-var)
 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# filter flows
split_filter_flows <- function(df){
 # split variable
  out <- dplyr::mutate(df, type = as.factor(sub("flow.*", "", var)), 
                           colour = as.factor(gsub("[^_]*_[^_]*_|_[^_]*$", "", var)),
                           rep = as.factor(gsub("[^0-9]", "", var))) %>%
          dplyr::select(-var)

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# filter times
split_filter_times <- function(df){
 # split variable
  out <- dplyr::mutate(df, type = as.factor(sub("_.*", "", var)), 
                           colour_1=gsub("[^_]*_[^_]*_|_[^_]*$", "", var),
                           colour_2=gsub("[^_]*_[^_]*_[^_]*_|_[^_]*$", "", var)) %>%
         dplyr::select(-var) %>%
         tidyr::gather(col_var, color, -id, -date, -value, -type) %>%
         dplyr::select(-col_var) %>%
         dplyr::mutate(color = as.factor(color))
             
 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# fivegas cal times
split_fivegas_cal_times <- function(df){
 # split variable
  out <- dplyr::mutate(df, type=as.factor(sub("_.*", "", df$var)),
                           pol=as.factor(sub(".*_", "", df$var))) %>%
         dplyr::select(-var)

 # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# Map id, based on date and time
map_id <- function(df, test_times){
  
  for(i in 1:nrow(test_times)){
    # extract data for time period
      tmp <- dplyr::filter(df, (df$date == test_times$date[i] & df$time >= test_times$start[i] & df$time <= test_times$end[i]))
      
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

  }
 # return
  return(out)
}
#________________________________________________________
