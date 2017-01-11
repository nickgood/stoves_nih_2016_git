#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# check for outliers
  is_outlier <- function(x) {
    
    out <- (x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | 
           (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
    
  # return
    return(out)
}
#________________________________________________________
  
#________________________________________________________
# convert POSIXct to seconds of day
  posixct_secsofday <- function(x) {

    x <- as.character(x)
      
    out <- as.numeric(substr(x,12,13))*60*60 + 
           as.numeric(substr(x,15,16))*60 +
           as.numeric(substr(x,18,19))
    
  # return
    return(out)
  }
#________________________________________________________
  
#________________________________________________________
# stats for last 30 seconds of a time window
  time_window_stats <- function(df, date_var, start_time_var, end_time_var, pol_var, stat_var = "mean", plot = FALSE) {
    
    df_win <- dplyr::filter(df, date == date_var &
                              time >= (start_time_var) &
                              time >= (end_time_var - 60) &
                              time <= (end_time_var - 30) &
                              pol == pol_var)

  # stat
    if(stat_var == "mean"){
      out <- mean(df_win$val, na.rm = TRUE)
    }
    
  # stat  
    if(stat_var == "sd"){
      out <- sd(df_win$val, na.rm = TRUE)
    }
    
  # plot
    if(plot == TRUE){
      head(df_win)
      
      p <- ggplot(df_win, aes(datetime, val)) +
                  geom_point()
      
      filename <- paste0("fivegascal_",pol_var,"_", as.character(date_var),".jpeg")
      
      ggsave(filename, p)
      
      print(filename)
    }
    
  # return
    return(out)
}
#________________________________________________________
  
#________________________________________________________ 
# add calibration date to dataframe
  add_caldate <- function(df, cal){

    cal_dates <- unique(arrange(cal, date)$date)
    
    num <- length(cal_dates)   # number of calibrations

    for(i in 1:num){
      
      # split
        if(i < num){
          tmp <- dplyr::filter(df, date >= cal_dates[i] & date < cal_dates[i+1])
        }else{
          tmp <- dplyr::filter(df, date >= cal_dates[i])
        }
      
      # recombine
        if(nrow(tmp)>0){
          if(!exists("out", inherits = FALSE)){
            tmp$cal_date <- cal_dates[i]
            out <- tmp
        }else{
            tmp$cal_date <- cal_dates[i]
            out <- rbind(out,tmp)
        }
        
        }else{
         print("No data:")
         print(cal_dates[i])
        }
    }
    
  # return
    return(out)
}  
#________________________________________________________  
  
#________________________________________________________
# convert ppmv to ug/m^3
# mw = molecular weight g/mol
# t = temperature oC
# p = pressure kPa
  convert_ppmv_ugpmc <- function(ppmv,mw,t,p){

    ug <- (1/(mw*ppmv))*8.3144*(t+273.15)/(p*1000)

    ug <- (1/ug)
 
  # return
    return(ug)
}
#________________________________________________________

#________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
filter_times <- function(times, df){

  ids <- unique(times$id)
 
  # loop ids
    for(i in 1:length(ids)){

      tmp <- dplyr::filter(df,
                           as.character(id) == as.character(times$id[i]),
                           time >= times$start[i],
                           time <= times$end[i])
      
  # if first match
      if(exists("out", inherits = FALSE) == FALSE & nrow(tmp) > 0){
        out <- tmp
      }
      
  # if not first match with data
      if(exists("out", inherits = FALSE) == TRUE & nrow(tmp) > 0){
        out <- rbind(out, tmp)
      }
  # end for loop
    }
    
  # return
    return(out)
}
#________________________________________________________
