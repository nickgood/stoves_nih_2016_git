#________________________________________________________
  library(dplyr)
  library(tidyr)
#________________________________________________________

#________________________________________________________
# 
  is_outlier <- function(x) {
    out <- (x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | 
    (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
  return(out)
}
#________________________________________________________
  
#________________________________________________________
# convert POSIXct to seconds of day
  posixct_secsofday <- function(x) {
    # convert to character
      x <- as.character(x)
      
    # time to seconds of day
      out <- as.numeric(substr(x,12,13))*60*60 + 
              as.numeric(substr(x,15,16))*60 +
              as.numeric(substr(x,18,19))
    
    # return
      return(out)
  }
#________________________________________________________
  
#________________________________________________________
# time window stats
  time_window_stats <- function(df, date_var, start_time_var, end_time_var, pol_var, stat_var = "mean", plot = FALSE) {
    
  # filter (penultimate 30 seconds of scan)
    df_win <- dplyr::filter(df, date == date_var &
                              time >= (start_time_var) &
                              time >= (end_time_var - 60) &
                              time <= (end_time_var - 30) &
                              pol == pol_var)
    
  # stat
    if(stat_var == "mean"){
      out <- mean(df_win$val, na.rm = TRUE)
    }
    if(stat_var == "sd"){
      out <- sd(df_win$val, na.rm = TRUE)
    }
  # plot
    if(plot == TRUE){
      head(df_win)
      p <- ggplot(df_win, aes(datetime, val)) + geom_point()
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
  # extract ordered dates
    cal_dates <- unique(arrange(cal, date)$date)
  # number of calibrations
    num <- length(cal_dates)
  # loop calibrations
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
convert_ppmv_ugpmc <- function(ppmv,mw,t,p){
  # mw = molecular weight g/mol
  # t = temperature oC
  # p = pressure kPa
  
  # convert
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
  # get ids for time windows
    ids <- unique(times$id)
    
  # loop ids
    for(i in 1:length(ids)){
      # filter out times
        tmp <- dplyr::filter(df, as.character(id) == as.character(times$id[i]),
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
    }
        
  # return
      return(out)
}



  
  
  
  
 




  
  