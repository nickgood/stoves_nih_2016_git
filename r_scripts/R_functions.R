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
  time_window_stats <- function(df, date_var, start_time_var, end_time_var, pol_var, val_var, stat_var = "mean", plot = FALSE) {

  df_win <- dplyr::filter(df, date == date_var &
                              time >= (start_time_var) &
                              time >= (end_time_var - 60) &
                              time <= (end_time_var - 30) &
                              pol == pol_var)

 # stat
  if(stat_var == "mean"){
    out <- dplyr::summarise_(df_win, val = paste0("mean(", as.character(val_var), ")"))
    out <- out$val[1]
  }
    
  # stat  
  if(stat_var == "sd"){
    out <- dplyr::summarise_(df_win, val = paste0("sd(", as.character(val_var), ")"))
    out <- out$val[1]
  }

 # plot
  if(plot == TRUE){
    head(df_win)

    p <- ggplot(df_win, aes(datetime, val)) +
                geom_point()

    filename <- paste0("fivegascal_", pol_var,"_", as.character(date_var),".jpeg")

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

    ug <- (1 / (mw * ppmv)) * 8.3144 * (t + 273.15 ) / (p * 1000)

    ug <- (1 / ug)
 
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

#________________________________________________________
# plot mass based emission factors 
plot_mass_ef <- function(df, pol_name){
  
  p1 <- ggplot(df, aes(x = stove_fuel, y = mass_ef, colour = fuel)) +
    geom_point(size = 2) +
    facet_grid(pol ~ stovecat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by stove/fuel combination")) +
    xlab("stove type") +
    ylab("mass based emissions factors (mg/kg of fuel)") +
    theme_minimal() +
    scale_x_discrete(label=function(x) sub(" [: : :].*", "", x)) +
    theme(text = element_text(size=18),
          legend.position = "top", 
          axis.text.x = element_text(angle = 45, vjust = 1.18, hjust = 1, size=7.5),
          panel.spacing = unit(2, "lines"))
  
  p2 <- ggplot(df, aes(x = stove, y = mass_ef, colour = fuel)) +
    geom_point(size = 1) +
    facet_grid(pol ~ stovecat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by stove type")) +
    xlab("stove type") +
    ylab("mass based emissions factors (mg/kg of fuel)") +
    theme_minimal() +
    theme(text = element_text(size=18),
          legend.position = "top", 
          axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1, size=10),
          panel.spacing = unit(2, "lines"))
  
  p3 <- ggplot(df, aes(x = fuel, y = mass_ef, colour = stove)) +
    geom_point(size = 1) +
    facet_grid(pol ~ fuelcat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by fuel type")) +
    xlab("fuel type") +
    ylab("mass based emissions factors (mg/kg of fuel)") +
    theme_minimal() +
    theme(text = element_text(size=18),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=14),
          panel.spacing = unit(2, "lines"))
  
  #print(p1)
  print(p2)
  #print(p3)
}
#________________________________________________________

#________________________________________________________
# plot energy based emission factors 
plot_energy_ef <- function(df, pol_name){
  
  p1 <- ggplot(df, aes(x = stove_fuel, y = energy_ef, colour = fuel)) +
    geom_point(size = 1) +
    facet_grid(pol ~ stovecat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by stove/fuel combination")) +
    xlab("stove type") +
    ylab("fuel energy based emissions factors (g/MJ of fuel)") +
    theme_minimal() +
    scale_x_discrete(label=function(x) sub(" [: : :].*", "", x)) +
    theme(text = element_text(size=18),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1.18, hjust = 1, size=7.5),
          panel.spacing = unit(2, "lines"))
  
  p2 <- ggplot(df, aes(x = stove, y = energy_ef, colour = fuel)) +
    geom_point(size = 1) +
    facet_grid(pol ~ stovecat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by stove type")) +
    xlab("stove type") +
    ylab("fuel energy based emissions factors (g/MJ of fuel)") +
    theme_minimal() +
    theme(text = element_text(size=18),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1, size=10),
          panel.spacing = unit(2, "lines"))
  
  p3 <- ggplot(df, aes(x = fuel, y = energy_ef, colour = stove)) +
    geom_point(size = 1) +
    facet_grid(pol ~ fuelcat, scales = 'free') +
    ggtitle(paste(pol_name, "ef by fuel type")) +
    xlab("fuel type") +
    ylab("fuel energy based emissions factors (g/MJ of fuel)") +
    theme_minimal() +
    theme(text = element_text(size=18),
          legend.position = "top",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=14),
          panel.spacing = unit(2, "lines"))
  
  print(p1)
  print(p2)
  print(p3)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_bar <- function(emission_factors, pol_name){
  
  ef_summary <- dplyr::group_by(emission_factors, stove_fuel = paste(stove, ":", fuel)) %>%
                dplyr::summarise(mean_ef = mean(mass_ef_comb, na.rm = TRUE),
                                 min_ef = min(mass_ef_comb, na.rm = TRUE),
                                 max_ef = max(mass_ef_comb, na.rm = TRUE),
                                 std_ef = sd(mass_ef_comb, na.rm = TRUE),
                                 stove = first(stove),
                                 fuel = first(fuel),
                                 stovecat = first(stovecat),
                                 fuelcat = first(fuelcat))
  
  p1 <- ggplot(ef_summary, aes(x = stove, y = mean_ef, ymax = max_ef,
                               ymin = min_ef, group = fuel, fill = fuel)) +   
          geom_col(position = "dodge") +
          geom_errorbar(position = "dodge", size = 1) +
          facet_grid( ~ fuelcat, scales = 'free') +
          theme_minimal() +
          ylab("") +
          xlab("") +
          theme_bw() +
          scale_y_log10() +
          scale_x_discrete(label=function(x) sub(" [: ( :]", "\n (", x)) +
          ggtitle(paste(pol_name, "EF (mg/kg of fuel) ")) +
          theme(text = element_text(size = 14),
                legend.position = "top",
                legend.text = element_text(size = 10),
                legend.key.size = unit(0.5, "cm"),
                axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 8.5),
                strip.text.x = element_text(size = 16))

  print(p1)
}
#________________________________________________________
#________________________________________________________
# plot ef summary
plot_ef_box <- function(emission_factors, pol_name){

  p1 <- ggplot(emission_factors, aes(x = fuelcat, y = mass_ef_comb, fill = fuelcat)) +   
    geom_boxplot() +
    theme_bw() +
    ylab("") +
    xlab("") +
    theme_bw() +
    scale_y_log10() +
    ggtitle(paste(pol_name, "EF (mg/kg of fuel) ")) +
    theme(text = element_text(size = 18),
          legend.position = "none")
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot correlation
plot_correlation <- function(ef_1, ef_2, pol_name_1, pol_name_2){

  ef_2 <- dplyr::mutate(ef_2, mass_ef_comb_2 = mass_ef_comb)

  ef_summary <- dplyr::left_join(ef_1, dplyr::select(ef_2, id, mass_ef_comb_2),
                                 by = "id")

    p1 <- ggplot(ef_summary, aes(x = mass_ef_comb, y = mass_ef_comb_2, colour = fuelcat)) + 
          geom_point(size = 3) +
          geom_smooth(method = "rlm") +
          ylab(paste(pol_name_2, "EF (mg/kg of fuel) ")) +
          xlab(paste(pol_name_1, "EF (mg/kg of fuel) ")) +
          scale_x_log10() +
          theme_bw() +
          scale_y_log10() +
          theme(text = element_text(size = 18),
                legend.position = "top",
                legend.key.size = unit(0.1, "cm"),
                legend.title = element_blank())

  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot correlation maps
plot_cormap <- function(data, method = "spearman"){
  ef_w <- dplyr::select(emission_factors, id, pol, mass_ef_comb) %>%
          dplyr::distinct() %>%
          dplyr::group_by_(.dots = c("id","pol")) %>% 
          dplyr::summarise(mass_ef_comb = mean(mass_ef_comb, na.rm = TRUE))%>% 
          tidyr::spread(pol, mass_ef_comb)
  
  ef_corr <- round(cor(emission_factors_w,
                       use = "pairwise.complete.obs",
                       method = method), 2)
  
  ef_corr[lower.tri(ef_corr)] <- NA
  ef_corr_l <- tidyr::gather(ef_corr, na.rm = TRUE)
  
  p <- ggplot(data = ef_corr_l, aes(Var2, Var1, fill = value, label = value))+
       geom_tile(color = "white")+
       geom_text(color = "black", size = 7) +
       scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Spearman\nCorrelation") +
       theme_bw() + 
       theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 18, hjust = 1),
             axis.text.y = element_text(size = 18),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank(),
             axis.ticks = element_blank(),
             legend.justification = c(1, 0),
             legend.position = c(0.4, 0.8),
             legend.direction = "horizontal", 
             legend.title = element_text(size = 18),
             legend.text = element_text(size = 18)) +
       guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                                    title.position = "top", title.hjust = 0.5)) +
       coord_fixed()
}
#________________________________________________________

#________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
# appends rep variable
filter_temp <- function(times, df){

  rows <- nrow(times)

 # loop ids
  for(i in 1:rows){
    tmp <- dplyr::filter(df,
                         as.character(id) == as.character(times$id[i]),
                         time >= times$start[i],
                         time <= times$end[i]) %>%
           dplyr::mutate(rep = times$rep[i])

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

#________________________________________________________
# Calculate the molecular weight of study pollutants
# Molecuar weights are calculated using the average
# standard atomic weights of each individual elements
#
# Atomic weights are from the NIST Physical Reference Data Website
calc_mw <- function(pol_properties){

  pol_properties$mw <- (pol_properties$num_c * 12.0106) +
                       (pol_properties$num_h * 1.007975) +
                       (pol_properties$num_o * 15.9994)

  pol_properties <- dplyr::mutate(pol_properties,
                                  mw = ifelse(ions == "Na" & !is.na(ions),
                                  mw + 22.98976928, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "N" & !is.na(ions),
                                  mw + 14.006855, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "K" & !is.na(ions),
                                  mw + 39.0983, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "Mg" & !is.na(ions),
                                  mw + 24.3055, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "Ca" & !is.na(ions),
                                  mw + 40.078, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "Cl" & !is.na(ions),
                                  mw + 35.4515, mw)) %>%
        dplyr::mutate(mw = ifelse(ions == "Cl" & !is.na(ions),
                                  mw + 32.0675, mw)) 

  # return the molecular weight
  return(pol_properties$mw)
}
#________________________________________________________
