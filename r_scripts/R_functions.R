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
# plot ef summary
plot_ef_bar <- function(emission_factors, pol_name){
  
  ef_summary <- dplyr::group_by(emission_factors, stove_fuel = paste(stove, ":", fuel)) %>%
                dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE),
                                 min_ef = min(energy_ef_comb, na.rm = TRUE),
                                 max_ef = max(energy_ef_comb, na.rm = TRUE),
                                 std_ef = sd(energy_ef_comb, na.rm = TRUE),
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
          #scale_y_log10() +
          scale_x_discrete(label=function(x) sub(" [: ( :]", "\n (", x)) +
          ggtitle(paste(pol_name, "EF (g/MJ of fuel) ")) +
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
plot_ef_polar_all <- function(emission_factors){
  
  
  ef_summary <- dplyr::distinct(emission_factors) %>%
                dplyr::filter(pol != "co2") %>%
                dplyr::filter(inst != "ions") %>%
                dplyr::mutate(inst = ifelse(inst == "fivegas", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = ifelse(inst == "ecoc", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = factor(inst, levels = c("carbs", "ec", "oc", "grav", "voc", "ch4", "co"))) %>%
                dplyr::group_by_(.dots = c("id", "pol","inst", "stove", "fuel", "fuelcat")) %>% 
                dplyr::summarise(energy_ef_comb = mean(energy_ef_comb, na.rm = TRUE)) %>%
                dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel", "fuelcat")) %>% 
                dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE)) %>%
                dplyr::group_by_(.dots = c("inst", "fuelcat")) %>% 
                dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE))
  
  
  p1 <- ggplot(ef_summary, aes(x = inst, y = mean_ef, fill = inst)) +   
        geom_col(position = "dodge") +
        facet_grid(~ fuelcat) +
        theme_minimal() +
        ylab("") +
        xlab("") +
        coord_polar(theta = "y") +
        scale_y_log10() + 
        theme_bw() +
        theme(text = element_text(size = 25),
              legend.position = "none",
              legend.text = element_text(size = 25),
              legend.key.size = unit(0.5, "cm"),
              axis.text.x = element_text(size = 25),
              strip.text.x = element_text(size = 30),
              strip.text.y = element_text(size = 30))
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_bar_all <- function(emission_factors){
  
  
  ef_summary <- dplyr::distinct(emission_factors) %>%
    dplyr::filter(pol != "co2") %>%
    dplyr::filter(inst != "ions") %>%
    dplyr::mutate(inst = ifelse(inst == "fivegas", as.character(pol), as.character(inst))) %>%
    dplyr::mutate(inst = ifelse(inst == "ecoc", as.character(pol), as.character(inst))) %>%
    dplyr::mutate(inst = factor(inst, levels = c("carbs", "ec", "oc", "grav", "voc", "ch4", "co"))) %>%
    dplyr::group_by_(.dots = c("id", "pol","inst", "stove", "fuel", "fuelcat")) %>% 
    dplyr::summarise(energy_ef_comb = mean(energy_ef_comb, na.rm = TRUE)) %>%
    dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel", "fuelcat")) %>% 
    dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE)) %>%
    dplyr::group_by_(.dots = c("inst", "fuelcat")) %>% 
    dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE))
  
  
  p1 <- ggplot(ef_summary, aes(x = fuelcat, y = mean_ef, fill = inst)) +   
    geom_bar(position = "stack", stat = "identity") +
    theme_minimal() +
    ylab("Emission Factor (g/kJ fuel)") +
    xlab("") +
    scale_y_log10() + 
    theme_bw() +
    coord_flip () +
    scale_fill_brewer(palette = "Set1") + 
    theme(text = element_text(size = 20),
          legend.position = "top",
          legend.text = element_text(size = 20),
          legend.key.size = unit(0.5, "cm"),
          axis.text.x = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          strip.text.y = element_text(size = 25))
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_pie_all <- function(emission_factors, pollutant){
  
  
  ef_summary <- dplyr::distinct(emission_factors) %>%
    dplyr::filter(inst == pollutant) %>%
    dplyr::group_by_(.dots = c("id", "pol", "stove", "fuel", "fuelcat")) %>% 
    dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE)) #%>%
    #dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel", "fuelcat")) %>% 
    #dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE)) %>%
    #dplyr::group_by_(.dots = c("inst", "fuelcat")) %>% 
    #dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE))
  
  
  p1 <- ggplot(ef_summary, aes(x = fuelcat, y = mean_ef, fill = pol)) +   
    geom_bar(position = "stack") +
    coord_polar("y") +
    theme_minimal() +
    ylab("Emission Factor (g/kJ fuel)") +
    xlab("") +
    scale_y_log10() + 
    theme_bw() +
    coord_flip () +
    scale_fill_brewer(palette = "Set1") + 
    theme(text = element_text(size = 20),
          legend.position = "top",
          legend.text = element_text(size = 20),
          legend.key.size = unit(0.5, "cm"),
          axis.text.x = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          strip.text.y = element_text(size = 25))
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_bar_2_all <- function(emission_factors){
  
  
  ef_summary <- dplyr::distinct(emission_factors) %>%
    dplyr::filter(pol != "co2") %>%
    dplyr::filter(inst != "ions") %>%
    dplyr::mutate(inst = ifelse(inst == "fivegas", as.character(pol), as.character(inst))) %>%
    dplyr::mutate(inst = ifelse(inst == "ecoc", as.character(pol), as.character(inst))) %>%
    dplyr::mutate(inst = factor(inst, levels = c("carbs", "ec", "oc", "grav", "voc", "ch4", "co"))) %>%
    dplyr::group_by_(.dots = c("id", "pol","inst", "stove", "fuel", "fuelcat")) %>% 
    dplyr::summarise(energy_ef_comb = mean(energy_ef_comb, na.rm = TRUE)) %>%
    dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel", "fuelcat")) %>% 
    dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE)) %>%
    dplyr::group_by_(.dots = c("inst", "fuelcat")) %>% 
    dplyr::summarise(mean_ef = mean(energy_ef_comb, na.rm = TRUE))
  
  
  p1 <- ggplot(ef_summary, aes(x = inst, y = mean_ef, fill = inst)) + 
    geom_bar(stat = "identity", width = 1) +
    facet_grid(~ fuelcat) +
    theme_minimal() +
    ylab("") +
    xlab("") +
    scale_y_log10() + 
    coord_polar() +
    theme_bw() +
    theme(text = element_text(size = 25),
          legend.position = "none",
          legend.text = element_text(size = 25),
          legend.key.size = unit(0.5, "cm"),
          axis.text.x = element_text(size = 25),
          strip.text.x = element_text(size = 30),
          strip.text.y = element_text(size = 30))
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_polar_2_all <- function(emission_factors){

  ef_summary <- dplyr::distinct(emission_factors) %>%
                dplyr::filter(pol != "co2") %>%
                dplyr::filter(inst != "ions") %>%
                dplyr::mutate(inst = ifelse(inst == "fivegas", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = ifelse(inst == "ecoc", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = factor(inst, levels = c("carbs", "ec", "oc", "grav", "voc", "ch4", "co"))) %>%
                dplyr::group_by_(.dots = c("id", "pol","inst", "stove", "fuel")) %>% 
                dplyr::summarise(energy_ef_comb = mean(energy_ef_comb, na.rm = TRUE)) %>%
                dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel")) %>% 
                dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE),
                                 mean_ef = mean(energy_ef_comb, na.rm = TRUE),
                                 min_ef = min(energy_ef_comb, na.rm = TRUE),
                                 max_ef = max(energy_ef_comb, na.rm = TRUE))
  
  
  p1 <- ggplot(ef_summary, aes(x = factor(inst), y = mean_ef, fill = inst)) +   
        geom_bar(width = 1) +
        facet_grid(stove ~ fuel) +
        coord_polar() +
        theme_minimal() +
        ylab("") +
        xlab("") +
        coord_polar(theta = "y") +
        scale_y_log10() + 
        theme_bw() +
        theme(text = element_text(size = 25),
              legend.position = "none",
              legend.text = element_text(size = 25),
              legend.key.size = unit(0.5, "cm"),
              axis.text.x = element_text(size = 25),
              strip.text.x = element_text(size = 25),
              strip.text.y = element_text(size = 25))

  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_polar <- function(emission_factors){
  
  
  ef_summary <- dplyr::distinct(emission_factors) %>%
                dplyr::filter(pol != "co2") %>%
                dplyr::filter(inst != "ions") %>%
                dplyr::mutate(inst = ifelse(inst == "fivegas", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = ifelse(inst == "ecoc", as.character(pol), as.character(inst))) %>%
                dplyr::mutate(inst = factor(inst, levels = c("carbs", "ec", "oc", "grav", "voc", "ch4", "co"))) %>%
                dplyr::mutate(energy_ef_comb = ifelse(inst == "co", energy_ef_comb/5, energy_ef_comb)) %>%
                dplyr::group_by_(.dots = c("id", "pol","inst", "stove", "fuel")) %>% 
                dplyr::summarise(energy_ef_comb = mean(energy_ef_comb, na.rm = TRUE)) %>%
                dplyr::group_by_(.dots = c("id", "inst", "stove", "fuel")) %>% 
                dplyr::summarise(energy_ef_comb = sum(energy_ef_comb, na.rm = TRUE),
                                 mean_ef = mean(energy_ef_comb, na.rm = TRUE),
                                 min_ef = min(energy_ef_comb, na.rm = TRUE),
                                 max_ef = max(energy_ef_comb, na.rm = TRUE))

  
  p1 <- ggplot(ef_summary, aes(x = inst, y = mean_ef, fill = inst)) +   
        geom_col(position = "dodge") +
        facet_grid(stove ~ fuel) +
        theme_minimal() +
        ylab("") +
        xlab("") +
        coord_polar(theta = "y") +
        theme_bw() +
        theme(text = element_text(size = 25),
              legend.position = "none",
              legend.text = element_text(size = 25),
              legend.key.size = unit(0.5, "cm"),
              axis.text.x = element_text(size = 25),
              strip.text.x = element_text(size = 25),
              strip.text.y = element_text(size = 25))
  
  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot ef summary
plot_ef_box <- function(emission_factors, pol_name){

  p1 <- ggplot(emission_factors, aes(x = fuelcat, y = energy_ef_comb, fill = fuelcat)) +   
    geom_boxplot() +
    theme_bw() +
    ylab("") +
    xlab("") +
    theme_bw() +
    #scale_y_log10() +
    ggtitle(paste(pol_name, "EF (g/MJ of fuel) ")) +
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

    p1 <- ggplot(ef_summary, aes(x = mass_ef_comb, y = mass_ef_comb_2, colour = fuelcat, shape = stove)) + 
          geom_point(size = 3, stroke = 1.5) +
          geom_smooth(method = "rlm") +
          scale_shape_manual(values=1:nlevels(ef_1$stove)) +
          ylab(paste(pol_name_2, "EF (mg/kg of fuel) ")) +
          xlab(paste(pol_name_1, "EF (mg/kg of fuel) ")) +
          scale_x_log10() +
          theme_bw() +
          scale_y_log10() +
          theme(text = element_text(size = 12),
                legend.key = element_rect(fill = 'white'),
                legend.position = "right",
                legend.title = element_blank())

  print(p1)
}
#________________________________________________________

#________________________________________________________
# plot correlation maps
plot_cormap <- function(data, cor_method){

  ef_corr <- round(cor(data[-1],
                       use = "pairwise.complete.obs",
                       method = cor_method), 2)
  
  ef_corr[lower.tri(ef_corr)] <- NA
  ef_corr_l <- melt(ef_corr, na.rm = TRUE)  # fix this function
  ef_corr_l <- ef_corr_l[!(ef_corr_l$value == 1),]
  
  p <- ggplot(data = ef_corr_l, aes(Var2, Var1, fill = value, label = value))+
       geom_tile(color = "white")+
       geom_text(color = "black", size = 3.5) +
       scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Spearman Correlation") +
       theme_bw() + 
       theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1),
             axis.text.y = element_text(size = 14),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank(),
             axis.ticks = element_blank(),
             legend.justification = c(1, 0),
             legend.position = c(0.4, 0.8),
             legend.direction = "horizontal", 
             legend.title = element_text(size = 12),
             legend.text = element_text(size = 16)) +
       guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                                    title.position = "top", title.hjust = 0.5)) +
       coord_fixed()
  return(p)
}
#________________________________________________________

#________________________________________________________
# plot color maps of replicate counts
summarise_reps <- function(emission_factors, type){

  if(type == "pol") {
    replicates <- dplyr::distinct(emission_factors) %>%
                  dplyr::filter(grepl(measure_names, inst)) %>%
                  dplyr::group_by_(.dots = c("pol", "stove", "fuel", "fuelcat")) %>% 
                  dplyr::count() %>%
                  tidyr::spread(pol, n) %>%
                  dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
                  tidyr::gather("pol", "n", 4:ncol(.)) %>%
                  dplyr::mutate(stove_fuel = paste(stove, ":", fuel)) %>%
                  dplyr::mutate(fuelcat = factor(fuelcat, levels = c("wood", "pellets",
                                                                     "charcoal", "advanced")))
  } else {
    replicates <- dplyr::distinct(emission_factors) %>%
                  dplyr::group_by_(.dots = c("inst", "id", "stove", "fuel", "fuelcat")) %>% 
                  dplyr::summarise(conc = mean(conc)) %>%
                  dplyr::group_by_(.dots = c("inst", "stove", "fuel", "fuelcat")) %>% 
                  dplyr::count() %>%
                  tidyr::spread(inst, n) %>%
                  dplyr::mutate_all(funs(replace(., is.na(.), 0))) %>%
                  tidyr::gather("pol", "n", 4:ncol(.)) %>%
                  dplyr::mutate(stove_fuel = paste(stove, ":", fuel)) %>%
                  dplyr::mutate(fuelcat = factor(fuelcat, levels = c("wood", "pellets",
                                                                     "charcoal", "advanced")))
  }
  

  p <- ggplot(data = replicates, aes(stove_fuel, pol, fill = n, label = n))+
       geom_tile(color = "black") +
       geom_text(color = "black", size = 7) +
       scale_fill_gradient2(low = "blue", high = "white",
                            midpoint = 3, limit = c(0, max(replicates$n)), space = "Lab", 
                            name = "Number of replicates") +
       facet_wrap( ~ fuelcat, scales = "free") +
       theme_bw() + 
       theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
             axis.text.y = element_text(size = 12),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank(),
             axis.ticks = element_blank(),
             legend.position = "top",
             legend.direction = "horizontal", 
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 12),
             strip.text.x = element_text(size = 18),
             strip.text.y = element_text(size = 12),
             plot.margin = margin(10, 10, 10, 150)) +
       scale_x_discrete(label=function(x) sub(" [: : :]", "\n", x)) +
       guides(fill = guide_colorbar(barwidth = 20, barheight = 1,
                                    title.position = "top", title.hjust = 0.5)) +
       coord_equal()

  return(p)
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
        dplyr::mutate(mw = ifelse(ions == "S" & !is.na(ions),
                                  mw + 32.065, mw)) 

  # return the molecular weight
  return(pol_properties$mw)
}
#________________________________________________________
#________________________________________________________
# summarize data by category
# default is to group by stove type
isee_summarize <- function(data,
                           group_var = "stove",
                           cat_filter = "",
                           metric = emissions_metric,
                           sample_info = samples){

if(group_var == "stove"){

  q1 <-
    data %>%
    #dplyr::select(-qc) %>% 
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    tidyr::gather("pol", "value", 2:ncol(.)) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, paste0(group_var, "cat"))) %>% 
    dplyr::group_by_(group_var, paste0(group_var, "cat")) %>% 
    dplyr::summarise_if(is.numeric, funs(q1 = quantile), probs = 0.25, na.rm = TRUE)
  
  q3 <-
    data %>%
    #dplyr::select(-qc) %>% 
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    tidyr::gather("pol", "value", 2:ncol(.)) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, paste0(group_var, "cat"))) %>% 
    dplyr::group_by_(group_var, paste0(group_var, "cat")) %>% 
    dplyr::summarise_if(is.numeric, funs(q3 = quantile), probs = 0.75, na.rm = TRUE)

  data_p <-
    data %>%
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, paste0(group_var, "cat"))) %>% 
    dplyr::group_by_(group_var, paste0(group_var, "cat")) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    tidyr::gather("pol", "value", 3:ncol(.)) %>% 
    dplyr::left_join(q1, by = c("stove", "stovecat")) %>% 
    dplyr::left_join(q3, by = c("stove", "stovecat")) 

}else{

  q1 <-
    data %>%
    #dplyr::select(-qc) %>% 
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    tidyr::gather("pol", "value", 2:ncol(.)) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, fuelcat)) %>% 
    dplyr::group_by_("stove",group_var, paste0(group_var, "cat")) %>% 
    dplyr::summarise_if(is.numeric, funs(q1 = quantile), probs = 0.25, na.rm = TRUE)

  q3 <-
    data %>%
    #dplyr::select(-qc) %>% 
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    tidyr::gather("pol", "value", 2:ncol(.)) %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, fuelcat)) %>% 
    dplyr::group_by_("stove",group_var, paste0(group_var, "cat")) %>% 
    dplyr::summarise_if(is.numeric, funs(q3 = quantile), probs = 0.75, na.rm = TRUE)

  data_p <-
    data %>%
    tidyr::spread_("pol", metric) %>%
    na.omit %>%
    dplyr::left_join(sample_info %>% 
                       dplyr::select(id, stove, stovecat, fuel, fuelcat),
                     by = "id") %>%
    dplyr::filter(grepl(cat_filter, fuelcat)) %>% 
    dplyr::group_by_("stove", group_var, paste0(group_var, "cat")) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    tidyr::gather("pol", "value", 4:ncol(.)) %>% 
    dplyr::left_join(q1, by = c("stove", "fuel", "fuelcat")) %>% 
    dplyr::left_join(q3, by = c("stove", "fuel", "fuelcat"))
}

}
#________________________________________________________
