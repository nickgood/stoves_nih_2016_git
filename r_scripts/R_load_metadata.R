#________________________________________________________
# Libraries
  library(tidyverse)
  library(readxl)
  library(forcats)
#________________________________________________________

#________________________________________________________
# Load sample tracking log
# file <- "data/logs/Sample Tracking Log.xlsx"
# df <- load.samples(file)
load_samples <- function(file, sheet = "Completed Test Log"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 1)

  df <- df[1:10,]
  df <- as.data.frame(t(df[-1]))

  names(df) <- c("num",
                 "date",
                 "id",
                 "blank",
                 "stove",
                 "fuel",
                 "type",
                 "person",
                 "notes",
                 "rep")

  df_dat <- subset(df, select = date)
  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  df_char <- subset(df, select = notes)
  df_char <- as.data.frame(lapply(df_char, 
                                  function(x) as.character(x)), stringsAsFactors=FALSE) 

  df_num <- subset(df, select = c(num))
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x)))) 

  df_fac <- subset(df, select = c(-date, -notes, -num))

  out <- dplyr::bind_cols(df_dat, df_num, df_char, df_fac)
      
 # return
  return(out)
}
#________________________________________________________
 
#________________________________________________________
# Load sample batch log
# file <- "../data/logs/Transcribed Batch Fed Stove Sampling Forms.xlsx"
# df <- load_batch(file) 
load_batch <- function(file, sheet = "Batch Fed Sampling Forms"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)

  df <- df[1:53,]

  df <- as.data.frame(t(df[c(-1,-2)]))

  names(df) <- c("id",
                 "stove",
                 "fuel",
                 "date",
                 "person",
                 "lab_t",
                 "lab_p",
                 "lab_rh",
                 "wgt_pot_a",
                 "wgt_pot_b",
                 "wgt_stove_pre",
                 "wgt_fuel",
                 "wgt_withstarter",
                 "wgt_prerefuel",
                 "wgt_postrefuel",
                 "wgt_refueled",
                 "wgt_preshutdown",
                 "time_ignite",
                 "time_sample",
                 "time_refuel",
                 "time_continue",
                 "time_shutdown",
                 "time_end",
                 "wgt_on_1",
                 "time_start_1",
                 "time_end_1",
                 "wgt_off_1",
                 "pot_1",
                 "wgt_on_2",
                 "time_start_2",
                 "time_end_2",
                 "wgt_off_2",
                 "pot_2",
                 "wgt_on_3",
                 "time_start_3",
                 "time_end_3",
                 "wgt_off_3",
                 "pot_3",
                 "wgt_on_4",
                 "time_start_4",
                 "time_end_4",
                 "wgt_off_4",
                 "pot_4",
                 "wgt_on_5",
                 "time_start_5",
                 "time_end_5",
                 "wgt_off_5",
                 "pot_5",
                 "wgt_ashpot",
                 "wgt_ashandpot",
                 "wgt_stove_end",
                 "other_stoves",
                 "notes")

  df_dat <- subset(df, select = date)

  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^wgt|^lab_t|^lab_p",colnames(df))==TRUE)
  
  df_num <- subset(df, select = cols)

  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)
  
  df_time <- subset(df, select = cols)

  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60)) 

  df_rh <- subset(df, select = lab_rh)

  df_rh$lab_rh <- as.character(df_rh$lab_rh)

  df_rh$lab_rh <- gsub("% RH", "", df_rh$lab_rh)

  df_rh$lab_rh <- as.numeric(df_rh$lab_rh)

  df_rh$lab_rh <- ifelse(df_rh$lab_rh<1,df_rh$lab_rh*100,df_rh$lab_rh)
        
  df_char <- subset(df, select = notes)

  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)))

  df_fac <- subset(df, select = c(id, stove, fuel, person))
  
  out <- dplyr::bind_cols(df_dat, df_num, df_time, df_rh, df_char, df_fac)

 # return
  return(out)
}
#________________________________________________________  
 
#________________________________________________________
# Load five gas calibration
# file <- "../data/logs/Transcribed Emissions Tester 1 Calibration Log.xlsx"
# df <- load_fivegascal(file)
load_fivegascal <- function(file, sheet = "Sheet1"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)

  df <- df[1:32,]

  df <- as.data.frame(t(df[c(-1,-2)])) 

  names(df) <- c("date",
                 "conc_zero",
                 "time_start_zero",
                 "time_end_zero",
                 "conc_co2",
                 "time_start_co2",
                 "time_end_co2",
                 "conc_co",
                 "time_start_co",
                 "time_end_co",
                 "conc_ch4",
                 "time_start_ch4",
                 "time_end_ch4",
                 "conc_o2",
                 "time_start_o2",
                 "time_end_o2",
                 "conc_nox",
                 "time_start_nox",
                 "time_end_nox",
                 "preflow_inlet_1",
                 "preflow_inlet_2",
                 "preflow_inlet_3",
                 "preflow_exit_1",
                 "preflow_exit_2",
                 "preflow_exit_3",
                 "postflow_inlet_1",
                 "postflow_inlet_2",
                 "postflow_inlet_3",
                 "postflow_exit_1",
                 "postflow_exit_2",
                 "postflow_exit_3",
                 "notes")

  df_dat <- subset(df, select = date)
  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^conc|^preflow|^postflow",colnames(df))==TRUE)

  df_num <- subset(df, select = cols)

  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)

  df_time <- subset(df, select = cols)

  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60)) 

  df_char <- subset(df, select = notes)
  
  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)), stringsAsFactors=FALSE)

  out <- dplyr::bind_cols(df_dat, df_num, df_time, df_char)
    
 # return
  return(out)
}
#________________________________________________________  
    
#________________________________________________________
# Load fivegas and filter metadata
# file <- "../data/logs/Transcribed Emissions Tester 1 Data Sheets.xlsx"
# df <- load_fivegas_filter_meta(file)
load_fivegas_filter_meta <- function(file, sheet = "Tester 1 Data Sheet"){
  
  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)

  df <- df[1:50,]
  
  df <- as.data.frame(t(df[c(-1,-2)])) 

  names(df) <- c("date",
                 "id",
                 "person",
                 "preflow_cart_white_1",
                 "preflow_cart_white_2",
                 "preflow_cart_white_3",
                 "preflow_cart_white_avg",
                 "preflow_cart_orange_1",
                 "preflow_cart_orange_2",
                 "preflow_cart_orange_3",
                 "preflow_cart_orange_avg",
                 "preflow_cart_green_1",
                 "preflow_cart_green_2",
                 "preflow_cart_green_3",
                 "preflow_cart_green_avg",
                 "preflow_cart_red_1",
                 "preflow_cart_red_2",
                 "preflow_cart_red_3",
                 "preflow_cart_red_avg",
                 "time_start_fivegas_prebg",
                 "time_end_fivegas_prebg",
                 "time_start_fivegas_sample",
                 "time_end_fivegas_sample",
                 "time_start_cart_white_orange",
                 "time_end_cart_white_orange",
                 "time_start_cart_green_red",
                 "time_end_cart_green_red",
                 "time_start_voc",
                 "time_end_voc",
                 "time_start_fivegas_post_bg",
                 "time_end_fivegas_post_bg",
                 "postflow_cart_white_1",
                 "postflow_cart_white_2",
                 "postflow_cart_white_3",
                 "postflow_cart_white_avg",
                 "postflow_cart_orange_1",
                 "postflow_cart_orange_2",
                 "postflow_cart_orange_3",
                 "postflow_cart_orange_avg",
                 "postflow_cart_green_1",
                 "postflow_cart_green_2",
                 "postflow_cart_green_3",
                 "postflow_cart_green_avg",
                 "postflow_cart_red_1",
                 "postflow_cart_red_2",
                 "postflow_cart_red_3",
                 "postflow_cart_red_avg",
                 "voc_start_p",
                 "voc_end_p",
                 "notes")

  df_dat <- subset(df, select = date)

  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^pre|^post|^cart|^voc",colnames(df))==TRUE)

  df_num <- subset(df, select = cols)
  
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)

  df_time <- subset(df, select = cols)

  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60)) 

  df_char <- subset(df, select = notes)

  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)), stringsAsFactors=FALSE)

  df_fac <- subset(df, select = c(id, person))
    
  out <- dplyr::bind_cols(df_dat, df_num, df_time, df_char, df_fac)
    
 # return
  return(out)
} 
#________________________________________________________
  
#________________________________________________________
# Load five gas and filter metadata
# file <- "../data/logs/Transcribed Emissions Tester 2 Calibration Log.xlsx"
# df <- load_co2_cal_flows_meta(file)
load_co2_cal_flows_meta <- function(file, sheet = "Sheet1"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)
      
  df <- df[1:32,]

  df <- as.data.frame(t(df[c(-1,-2)]))

  names(df) <- c("date",
                 "sensor_1_zero_std",
                 "sensor_1_zero_pre",
                 "sensor_1_zero_post",
                 "sensor_1_co2_std",
                 "sensor_1_co2_pre",
                 "sensor_1_co2_post",
                 "sensor_2_zero_std",
                 "sensor_2_zero_pre",
                 "sensor_2_zero_post",
                 "sensor_2_co2_std",
                 "sensor_2_co2_pre",
                 "sensor_2_co2_post",
                 "preflow_smps_1",
                 "preflow_smps_2",
                 "preflow_smps_3",
                 "preflow_paxinlet_1",
                 "preflow_paxinlet_2",
                 "preflow_paxinlet_3",
                 "preflow_paxexit_1",
                 "preflow_paxexit_2",
                 "preflow_paxexit_3",
                 "postflow_smps_1",
                 "postflow_smps_2",
                 "postflow_smps_3",
                 "postflow_paxinlet_1",
                 "postflow_paxinlet_2",
                 "postflow_paxinlet_3",
                 "postflow_paxexit_1",
                 "postflow_paxexit_2",
                 "postflow_paxexit_3",
                 "notes")

  df_dat <- subset(df, select = date)

  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  df_num <- subset(df, select = c(-date, -notes))

  df_num$sensor_1_zero_std <- as.numeric(0)  # convert standard acronym to zero

  df_num$sensor_2_zero_std <- as.numeric(0)  # convert standard acronym to zero

  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  df_char <- subset(df, select = notes)

  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)), stringsAsFactors=FALSE)

  out <- dplyr::bind_cols(df_dat, df_num, df_char)
        
 # return
  return(out)
}
#________________________________________________________
 
#________________________________________________________
# Load flows and five gas metadata
# file <- "../data/logs/Transcribed Emissions Tester 2 Data Sheets.xlsx"
# df <- load_flow_fivegas_meta(file)
load_flow_fivegas_meta <- function(file, sheet = "Tester 2 Data Sheet"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)

  df <- df[1:26,]

  df <- as.data.frame(t(df[c(-1,-2)])) 

  names(df) <- c("date",
                 "id",
                 "person",
                 "preflow_carb_1",
                 "preflow_carb_2",
                 "preflow_carb_3",
                 "preflow_carb_avg",
                 "preflow_iso_1",
                 "preflow_iso_2",
                 "preflow_iso_3",
                 "time_start_smps_pax_bg_pre",
                 "time_end_smps_pax_bg_pre",
                 "time_start_smps_pax",
                 "time_end_smps_pax",
                 "time_start_carb",
                 "time_end_carb",
                 "time_start_smps_pax_bg_post",
                 "time_end_smps_pax_bg_post",
                 "postflow_carb_1",
                 "postflow_carb_2",
                 "postflow_carb_3",
                 "postflow_carb_avg",
                 "postflow_iso_1",
                 "postflow_iso_2",
                 "postflow_iso_3",
                 "notes")

  df_dat <- subset(df, select = date)

  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^preflow|^postflow",colnames(df))==TRUE)

  df_num <- subset(df, select = cols)

  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)

  df_time <- subset(df, select = cols)

  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60))  

  df_char <- subset(df, select = notes)

  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)), stringsAsFactors=FALSE)

  df_fac <- subset(df, select = c(id, person))

  out <- dplyr::bind_cols(df_dat, df_num, df_time, df_char, df_fac)
    
 # return
  return(out)
} 
#________________________________________________________
  
#________________________________________________________
# Load wood stove log
# file <- "//.data/logs/Transcribed Wood Stove Sampling Forms.xlsx"
# df <- load_wood(file)
load_wood <- function(file, sheet = "Wood Sampling Form"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)

  df <- df[1:72,]

  df <- as.data.frame(t(df[c(-1,-2)]))

  names(df) <- c("id",
                 "stove",
                 "fuel",
                 "date",
                 "person",
                 "lab_t",
                 "lab_p",
                 "lab_rh",
                 "wgt_pot_a",
                 "wgt_pot_b",
                 "wgt_fuel",
                 "wgt_starter",
                 "time_ignite",
                 "time_refuel_1",
                 "time_refuel_2",
                 "time_refuel_3",
                 "time_refuel_4",
                 "time_refuel_5",
                 "time_refuel_6",
                 "time_refuel_7",
                 "time_shutdown",
                 "time_end",
                 "wgt_on_1",
                 "time_start_1",
                 "time_end_1",
                 "wgt_off_1",
                 "pot_1",
                 "wgt_on_2",
                 "time_start_2",
                 "time_end_2",
                 "wgt_off_2",
                 "pot_2",
                 "wgt_on_3",
                 "time_start_3",
                 "time_end_3",
                 "wgt_off_3",
                 "pot_3",
                 "wgt_on_4",
                 "time_start_4",
                 "time_end_4",
                 "wgt_off_4",
                 "pot_4",
                 "wgt_on_5",
                 "time_start_5",
                 "time_end_5",
                 "wgt_off_5",
                 "pot_5",
                 "wgt_on_6",
                 "time_start_6",
                 "time_end_6",
                 "wgt_off_6",
                 "pot_6",
                 "wgt_on_7",
                 "time_start_7",
                 "time_end_7",
                 "wgt_off_7",
                 "pot_7",
                 "wgt_on_8",
                 "time_start_8",
                 "time_end_8",
                 "wgt_off_8",
                 "pot_8",
                 "wgt_on_9",
                 "time_start_9",
                 "time_end_9",
                 "wgt_off_9",
                 "pot_9",
                 "wgt_ashpot_lid",
                 "wgt_ashpot_unusedfuel",
                 "wgt_ashpot_char_ash",
                 "other_stoves",
                 "notes")

  df_dat <- subset(df, select = date)

  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

 cols <- subset(colnames(df), grepl("^wgt|^lab",colnames(df))==TRUE)

  df_num <- subset(df, select = cols)

  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)

  df_time <- subset(df, select = cols)

  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60)) 

  df_char <- subset(df, select = notes)

  df_char <- as.data.frame(lapply(df_char,
                                  function(x) as.character(x)), stringsAsFactors=FALSE)

  cols <- subset(colnames(df), grepl("^id$|^stove$|^fuel$|^person$|^pot|^other_stoves$", colnames(df))==TRUE)

  df_fac <- subset(df, select = cols)

  out <- dplyr::bind_cols(df_dat, df_num, df_time, df_char, df_fac)
    
 # return
  return(out)
}
#________________________________________________________   
  
#________________________________________________________
# load kb qc data
load_qc_kb <- function(file , grep_str){
 # read csv file
  notes_kb <- read_csv(file)
    
 # rename columns
  notes_kb <- dplyr::rename(notes_kb, inst = instrument)

 # filter for instrument
  notes_kb <- dplyr::filter(notes_kb, grepl(grep_str, notes_kb$inst) == TRUE)

 # classes
  notes_kb <- dplyr::mutate(notes_kb, 
                            id = factor(id),
                            inst = factor(inst),
                            qc = factor(qc, levels = c("bad", "maybe", "ok")))
 # return
  return(notes_kb)
}
#________________________________________________________   
