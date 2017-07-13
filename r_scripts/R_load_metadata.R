#_______________________________________________________________________________
# libraries
  library(tidyverse)
  library(readxl)
  library(forcats)
  library(lubridate)
#_______________________________________________________________________________

#_______________________________________________________________________________
# source files
  source("../r_scripts/R_functions.R")
#_______________________________________________________________________________

#_______________________________________________________________________________
# load fuel prep file
# file <- "../data/logs/Fuel Prep Final.xlsx"
# sheet <- "Fuel Prep"
# out <- load_fuel_prep(file)
load_fuel_prep <- function(file = "../data/logs/Fuel Prep Final.xlsx", sheet = "Fuel Prep"){
 # load raw file
  out <- read_excel(path = file, sheet = sheet, col_names = TRUE, skip = 0)
 # clean up
  out <- out[-(1:13),1:19]
  out <- dplyr::filter(out, !is.na(fuel_id))
 # rename
  out <- dplyr::rename(out, date_test = date,
                            fuel_id = id,
                            order = test_order,
                            mc_meas = mc_actual_test,
                            mass_end = mass_final,
                            mass_target = target_mass,
                            mass_start = mass_original,
                            mass_wood_start = kiln_wood_original,
                            mass_wood_post = kiln_wood_post,
                            mc_start = kiln_based_original_mc,
                            mass_dry = calculated_kiln_dry_mass,
                            mass_wet = weight_after_soak,
                            date_wet_start = date_into_water_a,
                            time_wet_start = time_into_water_a,
                            date_wet_end = date_out_water_a,
                            time_wet_end = time_out_water_a,
                            dur_wt = soak_hours_a,
                            notes = notes)
 # classes
  out <- dplyr::mutate_at(out,
                         .cols = vars(starts_with("date")),
                         .funs = excel_date) %>%
         dplyr::mutate_at(.cols = vars(matches("^order.*|^mc.*|mass.*|dur.*")),
                          .funs = as.numeric) %>%
         dplyr::mutate_at(.cols = vars(starts_with("time")),
                          .funs = excel_time)

 # split fuel id string

 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load tester 1
# file <- "../data/logs/Tester 1 Data Log Final.xlsx"
# sheet  <- "Tester 1 Data Sheet"
# df <- load_test_one(file)
load_test_one <- function(file = "../data/logs/Tester 1 Data Log Final.xlsx", sheet = "Tester 1 Data Sheet"){
 # read file
  out <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)
 # transpose
  out <- out[1:50,]
 # transpose and trim
  out <- as_tibble(t(out[c(-1,-2)])) 
 # rename columns
  names(out) <- c("date",
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
 # classes
  out <- dplyr::mutate_at(out,
                          .cols = vars(starts_with("date")),
                          .funs = excel_date) %>%
         dplyr::mutate_at(.cols = vars(matches("^pre.*|^post.*|^cart.*|^voc.*")),
                          .funs = as.numeric) %>%
         dplyr::mutate_at(.cols = vars(starts_with("time")),
                          .funs = excel_time) %>%
         dplyr::select(-matches(".*_red_.*|.*_green_.*"))
 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load tester 2 data
# file <- "../data/logs/Tester 2 Data Log Final.xlsx"
# sheet <- "Tester 2 Data Sheet"
# df <- load_test_two(file)
load_test_two <- function(file = "../data/logs/Tester 2 Data Log Final.xlsx",
                          sheet = "Tester 2 Data Sheet"){
 # read file
  out <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 0)
 # select columns
  out <- out[1:26,]
 # transpose
  out <- as_tibble(t(out[c(-1,-2)]))
 # rename columns
  names(out) <- c("date",
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
  # filter blank rows
  out <- dplyr::filter(out, !is.na(id))
  
 # classes
  out <- dplyr::mutate_at(out,
                          .cols = vars(starts_with("date")),
                          .funs = excel_date) %>%
         dplyr::mutate_at(.cols = vars(matches("^preflow.*|^postflow.*")),
                          .funs = as.numeric) %>%
         dplyr::mutate_at(.cols = vars(starts_with("time")),
                          .funs = excel_time) %>%
         dplyr::select(-matches(".*_red_.*|.*_green_.*"))

 # return
  return(out)
} 
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load flows and five gas metadata
#file <- "../data/logs/Tester 2 Cal Log Final.xlsx"
#out <- load_cal_two(file)
load_cal_two <- function(file = "../data/logs/Tester 2 Cal Log Final.xlsx"){
 # read file
  out <- read_excel(path = file, col_names = FALSE, skip = 0)
 # select rows
  out <- out[1:38,]
 # tranpose and drop unused columns
  out <- as_data_frame(t(out[c(-1,-2)]))
 # name columns
   names(out) <- c("date",
                  "zero_1",
                  "zero_1_pre",
                  "zero_1_post",
                  "co2_1_conc",
                  "co2_1_pre",
                  "co2_1_post",
                  "zero_2",
                  "zero_2_pre",
                  "zero_2_post",
                  "co2_2_conc",
                  "co2_2_pre",
                  "co2_2_post",
                  "smps_preflow_1",
                  "smps_preflow_2",
                  "smps_preflow_3",
                  "pax_preflow_1",
                  "pax_preflow_2",
                  "pax_preflow_3",
                  "pax_preflow_exit_1",
                  "pax_preflow_exit_2",
                  "pax_preflow_exit_3",
                  "smps_postflow_1",
                  "smps_postflow_2",
                  "smps_postflow_3",
                  "pax_postflow_1",
                  "pax_postflow_2",
                  "pax_postflow_3",
                  "pax_postflow_exit_1",
                  "pax_postflow_exit_2",
                  "pax_postflow_exit_3",
                  "dusttrak_preflow_1",
                  "dusttrak_preflow_2",
                  "dusttrak_preflow_3",
                  "dusttrak_postflow_1",
                  "dusttrak_postflow_2",
                  "dusttrak_postflow_3",
                  "notes")

 # classes
  out <- dplyr::mutate_at(out,
                          .cols = vars(starts_with("date")),
                          .funs = excel_date) %>%
         dplyr::mutate_at(.cols = vars(matches(".*pre.*|.*post.*")),
                          .funs = as.numeric)

 # return
 return(out)
} 
#_______________________________________________________________________________