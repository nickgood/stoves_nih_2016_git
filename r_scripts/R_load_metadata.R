#________________________________________________________
# Libraries
  library(tidyverse)
  library(readxl)
  library(forcats)
  library(lubridate)
#________________________________________________________

#________________________________________________________
# Load the test matrix
# file <- "data/logs/test_matrix.xlsx"
# out <- load_matrix(file)
# returns a list of 2 data tables
  
load_matrix <- function(file, sheet = "matrix"){
 # read excel file
  out <- as_data_frame(read_excel(path = file, 
                                  sheet = sheet, col_names = TRUE))

 # select first 15 cloumns
  out <- out[,1:15]
  
  # remove rows with no test_id
  out <- dplyr::filter(out, !is.na(test_id))

 # test id
  out <- dplyr::mutate(out, id_test = as.factor(test_id))

 # id number (create simplified id number)
  out <- dplyr::mutate(out, id = as.factor(row_number()))

 # date
  out <- dplyr::mutate(out, date = as.Date(testing_date,
                                           origin = "1899-12-30"))

 # fuel_quant
  out <- dplyr::mutate(out, 
                       fuel_quant = as.numeric(number_pieces))

  out$fuel_quant[28] <- 6 # fixes change in format
  
 # fuel_type
  out <- dplyr::mutate(out, fuel_type = as.factor(tolower(material)))

 # extract test information
  test_info <- dplyr::select(out, id, id_test, date, fuel_type, fuel_quant)

 # convert integrated sample ids to long format
  ids_int <- dplyr::select(out, date, matches(".*id.*")) %>%
             dplyr::select(-test_id) %>%
             tidyr::gather("sample", "id_sample", 2:12) %>%
             dplyr::mutate(sample = as.factor(sample),
                           id_sample = as.factor(id_sample))

 # save to file
  saveRDS(test_info, "../r_files/test_info.RDS")
  saveRDS(ids_int, "../r_files/ids_int.RDS")

 # return
  return(list(test_info, ids_int))
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load metadata
# file <- "../data/logs/metadata.xlsx"
# out <- load_metadata(file)
load_metadata <- function(file, sheet = "metadata"){
 # read excel file
  out <- as_data_frame(read_excel(path = file, 
                                  sheet = sheet,
                                  col_names = TRUE))

  out <- out[-1,1:(ncol(out))]  # remove empty rows and columns

 # read test info
  test_info <- dplyr::select(readRDS("../r_files/test_info.RDS"),
                             id,
                             id_test)
 # date
  out <- dplyr::mutate(out, date = as.Date(as.numeric(date),
                            origin = "1899-12-30"))

 # test id
  out <- dplyr::rename(out, id_test = test_id) %>%
         dplyr::right_join(test_info, by = "id_test")

 # flows
  flows <- dplyr::select(out, 
                         id, id_test, date,
                         matches(".*pax.*|.*white.*|.*red.*|.*carbonyl.*|.*isokinetic.*")) %>%
           tidyr::gather("var", "val", 4:45) %>%
           dplyr::filter(grepl(".*outlet.*", var) == FALSE,
                         grepl(".*average.*", var) == FALSE) %>%
           dplyr::mutate(var = sub("_inlet", "", var)) %>%
           tidyr::separate(var, c("when", "inst", "rep")) %>%
           dplyr::mutate(val = as.numeric(val),
                         id_test = as.factor(id_test),
                         when = as.factor(when),
                         inst = as.factor(inst),
                         rep = as.factor(rep)) %>%
           dplyr::group_by(id, date, id_test, when, inst) %>%
           dplyr::summarise(val = mean(val, na.rm = TRUE)) %>%
           dplyr::group_by(id, date, id_test, inst) %>%
           tidyr::spread(when, val) %>%
           dplyr::ungroup() %>%
           dplyr::mutate(delta = post - pre,
                         mean = (post + pre) / 2)

 # canister pressure
  pressure_can <- dplyr::select(out, id, id_test, date, matches(".*pressure.*")) %>%
                  dplyr::rename(pre = pre_canister_pressure,
                                post = post_canister_pressure) %>%
                  dplyr::mutate(pre = as.numeric(pre),
                                post = as.numeric(post),
                                dp = pre - post,
                                units = "hg")
 # background times
  times_bg <- dplyr::select(out, id, id_test, date, 
                            matches(".*background_start.*|.*background_end")) %>%
              tidyr::gather("var", "val", 4:7) %>%
              tidyr::separate(var, c("when", "type", "time")) %>%
              dplyr::mutate(val = as.numeric(seconds(hms(val)))) %>%
              tidyr::spread("time", "val")

 # test times
  times_test <- dplyr::select(out, id, id_test, date, 
                               matches("^test_start.*|^test_end.*")) %>%
                dplyr::rename(start = test_start,
                              end = test_end) %>%
                dplyr::mutate(start = as.numeric(seconds(hms(start))),
                              end = as.numeric(seconds(hms(end))),
                              dur = (end -start) / (60 * 60))
 # fuel times
  times_fuel <- dplyr::select(out, id, id_test, date,
                              matches("^fuel_added.*|^fuel_remove.*")) %>%
                tidyr::gather("var", "val", 4:17) %>%
                dplyr::rename(time = val) %>%
                dplyr::mutate(time = as.numeric(seconds(hms(time)))) %>%
                tidyr::separate(var, c("type", "when", "rep")) %>%
                dplyr::group_by(id, id_test, date, type, rep) %>%
                tidyr::spread(when, time) %>%
                dplyr::rename(on = added, off = remove) %>%
                dplyr::mutate(dur = (off - on) / (60 * 60))

 # fuel mass
  mass_fuel <- dplyr::select(out, id, id_test, date,
                              matches(".*fuel.*weigh.*")) %>%
               tidyr::gather("var", "val", 4:17) %>%
               dplyr::rename(mass = val) %>%
               dplyr::mutate(mass = as.numeric(mass)) %>%
               tidyr::separate(var, c("type", "when", "rep")) %>%
               dplyr::group_by(id, id_test, date, type, rep, when) %>%
               dplyr::summarise(mass = mean(mass, na.rm = TRUE)) %>%
               tidyr::spread(when, mass) %>%
               dplyr::rename(pre = preweigh, post = postweigh) %>%
               dplyr::mutate(dm = pre - post,
                             units = "g")

 # notes
  notes <- dplyr::select(out, id, id_test, date, notes)

 # save
  saveRDS(flows, "../r_files/flows.RDS")
  saveRDS(pressure_can, "../r_files/pressure_can.RDS")
  saveRDS(times_test, "../r_files/times_test.RDS")
  saveRDS(times_bg, "../r_files/times_bg.RDS")
  saveRDS(times_fuel, "../r_files/times_fuel.RDS")
  saveRDS(mass_fuel, "../r_files/mass_fuel.RDS")
  saveRDS(notes, "../r_files/notes.RDS")

 # return
  return(list(flows, pressure_can, times_bg, times_fuel, mass_fuel, notes)) 

}
#_______________________________________________________________________________
