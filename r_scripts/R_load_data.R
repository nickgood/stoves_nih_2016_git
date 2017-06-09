#_______________________________________________________________________________
# Libraries
  library(tidyverse)
  library(readxl)
  library(reshape)
#_______________________________________________________________________________

#_______________________________________________________________________________
# load co2 file
# file <- "../data/co2/20170316_LL1_DT_CO2.csv"
load_co2_file <- function(file){
 # print
  print(file)
 # read file
  out <- read_csv(file = file, col_names = FALSE, skip = 7)
 # name columns
  names(out) <- c("date", "time", "ch1", "ch2", "ch3", "ch4")
 # classes
  out <- dplyr::mutate(out,
                       datetime = as.POSIXct(date, format = "%m/%d/%Y"),
                       datetime = datetime + time,
                       date = as.Date(date, format = "%m/%d/%Y"),
                       time = as.numeric(time),
                       id = (strsplit(basename(file), "_")[[1]])[2])
 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load ECOC file
# file <- "../data/ecoc/20170110_ECOC.csv"
load_ecoc_file <- function(file){
 # classes
  classes <- c("character",
                "factor",
                rep("numeric",20),
                "character",
                "character",
                rep("numeric",2),
                rep("factor",2),
                rep("numeric",3),
                "character",
                rep("numeric",2),
                "factor",
                rep("numeric",2),
                "factor",
                rep("numeric",4),
                rep("character",3))

  ecoc <- read.csv(file, header = TRUE, colClasses = classes, 
                   fill = TRUE, na.strings = c("-", "na"))

  ecoc <- dplyr::rename(ecoc, time = Time) %>%
          dplyr::mutate(time = as.character(as.POSIXct(strptime(time, "%I:%M:%S %p"))),
                        time = as.numeric(substr(time,12,13))*60*60 +
                               as.numeric(substr(time,15,16))*60 +
                               as.numeric(substr(time,18,19)))

  ecoc <- dplyr::rename(ecoc, date = Date,
                        ecoc_id = Sample.ID) %>%
          dplyr::mutate(date = as.Date(date, "%m/%d/%Y"),
                               datetime = as.POSIXct(as.character(date)))

 # determine type (test, pilot or NA)
  ecoc <- dplyr::mutate(ecoc,
                        type = as.character(ecoc_id),
                        type = sub(".*india.*", NA, type, ignore.case = TRUE),
                        type = sub("^C11-.*", "test", type),
                        type = sub(".*blank.*|.*start.*", "test", type, ignore.case = TRUE),
                        type = sub(".*BK.*|.*BG.*|^JAV.*", "pilot", type, ignore.case = TRUE),
                        type = sub("^BA.*|.*BA$", "test", type, ignore.case = TRUE),
                        type = sub("^B[0-9].*", "test", type, ignore.case = TRUE),
                        type = sub("^B63A$|^B63E$", "pilot", type),
                        type = sub("^P.*", "pilot", type),
                        type = sub("^[A-Z]-[0-9].*|^[A-Z] [0-9].*|^[0-9][A-Z]-.*|^[0-9][0-9][A-Z]-.*", "test", type),
                        type = sub("^G.*", "bg", type))

 # determine cassette (a, e or NA)
  ecoc <- dplyr::mutate(ecoc,
                        cassette = as.character(ecoc_id),
                        cassette = sub("^A-2016-2-15$|^E-2016-2-2 B9-BA$|^G 06-07-2016$",
                                   NA, cassette),
                        cassette = sub("^30A-3$", "e", cassette),
                        cassette = sub(".*-A.*|.*[0-9]A$", "a", cassette),
                        cassette = sub(".*-E.*|.*[0-9]E$", "e", cassette),
                        cassette = sub(".*bq.*|.*blank.*", NA, cassette, ignore.case = TRUE))

 # extract ids
  ecoc <- dplyr::mutate(ecoc,
                         id = as.character(ecoc_id),
                         id = sub("^B63A$|^B63E$", "lab_blank", id),
                         id = sub("5L-[A-Z]$", "5C", id),
                         id = sub("^G7E$", "G7", id),
                         id = sub("-[A-Z] repeat$", "", id),
                         id = sub(".*P5-A$", "5", id),
                         id = sub(".*india.*", NA, id, ignore.case = TRUE),
                         id = sub(".*blank.*|.*start.*", "system_blank", id, ignore.case = TRUE),
                         id = sub("^BG.*|.*BQ.*|^BK.*|.*BA$", "lab_blank", id),
                         id = gsub("^P.*", NA, id),
                         id = sub("-[0-9]$", "", id),
                         id = sub("-[A-Z] [A-Z]-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$", "", id),
                         id = gsub("^[A-Z]-[0-9][0-9][0-9][0-9]-[0-9]-[0-9] |-[A-Z]$", "", id),
                         id = sub("^[A-Z]-[0-9][0-9][0-9][0-9]-[0-9]-[0-9][0-9] ", "", id),
                         id = sub("^[A-Z]-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9] ", "", id),
                         id = sub("^[A-Z]-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ", "", id),
                         id = sub("^[A-Z] [0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9] ", "", id),
                         id = sub("^[A-Z]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9] ", "", id),
                         id = sub("^[A-Z] [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ", "", id))

 # rename columns
  names(ecoc) <- gsub("\\.$", "", colnames(ecoc))
  names(ecoc) <- tolower(gsub("\\.\\.", "_", colnames(ecoc)))
  names(ecoc) <- tolower(gsub("\\.", "_", colnames(ecoc)))
  
 # set classes
  ecoc <- dplyr::mutate(ecoc,
                        ecod_id = as.factor(ecoc_id),
                        id = as.factor(id),
                        type = as.factor(type),
                        cassette = as.factor(cassette))

 # return
  return(ecoc)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load fivegas file
# file <- "../data/fivegas/20170316_BG1_5GAS.csv"
# df <- load_fivegas_file(file)
load_fivegas_file <- function(file){
  print(file)
 # load 
  out <- read_tsv(file, col_names = TRUE)
 # rename
  names(out) <- c("ch4","o2","nox","co2", "co", "time_s", "datetime")
 # datetime format
  out <- dplyr::mutate(out, 
                       time = as.numeric(substr(datetime, 12, 13)) * 60 * 60 +
                              as.numeric(substr(datetime, 15, 16)) * 60 +
                              as.numeric(substr(datetime,18, 19)),
                       datetime = as.POSIXct(datetime,
                                             format = "%m/%d/%Y %H:%M:%OS"),
                                             date = as.Date(datetime),
                       id = (strsplit(basename(file), "_")[[1]])[2])
 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load gravimetric file
# file <- "../data/grav/Teflon Weight Log Final.xlsx"
# sheet <- "Teflon Filter Weights"
# out <- load_grav_file(file) 
load_grav_file <- function(file = "../data/grav/Teflon Weight Log Final.xlsx",
                           sheet = "Teflon Filter Weights"){
 # read file
  out <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 1)
 # transpose
  out <- as_tibble(t(out[1:29,-1]))
 # rename columns
  names(out) <- c("id_filter", 
                  "date_pre",
                  "person_pre",
                  "toc_pre",
                  "rh_pre",
                  "phpa_pre",
                  "wgt_cal_pre",
                  "id_blank",
                  "wgt_blank_avg_pre",
                  "wgt_pre_1",
                  "wgt_pre_2",
                  "wgt_pre_3",
                  "wgt_pre_avg",
                  "date_post",
                  "id_grav",
                  "time_post",
                  "person_post",
                  "toc_post",
                  "rh_post",
                  "phpa_post",
                  "wgt_cal_post",
                  "wgt_blank_avg_post",
                  "wgt_post_1",
                  "wgt_post_2",
                  "wgt_post_3",
                  "wgt_post_avg",
                  "wgt_dif",
                 "notes",
                  "lod")

 # classes
  out <- dplyr::mutate_at(out,
                          .cols = vars(starts_with("date")),
                          .funs = excel_date) %>%
         dplyr::mutate_at(.cols = vars(matches("^toc_.*|^rh_.*|phpa_.*|wgt_.*|lod_")),
                    .funs = as.numeric) %>%
         dplyr::mutate_at(.cols = vars(starts_with("time")),
                    .funs = excel_time) %>%
         dplyr::mutate(id = sub("-.*", "", id_grav)) %>%
         dplyr::filter(!is.na(id))

 # return 
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load ions and carbonyls file
# file <- "../data/ions/20161230_IONS.xls"
# set sheet to "ug" or "ug_m3"
load_ions_file <- function(file, sheet = "ug"){

  df <- read_excel(path = file, sheet = sheet, col_names = TRUE)
    
  # fix column names
    names(df)[1] <- "id_ions"
    names(df) <- tolower(colnames(df))
    names(df) <- sub(" \\(ug\\)|\\(ug c\\)", "", colnames(df))
    names(df) <- gsub("/|-|,", "_", colnames(df))

  # test type
    df$type <- df$id_ions
    df$type <- ifelse(grepl("[0-9]",substr(df$id_ions,1,1)),"test", df$type) 
    df$type <- ifelse(grepl("P",substr(df$id_ions,1,1)),"pilot", df$type) 
    df$type <- ifelse(grepl("G",substr(df$id_ions,1,1)),"bg", df$type)
    df$type <- as.factor(df$type)
    
  # id
    df$id <- as.factor(ifelse(grepl("^[0-9]|^G[0-9]",df$id_ions), sub("-.*","",df$id_ions),"NA"))
    df <- dplyr::mutate(df, id = sub("F$","",id))  # remove trailing "F" from id
    
  # return 
    return(df)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load pah file
# file <- "data/pah/20160804_PAH.xlsx"
# set sheet to "ug" or "ug_m3"
# incomplete need project data
load_pah_file <- function(file, sheet = "Summary"){

  df <- read_excel(path = file, sheet = sheet, col_names = TRUE, skip = 1)

  names(df) <- tolower(colnames(df))
  names(df) <- gsub("\\[", "_", colnames(df))
  names(df) <- gsub(" ", "_", colnames(df))
  names(df) <- gsub("\\]", "_", colnames(df))
  names(df) <- gsub("\\+", "_", colnames(df))
  names(df) <- gsub("\\(", "_", colnames(df))
  names(df) <- gsub("\\)", "_", colnames(df))
  names(df) <- gsub("(_)\\1+", "\\1", colnames(df))
  names(df)[1] <- "asu_id"

  out <- tidyr::gather(df, pol, value,  -asu_id, -csu_label)

  out <- dplyr::filter(out, !is.na(out$csu_label))  # filter empty rows

  out$lod <- ifelse(grepl("^<", out$value), as.numeric(sub("^<", "", out$value)), NA)  # add lod

  out$detect <- ifelse(grepl("^<", out$value), as.character("lod"), NA)
  out$detect <- ifelse(grepl("^ND$", out$value), as.character("nd"), out$detect)
  out$detect <- ifelse(!is.na(as.numeric(out$value)), as.character("ok"), out$detect)
  out$detect <- as.factor(out$detect)
  
  out$value <- as.numeric(out$value)

  out$pol <- as.factor(out$pol)

  out$id <- "tbd"

  # return 
    return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load transmissiometer
# file <- "../data/trans/MC Transmissometer Final.xlsx"
# out <- load_trans_file(file)
load_trans_file <- function(file){

  df <- read_excel(path = file, col_names = FALSE, skip = 1)

  df <- as.data.frame(t(df[-1]))
  
  df <- df[,1:41]

  col_names = c("id_filter",
                "id_blank",
                "date_pre",
                "person_pre",
                "toc_pre",
                "rh_pre",
                "phpa_pre",
                "id_diff_blank_pre",
                "uv_front_blank_pre",
                "ir_front_blank_pre",
                "uv_back_blank_pre",
                "ir_back_blank_pre",
                "id_diff_sample_pre",
                "uv_front_sample_pre",
                "ir_front_sample_pre",
                "uv_back_sample_pre",
                "ir_back_sample_pre",
                "uv_blank_diff_pre",
                "ir_blank_diff_pre",
                "uv_sample_diff_pre",
                "ir_sample_diff_pre",
                "date_post",
                "person_post",
                "toc_post",
                "rh_post",
                "phpa_post",
                "id_diff_blank_post",
                "uv_front_blank_post",
                "ir_front_blank_post",
                "uv_back_blank_post",
                "ir_back_blank_post",
                "id_diff_sample_post",
                "uv_front_sample_post",
                "ir_front_sample_post",
                "uv_back_sample_post",
                "ir_back_sample_post",
                "uv_blank_diff_post",
                "ir_blank_diff_post",
                "uv_sample_diff_post",
                "ir_sample_diff_post",
                "notes")    

  names(df) <- col_names

  cols <- subset(colnames(df), grepl("^date",colnames(df))==TRUE)
  df_dat <- subset(df, select = cols)
  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^toc|^rh|^phpa|^uv|ir",colnames(df))==TRUE)
  df_num <- subset(df, select = cols)
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  df_char <- subset(df, select = notes)
  df_char <- as.data.frame(lapply(df_char,
                                 function(x) as.character(x)), stringsAsFactors=FALSE)

  cols <- subset(colnames(df), grepl("^id|^person",colnames(df))==TRUE)
  df_fac <- subset(df, select = cols)

  out <- cbind(df_dat, df_num, df_char, df_fac)
    
  # return
    return(out)
} 
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load vocs
# file <- "../data/voc/20161215_VOC.xlsx"
load_voc_file <- function(file, sheet = "Sheet1"){

  df <- read_excel(path = file, sheet = sheet, col_names = TRUE)
    
  df <- df[-1,]
    
  names(df) <- paste0("voc_",colnames(df))
  names(df) <- tolower(gsub(" ", "_", colnames(df)))
  names(df) <- gsub("-", "_", colnames(df))
  names(df) <- gsub("\\+", "_", colnames(df))
  names(df) <- gsub(",", "_", colnames(df))
    
  names(df)[1] <- "id_can"
  names(df)[2] <- "id_voc"
  names(df)[3] <- "datetime_start"
  names(df)[4] <- "datetime_end"
  
  df_num <- subset(df, select = c(-id_can, -id_voc, -voc_na)) 
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(x)))

  df_fac <- subset(df, select = c(id_can, id_voc))
  df_fac <- as.data.frame(lapply(df_fac, 
                                 function(x) as.factor(x))) 

  df <- cbind(df_fac, df_num)
      
  df$type <- as.character("NA")
  df$type <- ifelse(grepl("SF",substr(df$id_voc,1,2)),"test", df$type) 
  df$type <- ifelse(grepl("P",substr(df$id_voc,1,1)),"pilot", df$type) 
  df$type <- ifelse(grepl("BG",substr(df$id_voc,1,2)),"bg", df$type)
  df$type <- as.factor(df$type)

  df$id_voc <- gsub(" ", "", df$id_voc)
  df$id <- sub(".*-", "", df$id_voc)
  df$id <- as.factor(df$id)
    
  df$datetime_start <- as.POSIXct(df$datetime_start*60*60*24, origin = "1899-12-30", tz = "GMT")
  df$datetime_end <- as.POSIXct(df$datetime_end*60*60*24, origin = "1899-12-30", tz = "GMT")

  df$date_start <- as.Date(df$datetime_start)
  df$date_end <- as.Date(df$datetime_end)
    
  # return 
    return(df)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load temperature file
# file <- "../data/temp/20160107_16A_TEMP.csv"
load_temp_file <- function(file){

  classes = c("character",
              "character",
              "numeric",
              "numeric",
              "numeric",
              "numeric")

  col_names = c("date",
                "time",
                "t_1",
                "t_2",
                "t_3",
                "t_4")

  df <- read.csv(file, header = FALSE, colClasses = classes, 
                 fill = TRUE, na.strings = c("OL"), col.names = col_names)
  
  df$datetime <- as.POSIXct(paste(df$date, df$time),
                            format = "%m/%d/%Y %I:%M:%S %p") 

  df$time <- as.numeric(substr(df$datetime, 12, 13)) * 60 * 60 + 
             as.numeric(substr(df$datetime, 15, 16)) * 60 +
             as.numeric(substr(df$datetime,18, 19))

  df$date <- as.Date(df$date, format = "%m/%d/%Y")

  df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
 
  # return
    return(df)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load pax file
# file <- "../data/pax/20160629_ALLDAY_PAX.csv"
load_pax_file <- function(file){

  df <- read.csv(file, header = TRUE, fill = TRUE, stringsAsFactors = FALSE, colClasses = "character")

  names(df) <- tolower(gsub("(\\.)\\1+", "\\1", colnames(df)))
  names(df) <- tolower(gsub("\\.", "_", colnames(df)))
  names(df) <- tolower(gsub("_$", "", colnames(df)))

  df$datetime <- as.POSIXct(paste(df$year_local, df$doy_local), format = "%Y %j") + as.numeric(df$sec_local)

  df$time <- as.numeric(df$sec_local)

  df$date <- as.Date(as.character(df$datetime), format = "%Y-%m-%d %T")

  df_num <- subset(df, select = c(-alarm, -local_date, -local_time, -date, -time, -datetime))
  
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))
  
  df_other <- subset(df, select = c(alarm, local_date, local_time, date, time, datetime))
   
 # combine dataframes
    df <- dplyr::bind_cols(df_other, df_num)

 # return
  return(df)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load smps file
# file <- "../data/smps/2017_03_16_16_18_59_SMPS.txt"
# out <- load_smps_file(file)
load_smps_file <- function(file){
 # print filename
  print(file)
 # read file body
  out <- read_csv(file, skip = 25)
 # read meta data
  out_head <- read_csv(file, col_names = FALSE, n_max = 25)
  names(out_head) <- c("var", "val")
 # clean up column names
  out <- clean_names(out)
 # organize size distributions
  out <- tidyr::gather(out, "size_nm", "dw", 10:ncol(out)) %>%
         dplyr::filter(!is.na(dw)) %>%
         dplyr::arrange(sample, size_nm) %>%
         dplyr::mutate(size_nm = as.numeric(sub("_", ".", size_nm)),
                       datetime = as.POSIXct(paste(as.character(date),
                                  as.character(seconds_to_period(as.numeric(start_time)))),
                                  format = "%F %HH %MM %SS"),
                       sample_file = basename(file)) %>%
         dplyr::select(-diameter_midpoint_nm)
 # organize header
  out_head <- tidyr::spread(out_head, var, val) %>% clean_names()
 # combine
  out <- left_join(out, out_head, by = "sample_file")
 # return
  return(out)
}
#_______________________________________________________________________________

#________________________________________________________
# Load single files
load_singlefiles <- function(log){
 # ecoc
  if(log == "ecoc"){
    filelist <- list.files("../data/ecoc", "ECOC.csv$", full.names = TRUE)
    out <- load_ecoc_file(filelist[1])
  }
  
 # grav
  if(log == "grav"){
    filelist <- list.files("../data/grav", "^Teflon Filter Weight Log.xlsx$", full.names = TRUE)
    out <- load_grav_file(filelist[1])
  }
  
 # ions
  if(log == "ions"){
    filelist <- list.files("../data/ions", "IONS.xls$", full.names = TRUE)
    out <- load_ions_file(filelist[1])
  }
  
 # pahs
  if(log == "pah"){
    filelist <- list.files("../data/pah", "PAH.xlsx$", full.names = TRUE)
    out <- load_pah_file(filelist[1])
  }
  
 # transmissometer
  if(log == "trans"){
    filelist <- list.files("../data/trans", "^Transmissometer Log.xlsx$", full.names = TRUE)
    out <- load_trans_file(filelist[1])
  }

 # vocs
  if(log == "voc"){
    filelist <- list.files("../data/voc", ".xlsx$", full.names = TRUE)
    out <- load_voc_file(filelist[1])
  }
  
 # sample log
  if(log == "sample"){
    filelist <- list.files("../data/logs", "^Sample Tracking Log.xlsx$", full.names = TRUE)
    out <- load_samples(filelist[1])
  }
  
 # batch sample log
  if(log == "batch"){
    filelist <- list.files("../data/logs", "^Transcribed Batch Fed Stove Sampling Forms.xlsx$", full.names = TRUE)
    out <- load_batch(filelist[1])
  }
  
 # wood sample log
  if(log == "wood"){
  filelist <- list.files("../data/logs", "^Transcribed Wood Stove Sampling Forms.xlsx$", full.names = TRUE)
    out <- load_wood(filelist[1])
  }

 # calibration tester 1 log
  if(log == "cal_1"){
    filelist <- list.files("../data/logs", "^Transcribed Emissions Tester 1 Calibration Log.xlsx$", full.names = TRUE)
    out <- load_fivegascal(filelist[1])
  }
  
# data sheet tester 1
  if(log == "data_1"){
    filelist <- list.files("../data/logs", "^Transcribed Emissions Tester 1 Data Sheets.xlsx$", full.names = TRUE)
    out <- load_fivegas_filter_meta(filelist[1])
  }
  
 # calibration tester 2 log
  if(log == "cal_2"){
    filelist <- list.files("../data/logs", "^Transcribed Emissions Tester 2 Calibration Log.xlsx$", full.names = TRUE)
    out <- load_co2_cal_flows_meta(filelist[1])
  }
  
 # data sheet tester 2
  if(log == "data_2"){
    filelist <- list.files("../data/logs", "^Transcribed Emissions Tester 2 Data Sheets.xlsx$", full.names = TRUE)
    out <- load_flow_fivegas_meta(filelist[1])
  }
  
 # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# Load multifile folders
load_multifile <- function(fldr, pattern, inst){

  filelist <- list.files(fldr, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

 # loop files
  for(i in 1:length(filelist)){

  # print(filelist[i])

 # co2
  if(inst == "co2"){
    ifelse(i==1, out <- load_co2_file(filelist[i]), out <- rbind(out, load_co2_file(filelist[i])))
  }

 # pax
  if(inst == "pax"){
    ifelse(i==1, out <- load_pax_file(filelist[i]), out <- rbind(out, load_pax_file(filelist[i])))
  }

 # scale
  if(inst == "scale"){
    ifelse(i==1, out <- load_scale_file(filelist[i]), out <- rbind(out, load_scale_file(filelist[i])))
  }
    
 # temp
  if(inst == "temp"){
    ifelse(i==1, out <- load_temp_file(filelist[i]), out <- rbind(out, load_temp_file(filelist[i])))
  }

 # smps
  if(inst == "smps"){
    ifelse(i==1, out <- load_smps_file(filelist[i]), out <- rbind(out, load_smps_file(filelist[i])))
  }

 # end for loop
  }

 # return
  return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load fivegas files
# df <- load_fivegas()
load_fivegas <- function(fldr = "../data/fivegas"){
 
 filelist <- list.files(fldr, pattern = ".csv$", full.names = TRUE)
 
 # loop files
 for(i in 1:length(filelist)){
  print(filelist[i])
  if(exists("out", inherits = FALSE)==FALSE){
   out <- load_fivegas_file(filelist[i])
  }else{
   out <- rbind(out, load_fivegas_file(filelist[i]))
  }
 }
 
 # return
 return(out)
}
#________________________________________________________
