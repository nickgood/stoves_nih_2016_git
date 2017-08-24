#________________________________________________________
# Libraries
  library(tidyverse)
  library(readxl)
  library(reshape)
#________________________________________________________

#________________________________________________________
# Load CO2 file
# file <- "data/co2/20160105_2A_DILUTION1.csv" # ppm
# file <- "data/co2/20160615_8D_DILUTION.csv"  # voltage
load_co2_file <- function(file){

  df <- read.csv(file, header = FALSE, nrows = 1, sep = " ",
                 fill = TRUE, na.strings = c("NAN"), colClasses = "character")

  type <- ifelse(grepl("^Device", df[1,1]), "volts", "ppm") # determkne file type: if first cell contains...


  # load based on type
    
  if(type == "ppm"){
    
    classes = c("character", "numeric", "numeric", "numeric")

    col_names = c("time", "co2", "toc", "pkpa")

    df <- read.csv(file, header = FALSE, colClasses = classes, skip = 2, 
                         fill = TRUE, na.strings = c("NAN"), col.names = col_names)

    df$datetime <- as.POSIXct(paste((strsplit(basename(file), "_")[[1]])[1], df$time),
                              format = "%Y%m%d %T") # error if fil crosses midnight

    df$date <- as.Date(df$datetime)
    
    df$time <- as.character(df$datetime)
    
    df$time <- as.numeric(substr(df$time,12,13))*60*60 + 
               as.numeric(substr(df$time,15,16))*60 +
               as.numeric(substr(df$time,18,19))

    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  }
  
  if(type == "volts"){

    classes = c("character", "character", "numeric", 
                  "numeric", "numeric", "numeric")

    col_names = c("date", "time", "lab", "sample", "ch3", "ch4")

    df <- read.csv(file, header = FALSE, colClasses = classes, skip = 7, 
                   fill = TRUE, na.strings = c("NAN"), col.names = col_names)
    
    df$datetime <- as.POSIXct(paste(df$date, df$time),
                              format = "%m/%d/%Y %I:%M:%S %p") 

    df$time <- posixct_secsofday(df$datetime)
      
    df$date <- as.Date(df$datetime)

    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  }
  
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
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
#________________________________________________________

#________________________________________________________
# Load fivegas file
# file <- "../data/fivegas/20160105_13A_FIVEGAS_OLD.csv"    # voltage
# file <- "../data/fivegas/20160811_25B_FIVEGAS_FIXED.csv"  # conc
# df <- load_fivegas_file(file)
load_fivegas_file <- function(file){

  df <- read.csv(file, header = FALSE, nrows = 1, colClasses = "character", sep = " ")  # check file type from first line

 # load 
  if(grepl("^Device", df[1][1])){

  classes = c(rep("character",2), rep("numeric",4))

  col_names = c("date", "time", "co2", "o2", "co", "ch4")

  df <- read.csv(file, header = FALSE, colClasses = classes,
                       skip = 7, fill = TRUE, col.names = col_names)
    
 # check year format
  if(grepl("[0-9][0-9][0-9][0-9]$", df$date[1])){
    df$datetime <- as.POSIXct(paste(df$date, df$time), format = "%m/%d/%Y %I:%M:%S %p")
  }else{
    df$datetime <- as.POSIXct(paste(df$date, df$time), format = "%m/%d/%y %I:%M:%S %p")
  }

 # convert time string to seconds of day
  df$time <- as.character(df$datetime)
  df$time <- as.numeric(substr(df$time,12,13))*60*60 +
             as.numeric(substr(df$time,15,16))*60 +
             as.numeric(substr(df$time,18,19)) 

 # convert date
  df$date <- as.Date(df$datetime)

  }else{
 # check time format
  df <- read.csv(file, header = FALSE,
                   nrows = 1,
                   colClasses = "character",
                   skip = 1)

  time_format <- ifelse(grepl("AM$|PM$", df$V7[1]), "us", "mil")
    
  classes = c(rep("numeric",6), "character")

  col_names = c("ch4", "o2", "nox", "co2", "co", "datetime_secs", "time_str")

  df <- read.csv(file, header = FALSE, colClasses = classes, sep = ",",
                       skip = 1, fill = TRUE, col.names = col_names)

  if(time_format == "mil"){
    df <- dplyr::mutate(df, datetime =
                            as.POSIXct(paste((strsplit(basename(file),
                            "_")[[1]])[1],
                            df$time),
                            format = "%Y%m%d %H:%M:%S"),
                            date = as.Date(datetime),
                            time = as.character(datetime),
                            time = as.numeric(substr(datetime, 12, 13)) * 60 * 60 +
                                 as.numeric(substr(datetime, 15, 16)) * 60 +
                                 as.numeric(substr(datetime, 18, 19)))
  }else{
    df <- dplyr::mutate(df, datetime =
                            as.POSIXct(paste((strsplit(basename(file),
                            "_")[[1]])[1],
                            df$time),
                            format = "%Y%m%d %I:%M:%S %p"),
                            date = as.Date(datetime),
                            time = as.character(datetime),
                            time = as.numeric(substr(datetime, 12, 13)) * 60 * 60 +
                            as.numeric(substr(datetime, 15, 16)) * 60 +
                            as.numeric(substr(datetime, 18, 19)))
  }
}

  df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])

 # convert percents to ppm
  df$co2 <- df$co2*10^4
  df$o2 <- df$o2*10^4  

 # return
  return(df)
}
#________________________________________________________

#________________________________________________________
# Load gravimetric file
# file <- "data/grav/Teflon Filter Weight Log.xlsx"
#
load_grav_file <- function(file, sheet = "Teflon Filter Weights"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 1)
  
  df <- as_tibble(t(df[1:29,-1]))  # transpose
    
  names(df) <- c("id_filter", 
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

  cols <- subset(colnames(df), grepl("^date",colnames(df))==TRUE)
  df_dat <- subset(df, select = cols)
  df_dat <- as.data.frame(lapply(df_dat, 
                                 function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))

  cols <- subset(colnames(df), grepl("^toc_|^rh_|^phpa_|^wgt_|lod",colnames(df))==TRUE)
  df_num <- subset(df, select = cols)
  df_num <- as.data.frame(lapply(df_num, 
                                 function(x) as.numeric(as.character(x))))

  cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)
  df_time <- subset(df, select = cols)
  df_time <- as.data.frame(lapply(df_time, 
                                  function(x) as.numeric(as.character(x))*24*60*60))  

  df_char <- subset(df, select = notes)
    
  cols <- subset(colnames(df), grepl("^person_|^id",colnames(df))==TRUE)
  df_fac <- subset(df, select = cols)
  df_fac <- as.data.frame(lapply(df_fac, 
                                 function(x) as.factor(x)))

  out <- cbind(df_dat, df_num, df_time, df_char, df_fac)               

  out$id <- as.factor(sub("-.*", "", out$id_grav))
  
  # return 
    return(out)
}
#________________________________________________________

#________________________________________________________
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
#________________________________________________________

#________________________________________________________
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
#________________________________________________________

#________________________________________________________
# Load transmissiometer
# file <- "data/trans/Transmissometer Log.xlsx" 
load_trans_file <- function(file, sheet = "Transmissometer Log"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE, skip = 1)

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
#________________________________________________________

#________________________________________________________
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
  
  df_num <- subset(df, select = c(-id_can, -id_voc))
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
#________________________________________________________

#________________________________________________________
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
#________________________________________________________

#________________________________________________________
# Load stove weight
# file <- "../data/scale/20160107_16A_SCALE.xlsx"
load_scale_file <- function(file, sheet = "Sheet1"){

  df <- read_excel(path = file, sheet = sheet, col_names = FALSE)

  df <- df[,1:4]

  names(df) <- c("date",
                 "time",
                 "wgt_stove",
                 "units")

  df$datetime <- as.POSIXct(paste(as.character(df$date),
                            substr(as.character(df$time),12,19)), format = "%Y-%m-%d %T")

  df$time <- as.numeric(substr(as.character(df$time),12,13))*60*60 +
             as.numeric(substr(as.character(df$time),15,16))*60 +
             as.numeric(substr(as.character(df$time),18,19))

  df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
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
#________________________________________________________

#________________________________________________________
# Load smps file
# file <- "../data/smps/20160105_6A_SMPS.csv"
# df <- load_smps_file(file)
load_smps_file <- function(file){

  df <- read.csv(file, header = TRUE, fill = TRUE,
                       stringsAsFactors = FALSE, skip = 25)

  df_meta <- read.csv(file, header = FALSE, fill = TRUE,
                       stringsAsFactors = FALSE, nrows = 25)

  df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])

  names(df) <- tolower(gsub("\\.\\.\\.\\.", "_", colnames(df)))
  names(df) <- gsub("\\.\\.\\.", "_", colnames(df))
  names(df) <- gsub("\\.\\.", "_", colnames(df))
  names(df) <- gsub("\\.", "_", colnames(df))
  names(df) <- gsub("_$", "", colnames(df))

  df_dw <- subset(df, select = grep("^x[0-9]", colnames(df), value = TRUE))
  df_vals <- subset(df, select = grep("^[^x][^0-9]", colnames(df), value = TRUE))

  df_meta_1 <- df_meta[,1:2]
  df_meta_2 <- df_meta[,3:4]
  df_meta_3 <- df_meta[,5:6]
  
  names(df_meta_1) <- c("var", "val")
  names(df_meta_2) <- c("var", "val")
  names(df_meta_3) <- c("var", "val")

  df_meta <- na.omit(rbind(df_meta_1, df_meta_2, df_meta_3))

  col_names <- df_meta[,1]

  df_meta <- as.data.frame(t(df_meta[-1]))

  names(df_meta) <- tolower(gsub(" ", "_", col_names))

  df_meta$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])

  df_allvals <- merge(df_vals, df_meta, by.x = "id")
    
  df_dw$sample <- df$sample

  df_dw <- melt(df_dw, id.vars = "sample")

  df_dw$size <- gsub("_", ".", df_dw$variable)

  df_dw$size <- as.numeric(gsub("x", "", df_dw$size))

  df_dw$variable <- NULL

  out <- merge(df_allvals,df_dw, by.x = "sample")

  out <- out[!is.na(out$value), ]

  out <- arrange(out, sample, size)

 # match d/../yy or dd/../yy
  if(grepl("(^[0-9]/)|(^[0-9][0-9]/)[0-9].*/[0-9][0-9]$", out$date[1])){
    out$date <- as.Date(out$date, format = "%m/%d/%y")
  }
  # match yyyy/...
  if(grepl("^[0-9][0-9][0-9][0-9]/.*$", out$date[1])){
    out$date <- as.Date(out$date, format = "%Y/%m/%d")
  }
  # match .../yyyy
  if(grepl(".*/[0-9][0-9][0-9][0-9]$", out$date[1])){
    out$date <- as.Date(out$date, format = "%m/%d/%Y")
  }

  out$time <- as.numeric(substr(out$start_time,1,2))*60*60 + 
              as.numeric(substr(out$start_time,4,5))*60 +
              as.numeric(substr(out$start_time,7,8))

  out$datetime <- as.POSIXct(paste(as.character(out$date), out$start_time), 
                             format = "%Y-%m-%d %H:%M:%S")

 # return
  return(out)
}
#________________________________________________________

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
#________________________________________________________

#________________________________________________________
# Load fivegas
# file <- "data/fivegas/20160105_6A_FIVEGAS_OLD.csv"
# file <- "data/fivegas/20160615_2D_FIVEGAS_FIXED.csv"

load_fivegas <- function(fldr = "../data/fivegas", 
                         pattern = "[0-9][A-Z]_FIVEGAS_FIXED.csv$|[0-9][A-Z]_FIVEGAS_OLD.csv$",
                         type = "conc"){

  filelist <- list.files(fldr, pattern = pattern, full.names = TRUE)

 # loop files
  for(i in 1:length(filelist)){

 # determine file type
  df <- read.csv(filelist[i], header = FALSE, nrows = 1, colClasses = "character", sep = " ")

 # check type
  filetype <- ifelse(grepl("^Device", df[1][1]), "volts", "conc")

 # load 
  if(filetype == type){

  if(exists("out", inherits = FALSE)==FALSE){
    out <- load_fivegas_file(filelist[i])
  }else{
    out <- rbind(out, load_fivegas_file(filelist[i]))
  }
 # end if
  }
 # end for
  }

 # return
  return(out)
}
#________________________________________________________
