#________________________________________________________
# Libraries
  require(xlsx)
  require(dplyr)
  require(reshape)
  require(tidyr)
#________________________________________________________

#________________________________________________________
# Load CO2 file
# file <- "data/co2/20160105_2A_DILUTION1.csv" # ppm
# file <- "data/co2/20160615_8D_DILUTION.csv"  # voltage
load.co2.file <- function(file){
  
  # determine file type
    df <- read.csv(file, header = FALSE, nrows = 1, sep = " ",
                 fill = TRUE, na.strings = c("NAN"), colClasses = "character")
    
    type <- ifelse(grepl("^Device", df[1,1]), "volts", "ppm") # if first cell contains...
  
    print(type)
  # load based on type
    
  if(type == "ppm"){
  # column classes
    classes = c("character", "numeric", "numeric", "numeric")
  
  # column names
    col_names = c("time", "co2", "toc", "pkpa")
  
  # read file
    df <- read.csv(file, header = FALSE, colClasses = classes, skip = 2, 
                 fill = TRUE, na.strings = c("NAN"), col.names = col_names)
    
  # add datetime  
    df$datetime <- as.POSIXct(paste((strsplit(basename(file), "_")[[1]])[1], df$time),
                              format = "%Y%m%d %T") # error if fil crosses midnight
   
  # add date  
    df$date <- as.Date(df$datetime)
    
  # convert datetime string to seconds of day
    df$time <- as.character(df$datetime)
    df$time <- as.numeric(substr(df$time,12,13))*60*60 + 
      as.numeric(substr(df$time,15,16))*60 +
      as.numeric(substr(df$time,18,19))  
  
  # add id
    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  } 
  if(type == "volts"){
    # column classes
      classes = c("character", "character", "numeric", 
                  "numeric", "numeric", "numeric")
    
    # column names
      col_names = c("date", "time", "lab", "sample", "ch3", "ch4")
    
    # read file
      df <- read.csv(file, header = FALSE, colClasses = classes, skip = 7, 
                   fill = TRUE, na.strings = c("NAN"), col.names = col_names)
    
    # add datetime  
      df$datetime <- as.POSIXct(paste(df$date, df$time),
                                format = "%m/%d/%Y %I:%M:%S %p") 
      
    # convert time string to seconds of day
      df$time <- posixct_secsofday(df$datetime)
      
    # add date  
      df$date <- as.Date(df$datetime)
      
    # add id
      df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  }
  
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load ECOC file
# file <- "data/ecoc/20161102_ECOC.csv"
load.ecoc.file <- function(file){
  
  # column classes
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
  
  # read file
    df <- read.csv(file, header = TRUE, colClasses = classes, 
                 fill = TRUE, na.strings = c("-", "na"))
  
  # convert time string to seconds of day
    df <- dplyr::rename(df, time = Time)
    df$time <- as.character(as.POSIXct(strptime(df$time, "%I:%M:%S %p")))
    df$time <- as.numeric(substr(df$time,12,13))*60*60 + 
    as.numeric(substr(df$time,15,16))*60 +
    as.numeric(substr(df$time,18,19))
  
  # convert date
    df <- dplyr::rename(df, date = Date)
    df$date <- as.Date(df$date, "%m/%d/%Y")
    
  # add datetime  
    df$datetime <- as.POSIXct(as.character(df$date))
    
  # id
    df <- dplyr::rename(df, ecoc_id = Sample.ID)
  
  # test type
    df$type <- ifelse(grepl("^[0-9]",df$ecoc_id),"test", "NA") 
    df$type <- ifelse(grepl("^P", df$ecoc_id),"pilot", df$type) 
    df$type <- ifelse(grepl("^G", df$ecoc_id),"bg", df$type)
  
  # filter cassette
    df$cassette <- ifelse(grepl("-A$", df$ecoc_id),"A", "NA")
    df$cassette <- ifelse(grepl("-E$", df$ecoc_id),"E", df$cassette)
    
  # id  
    #df$id <- ifelse(grepl("^[0-9]", df$ecoc_id), sub("-.*","",df$ecoc), df$ecoc_id)
    df$id <- sub("-.*","",df$ecoc)
    
  # rename columns
    names(df) <- gsub("\\.$", "", colnames(df))
    names(df) <- tolower(gsub("\\.\\.", "_", colnames(df)))
    names(df) <- tolower(gsub("\\.", "_", colnames(df)))
  
  # set class
    df$ecoc_id <- as.factor(df$ecoc_id)
    df$id <- as.factor(df$id)
    df$type <- as.factor(df$type)
    df$cassette <- as.factor(df$cassette)
    
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load fivegas file
# file <- "data/fivegas/20160105_13A_FIVEGAS_OLD.csv" # voltage
# file <- "data/fivegas/20160429_3B_FIVEGAS_FIXED.csv" # conc
# df <- load.fivegas.file(file)
load.fivegas.file <- function(file){
  # read first row
    df <- read.csv(file, header = FALSE, nrows = 1, colClasses = "character", sep = " ")
  
  # load 
    if(grepl("^Device", df[1][1])){
      
      # column classes
        classes = c(rep("character",2), rep("numeric",4))
      
      # column names
        col_names = c("date", "time", "co2", "o2", "co", "ch4")
      
      # read file
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
      
      # column classes
        classes = c(rep("numeric",6), "character")
      
      # column names
        col_names = c("ch4", "o2", "nox", "co2", "co", "datetime_secs", "time_str")
        
      # read file
        df <- read.csv(file, header = FALSE, colClasses = classes, sep = ",",
                     skip = 1, fill = TRUE, col.names = col_names)
        
      # datetime
        #df$datetime <- as.POSIXct(df$datetime_secs, origin = "1899-12-30") 
        df$datetime <- as.POSIXct(paste((strsplit(basename(file), "_")[[1]])[1], df$time), format = "%Y%m%d %H:%M:%S") # fix
      # date
        df$date <- as.Date(df$datetime)
        
      # time
        df$time <- as.numeric(substr(df$time_str,1,2))*60*60 + 
                    as.numeric(substr(df$time_str,4,5))*60 +
                    as.numeric(substr(df$time_str,7,8))
    }
      
    # id
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

load.grav.file <- function(file, sheet = "Teflon Filter Weights"){
  
  # read file
    df <- read.xlsx(file, sheetName = sheet, header = FALSE, colClasses = "character", startRow = 2, endRow = 30)
  
  # transpose
    df <- as.data.frame(t(df[-1]))
    
  # column names
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
   
    # convert to date
      cols <- subset(colnames(df), grepl("^date",colnames(df))==TRUE)
      df_dat <- subset(df, select = cols)
      df_dat <- as.data.frame(lapply(df_dat, 
                  function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))
    
    # convert to numeric
      cols <- subset(colnames(df), grepl("^toc_|^rh_|^phpa_|^wgt_|lod",colnames(df))==TRUE)
      df_num <- subset(df, select = cols)
      df_num <- as.data.frame(lapply(df_num, 
                  function(x) as.numeric(as.character(x))))
    
    # convert time to seconds of day
      cols <- subset(colnames(df), grepl("^time",colnames(df))==TRUE)
      df_time <- subset(df, select = cols)
      df_time <- as.data.frame(lapply(df_time, 
                   function(x) as.numeric(as.character(x))*24*60*60))  
    
    # convert to character
      df_char <- subset(df, select = notes)
    
    # factors (default)
      cols <- subset(colnames(df), grepl("^person_|^id",colnames(df))==TRUE)
      df_fac <- subset(df, select = cols)
      df_fac <- as.data.frame(lapply(df_fac, 
                    function(x) as.factor(x)))
    
    # combine dataframes
      out <- cbind(df_dat, df_num, df_time, df_char, df_fac)               
  
    # id
      out$id <- as.factor(sub("-.*", "", out$id_grav))
  
  # return 
    return(out)
}
#________________________________________________________

#________________________________________________________
# Load ions file
# file <- "data/ions/20160602_IONS.xls"
# set sheet to "ug" or "ug_m3"
load.ions.file <- function(file, sheet = "ug"){
  
  # column classes
    classes <- c("character", rep("numeric",36))
               
  # read file
    df <- read.xlsx(file, sheetName = sheet, header = TRUE, colClasses = classes)
    names(df) <- tolower(sub("\\.\\.ug\\.m3\\.", "", colnames(df)))
    names(df) <- tolower(sub("\\.\\.ug\\.", "", colnames(df)))
    names(df) <- sub("o\\.", "o_", colnames(df))
    names(df) <- sub("m\\.p\\.", "mp_", colnames(df))
    names(df) <- sub("x2\\.5\\.", "twofive_", colnames(df))
    names(df) <- sub("\\.\\.ug\\.c\\.m3\\.", "", colnames(df))
    names(df)[1] <- "id_ions"

  # test type
    df$type <- df$id_ions
    df$type <- ifelse(grepl("[0-9]",substr(df$id_ions,1,1)),"test", df$type) 
    df$type <- ifelse(grepl("P",substr(df$id_ions,1,1)),"pilot", df$type) 
    df$type <- ifelse(grepl("G",substr(df$id_ions,1,1)),"bg", df$type)
    df$type <- as.factor(df$type)
    # others types? Add code to remove others?
    
  # id
    df$id <- as.factor(ifelse(grepl("^[0-9]|^G[0-9]",df$id_ions), sub("-.*","",df$id_ions),"NA"))
    
  # return 
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load pah file
# file <- "data/pah/20160804_PAH.xlsx"
# set sheet to "ug" or "ug_m3"
load.pah.file <- function(file, sheet = "Summary"){
  
  # read file
    df <- read.xlsx(file, sheetName = sheet, header = TRUE, colClasses = "character", startRow = 2)
  
  # column names
    names(df) <- tolower(gsub("(\\.)\\1+", "\\1", colnames(df)))
    names(df) <- gsub("\\.", "_", colnames(df))
    names(df) <- sub("_$", "", colnames(df))
  
  # melt 
    out <- tidyr::gather(df, pol, value,  -asu, -csu_label)
  
  # filter empty rows
    out <- dplyr::filter(out, !is.na(out$csu_label)) 
    
  # add lod
    out$lod <- ifelse(grepl("^<", out$value), as.numeric(sub("^<", "", out$value)), NA)
    
  # detected
    out$detect <- ifelse(grepl("^<", out$value), as.character("lod"), NA)
    out$detect <- ifelse(grepl("^ND$", out$value), as.character("nd"), out$detect)
    out$detect <- ifelse(!is.na(as.numeric(out$value)), as.character("ok"), out$detect)
    out$detect <- as.factor(out$detect)
  
  # value to numeric or NA
    out$value <- as.numeric(out$value)
  
  # classes
      out$pol <- as.factor(out$pol)
    
  # id
    out$id <- "tbd"
  
  # return 
    return(out)
}
#________________________________________________________

#________________________________________________________
# Load transmissiometer
# file <- "data/trans/Transmissometer Log.xlsx" 
load.trans.file <- function(file){
  
  # column classes
    classes = c(rep("factor",2),
                "character",
                "factor",
                rep("numeric",3),
                "factor",
                rep("numeric",4),
                "factor",
                rep("numeric",8),
                "character",
                "factor",
                rep("numeric",3),
                rep("factor",2),
                rep("numeric",3),
                "factor",
                rep("numeric",2),
                "factor",
                rep("numeric",5),
                "character")
                
  # column names
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
  
  # read file
      df <- read.xlsx(file, sheetName = "Transmissometer Log", startRow = 2,
                    header = FALSE)
  # transpose
      df <- as.data.frame(t(df[-1]))
      df <- df[,1:41]
  
  # names
      names(df) <- col_names
      
  # convert to date
      cols <- subset(colnames(df), grepl("^date",colnames(df))==TRUE)
      df_dat <- subset(df, select = cols)
      df_dat <- as.data.frame(lapply(df_dat, 
                    function(x) as.Date(as.numeric(as.character(x)), origin = "1899-12-30")))
      
  # convert to numeric
      cols <- subset(colnames(df), grepl("^toc|^rh|^phpa|^uv|ir",colnames(df))==TRUE)
      df_num <- subset(df, select = cols)
      df_num <- as.data.frame(lapply(df_num, 
                    function(x) as.numeric(as.character(x))))
    
  # convert to character
      df_char <- subset(df, select = notes)
      df_char <- as.data.frame(lapply(df_char,
                    function(x) as.character(x)), stringsAsFactors=FALSE)
      
  # factors (default)
      cols <- subset(colnames(df), grepl("^id|^person",colnames(df))==TRUE)
      df_fac <- subset(df, select = cols)
      
  # combine dataframes
      out <- cbind(df_dat, df_num, df_char, df_fac)
      
  # return
      return(out)
} 
#________________________________________________________

#________________________________________________________
# Load vocs
# file <- "data/voc/Data_20160706.xlsx"
load.voc.file <- function(file){
  
  # read file
    df <- read.xlsx(file, sheetName = "Sheet1", startRow = 1,
                  header = TRUE, stringsAsFactors=FALSE)
    
  # remove second row
    df <- df[-1,]
    
  # rename columns
    names(df) <- paste0("voc_",colnames(df))
    names(df) <- sub("voc_X","voc_", colnames(df))
    names(df) <- tolower(gsub("\\.", "_", colnames(df)))
    names(df)[1] <- "id_can"
    names(df)[2] <- "id_voc"
    names(df)[3] <- "datetime_start"
    names(df)[4] <- "datetime_end"
  
  # classes
    # convert to numeric
      df_num <- subset(df, select = c(-id_can, -id_voc, -voc_na_)) 
      df_num <- as.data.frame(lapply(df_num, 
                                  function(x) as.numeric(x)))

    # factors (default)
      df_fac <- subset(df, select = c(id_can, id_voc))
      df_fac <- as.data.frame(lapply(df_fac, 
                                     function(x) as.factor(x))) 
  
    # combine dataframes
      df <- cbind(df_fac, df_num)
      
  # test type
    df$type <- as.character("NA")
    df$type <- ifelse(grepl("SF",substr(df$id_voc,1,2)),"test", df$type) 
    df$type <- ifelse(grepl("P",substr(df$id_voc,1,1)),"pilot", df$type) 
    df$type <- ifelse(grepl("BG",substr(df$id_voc,1,2)),"bg", df$type)
    df$type <- as.factor(df$type)
    # others types? Add code to remove others?
    
  # test id
    df$id <- sub("^[A-Z][A-Z]-|^[A-Z][A-Z] - ", "", df$id_voc)
    df$id[c(1,2,3,4,5,7,8,9)] <- "NA"
    df$id <- as.factor(df$id)
    
  # datetime
    df$datetime_start <- as.POSIXct(df$datetime_start*60*60*24 + 60*60*6, origin = "1899-12-30")
    df$datetime_end <- as.POSIXct(df$datetime_end*60*60*24 + 60*60*6, origin = "1899-12-30")
  
  # date
    df$date_start <- as.Date(df$datetime_start)
    df$date_end <- as.Date(df$datetime_end)
    
  # return 
      return(df)
}
#________________________________________________________

#________________________________________________________
# Load temperature file
# file <- "data/temp/20160107_16A_TEMP.csv"
load.temp.file <- function(file){
  
  # column classes
  classes = c("character",
              "character",
              "numeric",
              "numeric",
              "numeric",
              "numeric")
  
  # column names
    col_names = c("date",
                  "time",
                  "tamb",
                  "ch2",
                  "ch3",
                  "thood")
  
  # read file
    df <- read.csv(file, header = FALSE, colClasses = classes, 
                 fill = TRUE, na.strings = c("OL"), col.names = col_names)
  
  # add datetime  
    df$datetime <- as.POSIXct(paste(df$date,df$time),
                              format = "%m/%d/%Y %I:%M:%S %p") 
      
  # convert time string to seconds of day
    df$time <- as.numeric(substr(df$time,1,2))*60*60 + 
                as.numeric(substr(df$time,4,5))*60 +
                as.numeric(substr(df$time,7,8))
    
  # convert date string to date class
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
  
  # add id
    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
    
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load stove weight
# file <- "data/scale/20160107_16A_SCALE.xlsx"
load.scale.file <- function(file){
  
  # read file
    df <- read.xlsx(file, sheetName = "Sheet1", header = FALSE)
    
  # grab first four columns
    df <- df[,1:4]
    
  # rename columns
    names(df) <- c("date",
                   "time",
                   "wgt_stove",
                   "units")
  
  # add datetime
    df$datetime <- as.POSIXct(paste(as.character(df$date),
                                    substr(as.character(df$time),12,19)), format = "%Y-%m-%d %T")
    
  # convert time to seconds of day
    df$time <- as.numeric(substr(as.character(df$time),12,13))*60*60 +
      as.numeric(substr(as.character(df$time),15,16))*60 +
      as.numeric(substr(as.character(df$time),18,19))
  
  # add id
    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load pax file
# file <- "data/pax/20160524_ALLDAY_PAX.csv"
load.pax.file <- function(file){
  
  # read file
    df <- read.csv(file, header = TRUE, fill = TRUE, stringsAsFactors = FALSE, colClasses = "character")
  
  # column names
    names(df) <- tolower(gsub("(\\.)\\1+", "\\1", colnames(df)))
    names(df) <- tolower(gsub("\\.", "_", colnames(df)))
    names(df) <- tolower(gsub("_$", "", colnames(df)))
    
  # date
    df$datetime <- as.POSIXct(paste(df$year_local, df$doy_local), format = "%Y %j") + as.numeric(df$sec_local)
    
  # time
    df$time <- as.numeric(df$sec_local)
    
  # add datetime  
    df$date <- as.Date(df$datetime)

  # convert to numeric
    df_num <- subset(df, select = c(-alarm, -local_date, -local_time, -date, -time, -datetime))
    df_num <- as.data.frame(lapply(df_num, 
                                   function(x) as.numeric(as.character(x))))
  # other columns
    df_other <- subset(df, select = c(alarm, local_date, local_time, date, time, datetime))
   
  # combine dataframes
    df <- cbind(df_other, df_num)
    
  # return
    return(df)
}
#________________________________________________________

#________________________________________________________
# Load smps file
# file <- "data/smps/20160330_16C_SMPS.csv"
# df <- load.smps.file(file)
load.smps.file <- function(file){
  
  # read file
    df <- read.csv(file, header = TRUE, fill = TRUE,
                   stringsAsFactors = FALSE, skip = 25)
    
  # read meta data
    df_meta <- read.csv(file, header = FALSE, fill = TRUE, stringsAsFactors = FALSE, nrows = 25)
  
  # read sample id
    df$id <- as.factor((strsplit(basename(file), "_")[[1]])[2])
  
  # rename columns
    names(df) <- tolower(gsub("\\.\\.\\.\\.", "_", colnames(df)))
    names(df) <- gsub("\\.\\.\\.", "_", colnames(df))
    names(df) <- gsub("\\.\\.", "_", colnames(df))
    names(df) <- gsub("\\.", "_", colnames(df))
    names(df) <- gsub("_$", "", colnames(df))
     
  # split data (size distribution versus the rest) 
    df_dw <- subset(df, select = grep("^x[0-9]", colnames(df), value = TRUE))
    df_vals <- subset(df, select = grep("^[^x][^0-9]", colnames(df), value = TRUE))
    

  # merge df_vals and metadata
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
    
  # add sample number to dw
    df_dw$sample <- df$sample
  
  # dw to rows
    df_dw <- melt(df_dw, id.vars = "sample")
  
  # size to numeric
    df_dw$size <- gsub("_", ".", df_dw$variable)
    df_dw$size <- as.numeric(gsub("x", "", df_dw$size))
    df_dw$variable <- NULL
    
  # merge
    out <- merge(df_allvals,df_dw, by.x = "sample")
  
  # remove na's
    out <- out[!is.na(out$value),]
  
  # order
    out <- arrange(out,sample,size)
    
  # date
    out$date <- as.Date(out$date, format = "%m/%d/%Y")
  
  # time
    out$time <- as.numeric(substr(out$start_time,1,2))*60*60 + 
                as.numeric(substr(out$start_time,4,5))*60 +
                as.numeric(substr(out$start_time,7,8))
    
  # datetime
    out$datetime <- as.POSIXct(paste(as.character(out$date), out$start_time), 
                              format = "%Y-%m-%d %H:%M:%S")
    
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________ 
# Load single files
load.singlefiles <- function(log){
  # ecoc
  if(log == "ecoc"){
    filelist <- list.files("data/ecoc", "ECOC.csv$", full.names = TRUE)
    out <- load.ecoc.file(filelist[1])
  }
  
  # grav
  if(log == "grav"){
    filelist <- list.files("data/grav", "^Teflon Filter Weight Log.xlsx$", full.names = TRUE)
    out <- load.grav.file(filelist[1])
  }
  
  # ions
  if(log == "ions"){
    filelist <- list.files("data/ions", "IONS.xls$", full.names = TRUE)
    out <- load.ions.file(filelist[1])
  }
  
  # pahs
  if(log == "pah"){
    filelist <- list.files("data/pah", "PAH.xlsx$", full.names = TRUE)
    out <- load.pah.file(filelist[1])
  }
  
  
  # transmissometer
  if(log == "trans"){
    filelist <- list.files("data/trans", ".xlsx$", full.names = TRUE)
    out <- load.trans.file(filelist[1])
  }
  
  # vocs
  if(log == "voc"){
    filelist <- list.files("data/voc", ".xlsx$", full.names = TRUE)
    out <- load.voc.file(filelist[1])
  }
  
  # sample log
  if(log == "sample"){
    filelist <- list.files("data/logs", "^Sample Tracking Log.xlsx$", full.names = TRUE)
    out <- load.samples(filelist[1])
  }
  
  # batch sample log
  if(log == "batch"){
    filelist <- list.files("data/logs", "^Transcribed Batch Fed Stove Sampling Forms.xlsx$", full.names = TRUE)
    out <- load.batch(filelist[1])
  }
  
  # wood sample log
  if(log == "wood"){
    filelist <- list.files("data/logs", "^Transcribed Wood Stove Sampling Forms.xlsx$", full.names = TRUE)
    out <- load.wood(filelist[1])
  }
  
  # calibration tester 1 log
  if(log == "cal_1"){
    filelist <- list.files("data/logs", "^Transcribed Emissions Tester 1 Calibration Log.xlsx$", full.names = TRUE)
    out <- load.fivegascal(filelist[1])
  }
  
  # data sheet tester 1
  if(log == "data_1"){
    filelist <- list.files("data/logs", "^Transcribed Emissions Tester 1 Data Sheets.xlsx$", full.names = TRUE)
    out <- load.fivegas_filter_meta(filelist[1])
  }
  
  # calibration tester 2 log
  if(log == "cal_2"){
    filelist <- list.files("data/logs", "^Transcribed Emissions Tester 2 Calibration Log.xlsx$", full.names = TRUE)
    out <- load.co2_cal_flows_meta(filelist[1])
  }
  
  # data sheet tester 2
  if(log == "data_2"){
    filelist <- list.files("data/logs", "^Transcribed Emissions Tester 2 Data Sheets.xlsx$", full.names = TRUE)
    out <- load.flow_fivegas_meta(filelist[1])
  }
  
  # return
    return(out)
  
}
#________________________________________________________

#________________________________________________________
# Load multifile folders
load.multifile <- function(fldr, pattern, inst){
  
  # list of files to load
  filelist <- list.files(fldr, pattern = pattern, full.names = TRUE)
  
  # loop files
  for(i in 1:length(filelist)){
    # print file name
    print(filelist[i])
    
    # co2
    if(inst == "co2"){
      ifelse(i==1, out <- load.co2.file(filelist[i]), out <- rbind(out, load.co2.file(filelist[i])))
    }
    
    # pax
    if(inst == "pax"){
      ifelse(i==1, out <- load.pax.file(filelist[i]), out <- rbind(out, load.pax.file(filelist[i])))
    }
    
    # scale
    if(inst == "scale"){
      ifelse(i==1, out <- load.scale.file(filelist[i]), out <- rbind(out, load.scale.file(filelist[i])))
    }
    
    # temp
    if(inst == "temp"){
      ifelse(i==1, out <- load.temp.file(filelist[i]), out <- rbind(out, load.temp.file(filelist[i])))
    }
    
    # smps
    if(inst == "smps"){
      ifelse(i==1, out <- load.smps.file(filelist[i]), out <- rbind(out, load.smps.file(filelist[i])))
    }
    #
  }
  
  # return
  return(out)
}
#________________________________________________________

#________________________________________________________
# Load fivegas
# file <- "data/fivegas/20160105_6A_FIVEGAS_OLD.csv"
# file <- "data/fivegas/20160615_2D_FIVEGAS_FIXED.csv"

load.fivegas <- function(fldr = "data/fivegas", 
                         pattern = "[0-9][A-Z]_FIVEGAS_FIXED.csv$|[0-9][A-Z]_FIVEGAS_OLD.csv$",
                         type = "conc"){
  
  # list of files to load
    filelist <- list.files(fldr, pattern = pattern, full.names = TRUE)
  
  # loop files
    for(i in 1:length(filelist)){
    
    # determine file type
      df <- read.csv(filelist[i], header = FALSE, nrows = 1, colClasses = "character", sep = " ")
    
    # check type
      filetype <- ifelse(grepl("^Device", df[1][1]), "volts", "conc")
    
    # load 
    if(filetype == type){
      # matches type?
      if(exists("out", inherits = FALSE)==FALSE){
        out <- load.fivegas.file(filelist[i])
      }else{
        out <- rbind(out, load.fivegas.file(filelist[i]))
      }
    }
  }
  
  # return
    return(out)
}
#________________________________________________________  
