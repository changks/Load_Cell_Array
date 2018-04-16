LC_Pull_Out_Data_Every_10M30M1H2H3H <- function(Input_FileName) {

# R Functions to process dataset from Load Cell Array
# It pulls data based on time intervals
#
# Author : Kuo-Hsine Chang, Ph.D. Dept. of Plant Agricuture, University of Guelph
# Contact: changks.github.io
# Version: 04162018

  # load library
    library(zoo)
    library(data.table)
    
  # read the csv file  
    dt = fread(Input_FileName, header = T, sep = ','); 

  # timestamp should be POSIXct
    dt[, Time := as.POSIXct(Time)];

  # filter for entries every 10 mins
   dt_10m <- subset(dt, format(Time,'%M') %in% c('00','10','20','30','40','50'))  
  
  # filter for entries every 30 mins
   dt_30m <- subset(dt, format(Time,'%M') %in% c('00','30'))  

  # filter for entries where the time is half past -- every 1 hour
    dt_1h <- dt[format(Time, "%M") == 30];

  # filter for entries where the time is half past -- every 2 hour
    dt_2h <- subset(dt_1h, strftime(Time, "%H", tz="Canada/Eastern") %in% c('01','03','05','07','09','11','13','15','17','19','21','23'));

  # filter for entries where the time is half past -- every 3 hour
    dt_3h <- subset(dt_1h, strftime(Time, "%H", tz="Canada/Eastern") %in% c('01','04','07','10','13','16','19','22'));

  # export to a csv file 
    write.table(dt_10m, paste0(tools::file_path_sans_ext(Input_FileName),"_10M.csv"), row.names=FALSE, col.names=TRUE, sep=",")     
    write.table(dt_30m, paste0(tools::file_path_sans_ext(Input_FileName),"_30M.csv"), row.names=FALSE, col.names=TRUE, sep=",")     
    write.table(dt_1h, paste0(tools::file_path_sans_ext(Input_FileName),"_1H.csv"), row.names=FALSE, col.names=TRUE, sep=",")     
    write.table(dt_2h, paste0(tools::file_path_sans_ext(Input_FileName),"_2H.csv"), row.names=FALSE, col.names=TRUE, sep=",")
    write.table(dt_3h, paste0(tools::file_path_sans_ext(Input_FileName),"_3H.csv"), row.names=FALSE, col.names=TRUE, sep=",")
    
}

