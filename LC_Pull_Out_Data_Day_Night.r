LC_Pull_Out_Data_Day_Night <- function(Input_FileName, DayTime_Start_in_H, DayTime_End_in_H) {

# R Functions to process dataset from Load Cell Array
# It pulls data based on user-defined daytime and nighttime 
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

  # filter for entries in hours - DayTime
    df_DayTime <- subset(dt, format(Time,'%H') %in% sprintf("%02d", c(DayTime_Start_in_H:DayTime_End_in_H)))

  # filter for entries in hours - NightTime
    df_NightTime <- subset(dt, format(Time,'%H') %in% sprintf("%02d", c(00:(DayTime_Start_in_H-1), (DayTime_End_in_H+1):23))) 
 
  # export to a csv file 
    write.table(df_DayTime, paste0(tools::file_path_sans_ext(Input_FileName),"_DayTime.csv"), row.names=FALSE, col.names=TRUE, sep=",")     
    write.table(df_NightTime, paste0(tools::file_path_sans_ext(Input_FileName),"_NightTime.csv"), row.names=FALSE, col.names=TRUE, sep=",")     
    
}

