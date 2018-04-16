LC_GapFilling <- function(FilePath, Output_Name) {

# R Functions to process dataset from Load Cell Array
# It pads missing timestamps and measurements and column names
#
# Author : Kuo-Hsine Chang, Ph.D. Dept. of Plant Agricuture, University of Guelph
# Contact: changks.github.io
# Version: 04162018

  # setup system time zone
    Sys.setenv(tz="Canada/Eastern")
  # include library
    library(zoo)
  # read the csv file  
    dt = read.csv(FilePath, header=TRUE);
  # provide the column name
    names(dt) <- c("Time", "LC1", "LC2", "LC3", "LC4", "LC5", "LC6", "LC7", "LC8", "LC9", "LC10", "LC11", "LC12")  
  # reformat timestamp
    dt$Time<-as.POSIXct(dt$Time,format="%Y-%m-%d %H:%M", tz = "Canada/Eastern")
  # set date to Index
    dt.zoo<-zoo(dt[,-1],dt[,1]);
  # merge two datasets 
    dt_m <- merge(dt.zoo,zoo(,seq(start(dt.zoo),end(dt.zoo),by="min")), all=TRUE)
  # export to a csv file    
    write.zoo(dt_m, file = Output_Name, index.name = 'Time', sep = ',', col.names = T) 
}