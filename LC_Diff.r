LC_Diff <- function(Input_FileName) {

# R Functions to process dataset from Load Cell Array
# It calculates the difference between two timesteps -- current vs. previsou one.
# Diff = current one - previous one
# Negetive Diff means lossing weight; Postive Diff means gaining weight
#
# Author : Kuo-Hsine Chang, Ph.D. Dept. of Plant Agricuture, University of Guelph
# Contact: changks.github.io
# Version: 04162018


  # load library
    library(zoo)
    library(data.table)
    
  # read the csv file  
    dt = fread(Input_FileName, header = T, sep = ','); 

  # calcualte difference between two timesteps (current - previous)
  
    Diff_dt <- dt[, lapply(.SD, function(x) {x1 <- diff(x)
            NA^(x1 <= -10 | x1 >=10)*x1}), .SDcols= 2:ncol(dt)
           ][, timestamp := dt[[1]][-1]]       
           
    Diff_dt[, c(ncol(Diff_dt), 1:(ncol(Diff_dt)-1)), with=FALSE] 
    
    Diff_dt <- data.frame(Diff_dt)

    Diff_dt <- Diff_dt[c(13,1:12)]

  # export to a csv file 
    write.table(Diff_dt, paste0(tools::file_path_sans_ext(Input_FileName),"_Diff.csv"), row.names=FALSE, col.names=TRUE, sep=",")         
}