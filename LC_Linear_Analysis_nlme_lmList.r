LC_Linear_Analysis_nlme_lmList <- function(Input_FileName) {

# R Functions to process dataset from Load Cell Array
# It calcuates slope and intercept.
#
# Author : Kuo-Hsine Chang, Ph.D. Dept. of Plant Agricuture, University of Guelph
# Contact: changks.github.io
# Version: 04162018

  # load library
    library(zoo)
    library(data.table)
    library(tidyverse)
    library(lubridate)
    library(stats)
    
  # read the csv file  
    dt  <- fread(Input_FileName, header = T, sep = ',');
    
    df  <- data.frame(dt)

  # reshape data to get Time repeated for each variable
    df2 <- df %>%  
    gather(var, value, -Time) %>%
         mutate(Time=ymd_hms(Time),                ## convert to date-time variable
         date=date(Time),                          ## date info only
         timeval=Time-floor_date(Time, "day"),      ## time since beginning of day
         datevar=interaction(date, var))            ## date/var combo
                     

   # Linear Regression Analysis
   
   # na.omit: observations are removed if they contain any missing values
   # na.exclude: functions will pad residuals and predictions to the correct length by inserting NAs for omitted cases.
   # na.pass: keep all data, including NAs
   # na.fail: returns the object only if it contains no missing values

   # LM <- nlme::lmList(value ~ timeval | datevar, df2, na.action=na.pass)    
    LM <- nlme::lmList(value ~ timeval | datevar, df2, na.action=na.exclude) 

   #  LM <- nlme::lmList(value ~ timeval | datevar, df2, na.action = na.omit) 

   # export to a csv file
     result <- data.frame(Subject = rownames(coef(LM)), coef(LM), check.names = FALSE)
     write.table(result, paste0(tools::file_path_sans_ext(Input_FileName), "_nlme_lmList.csv"), row.names = FALSE, col.names = TRUE, sep = ",")       
}