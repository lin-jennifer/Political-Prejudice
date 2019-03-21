#Convert ANES Stata file to R

#Load the Data
library(haven)
anes <- read_dta("~/Desktop/Working/ANES/anes_timeseries_cdf.dta")

#Set Working Directory
setwd("~/Desktop/Working/Political-Prejudice/Feeling")

library("rio")

#Export as R
export(anes, "anes.rds")

#Export as CSV
export(anes, "anes.csv")
