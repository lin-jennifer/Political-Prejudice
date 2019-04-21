###################################################
#File: Management File
#Author: Jennifer Lin
#Purpose: Create specific composite dataset from the "Knowledge" folder 
#Root data: ANES 2016 Time Series ( https://electionstudies.org/project/2016-time-series-study/)
#Requires: `Knowledge.R`, `Participation.R` and `Feel2016.R` in `Knowledge` folder 
#Git Repository: https://github.com/lin-jennifer/Political-Prejudice.git
###################################################

#Run script files for Knowledge, Participation and Feeling2016 - IN THAT ORDER!!!!
#By doing so, you can get the variables that are needed to create the composite dataset.

#Reset working directory to this project
setwd("~/Desktop/Working/Political-Prejudice/RM-Project")

#Political knowledge items
which( colnames(anes2016)=="biden" ) #1199
which( colnames(anes2016)=="roberts" ) #1203

#Political Participation Variables
which( colnames(anes2016)=="talk" ) #1204
which( colnames(anes2016)=="vote16" ) #1214

#Feeling Thermometer
which( colnames(anes2016)=="feeldem" ) #1216
which( colnames(anes2016)=="feelcons" ) #1219

which( colnames(anes2016)=="pid3" ) #1197
which( colnames(anes2016)=="party" ) #1198
which( colnames(anes2016)=="pid7" ) #1220
which( colnames(anes2016)=="ideo3" ) #1222

#Find items needed for the study and build composite dataset for export
#It is easier to work with a smaller, already cleaned dataset rather than something larger
composite <-anes2016[, c(2, 1197:1214, 1216:1222)]

library(rio)
export(composite, "RMProjData.csv")

