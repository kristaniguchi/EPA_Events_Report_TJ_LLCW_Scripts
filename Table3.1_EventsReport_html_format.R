#To format the html table 3.1 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with SSC data summary

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.1.ssc = read.csv("summary_table.3.1.csv") #table with both IBWC and PT data

###############################################################################################################
#HTML Tables
library(htmlTable)

#Format html table for PT & IBWC data all
#Create dataframes, make sure all classes of numeric are set for rounding values
table.3.1.ssc.df = data.frame(table.3.1.ssc)
#Round the values 
round1 = txtRound(table.3.1.ssc.df[,2:3],2) #round the numeric columns

table.3.1.final = data.frame(cbind(as.character(table.3.1.ssc.df[,1]), round1[,1], round1[,2], as.character(table.3.1.ssc.df[,4])))
names(table.3.1.final) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
#set the text columns as as.character
table.3.1.final[,1] <- as.character(table.3.1.ssc.df[,1])
table.3.1.final[,2] <- as.character(table.3.1.final[,2]) 
table.3.1.final[,3] <- as.character(table.3.1.final[,3]) 
table.3.1.final[,4] <- as.character(table.3.1.ssc.df[,4]) 

table.3.1.SSC.tableout = htmlTable(table.3.1.final, 
                                       rnames = rep("", times=length(table.3.1.ssc.df[,1])), #no row names
                                       header = c("Date", "SSC (g/L)", "Q (cms)", "Event"),
                                       rgroup= c("Storm 1",  "Storm 2",  "Storm 3",  "Storm 6",  "Storm 9",  "Storm 10"), 
                                       n.rgroup=c(3,5,2,7,4,2),
                                       caption="Table 3.1.  Suspended sediment concentration (SSC) for all collected samples.  EMC is the event-mean concentration.")
print(table.3.1.SSC.tableout)

###############################################################################################################


