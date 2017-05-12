#To format the html table 3.1 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with SSC data summary

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.3.ssc = read.csv("summary_table.3.3.csv") #table with both IBWC and PT data

###############################################################################################################
#HTML Tables
library(htmlTable)

#Format html table for PT & IBWC data all
#Create dataframes, make sure all classes of numeric are set for rounding values
table.3.3.ssc.df = data.frame(table.3.3.ssc)
#Round the values 
round1 = txtRound(table.3.3.ssc.df[,2:6],2) #round the numeric columns
round2 = round(table.3.3.ssc.df[,3],0)
round3 = round(table.3.3.ssc.df[,4],1)
#load.round = signif(table.3.3.ssc.df[,4], 3)

table.3.3.final = data.frame(cbind(as.character(table.3.3.ssc.df[,1]), round1[,1], round2, round3, round1[,4], round1[,5]))
names(table.3.3.final) <- c("Event Date*", "Total Q (mm) ", "Total Q (m3)", "Load (tons)", "Volume weighted mean concentration (g/L)", "EMC")
#set the text columns as as.character
table.3.3.final[,1] <- as.character(table.3.3.final[,1])
table.3.3.final[,2] <- as.character(table.3.3.final[,2]) 
table.3.3.final[,3] <- as.character(table.3.3.final[,3]) 
table.3.3.final[,4] <- as.character(table.3.3.final[,4])
table.3.3.final[,5] <- as.character(table.3.3.final[,5])
table.3.3.final[,6] <- as.character(table.3.3.final[,6])

table.3.3.SSC.tableout = htmlTable(table.3.3.final, 
                                   rnames = rep("", times=length(table.3.3.ssc.df[,1])), #no row names
                                   header = c("", "mm", "m3", "tons", "g/L", "g/L"),
                                   cgroup = c("Event Date*", "Total Q", "Load", "Volume weighted mean concentration", "Event Mean Concentration"), 
                                   n.cgroup = c(1, 2, 1, 1,1),
                                   caption="Table 3.3.  Total event suspended sediment load at the PT location for the events with SSC data. Drainage area is 10.23 km2 at the observation point.")
print(table.3.3.SSC.tableout)

###############################################################################################################


