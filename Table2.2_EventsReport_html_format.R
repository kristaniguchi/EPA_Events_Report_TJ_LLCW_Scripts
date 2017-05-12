#To format the html table 2.2 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with IBWC and PT data

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.ibwc.pt = read.csv("summary_table_PT_IBWC_allevents.csv") #table with both IBWC and PT data

###############################################################################################################
#HTML Tables
library(htmlTable)

#Format html table for PT & IBWC data all
#Create dataframes, make sure all classes of numeric are set for rounding values
table.ibwc.pt.df = data.frame(as.character(table.ibwc.pt[,1]), as.numeric(table.ibwc.pt[,2]), as.numeric(table.ibwc.pt[,3]), as.numeric(table.ibwc.pt[,4]), as.numeric(table.ibwc.pt[,5]), as.numeric(table.ibwc.pt[,6]), as.character(table.ibwc.pt[,7]), as.character(table.ibwc.pt[,8]), as.numeric(table.ibwc.pt[,9]), as.numeric(table.ibwc.pt[,10])) #in order to round, must be dataframe, create fake dataframe to round
#Round the values 
round1 = txtRound(table.ibwc.pt.df[,2:6],2) #round the numeric columns
round2 = txtRound(table.ibwc.pt.df[,9:10],2) #round the numeric columns

table.2.1.PT.IBWC = data.frame(cbind(as.character(table.ibwc.pt.df[,1]), round1, round2, as.character(table.ibwc.pt.df[,7]), as.character(table.ibwc.pt.df[,8])))
names(table.2.1.PT.IBWC) <- c("Event Date", "Total Precip. (mm)", "PT.peak.q.obs.cms", "IBWC.peak.q.obs.cms", "PT.total.q.obs.mm", "IBWC.total.q.obs.mm","runoff.coeff.PT","runoff.coeff.ibwc", "event", "source")
#set the text columns as as.character
table.2.1.PT.IBWC[,1] <- as.character(table.ibwc.pt.df[,1])
table.2.1.PT.IBWC[,9] <- as.character(table.ibwc.pt.df[,7]) #changed order of source and event to the end
table.2.1.PT.IBWC[,10] <- as.character(table.ibwc.pt.df[,8]) #changed order of source and event to the end

table.2.1.PT.IBWC.tableout = htmlTable(table.2.1.PT.IBWC, 
                       rnames = rep("", times=length(table.ibwc.pt.df[,1])), #no row names
                       header = c("", "", "PT", "IBWC", "PT", "IBWC", "PT", "IBWC", "", ""),
                       cgroup = c("Event Date*", "Rainfall (mm)", "Peak Discharge (cms)", "Total Runoff (mm)","Runoff Ratio (Q/P)", "Event", "Source"), 
                       n.cgroup = c(1, 1, 2, 2, 2, 1, 1),
                       rgroup=c("Storm 1","Storm 2","Storm 3","Storm 4","Storm 5","Storm 6","Storm 7","Storm 8","Storm 9","Storm 10"), 
                       n.rgroup=c(3,2,1,1,1,2,1,2,1,1),
                       caption="Table 2.2 Summary of storm events defined in Table 2. Source refers to which dataset was used as the final observed data.")
print(table.2.1.PT.IBWC.tableout)

###############################################################################################################


