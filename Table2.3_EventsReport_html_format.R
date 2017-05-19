#To format the html table 2.3 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#Table with only data used as observed

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.obs = read.csv("summary_table_obs_allevents.csv") #table with just the usable observed data (A Rating) and used as observed in comparison with simulated results
#calculate total discharge in MCM calculation for the table!
total.q.obs.MCM = (table.obs$total.q.obs.mm/1000*10231811.9)/1000000 #from mm to m3 to million m3

###############################################################################################################
#HTML Tables
library(htmlTable)

#Format html table for only A rating, usable as the final observed data
#Create dataframes, make sure all classes of numeric are set for rounding values
table.obs.df = data.frame(as.character(table.obs[,1]), as.numeric(table.obs[,2]), as.numeric(table.obs[,3]), as.numeric(table.obs[,4]), as.numeric(total.q.obs.MCM), as.numeric(table.obs[,7]), as.character(table.obs[,5]),  as.character(table.obs[,6])) #in order to round, must be dataframe, create fake dataframe to round

#Round the values 
round1 = txtRound(table.obs.df[,2:6], 2) #round the numeric columns to 2 digits, use round1[,1:3] and round1[,5]
round2 = txtRound(table.obs.df[,5:6], 3) #total q MCM needs to be rounded to 3 digits: round2[,1]

#Table!
table.2.1.obs = data.frame(cbind(as.character(table.obs.df[,1]), round1[,1:3], as.numeric(round2[,1]), as.numeric(round1[,5]), as.character(table.obs.df[,7]), as.character(table.obs.df[,8])))
names(table.2.1.obs) <- c("Event Date", "Total Precip. (mm)", "Peak.q.obs.cms", "Total.q.obs.mm", "Total.q.obs.MCM","runoff.coeff", "event", "source")
#set the text columns as as.character, was getting reformatted and not displaying correctly without doing so
table.2.1.obs[,1] <- as.character(table.obs.df[,1])
table.2.1.obs[,7] <- as.character(table.obs.df[,7]) 
table.2.1.obs[,8] <- as.character(table.obs.df[,8]) 
#HTML Table
table.2.1.obs.tableout = htmlTable(table.2.1.obs, 
                                   rnames = rep("", times=length(table.obs.df[,1])), #no row names
                                   header = c("", "", "Obs", "Obs (mm)", "Obs (MCM)", "Obs","", ""),
                                   cgroup = c("Event Date*", "Rainfall (mm)", "Peak Discharge (cms)", "Total Runoff","Runoff Ratio (Q/P)", "Event", "Source"), 
                                   n.cgroup = c(1, 1, 1, 2, 1, 1, 1),
                                   rgroup=c("Storm 1","Storm 2","Storm 3","Storm 4","Storm 5","Storm 6","Storm 8","Storm 9","Storm 10"), 
                                   n.rgroup=c(3,2,1,1,1,2,2,1,1),
                                   caption="Table 2.3 Summary of storm events defined in Table 2, final observed data (obs) used for calibration/validation.")
print(table.2.1.obs.tableout)

#Note: will need to update second table with AGNPS and CONCEPTS values for calibration report

###############################################################################################################
