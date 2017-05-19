#Figure 2.5
#Total event discharge vs. total precip.
#Using data from EPA Events Report Table 2.3
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Read in table 2.3 summary table of observed events
data = read.csv("summary_table_obs_allevents.csv")

###############################################################################################################

#Figure 2.5 - rainfall-runoff relationship
#plot in log-log space
par(mar = c(4, 4.1, 0, 0.1))
plot(data$total.precip.mm, data$total.q.obs.mm, log = "xy", ylab = "Event Total Q (mm)", xlab = "Event Total Precip. (mm)", pch=16, cex = 1.2)
  