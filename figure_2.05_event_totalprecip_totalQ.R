#Figure 2.4
#Total event discharge vs. total precip.
#Using data from EPA Events Report Table 2.3
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory to where the scripts are saved!
setwd(dir)

###############################################################################################################

#Read in table 2.3 summary table of observed events
data = read.csv("summary_table_obs_allevents.csv")

###############################################################################################################

#Figure 2.4 - rainfall-runoff relationship
#plot in log-log space
par(mar = c(4, 4.1, 0, 0.1))
plot(data$total.precip.mm, data$total.q.obs.mm, log = "xy", ylab = "Event Total Q (mm)", xlab = "Event Total Precip. (mm)", pch=16, cex = 1.2)
  