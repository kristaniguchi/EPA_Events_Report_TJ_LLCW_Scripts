#Figure 3.9 in EPA Events Report, SSC plots
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#2 Panel plot of EMC vs. total Q and Load vs. total Q

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.3.ssc = read.csv("summary_table.3.3.csv") #table with both IBWC and PT data

###############################################################################################################

#Figure 3.7
#2 Panel plots for SSC
layout(matrix(1:2, ncol = 1), widths = 2, respect = FALSE)
par(mar = c(4, 4.1, 0, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$EMC), pch=16, cex = 1.5, ylab="Event-mean SSC (g/L)", xlab = "Total Q (mm)")
par(mar = c(4, 4.1, 1, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$load.ton), pch=16, cex = 1.5,ylab="Suspended Sediment Load (tons)", xlab = "Total Q (mm)")
