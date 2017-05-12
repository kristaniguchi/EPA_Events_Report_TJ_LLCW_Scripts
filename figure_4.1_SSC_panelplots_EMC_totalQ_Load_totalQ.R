#Figure 4.1 in EPA Events Report, Annual Precip vs. Sediment removed from traps TJE
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)


dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.4.1 = read.csv("sed_loadings_trap_excavation_annual.csv") #table with date and total excavation from TJE traps
tje.2006.2012 = table.4.1[3:9,]
###############################################################################################################

#Annual Precip from NOAA TJE station: downloaded from http://cdmo.baruch.sc.edu/get/export.cfm
precip = read.csv("TJRTLMET_precip_2006_2012.csv") 
date.time = strptime(precip$DateTimeStamp,"%m/%d/%Y %H:%M")
date = format(date.time,"%Y-%m-%d" )
month = format(date.time,"%m" )
year = format(date.time, "%Y")

#water year
fname.wy = paste(dir, "/","function_water_year.R", sep="") #function to calc water year from month and year
source(fname.wy)
wateryear = water.year(month, year)
#precip.wy = data.frame(cbind(precip, wateryear))

#sum total precip by wy
annual.precip.mm = aggregate(as.numeric(as.character(precip$TotPrcp_mm)), by= list(as.character(wateryear)), FUN=sum)
annual.precip.mm.2006.2012 = annual.precip.mm[1:7,] #only use wy 2006-2012

###############################################################################################################

#Figure 4.1
par(mar = c(4, 4.1, 0.5, 0.1))
plot(annual.precip.mm.2006.2012[,2], as.numeric(as.character(tje.2006.2012$Mass_remove_tons)), xlab="Annual Precip. (mm)", ylab= "Metric tons of sediment removed",cex=1.25, pch=16)
