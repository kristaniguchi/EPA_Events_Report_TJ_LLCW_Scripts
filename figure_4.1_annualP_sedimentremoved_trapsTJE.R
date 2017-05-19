#Figure 4.1 in EPA Events Report, Annual Precip vs. Sediment removed from traps TJE
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)


dir = "C:/Users/Kris/Documents/GitHub/EPA_Events_Report_TJ_LLCW_Data" #folder containing the data
setwd(dir) #set working directory to the data directory

script.dir = "C:/Users/Kris/Documents/GitHub/EPA_Events_Report_TJ_LLCW_Scripts" #folder containing the scripts

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts

#Sediment loadings, uncorrected and corrected for trap efficiency from table 4.2
table.trap.efficiency = read.csv("sediment_loads_2006_2012_corrected_uncorrected_05182017.csv") #table with date and total excavation from TJE traps
tje.corr.2006.2012.totals = table.trap.efficiency[as.character(table.trap.efficiency$Sed_size) == "Total",] #only use annual totals

###############################################################################################################

#Annual Precip from Imperial Beach (IB) Airport Met station: from Napo, used in AGNPS rainfall timeseries
precip = read.csv("climate_daily_IB_Napo.csv") 
date.time0 = paste(precip$Month, "/", precip$Day, "/", precip$Year, sep="")
date.time = strptime(date.time0,"%m/%d/%Y")
date = format(date.time,"%Y-%m-%d" )
month = format(date.time,"%m" )
year = format(date.time, "%Y")
precip2 = cbind(precip, date, month, year)

#sum total precip by wy
annual.precip.mm.year = aggregate(as.numeric(as.character(precip2$Precip)), by= list(as.character(year)), FUN=sum) #sum by water year
annual.precip.mm.2006.2012.year = annual.precip.mm.year[2:8,] #only use wy 2006-2012

###############################################################################################################

#Figure 4.1
par(mar = c(4, 4.1, 0.5, 0.1))
plot(annual.precip.mm.2006.2012.year[,2], as.numeric(tje.corr.2006.2012.totals$Corrected_load_tons),  col="dark grey", ylim = c(30000,80000), xlab="Annual Precip. (mm)", ylab= "Metric tons of sediment removed",cex=1.25, pch=16) #log="xy"
points(annual.precip.mm.2006.2012.year[,2], as.numeric(tje.corr.2006.2012.totals$Tons_removed_uncorrected), xlab="Annual Precip. (mm)", ylab= "Metric tons of sediment removed",cex=1.25, pch=16)
legend("bottomright", c("Uncorrected", "Trap Efficiency Corrected"), col=c("black", "dark grey"), pch=16)
