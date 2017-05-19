#Storm Event 4: 9/15/2015
#Figure 2.11 in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated, no IBWC data for this event
#1 storm event (E1)

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Read in barometric and PT data, do atm correction
#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)

#1. not using barometric from TJE, only TJE naval

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_sep15_2015.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = cbind(obs0, date.time, date, time)

#step 3 using the TJE naval barometric data
#TJE Naval Base Pressure data (time stamp is from GMT to PSD)
tje.naval0 = read.table(file = "atm_pressure_naval_base_TJEstuary.txt", skip=1, header=TRUE, sep=",", colClasses="character")
year = substr(tje.naval0$Date, 1,4) #to format the date extract out the year, month, day and time
month = substr(tje.naval0$Date, 5,6)
day = substr(tje.naval0$Date, 7,8)
hour = substr(tje.naval0$HrMn, 1,2)
#hour = as.character(as.numeric(hour0)-8)
minute = substr(tje.naval0$HrMn, 3,4)
date.time0 = paste(month, "/", day, "/",year, " ", hour, ":", minute, sep = "")
date.time.gmt = strptime(date.time0,"%m/%d/%Y %H:%M",tz="GMT") #orginial data in GMT UTC time zone!
timezone.gmt.ct = as.POSIXct(date.time.gmt)
date.time = strptime(as.character(format(timezone.gmt.ct,"%m/%d/%Y %H:%M", tz="America/Los_Angeles")), "%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje.naval = cbind(tje.naval0, date.time, date, time)
pressure.tje.naval.m = as.numeric(tje.naval$Slp)/(0.09806649999980076*1000)  #from mb to m
approx.tje.naval = data.frame(approx(tje.naval$date.time, pressure.tje.naval.m, obs$date.time)) #pressure in mb
pressure.tje.naval.m.approx = approx.tje.naval$y 

#ORIGINAL Adjustment of Barometric data for 9/2015 Event
#TJE Naval data ajusted based on Friday 2015-09-15 til noon mean (baseflow day)
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2015-09-15 12:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval

#plot the pressure from the pt and other barometric sources just to see
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n") 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
abline(v=as.POSIXct("2015-09-15 12:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)

###############################################################################################################

#Updated Atmospheric Adjustment using TJE Naval (KT 4/20/2017), interpolation at the breaks
#Adjust the TJE naval atm pressure to match better! 4/20/2017
lines(approx.tje.naval$x, tje.naval.adj-0.095, type = "l", col="green")
abline(v=as.POSIXct("2015-09-16 19:00", format="%Y-%m-%d %H:%M"), lty=2)
lines(approx.tje.naval$x, tje.naval.adj-0.145, type = "l", col="green")
abline(v=as.POSIXct("2015-09-15 00:00", format="%Y-%m-%d %H:%M"), lty=2)

plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n") 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
abline(v=as.POSIXct("2015-09-15 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-15 12:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 19:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct(approx.tje.naval$x[length(approx.tje.naval$x)], format="%Y-%m-%d %H:%M"), lty=2) #add break with tje.naval.adj+0.01
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
lines(approx.tje.naval$x, tje.naval.adj-0.09, type = "l", col="green")
lines(approx.tje.naval$x, tje.naval.adj-0.145, type = "l", col="green")
#breaks
break.dates = c(as.POSIXct("2015-09-15 00:00:00 PST"), as.POSIXct("2015-09-15 12:00:00 PST"), as.POSIXct("2015-09-16 05:52:00 PST"), as.POSIXct("2015-09-16 19:00:00 PST"), as.POSIXct(approx.tje.naval$x[length(approx.tje.naval$x)]))
corr = c(diff.mean.tje.naval, diff.mean.tje.naval, diff.mean.tje.naval+0.09, diff.mean.tje.naval+0.145, diff.mean.tje.naval+0.145)
atm.corr.interp = approx(break.dates, corr, obs$date.time)
adj2.atm.corr.tje.naval  = pressure.tje.naval.m.approx - atm.corr.interp$y
#Updated plot
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green") #matches the beginning best
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=2)

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_sep15_2015_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)

###############################################################################################################

#CALCULATE THE ADJUSTED STAGE using TJE Naval (atm corrected) and Q

stage.m = obs$PT - adj2.atm.corr.tje.naval 
stage.m[stage.m<0]<-0 #replace all neg values with 0

#plot the stage
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n")
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

#calculate Q
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_Q_mannings.R') #functions are saved in script directory
q.cms = calculateQ.mannings(stage.m, 0.013) #calc q based on PT stage using 0.013 n

#calculate peak q and total q in mm for whole storm 
peakq.cms = max(q.cms, na.rm = TRUE) 
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_total_Q_mm.R ') #functions are saved in script directory
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 
mcm = (total.q.mm/1000*10231811.9)/1000000 #formula to calculate MCM

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

###############################################################################################################

#2. FIGRUE 2.11 UPDATED 4/20/2017
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(as.character(obs$date.time[1])),as.POSIXct("2015-09-16 05:52:00"))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=1)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=1)
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", ylim=c(10,10.75), xlim = c(as.POSIXct(as.character(obs$date.time[1])),as.POSIXct("2015-09-16 05:52:00"))) #, xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green")
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"),lwd=1, inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=1)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=1)
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)", xlim = c(as.POSIXct(as.character(obs$date.time[1])),as.POSIXct("2015-09-16 05:52:00")))#xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=1)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=1)
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', xlim = c(as.POSIXct(as.character(obs$date.time[1])),as.POSIXct("2015-09-16 05:52:00")))#,xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-03-03 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2015-09-15 10:47", format="%Y-%m-%d %H:%M"), lty=1)
abline(v=as.POSIXct("2015-09-16 05:52", format="%Y-%m-%d %H:%M"), lty=1)


#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#Events summary total precip, peak Q, and total Q for E1

#Only one event 
#daily total precip for 2015-09-15
precip1 = precip[precip$date == "2015-09-15",]
precip_20150915 = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2015-09-15 is the last value for that day

#daily total precip for 2015-09-16
precip2 = precip[precip$date == "2015-09-16",]
precip_20150916.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2015-09-15 is the last value for that day
precip_20150916 = precip_20150916.cum - precip_20150915

#Event 1: start 2015-09-15 10:47 to 2015-09-16 05:52
#calc total rainfall, total q, peak q for this new subset
totalp.event1 = precip$Cumulative.rain.mm[length(precip$date)]
ind.start = grep(as.POSIXct("2015-09-15 10:45:00"),obs$date.time)
ind.end1 = grep(as.POSIXct("2015-09-16 05:50:00"),obs$date.time)
event1.q.cms = q.cms[ind.start:ind.end1]
event1.date.time = obs$date.time[ind.start:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms, event1.date.time)
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="09/15/15", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - event1.date.time[1] #start rainfall at 2015-09-15 10:45:00

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from PT 
date.time2 = data.frame(obs$date.time)
Event = rep("E1",times=length(obs$date.time))
data = cbind(date.time2, q.cms, stage.m, Event)
source = rep("PT", times=length(q.cms))

#OUTPUT discharge timeseries for A rating only! (this case, PT data)
q.data.all = cbind(date.time2, q.cms, stage.m, source, Event)
names(q.data.all) <- c("date.time", "q.cms", "source", "event")

#output summary for observed data
date = c("2015-09-15")
event = c("E1")
source = c("PT") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1)
total.q.obs.mm = c(total.q.mm.1)
total.precip.mm = c(totalp.event1)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary, file="summary_20150915_observed_q.csv", row.names=F)

#output summary for both PT and IBWC data (No IBWC, will just put NA)! Use date, event, source, 
PT.peak.q.obs.cms = peak.q.obs.cms
IBWC.peak.q.obs.cms = c(NA)
PT.total.q.obs.mm = total.q.obs.mm
IBWC.total.q.obs.mm = c(NA)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary.PT.IBWC, file="summary_20150915_observed_q_PT_IBWC.csv", row.names=F)

