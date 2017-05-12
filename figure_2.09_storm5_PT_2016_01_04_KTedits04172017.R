#Storm Event 5: 1/2016
#Figure 6 in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#1 storm event (E1)

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
dir.concepts.main = "C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016" #for CONCEPTS output summary from Main

###############################################################################################################

#Read in barometric and PT data, do atm correction
#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)

#1. not using barometric from TJE, only TJE naval

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_Jan05_2016.csv", header=TRUE)
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
#Adjustment of Barometric data for 01/2016 Event
#TJE Naval data ajusted based on 01/04/2016 at 10:00 am mean
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2016-01-04 10:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2016-01-04 10:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval
#plot the pressure from the pt and other barometric sources
plot(obs$date.time, obs$PT, type="l", xaxt = "n", col="blue",  xlab = "Date", ylab = "Pressure (m)")# , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
#also tried adjusting based on Sunday, but did not work well, see PT_script_atmpressure_waterlevel.R to see sunday


###############################################################################################################

#Updated Atmospheric Adjustment using TJE Naval (KT 4/20/2017), interpolation at the breaks

#Adjust the TJE naval atm pressure to match better! 4/20/2017
plot(obs$date.time, obs$PT, type="l", xaxt = "n", col="blue",  xlab = "Date", ylab = "Pressure (m)")# , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
lines(approx.tje.naval$x, tje.naval.adj-0.08, type = "l", col="green")
abline(v=as.POSIXct("2016-01-04 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2016-01-05 18:30", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2016-01-06 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2016-01-09 00:00", format="%Y-%m-%d %H:%M"), lty=2) #add break with tje.naval.adj+0.01
#breaks
break.dates = c(as.POSIXct("2016-01-04 00:00:00 PST"), as.POSIXct("2016-01-05 18:30:00 PST"), as.POSIXct("2016-01-06 00:00:00 PST"), as.POSIXct("2016-01-09 00:00:00 PST"))
corr = c(diff.mean.tje.naval, diff.mean.tje.naval, diff.mean.tje.naval+0.08, diff.mean.tje.naval+0.08)
atm.corr.interp = approx(break.dates, corr, obs$date.time)
adj2.atm.corr.tje.naval  = pressure.tje.naval.m.approx - atm.corr.interp$y
#Updated plot
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green") #matches the beginning best

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_Jan05_2016_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)

###############################################################################################################

#CACULATE THE ADJUSTED STAGE
#calculate the stage, us for loop to replace all of the negative values with 0
stage.m = obs$PT - adj2.atm.corr.tje.naval 
stage.m[stage.m<0]<-0 #replace all neg values with 0

#plot the stage
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n")
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

#calculate Q
path.name.calcQ.mannings = paste(dir, "/", "function_calc_Q_mannings.R", sep="")
source(path.name.calcQ.mannings)
q.cms = calculateQ.mannings(stage.m, 0.013) #calc q based on PT stage using 0.013 n

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate peak q and total q in mm for whole storm 
peakq.cms = max(q.cms, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 
mcm = (total.q.mm/1000*10231811.9)/1000000 #formula to calculate MCM

###############################################################################################################

#IBWC Bubbler data at outlet of GC!
#IBWC data vs obs_PT
ibwc = read.csv(file = "IBWC_GC_Bubbler2_2014_72016.csv", skip=2, sep=",", header=TRUE) #sheet from TJE for barometric data
date.time = strptime(ibwc$Date, "%m/%d/%Y %H:%M") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc.stage.m= ibwc$Data..ft.*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$Data..ft.-1283.6
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847
ibwc2 = data.frame(cbind(ibwc, date.time, date, time, ibwc.stage.m, ibwc.q.cms))

plot(approx.tje.naval$x, stage.m,  ylim=c(0,2), type="l",  xlab = 'time',   ylab = "Stage (m)")
lines(ibwc2$date.time,ibwc2$ibwc.stage.m)
plot(ibwc2$date.time,ibwc.q.cms, type="l",lty=2)

#Rising and Falling limb subset
ind.start = grep(as.POSIXct(as.character(precip$date[1]), format="%Y-%m-%d"), ibwc2$date)
ind.end = grep(as.POSIXct(as.character(precip$date[length(precip00$date.time)]), format="%Y-%m-%d"), ibwc2$date)
ibwc3 = ibwc2[ind.start[1]: ind.end[length(ind.end)],]
  plot(ibwc3$date.time, ibwc3$Data..ft., type="l")
  abline(v=as.POSIXct("2016-01-05 17:08", format="%Y-%m-%d %H:%M"),lty=2) #the peak, rising 
  ind.peak = grep(as.POSIXct("2016-01-05 17:08", format="%Y-%m-%d %H:%M"), ibwc3$date.time) #index of the max, which is also the #of values that are on the rising limb
rising = rep("rising", ind.peak) 
falling = rep("falling", length(ibwc3$date.time)- ind.peak)
rising.falling = c(rising, falling)
ibwc.4 = cbind(ibwc3, rising.falling)

#Calculate IBWC Q with updated rising and falling rating curves
#rising: Q = 25.26374*stage - 21.61612
rising.sub = ibwc.4[ibwc.4$rising.falling== "rising",] 
ibwc.Q.cms.adj.rating.rising = 19.60762*rising.sub$ibwc.stage.m -17.76322
ibwc.Q.cms.adj.rating.rising[ibwc.Q.cms.adj.rating.rising<0]<-0
#falling rating: Q = 30.29165*stage - 29.67138
falling.sub = ibwc.4[ibwc.4$rising.falling== "falling",]
ibwc.Q.cms.adj.rating.falling = 21.20415*falling.sub$ibwc.stage.m -20.76996
ibwc.Q.cms.adj.rating.falling[ibwc.Q.cms.adj.rating.falling<0]<-0
#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted = c(ibwc.Q.cms.adj.rating.rising,ibwc.Q.cms.adj.rating.falling)

#Plot the updated rising/falling rating curve
plot(ibwc.4$date.time, ibwc.Q.cms.adjusted, type="l")

###############################################################################################################
#Panel Plots --> Use 2nd one!

#1. ORIGINAL FIGURE 5
#FIGRUE 2
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[1440]))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)")#,  ylim=c(10.2,10.45+.01))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
legend("topleft", c("TJE","TJE Naval","PT"),  col=c("red","green","blue"),lwd=1, inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)")
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n')
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=2)


#IBWC Overall Panel Plots of everything - Updated atm 04/20/2017
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[1440]))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085))
legend("topleft", "E1.PT", bty="n", cex=1.25, inset=c(0.265,-.085)) 
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-06 03:00", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 05:08", format="%Y-%m-%d %H:%M"),lty=2) #start time for ibwc q
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)")#,  ylim=c(10.2,10.45+.01))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green")
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"), lty=1,inset=c(.05,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-06 03:00", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 05:08", format="%Y-%m-%d %H:%M"),lty=2) #start time for ibwc q
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)", ylim=c(0,2))
lines(ibwc2$date.time,ibwc2$ibwc.stage.m, lty=2)
legend("topleft", c("IBWC Stage","PT","Visual Stage"),lwd=c(1,1,NA), lty=c(2,1,NA), pch=c(NA,NA,8), inset=c(.05,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-06 03:00", format="%Y-%m-%d %H:%M"),lty=2)
abline(v=as.POSIXct("2016-01-05 05:08", format="%Y-%m-%d %H:%M"),lty=2) #start time for ibwc q
#add in visual stage pt
points(as.POSIXct("2016-01-06 11:00", format="%Y-%m-%d %H:%M"), 0.25, pch=8) #visual stage outlet
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', ylim=c(0,20))
lines(ibwc.4$date.time, ibwc.Q.cms.adjusted, lty=2, col="black")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2016-01-05 09:18", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-05 17:33", format="%Y-%m-%d %H:%M"),lty=1)
abline(v=as.POSIXct("2016-01-06 03:00", format="%Y-%m-%d %H:%M"),lty=2) #end time for ibwc q
abline(v=as.POSIXct("2016-01-05 05:08", format="%Y-%m-%d %H:%M"),lty=2) #start time for ibwc q
#add in q pt from the visual stage at outlet
visual.q.calc = calculateQ.mannings(0.25, 0.013)
points(as.POSIXct("2016-01-06 11:00", format="%Y-%m-%d %H:%M"), visual.q.calc, pch=8) #visual stage outlet
legend("topleft", c("PT", "IBWC", "Visual Q calc"), pch=c(NA,NA,8), lty=c(1,2,NA), col=c("black","black","black"), inset=c(.05,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
#legend("topleft", c("IBWC Orig Rating","PT", "IBWC Updated Rating", "Visual Q calc"), pch=c(NA,NA,NA,8), lty=c(2,1,2,NA), col=c("black","black","red","black"), inset=c(.05,-.05), cex=0.75,bty = "n" ) #bty="n" means no box

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#To figure out the ibwc stage during visual stage measurement and PT
#approx the ibwc stage during the visual stage measurement time and PT stage&q at that time
ibwc.stage.visual.approx = approx(ibwc2$date.time,ibwc2$ibwc.stage.m, as.POSIXct("2016-01-06 11:00:00", format="%Y-%m-%d %H:%M")) 
ibwc.q.visual.approx = approx(ibwc.4$date.time,ibwc.Q.cms.adjusted, as.POSIXct("2016-01-06 11:00:00", format="%Y-%m-%d %H:%M")) 
PT.stage.visual.approx = approx(approx.tje.naval$x, stage.m, as.POSIXct("2016-01-06 11:00:00", format="%Y-%m-%d %H:%M")) 
PT.q.visual.approx = approx(obs$date.time, q.cms, as.POSIXct("2016-01-06 11:00:00", format="%Y-%m-%d %H:%M")) 

###############################################################################################################

#Events summary total precip, peak Q, and total Q for E1
#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall

#daily total precip for 2016-01-04
precip1 = precip[precip$date == "2016-01-04",]
precip_20160104 = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2016-01-04 is the last value for that day
#daily total precip for 2016-01-05
precip2 = precip[precip$date == "2016-01-05",]
precip_20160105.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #cum precip up to 2016-01-05
precip_20160105 = precip_20160105.cum - precip_20160104 #total precip for 2016-01-05
#daily total precip for 2016-01-06
precip3 = precip[precip$date == "2016-01-06",]
precip_20160106.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #cum precip up to 2016-01-06
precip_20160106 = precip_20160106.cum - precip_20160105.cum  #total precip for 2016-01-06
#daily total precip for 2016-01-07
precip4 = precip[precip$date == "2016-01-07",]
precip_20160107.cum = precip4$Cumulative.rain.mm[length(precip4$date)] #cum precip up to 2016-01-07
precip_20160107 = precip_20160107.cum - precip_20160106.cum  #total precip for 2016-01-07
#daily total precip for 2016-01-07
precip5 = precip[precip$date == "2016-01-08",]
precip_20160108.cum = precip5$Cumulative.rain.mm[length(precip5$date)] #cum precip up to 2016-01-07
precip_20160108 = precip_20160108.cum - precip_20160107.cum  #total precip for 2016-01-07

total = precip_20160104+precip_20160105+precip_20160106+precip_20160107+precip_20160108

#total rain prior to event: start rainfall to 2016-01-05 09:18 
prior.rain = precip2$Cumulative.rain.mm[precip2$time == "09:18:00"]

#Event 1: start 2016-01-05 09:18 to 2016-01-05 17:33 (will use PT for E1)
#calc total rainfall, total q, peak q for this new subse
totalp.event1 = precip2$Cumulative.rain.mm[precip2$time == "17:33:00"] - prior.rain
ind.start = grep(as.POSIXct("2016-01-05 09:20:00"),obs$date.time)
ind.end1 = grep(as.POSIXct("2016-01-05 17:35:00"),obs$date.time)
event1.q.cms = q.cms[ind.start:ind.end1]
event1.date.time = obs$date.time[ind.start:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms, event1.date.time)
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="01/05/16", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2016-01-05 13:03:00") #start rainfall at 1/5/2016 13:03

#remaining rainfall 2016-01-05 17:33 to 2016-01-06 20:08
remaining.rain = precip3$Cumulative.rain.mm[precip3$time == "20:08:00"]  - precip2$Cumulative.rain.mm[precip2$time == "17:33:00"]

#IBWC E1: total rain prior to event: start rainfall to 2016-01-05 09:18 
  #abline(v=as.POSIXct("2016-01-06 03:00", format="%Y-%m-%d %H:%M"),lty=1) #end time for ibwc q
  #abline(v=as.POSIXct("2016-01-05 05:08", format="%Y-%m-%d %H:%M"),lty=1) #start time for ibwc q
prior.rain = precip2$Cumulative.rain.mm[precip2$time == "09:18:00"] #no rain 

#IBWC Event 1: start 2016-01-05 09:18 to 2016-01-06 03:00
#calc total rainfall, total q, peak q for this new subse
totalp.event1.ibwc = precip2$Cumulative.rain.mm[precip2$time == "17:33:00"] - prior.rain #no rain from the end 1/5 to 2016-01-06 04:13:00
ibwc2$date.time[8000:8500]
ind.start = grep(as.POSIXct("2016-01-05 05:08"), ibwc.4$date.time) 
ind.end1 = grep(as.POSIXct("2016-01-06 04:13:00"), ibwc.4$date.time) #end at 
event1.q.cms.ibwc = ibwc.Q.cms.adjusted[ind.start:ind.end1]
event1.date.time.ibwc = ibwc.4$date.time[ind.start:ind.end1]
peakq.event1.ibwc = max(event1.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.1.ibwc = calculate.total.Q.mm(event1.q.cms.ibwc, event1.date.time.ibwc)
runoff.coeff.ibwc = total.q.mm.1.ibwc/totalp.event1.ibwc 
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="01/05/16", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(ibwc.Q.cms.adjusted==peakq.event1.ibwc)
peak.date.time = ibwc.4$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2016-01-05 13:03:00") #start rainfall at 1/5/2016 13:03

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from PT only (this is the usable q timeseries with all the pt data)
date.time2 = data.frame(obs$date.time)
Event = rep("E1",times=length(obs$date.time))
data = cbind(date.time2, q.cms, stage.m, Event)
source = rep("PT", times=length(q.cms))

#OUTPUT discharge timeseries for A rating only! --> PT only data during that event 1 subset
#E1
source.E1 = rep("PT", times=length(event1.q.cms))
event1 = rep("E1",times=length(event1.q.cms))
q.data.all = data.frame(cbind(as.character(event1.date.time), event1.q.cms, source.E1, event1))
names(q.data.all) <- c("date.time", "q.cms", "source", "event")

#output summary for observed data (with "A" rating, use as observed)
date = c("2016-01-05")
event = c("E1")
source = c("PT") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1)
total.q.obs.mm = c(total.q.mm.1)
total.precip.mm = c(totalp.event1)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv = paste(dir, "/", "summary_20160105_observed_q.csv", sep="")
write.csv(obs.summary, file=fpathcsv, row.names=F)

#output summary for both PT and IBWC data! Use date, event, source,
PT.peak.q.obs.cms = c(peakq.event1)
IBWC.peak.q.obs.cms = c(peakq.event1.ibwc)
PT.total.q.obs.mm = c(total.q.mm.1)
IBWC.total.q.obs.mm = c(total.q.mm.1.ibwc)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv2 = paste(dir, "/", "summary_20160105_observed_q_PT_IBWC.csv", sep="")
write.csv(obs.summary.PT.IBWC, file=fpathcsv2, row.names=F)


##############################################################################################################################
#For Doug Liden (EPA) aggregate precip to 15 minute intervals
#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_Jan05_2016_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)
x = 0.25 #0.25 mm for each tip
rain.mm.depth = rep(x, length(precip$Date.time))
precip.new = cbind(precip, rain.mm.depth)

timebreak1 = ISOdatetime(2016, 1, 4, 2, 0, 0) + (seq(0,4*24*5)*15*60) #4 breaks per 24 hours for 5 days; 15 min breaks
fac = cut(precip.new$date.time, breaks = timebreak1)
data2 = data.frame(cbind(precip.new, fac))
agg = data.frame(aggregate(rain.mm.depth, by  = list(fac), FUN = sum))
write.csv(agg, file="01042016_precip_15min_LLC_Doug.csv")

#For Napo Gudino (CICESE) break into every 6 minute intervals
timebreak1 = ISOdatetime(2016, 1, 4, 2, 0, 0) + (seq(0,10*24*5)*6*60) #10 breaks every 24 hours for 5 days; every 6 minutes
fac = cut(precip.new$date.time, breaks = timebreak1)
data2 = data.frame(cbind(precip.new, fac))
agg = data.frame(aggregate(rain.mm.depth, by  = list(fac), FUN = sum)) #second column has precip depth mm at each time stamp

cum.rain.new.mm = cumsum(agg$x) #cumulative rainfall mm
date.time.new = agg$Group.1 #the new date time in 6 minute intervals

##############################################################################################################################
#For the Main channel outlet timeseries in CONCEPTS Summary 
#f.concepts = paste(dir.concepts.main, "/", "TimeSeries_01.txt", sep="")
#setwd(f.concepts) 

#main = read.table("TimeSeries_01.txt",skip = 9,header = FALSE)
#names(main)<- c("DATE", 
                #"TIME",
                #"DISCHARGE.cms", 
                #"VELOCITY.ms",   
                #"DEPTH.m"     ,
                #"STAGE.m"     ,
                #"AREA.m2"    ,
                #"TOP WIDTH.m2", 
                #"PERIMETER.m" , 
               # "RADIUS.m",
                #"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR","SILTDIS","SANDDIS","GRAVELDIS","TOTALDIS","SILTYLD","SANDYLD","GRAVELYLD","TOTALYLD","CUMBED","THALWEG","CUMLAT","SAFETYL","SAFETYR_APCOHL_APCOHR_POREL_PORER","MATRICL","MATRICR","WBLKL","WBLKR","WWATERL","WWATERR","HYDPRL","HYDPRR","PHREAL","PHREAR","BANKTOPL","BANKTOPR")
                #"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR")

#date.time0 = paste(as.character(main$DATE), as.character(main$TIME), sep = " ")
#time = strptime(as.character(main$TIME),"%H:%M:%S")
#date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
#month = as.numeric(format(date.time, "%m"))
#month.day = format(date.time, "%m/%d")
#year = format(date.time, "%Y")
#month.day.year = format(date.time, "%m/%d/%Y")
#min.q = min(main$DISCHARGE.cms) #check to see what the minimum value is in concepts timeseries, is it diff than baseflow.sum?
#q.adjusted.cms = main$DISCHARGE.cms - 0.133 #0.133 is the baseflow sum to be subtracted out
#main2 = cbind(main, date.time, month, month.day, year, month.day.year, time, q.adjusted.cms)

#CONCEPTS hydrograph in comparison with Observed: 01/05/16
#sub = main2[main2$month.day.year=="01/05/2016",]
#peakq.concepts1 = max(sub$q.adjusted.cms)
#peak.depth = max(sub$DEPTH.m)
#peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
#peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
#time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts

#plot(obs$date.time, q.cms, type="l", main="01/05/16", xlab="Time", ylab="Discharge (cms)", xlim = c(as.POSIXct("2016-01-05 00:00:00"),as.POSIXct("2016-01-06 00:00:00")))
#lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
#legend("topleft", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box







