#Storm Event 1: 3/2014 
#Figure 2.2 in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013
#3 storm events (E1-E3)
#SSC samples collected see .R

#Set working directory to the data folder, script directory will be used if sourcing functions 
getwd() #the directory where the script is saved 
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Read in barometric and PT data, do atm correction
#1. read in barometric data from TJE
tje0 = read.csv(file = "TJE_atm_pressure.csv", skip=2, header=TRUE) #sheet from TJE for barometric data
date.time = strptime(tje0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje = cbind(tje0, date.time, date, time)
#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_March2014.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = cbind(obs0, date.time, date, time)
#3. interpolate the barometric data to get the same time stamp as PT_level data
approx.tje = data.frame(approx(tje$date.time, tje$Data..mb., obs$date.time)) #pressure in mb
pressure.tje.m.approx = approx.tje$y /(0.09806649999980076*1000)  #from mb to m of water column

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

#ORIGINAL Adjustment of Barometric data for 3/2014 Event, using the av during baseflow
#tje data ajusted based on friday (2014-02-28) til noon mean (baseflow day, right before peak)
date = as.Date(approx.tje$x)
approx.tje2 = cbind(approx.tje, date)
ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),approx.tje2$x)
friday.tje = approx.tje2[1:ind,]
av.tje = mean(friday.tje$y/(0.09806649999980076*1000) ) #from mb to m
#TJE Naval data ajusted based on Friday til noon mean (baseflow day)
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2014-02-28 12:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)

#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje = av.tje - av.obs
tje.adj = approx.tje2$y/(0.09806649999980076*1000) - diff.mean.tje #from mb to m
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval2$y - diff.mean.tje.naval
#plot the pressure from the pt and other barometric sources
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)" , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje$x, pressure.tje.m.approx, col="red", type="l",  xlab = "Date", ylab = "Barometric Pressure (m)")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje$x, tje.adj ,type="l", col="red") #adjusted tje
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
#also tried adjusting based on Sunday, but did not work well, see PT_script_atmpressure_waterlevel.R to see sunday

###############################################################################################################

#Updated Atmospheric Adjustment using TJE Naval (KT 4/19/2017), interpolation at the breaks
#plot the pressure from the pt and TJE naval
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)" , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
#adjustment
approx.tje.naval2 #the dataset for tje naval at the times of the PT
#figure out where the breaks, areas in between breaks will be interpolated
  abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 12:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 12:10", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"), lty=2)
  abline(v=as.POSIXct("2014-03-02 17:00", format="%Y-%m-%d %H:%M"), lty=2)
  lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green") #adjusted tje.naval
  lines(approx.tje.naval$x, tje.naval.adj-0.02, type = "l", col="green") #adjusted tje.naval
  lines(approx.tje.naval$x, tje.naval.adj-0.035, type = "l", col="green") #adjusted tje.naval
break.dates = c(as.POSIXct("2014-02-28 00:00:00 PST"), as.POSIXct("2014-02-28 12:00:00 PST"), as.POSIXct("2014-02-28 15:50:00 PST"),as.POSIXct("2014-03-01 00:00:00 PST"), as.POSIXct("2014-03-01 12:10:00 PST"), as.POSIXct("2014-03-01 15:57:00 PST"), as.POSIXct("2014-03-02 17:00:00 PST")) 
  
#amt to correct by at each break date  
corr = c(diff.mean.tje.naval, diff.mean.tje.naval, diff.mean.tje.naval+0.02, diff.mean.tje.naval, diff.mean.tje.naval+0.035, diff.mean.tje.naval+0.035, diff.mean.tje.naval+0.035)
atm.corr.interp = approx(break.dates, corr, obs$date.time)
adj2.atm.corr.tje.naval  = pressure.tje.naval.m.approx - atm.corr.interp$y
#Updated plot
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)" , ylim=c(10.2,10.45),xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(atm.corr.interp$x, adj2.atm.corr.tje.naval, type = "l", col="green") #adjusted tje.naval
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-02-28 12:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-01 12:10", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-02 17:00", format="%Y-%m-%d %H:%M"), lty=2)

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_March2014_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)
###############################################################################################################

#CACULATE THE ADJUSTED STAGE using TJE Naval (atm corrected) and Q
#EVENT1: calculate the stage, replace all of the negative values with 0 using TJE naval

#stage.m = obs$PT - tje.naval.adj #old stage calc
stage.m = obs$PT - adj2.atm.corr.tje.naval 
stage.m[stage.m<0]<-0 #replace all neg values with 0

#calculate Q
source("../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_Q_mannings.R") #functions are saved in script directory
q.cms = calculateQ.mannings(stage.m, 0.013) #calc q based on PT stage using 0.013 n

#calculate peak q and total q in mm for whole storm
peakq.cms = max(q.cms, na.rm = TRUE) 
source("../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_total_Q_mm.R") #functions are saved in script directory
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 

###############################################################################################################

#IBWC Bubbler data at outlet of GC!
#IBWC data vs obs_PT
ibwc = read.csv(file = "IBWC_GC_Bubbler2_2014_72016.csv", skip=2, sep=",", header=TRUE) #sheet from TJE for barometric data
date.time = strptime(ibwc$Date, "%m/%d/%Y %H:%M") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc.stage.m= ibwc$Data..ft.*0.3048 #feet to meters conv
#Original IBWC rating curve
ibwc.q.cfs= 588.24*ibwc$Data..ft.-1283.6 
ibwc.q.cfs[ibwc.q.cfs<0]<- 0 #set all negative to zero
ibwc.q.cms = ibwc.q.cfs*0.028316847 #cfs to cms
ibwc2 = data.frame(cbind(ibwc, date.time, date, time, ibwc.stage.m, ibwc.q.cms))

#Split ibwc into rising and falling, use updated rating curves from manning's n 0.013 from each subset
#subset IBWC data from 2014-02-28 to 2014-03-01
ind1 = grep("2014-02-28", as.character(ibwc2$date)) #to get the index of start of subset on 2014-02-28
ind2 = grep("2014-03-01", as.character(ibwc2$date)) #to get the index of end of subset on 2014-02-28
sub.ibwc = ibwc2[ind1[1]:ind2[length(ind2)],] #2/28/2014-3/1/2014
#plot the subset
plot(sub.ibwc$date.time, sub.ibwc$ibwc.stage.m, type="l")

#there are 3 peaks/3 events, do rising and falling for each event!
ind.peak1 = grep(0.908304, sub.ibwc$ibwc.stage.m) #peak 1 is 0.908304 stage m
ind.end.1 = ind.peak1+2 #2 values on the falling limb
ind.peak2 = grep(0.984504, sub.ibwc$ibwc.stage.m) #peak 2
ind.end.2 = grep(0.658368, sub.ibwc$ibwc.stage.m)
peak3 = max(sub.ibwc$ibwc.stage.m) #third peak is the largest peak, use max
ind.peak3 = grep(peak3, sub.ibwc$ibwc.stage.m)
  sub.ibwc[ind.peak3,] #just to check got right index for the peak

  #multiple subsets for rising and falling limbs
ibwc.rising1.0.013 = sub.ibwc[1:ind.peak1,]
ibwc.falling1.0.013 = sub.ibwc[(ind.peak1+1):ind.end.1,]
ibwc.rising2.0.013 = sub.ibwc[(ind.end.1+1):ind.peak2,]
ibwc.falling2.0.013 = sub.ibwc[(ind.peak2+1):ind.end.2,]
ibwc.rising3.0.013 = sub.ibwc[(ind.end.2+1):ind.peak3,]
ibwc.falling3.0.013 = sub.ibwc[(ind.peak3+1):length(sub.ibwc$date.time),]
                                 
#rating curve for 0.013 IBWC stage --> llcw Q
q.cms.rating.rising1.0.013 = 19.60762*ibwc.rising1.0.013$ibwc.stage.m -17.76322 #rising rating curve
q.cms.rating.falling1.0.013 = 21.20415*ibwc.falling1.0.013$ibwc.stage.m -20.76996
max(q.cms.rating.rising1.0.013)
max(q.cms.rating.falling1.0.013)
q.cms.rating.rising2.0.013 = 19.60762*ibwc.rising2.0.013$ibwc.stage.m -17.76322
q.cms.rating.falling2.0.013 = 21.20415*ibwc.falling2.0.013$ibwc.stage.m -20.76996
max(q.cms.rating.rising2.0.013)
max(q.cms.rating.falling2.0.013)
q.cms.rating.rising3.0.013 = 19.60762*ibwc.rising3.0.013$ibwc.stage.m -17.76322
q.cms.rating.falling3.0.013 = 21.20415*ibwc.falling3.0.013$ibwc.stage.m -20.76996
max(q.cms.rating.rising3.0.013)
max(q.cms.rating.falling3.0.013)
#combine all into one variable
q.cms.rating.0.013 = c(q.cms.rating.rising1.0.013, q.cms.rating.falling1.0.013, q.cms.rating.rising2.0.013, q.cms.rating.falling2.0.013, q.cms.rating.rising3.0.013, q.cms.rating.falling3.0.013)
q.cms.rating.0.013[q.cms.rating.0.013<0]<- 0 #set all negative to zero

#plot of discharge new rating with PT
plot(sub.ibwc$date.time, q.cms.rating.0.013, type="l", xlab="Date", ylab="Discharge (cms)",xaxt = 'n', main="2014-02-28")
lines(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', col="blue")
lines(ibwc2$date.time,ibwc.q.cms, lty=2, lwd=2)
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", c("PT", "IBWC-Orig Rating","IBWC-Adj Rating"),  col=c("blue", "black","black"),lwd=c(1,2,1), lty=c(1,2,1), cex=0.75,bty = "n" ) #bty="n" means no box
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"), lty=2)

###############################################################################################################

#Figure 2.2
#with IBWC Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
  par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
legend("topleft", "E1.PT", bty="n", cex=1.25, inset=c(0.065,-.085)) 
legend("topleft", "E2.IBWC", bty="n", cex=1.25, inset=c(0.24,-.085))
legend("topleft", "E3.IBWC", bty="n", cex=1.25, inset=c(0.4,-.085))
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"))
  par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)",  ylim=c(10.2,10.45+.01))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(atm.corr.interp$x, adj2.atm.corr.tje.naval, type = "l", col="green") #adjusted tje.naval
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"),lwd=1, inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"))
  par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, stage.m,  type="l", col="blue", xaxt = 'n',   ylab = "Stage (m)", ylim=c(0,1.5))
lines(ibwc2$date.time,ibwc2$ibwc.stage.m, lty=2)
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085))
legend("topleft", c("PT", "IBWC"),  col=c("blue", "black"),lwd=1, lty=c(1,2), inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"))
  par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", col="blue", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', ylim=c(0,6))
lines(sub.ibwc$date.time, q.cms.rating.0.013, type="l", lty=2, xlab="Date", ylab="Discharge (cms)")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085))
legend("topleft", c("PT","IBWC Rating Curve"),  col=c("blue", "black"),lwd=1, lty=c(1,2), inset=c(.1,-.05), cex=0.75,bty = "n" ) #bty="n" means no box
abline(v=as.POSIXct("2014-02-28 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-02-28 15:50", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 00:00", format="%Y-%m-%d %H:%M"))
abline(v=as.POSIXct("2014-03-01 15:57", format="%Y-%m-%d %H:%M"))

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#Events summary total precip, peak Q, and total Q for each E1-E3
  #Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall

#subset precip for each day
#daily total precip for 2/27/14
precip0 = precip[precip$date == "2014-02-27",]
precip_20140227 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2/28/14
precip1 = precip[precip$date == "2014-02-28",]
precip_20140228.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20140228 = precip_20140228.cum - precip_20140227
#daily total precip for 3/1/14
precip2 = precip[precip$date == "2014-03-01",]
precip_20140301.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #cum precip up to 3/1/14
precip_20140301 = precip_20140301.cum - precip_20140228.cum #total precip for 3/1/14
#daily total precip for 3/2/14
precip3 = precip[precip$date == "2014-03-02",]
precip_20140302.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #cum precip up to 3/2/14
precip_20140302 = precip_20140302.cum - precip_20140301.cum  #total precip for 3/2/14

#Event 1: start 2/28/14 00:00 to 2/28/14 15:50 (Will use PT data for E1)
#E1: PT calc total rainfall, PT: total q, peak q for this new subset
totalp.event1 = precip1$Cumulative.rain.mm[precip1$time == "15:50:00"] - precip_20140227
ind.end1 = grep(as.POSIXct("2014-02-28 15:50:00"),obs$date.time)
event1.q.cms = q.cms[1:ind.end1]
event1.date.time = obs$date.time[1:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms,event1.date.time)
PT.rating = "A" #PT data looks good, will use 
#E1 time to peak
#zoom plot of E1 to look for oscillations
par(mfrow=c(1,1),xpd=FALSE)
plot(event1.date.time,event1.q.cms, type="l", main="2/28/14 E1", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(event1.q.cms==peakq.event1)
peak.date.time = event1.date.time[peak.date.time.ind]
#calculate time to peak, assume start of rainfall is at 2/28/2014 10:09
time.2.peak.e1 = peak.date.time - as.POSIXct("2014-02-28 10:09:00")
#E1: IBWC calc total q and peak new rating curve
sub.ibwc$date.time #to find the closest time to start and end
ind.start.ibwc = grep(as.POSIXct("2014-02-28 00:02:00"),sub.ibwc$date.time)
ind.end.ibwc = grep(as.POSIXct("2014-02-28 16:01:00"),sub.ibwc$date.time)
event1.q.cms.ibwc = q.cms.rating.0.013[ind.start.ibwc:ind.end.ibwc] #subset e1 for ibwc q
event1.date.time.ibwc = sub.ibwc$date.time[ind.start.ibwc:ind.end.ibwc]
peakq.event1.ibwc = max(event1.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.1.ibwc = calculate.total.Q.mm(event1.q.cms.ibwc,event1.date.time.ibwc)
IBWC.rating = "C" #IBWC data no good, will use PT 

#Event 2: start 2/28/14 15:50 to 2014-03-01 00:00 (or to the last value before it hits 3/1)
#E2: PT calc total rainfall, total q, peak q for this new subset
totalp.event2 = precip1$Cumulative.rain.mm[length(precip1$Cumulative.rain.mm)] - precip1$Cumulative.rain.mm[precip1$time == "15:50:00"]
ind.end2 = grep(as.POSIXct("2014-03-01 00:00:00"),obs$date.time) #since 00:00:00 time gets dropped it give you all indexes for that day, use first one to get 00:00
event2.q.cms = q.cms[(ind.end1+1):ind.end2[1]]
event2.date.time = obs$date.time[(ind.end1+1):ind.end2[1]]
peakq.event2 = max(event2.q.cms, na.rm = TRUE) 
total.q.mm.2 = calculate.total.Q.mm(event2.q.cms, event2.date.time)
PT.rating[2] = "C" #PT data no good, use IBWC
#calculate time to peak
#zoom plot for E2
plot(event2.date.time,event2.q.cms, type="l", main="3/01/14 E2", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind2 = which(event2.q.cms==peakq.event2)
peak.date.time2 = event2.date.time[peak.date.time.ind2]
time.2.peak.e2 = peak.date.time2 - as.POSIXct("2014-02-28 15:50:00")
#E2: IBWC calc total q and peak
  #ibwc2$date.time[4500:5010] #to find the closest time to start and end
sub.ibwc$date.time 
ind.start.ibwc2 = grep(as.POSIXct("2014-02-28 16:01:00"),sub.ibwc$date.time )
ind.end.ibwc2 = grep(as.POSIXct("2014-03-01 00:01:00"),sub.ibwc$date.time )
event2.q.cms.ibwc = q.cms.rating.0.013[ind.start.ibwc2:ind.end.ibwc2]
event2.date.time.ibwc = sub.ibwc$date.time [ind.start.ibwc2:ind.end.ibwc2]
peakq.event2.ibwc = max(event2.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.2.ibwc = calculate.total.Q.mm(event2.q.cms.ibwc,event2.date.time.ibwc)
IBWC.rating[2] = "A" #Use IBWC data for E2

#Event 3: total precip depth for new E3 3/1 to "2014-03-01 15:57"
#E3: PT calc total rainfall, total q, peak q for this new subset
totalp.event3 = precip2$Cumulative.rain.mm[precip2$time == "15:57:00"] -precip1$Cumulative.rain.mm[length(precip1$Cumulative.rain.mm)]
#totalp.event3 = precip1$Cumulative.rain.mm[length(precip1$Cumulative.rain.mm)] - precip1$Cumulative.rain.mm[precip1$time == "15:50:00"]
ind.end3 = grep(as.POSIXct("2014-03-01 15:55:00"),obs$date.time) #since 00:00:00 time gets dropped it give you all indexes for that day, use first one to get 00:00
event3.q.cms = q.cms[(ind.end2[1]+1):ind.end3[1]]
event3.date.time = obs$date.time[(ind.end2[1]+1):ind.end3[1]]
peakq.event3 = max(event3.q.cms, na.rm = TRUE) 
total.q.mm.3 = calculate.total.Q.mm(event3.q.cms, event3.date.time)
PT.rating[3] = "C"
#calculate time to peak
#zoom plot for E3
plot(event3.date.time,event3.q.cms, type="l", main="3/01/14 E3", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind3 = which(event3.q.cms==peakq.event3)
peak.date.time3 = event3.date.time[peak.date.time.ind3]
time.2.peak.e3 = peak.date.time3 - as.POSIXct("2014-03-01 00:01:00")
#E3: IBWC calc total q and peak
#ibwc2$date.time[4500:5010] #to find the closest time to start and end
sub.ibwc$date.time 
ind.start.ibwc3 = grep(as.POSIXct("2014-03-01 00:01:00"),sub.ibwc$date.time )
ind.end.ibwc3 = grep(as.POSIXct("2014-03-01 16:03:00"),sub.ibwc$date.time )
event3.q.cms.ibwc = q.cms.rating.0.013[ind.start.ibwc3:ind.end.ibwc3]
event3.date.time.ibwc = sub.ibwc$date.time[ind.start.ibwc3:ind.end.ibwc3]
peakq.event3.ibwc = max(event3.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.3.ibwc = calculate.total.Q.mm(event3.q.cms.ibwc,event3.date.time.ibwc)
IBWC.rating[3] = "A" #use IBWC data for E3

totalp.lastday =  precip3$Cumulative.rain.mm[length(precip3$Cumulative.rain.mm)]-precip2$Cumulative.rain.mm[precip2$time == "15:57:00"]

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from PT only (this isn't the usable q timeseries)
date.time2 = data.frame(obs$date.time)
event1 = rep("E1",times=length(event1.q.cms))
event2 = rep("E2",times=length(event2.q.cms)) 
event3 = rep("E3",times=length(event3.q.cms))
noevent = rep("NA",times=(length(q.cms)-length(event1)-length(event2)-length(event3)))
Event = c(event1, event2, event3, noevent)
data = cbind(date.time2, q.cms, stage.m, Event) #note that this is just the PT data, this doesn't include IBWC rating curve data (E2, E3 use IBWC rating curve data)

#OUTPUT discharge timeseries for A rating only!
#E1
source.E1 = rep("PT", times=length(event1.q.cms))
E1.data = data.frame(cbind(as.character(event1.date.time), event1.q.cms, source.E1, event1))
names(E1.data) <- c("date.time", "q.cms", "source", "event")
#E2
source.E2 = rep("IBWC", times=length(event2.q.cms.ibwc))
event2.ibwc = rep("E2",times=length(event2.q.cms.ibwc)) 
E2.data = data.frame(cbind(as.character(event2.date.time.ibwc), event2.q.cms.ibwc, source.E2, event2.ibwc))
names(E2.data) <- c("date.time", "q.cms", "source", "event")
#E3
source.E3 = rep("IBWC", times=length(event3.q.cms.ibwc))
event3.ibwc = rep("E3",times=length(event3.q.cms.ibwc)) 
E3.data = data.frame(cbind(as.character(event3.date.time.ibwc), event3.q.cms.ibwc, source.E3, event3.ibwc))
names(E3.data) <- c("date.time", "q.cms", "source", "event")
#combining the good datasets together into one dataframe
q.data.all = rbind(E1.data, E2.data, E3.data)

#output summary for observed data (with "A" rating, use as observed)
date = c("2014-02-28","2014-03-01", "2014-03-02")
event = c("E1", "E2", "E3")
source = c("PT", "IBWC", "IBWC") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1,peakq.event2.ibwc, peakq.event3.ibwc)
total.q.obs.mm = c(total.q.mm.1,total.q.mm.2.ibwc, total.q.mm.3.ibwc)
total.precip.mm = c(totalp.event1, totalp.event2, totalp.event3)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary, file="summary_20140301_observed_q.csv", row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(peakq.event1,peakq.event2, peakq.event3)
IBWC.peak.q.obs.cms = c(peakq.event1.ibwc,peakq.event2.ibwc, peakq.event3.ibwc)
PT.total.q.obs.mm = c(total.q.mm.1,total.q.mm.2, total.q.mm.3)
IBWC.total.q.obs.mm = c(total.q.mm.1.ibwc,total.q.mm.2.ibwc, total.q.mm.3.ibwc)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary.PT.IBWC, file="summary_20140301_observed_q_PT_IBWC.csv", row.names=F)
