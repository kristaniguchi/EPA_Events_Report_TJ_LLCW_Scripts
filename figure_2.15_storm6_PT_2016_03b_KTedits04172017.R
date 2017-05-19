#Storm Event 7: 3/2016 
#Figure x in Events Report for EPA
#rained from 2016-03-05 to 2016-03-11
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#2 storm events: USE PT DATA for E1, IBWC E2
#SSC samples collected

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#1. No barometric data from TJE during this time period, there is SAN, but will use TJE Naval

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_March05_2016.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time0.sub = date.time0[2:10]
date.time.pt = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
time.pt = format(date.time.pt,"%H:%M:%S")
date.pt = format(date.time.pt,"%Y-%m-%d" )
obs = cbind(obs0, date.time.pt, date.pt, time.pt)

#step 3 using the TJE naval barometric data
#TJE Naval Base Pressure data (time stamp is from GMT to PSD)
tje.naval0 = read.csv(file = "TJE_naval_March2016.csv", skip=3,header=TRUE) #column x.21 is SlP
year = substr(tje.naval0$X, 1,4) #to format the date extract out the year, month, day and time
month = substr(tje.naval0$X, 5,6)
day = substr(tje.naval0$X, 7,8)
hour = substr(tje.naval0$X, 9,10)
#hour = as.character(as.numeric(hour0)-8)
minute = substr(tje.naval0$X, 11,12)
date.time0 = paste(month, "/", day, "/",year, " ", hour, ":", minute, sep = "")
date.time.gmt = strptime(date.time0,"%m/%d/%Y %H:%M",tz="GMT") #orginial data in GMT UTC time zone!
timezone.gmt.ct = as.POSIXct(date.time.gmt)
date.time = strptime(as.character(format(timezone.gmt.ct,"%m/%d/%Y %H:%M", tz="America/Los_Angeles")), "%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
tje.naval = cbind(tje.naval0, date.time, date, time)
pressure.tje.naval.m = as.numeric(as.character(tje.naval$X.21))/(0.09806649999980076*1000)  #from mb to m
#3. interpolate the barometric data to get the same time stamp as PT_level data
approx.tje.naval = data.frame(approx(tje.naval$date.time, pressure.tje.naval.m, obs$date.time)) #pressure in mb
pressure.tje.naval.m.approx = approx.tje.naval$y 

#ORIGINAL Adjustment of Barometric data for 9/2015 Event --> orig adjustment worked well, don't need to fix
#Adjustment of Barometric data for 3/5/2016 
#TJE Naval data ajusted based on 2016-03-05  mean (baseflow day)
ind = grep(as.POSIXct("2016-03-06  08:00:00 PST"),approx.tje.naval$x)
friday.tje.naval = approx.tje.naval[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for 2016-03-05 
ind = grep(as.POSIXct("2016-03-06  08:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval$y - diff.mean.tje.naval

#plot the pressure from the pt and other barometric sources just to see
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n", xlim = c(as.POSIXct(obs$date.time[1], "%m/%d/%Y %H:%M:%S"),as.POSIXct("2016-03-09 00:00:00", "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")), ylim = c(10.2,10.45)) 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green") 
#lines(approx.tje$x, pressure.tje.m.approx,type="l", col="red") #TJE data not good, too coarse
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
abline(v=as.POSIXct("2016-03-06 08:00", format="%Y-%m-%d %H:%M"))

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_March05_2016_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
precip = na.omit(precip00)

###############################################################################################################

#CALCULATE THE ADJUSTED STAGE
#calculate the stage, us for loop to replace all of the negative values with 0
stage.m = obs$PT - tje.naval.adj 
stage.m[stage.m<0]<-0 #replace all neg values with 0

#plot the stage
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n")
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

#calculate Q
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_Q_mannings.R') #functions are saved in script directory
q.cms = calculateQ.mannings(stage.m, 0.013) #calc q based on PT stage using 0.013 n

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate peak q and total q in mm for whole storm 
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_total_Q_mm.R ') #functions are saved in script directory
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
ind.peak1 = grep(as.POSIXct("2016-03-06 11:06", format="%Y-%m-%d %H:%M"), ibwc3$date.time) #index of the max, which is also the #of values that are on the rising limb
ind.fall1 = grep(as.POSIXct("2016-03-07 06:05", format="%Y-%m-%d %H:%M"), ibwc3$date.time)
rising1 = rep("rising", ind.peak1) 
falling1 = rep("falling", ind.fall1 - ind.peak1)
ind.peak2 = grep(as.POSIXct("2016-03-08 15:05", format="%Y-%m-%d %H:%M"), ibwc3$date.time) #index of the max, which is also the #of values that are on the rising limb
rising2 = rep("rising", ind.peak2-ind.fall1) 
falling2 = rep("falling", length(ibwc3$date.time) - ind.peak2)
rising.falling = c(rising1, falling1, rising2, falling2)
ibwc.4 = cbind(ibwc3, rising.falling)

#Calculate IBWC Q with updated rising and falling rating curves
#rising: Q = 25.26374*stage - 21.61612
rising.sub1 = ibwc.4[1:ind.peak1,]
ibwc.Q.cms.adj.rating.rising1 = 19.60762*rising.sub1$ibwc.stage.m -17.76322
ibwc.Q.cms.adj.rating.rising1[ibwc.Q.cms.adj.rating.rising1<0]<-0
#falling rating: Q = 30.29165*stage - 29.67138
falling.sub1 = ibwc.4[(ind.peak1+1):ind.fall1,]
ibwc.Q.cms.adj.rating.falling1 = 21.20415*falling.sub1$ibwc.stage.m -20.76996
ibwc.Q.cms.adj.rating.falling1[ibwc.Q.cms.adj.rating.falling1<0]<-0
#rising: Q = 25.26374*stage - 21.61612
rising.sub2 = ibwc.4[(ind.fall1+1):ind.peak2,]
ibwc.Q.cms.adj.rating.rising2 = 19.60762*rising.sub2$ibwc.stage.m -17.76322
ibwc.Q.cms.adj.rating.rising2[ibwc.Q.cms.adj.rating.rising2<0]<-0
#falling rating: Q = 30.29165*stage - 29.67138
falling.sub2 = ibwc.4[(ind.peak2+1):length(ibwc.4$date.time),]
ibwc.Q.cms.adj.rating.falling2 = 21.20415*falling.sub2$ibwc.stage.m -20.76996
ibwc.Q.cms.adj.rating.falling2[ibwc.Q.cms.adj.rating.falling2<0]<-0

#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted = c(ibwc.Q.cms.adj.rating.rising1,ibwc.Q.cms.adj.rating.falling1, ibwc.Q.cms.adj.rating.rising2,ibwc.Q.cms.adj.rating.falling2)

#Plot the updated rising/falling rating curve
plot(ibwc.4$date.time, ibwc.Q.cms.adjusted, type="l")

###############################################################################################################
#Figure 2.15
#IBWC Overall Panel Plots of everything 
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
  par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST"))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
legend("topleft", "E1.PT", bty="n", cex=1.5, inset=c(0.15,-.05))
legend("topleft", "E2.IBWC", bty="n", cex=1.5, inset=c(0.5,-.05))
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
abline(v=as.POSIXct("2016-03-08 10:00", format="%Y-%m-%d %H:%M")) #end of E2
  par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", ylim=c(10.22,10.55), xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"),inset=c(.08,0), cex=0.75,bty = "n") #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
  abline(v=as.POSIXct("2016-03-08 10:00", format="%Y-%m-%d %H:%M")) #end of E2
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l", ylim=c(0,1.5), xaxt = 'n',   ylab = "Stage (m)",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
lines(ibwc2$date.time,ibwc2$ibwc.stage.m, lty=2)
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
abline(v=as.POSIXct("2016-03-08 10:00", format="%Y-%m-%d %H:%M")) #end of E2
points(as.POSIXct("2016-03-06 09:40", format="%Y-%m-%d %H:%M"), 0.6, pch=8) #visual stage outlet
legend("topleft", c("IBWC","Visual","PT"), col="black",lwd=1, pch=c(NA,8,NA), lty=c(2,NA,1),inset=c(.08,0), cex=0.75,bty = "n" ) #bty="n" means no box
  par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", ylim=c(0,5), xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n',xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
lines(ibwc.4$date.time, ibwc.Q.cms.adjusted, lty=2, xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", c("Visual Q calc","PT", "IBWC Updated"), pch=c(8,NA,NA), lty=c(NA,1,2),inset=c(.08,0), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
abline(v=as.POSIXct("2016-03-08 10:00", format="%Y-%m-%d %H:%M")) #end of E2
#add in q pt from the visual stage at outlet
visual.q.calc = calculateQ.mannings(0.6, 0.013)
points(as.POSIXct("2016-03-06 09:40", format="%Y-%m-%d %H:%M"), visual.q.calc, pch=8) #visual stage outlet

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)

###############################################################################################################

#Visual Comparison: approx the ibwc stage during the visual stage measurement time and PT stage&q at that time
ibwc.stage.visual.approx = approx(ibwc2$date.time,ibwc2$ibwc.stage.m, as.POSIXct("2016-03-06 09:40:00", format="%Y-%m-%d %H:%M")) 
#ibwc.q.visual.approx = approx(ibwc2$date.time,ibwc.q.cms, as.POSIXct("2016-03-06 09:40:00", format="%Y-%m-%d %H:%M")) 
ibwc.q.visual.approx.adj = approx(ibwc.4$date.time,ibwc.Q.cms.adjusted, as.POSIXct("2016-03-06 09:40:00", format="%Y-%m-%d %H:%M")) 
PT.stage.visual.approx = approx(approx.tje.naval$x, stage.m, as.POSIXct("2016-03-06 09:40:00", format="%Y-%m-%d %H:%M")) 
PT.q.visual.approx = approx(obs$date.time, q.cms, as.POSIXct("2016-03-06 09:40:00", format="%Y-%m-%d %H:%M")) 

calculateQ.mannings(c(0.25,0.6,0.7,0.4,0.3,0.15,0.5,0.1,0.1), 0.013)

#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
#daily total precip for 2016-03-05
precip0 = precip[precip$date == "2016-03-05",]
precip_20160305 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2016-03-06
precip1 = precip[precip$date == "2016-03-06",]
precip_20160306.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20160306 = precip_20160306.cum - precip_20160305
#daily total precip for 2016-03-07
precip2 = precip[precip$date == "2016-03-07",]
precip_20160307.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2/28/14 is the last value for that day
precip_20160307 = precip_20160307.cum - precip_20160306.cum
#daily total precip for 2016-03-09 #no rain on the 8th
precip3 = precip[precip$date == "2016-03-09",]
precip_20160309.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #total precip for 2/28/14 is the last value for that day
precip_20160309 = precip_20160309.cum - precip_20160307.cum
#daily total precip for 2016-03-11 #no rain on the 10th
precip4 = precip[precip$date == "2016-03-11",]
precip_20160311.cum = precip4$Cumulative.rain.mm[length(precip4$date)] #total precip for 2/28/14 is the last value for that day
precip_20160311 = precip_20160311.cum - precip_20160309.cum 

#Event 1: start 2016-03-05 00:00 to 2016-03-06 23:55 USE PT DATA for E1, IBWC E2
#calc total rainfall, total q, peak q for this new subset
totalp.event1 = precip_20160306 + precip_20160305  #first day plus the second day (total mm)
ind.end1 = grep(as.POSIXct("2016-03-06 23:55:00"),obs$date.time)
event1.q.cms = q.cms[1:ind.end1] #new subset for the first E1 event
event1.date.time = obs$date.time[1:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms, event1.date.time)
runoff.coeff.event1 = total.q.mm.1/totalp.event1


#IBWC Event 1: start 2016-03-05 00:00 to 2016-03-06 23:55
#calc total rainfall, total q, peak q for this new subset
totalp.event1.ibwc = precip_20160306 + precip_20160305  #first day plus the second day (total mm)
ibwc.4$date.time
  #ibwc2$date.time[9500:9700]
in.start1.ibwc = grep(as.POSIXct("2016-03-05 00:07:00"),ibwc.4$date.time)
ind.end1.ibwc = grep(as.POSIXct("2016-03-06 23:07:00"),ibwc.4$date.time)
event1.q.cms.ibwc = ibwc.Q.cms.adjusted[in.start1.ibwc:ind.end1.ibwc] #new subset for the first E1 event
event1.date.time.ibwc = ibwc.4$date.time[in.start1.ibwc:ind.end1.ibwc]
peakq.event1.ibwc = max(event1.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.1.ibwc = calculate.total.Q.mm(event1.q.cms.ibwc, event1.date.time.ibwc)
runoff.coeff.event1.ibwc = total.q.mm.1.ibwc/totalp.event1.ibwc

plot(obs$date.time, q.cms, type ="l", ylim=c(0,10), xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n',xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
lines(ibwc2$date.time,ibwc.q.cms, lty=2)
lines(ibwc.4$date.time, ibwc.Q.cms.adjusted, lty=2, col="red", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", c("IBWC Orig","Visual Q calc","PT", "IBWC Updated"), col=c("black", "black", "black", "red"), pch=c(NA,8,NA,NA), lty=c(2,NA,1,2),inset=c(.08,0), cex=0.75,bty = "n" ) #bty="n" means no box
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
#add in q pt from the visual stage at outlet
visual.q.calc = calculateQ.mannings(0.6, 0.013)

plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST"))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))  
legend("topleft", "E1", bty="n", cex=1.5, inset=c(0.15,-.05))
legend("topleft", "E2?", bty="n", cex=1.5, inset=c(0.5,-.05))
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line
abline(v=as.POSIXct("2016-03-08 10:00", format="%Y-%m-%d %H:%M")) #end of E2

#Event 2: start 2016-03-07 00:00 to 2016-03-08  10:05 #Use E2 IBWC
totalp.event2 = precip_20160307 #no precip on the 8th
ind.end2 = grep(as.POSIXct("2016-03-08 10:05:00"),obs$date.time) #since 00:00:00 time gets dropped it give you all indexes for that day, use first one to get 00:00
event2.q.cms = q.cms[(ind.end1+1):ind.end2[1]]
event2.date.time = obs$date.time[(ind.end1+1):ind.end2[1]]
peakq.event2 = max(event2.q.cms, na.rm = TRUE) 
total.q.mm.2 = calculate.total.Q.mm(event2.q.cms, event2.date.time)

#Event2: IBWC calculations only using first hump
ibwc.4$date.time
abline(v=as.POSIXct("2016-03-08 10:07", format="%Y-%m-%d %H:%M")) #end of E2
#ibwc2$date.time[9500:9700]
ind.start2.ibwc = grep(as.POSIXct("2016-03-07 00:05:00"),ibwc.4$date.time)
ind.end2.ibwc = grep(as.POSIXct("2016-03-08 10:07:00"),ibwc.4$date.time)
event2.q.cms.ibwc = ibwc.Q.cms.adjusted[ind.start2.ibwc:ind.end2.ibwc] #new subset for the E2 event
event2.date.time.ibwc = ibwc.4$date.time[ind.start2.ibwc:ind.end2.ibwc]
peakq.event2.ibwc = max(event2.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.2.ibwc = calculate.total.Q.mm(event2.q.cms.ibwc, event2.date.time.ibwc)
runoff.coeff.event2.ibwc = total.q.mm.2.ibwc/totalp.event2

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from PT only (this isn't the usable q timeseries)
date.time2 = data.frame(obs$date.time)
event1 = rep("E1",times=length(event1.q.cms))
event2 = rep("E2",times=length(event2.q.cms)) 
noevent = rep("NA",times=(length(q.cms)-length(event1)-length(event2)))
Event = c(event1, event2, noevent)
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
#combining the good datasets together into one dataframe
q.data.all = rbind(E1.data, E2.data)

#output summary for observed data (with "A" rating, use as observed)
date = c("2016-03-06","2016-03-07")
event = c("E1", "E2")
source = c("PT", "IBWC") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1,peakq.event2.ibwc)
total.q.obs.mm = c(total.q.mm.1,total.q.mm.2.ibwc)
total.precip.mm = c(totalp.event1, totalp.event2)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary, file="summary_20160306_observed_q.csv", row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(peakq.event1,peakq.event2)
IBWC.peak.q.obs.cms = c(peakq.event1.ibwc,peakq.event2.ibwc)
PT.total.q.obs.mm = c(total.q.mm.1,total.q.mm.2)
IBWC.total.q.obs.mm = c(total.q.mm.1.ibwc,total.q.mm.2.ibwc)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary.PT.IBWC, file="summary_20160306_observed_q_PT_IBWC.csv", row.names=F)

