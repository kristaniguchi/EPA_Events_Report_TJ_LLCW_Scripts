#Storm Event x: 1/2017
#Figure x in Events Report for EPA
#PT did not work during event, comparison of visuals with IBWC
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#2 storm events: USE PT DATA for E1, IBWC E2

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
dir.concepts.main = "C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016" #for CONCEPTS output summary from Main

###############################################################################################################
#PT didnt' work, use IBWC data

#IBWC data 
ibwc = read.table(file = "onerain_Goat_Canyon_6680_Stream_Level_6682_52016_22017.txt", skip=0, sep=",", header=TRUE) #sheet from TJE for barometric data
date.time = strptime(ibwc$Reading, "%Y-%m-%d %H:%M:%S") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc.stage.m= ibwc$Value*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$Value-1283.6 #old rating curve
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847 
ibwc2 = data.frame(cbind(ibwc, date.time, date, time, ibwc.stage.m, ibwc.q.cms))

plot(ibwc2$date.time,ibwc.q.cms, type="l",lty=2)
plot(ibwc2$date.time, ibwc2$ibwc.stage.m, type="l",lty=2, xlim=c(as.POSIXct("2017-01-17 00:00", format="%Y-%m-%d %H:%M"),as.POSIXct("2017-01-28 00:00", format="%Y-%m-%d %H:%M")), ylim=c(0,1.5))
points(as.POSIXct("2017-01-20 09:30", format="%Y-%m-%d %H:%M"), 0.7, pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 16:00", format="%Y-%m-%d %H:%M"), 0.4, pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 17:30", format="%Y-%m-%d %H:%M"), 0.3, pch=8) #visual stage outlet
axis.POSIXct(side = 1, ibwc2$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5) #, inset=c(-.09,-.4))
abline(v=as.POSIXct("2016-03-07 00:00", format="%Y-%m-%d %H:%M")) #first event line

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_Jan2017_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
#precip = na.omit(precip00)

plot(date.time, precip00$Cumulative.rain.mm, type="l")

###############################################################################################################
#IBWC Updated Rating curve, rising and falling limb
#Rising and Falling limb subset
ind.start = grep(as.POSIXct(as.character(precip00$date[1]), format="%Y-%m-%d"), ibwc2$date)
ind.end = grep(as.POSIXct(as.character(precip00$date[length(precip00$date.time)]), format="%Y-%m-%d"), ibwc2$date)
ibwc3 = ibwc2[ind.start[1]: ind.end[length(ind.end[length(ind.end)])],]
plot(ibwc3$date.time, ibwc3$ibwc.stage.m, type="l")
abline(v=as.POSIXct("2017-01-20 02:30", format="%Y-%m-%d %H:%M"),lty=2) #end of E1
ind.peak1 = grep(as.POSIXct("2017-01-19 08:59:55", format="%Y-%m-%d %H:%M:%S"), ibwc3$date.time) #index of the max, which is also the #of values that are on the rising limb
ind.fall1 = grep(as.POSIXct("2017-01-20 03:02:49", format="%Y-%m-%d %H:%M:%S"), ibwc3$date.time) #end of E1
  ibwc3[ind.fall1,]
rising1 = rep("rising", ind.peak1) 
falling1 = rep("falling", ind.fall1 - ind.peak1)
ind.peak2 = grep(max(ibwc3$ibwc.stage.m), ibwc3$ibwc.stage.m) #index of the max, which is also the #of values that are on the rising limb
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
#IBWC Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.015), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip00$date.time, precip00$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)")
abline(v=as.POSIXct("2017-01-20 02:30", format="%Y-%m-%d %H:%M"),lty=1) 
abline(v=as.POSIXct("2017-01-22 02:02:22", format="%Y-%m-%d %H:%M:%S"),lty=1) 
legend("topleft", "E1.IBWC", bty="n", cex=1.5, inset=c(0.15,-.05))
legend("topleft", "E2.IBWC", bty="n", cex=1.5, inset=c(0.5,-.05))
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
par(mar = c(0, 4.1, 0, 2.1))
plot(ibwc2$date.time,ibwc2$ibwc.stage.m, lty=2,  type="l", ylim=c(0,1.5), xaxt = 'n',   ylab = "Stage (m)",xlim = c(as.POSIXct(precip00$date.time[1]),as.POSIXct(precip00$date.time[length(precip00$date.time)])))
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
abline(v=as.POSIXct("2017-01-20 02:30", format="%Y-%m-%d %H:%M"),lty=1) 
abline(v=as.POSIXct("2017-01-22 02:02:22", format="%Y-%m-%d %H:%M:%S"),lty=1) 
legend("topleft", c("IBWC","Visual","No PT"),  col="black",lwd=1, pch=c(NA,8,NA), lty=c(2,NA,NA),inset=c(.08,0), cex=1,bty = "n" ) #bty="n" means no box
points(as.POSIXct("2017-01-20 09:30", format="%Y-%m-%d %H:%M"), 0.7, pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 16:00", format="%Y-%m-%d %H:%M"), 0.4, pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 17:30", format="%Y-%m-%d %H:%M"), 0.3, pch=8) #visual stage outlet
par(mar = c(0, 4.1, 0, 2.1))
plot(ibwc.4$date.time, ibwc.Q.cms.adjusted, type="l", lty=2, ylim=c(0,8), xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', xlim = c(as.POSIXct(precip00$date.time[1]),as.POSIXct(precip00$date.time[length(precip00$date.time)])))
axis.POSIXct(side = 1, precip00$date.time, format = "%Y-%m-%d")
abline(v=as.POSIXct("2017-01-22 02:02:22", format="%Y-%m-%d %H:%M:%S"),lty=1) 
abline(v=as.POSIXct("2017-01-20 02:30", format="%Y-%m-%d %H:%M"),lty=1) 
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.03,-.085)) #inset first value is L-R, second is up-down
#add in q pt from the visual stage at outlet
visual.stage= c(0.7,0.4,0.3)
path.name.calcQmannings = paste(dir, "/", "function_calc_Q_mannings.R", sep="")
source(path.name.calcQmannings)
visual.q.calc = calculateQ.mannings(visual.stage, 0.013)
points(as.POSIXct("2017-01-20 09:30", format="%Y-%m-%d %H:%M"), visual.q.calc[1], pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 16:00", format="%Y-%m-%d %H:%M"), visual.q.calc[2], pch=8) #visual stage outlet
points(as.POSIXct("2017-01-23 17:30", format="%Y-%m-%d %H:%M"), visual.q.calc[3], pch=8) #visual stage outlet
legend("topleft", c("IBWC","Visual","No PT"),  col="black",lwd=1, pch=c(NA,8,NA), lty=c(2,NA,NA),inset=c(.08,0), cex=1,bty = "n" ) #bty="n" means no box

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)

###############################################################################################################
#Visual Comparison: approx the ibwc stage during the visual stage measurement time
visual.stage.date.time = c(as.POSIXct("2017-01-20 09:30", format="%Y-%m-%d %H:%M"), as.POSIXct("2017-01-23 16:00", format="%Y-%m-%d %H:%M"), as.POSIXct("2017-01-23 17:30", format="%Y-%m-%d %H:%M"))
ibwc.stage.visual.approx = approx(ibwc2$date.time,ibwc2$ibwc.stage.m, visual.stage.date.time) 
ibwc.q.visual.approx = approx(ibwc2$date.time,ibwc.q.cms, visual.stage.date.time) 

#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
#daily total precip for 2017-01-17
precip0 = precip00[precip00$date == "2017-01-17",]
precip_20170117 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2017-01-18
precip1 = precip00[precip00$date == "2017-01-18",]
precip_20170118.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20170118 = precip_20170118.cum - precip_20170117
#daily total precip for 2017-01-19
precip2 = precip00[precip00$date == "2017-01-19",]
precip_20170119.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2/28/14 is the last value for that day
precip_20170119 = precip_20170119.cum - precip_20170118.cum
#daily total precip for 2017-01-20 , last day of rainfall for E2
precip3 = precip00[precip00$date == "2017-01-20",]
precip_20170120.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #total precip for 2/28/14 is the last value for that day
precip_20170120 = precip_20170120.cum - precip_20170119.cum
#daily total precip for 2017-01-22, no rain on 01/21
precip4 = precip00[precip00$date == "2017-01-22",]
precip_20170122.cum = precip4$Cumulative.rain.mm[length(precip4$date)] #total precip for 2/28/14 is the last value for that day
precip_20170122 = precip_20170122.cum - precip_20170120.cum 
#daily total precip for 2017-01-23, last day of rain
precip5 = precip00[precip00$date == "2017-01-23",]
precip_20170123.cum = precip5$Cumulative.rain.mm[length(precip5$date)] #total precip for 2/28/14 is the last value for that day
precip_20170123 = precip_20170123.cum - precip_20170122.cum 

#IBWC Event 1: start to 2017-01-20 02:30 
#calc total rainfall, total q, peak q for this new subset
totalp.event1.ibwc = precip_20170119.cum #no rain on 1-20 prior to end of event at 2:30
precip00$date.time
ibwc.4$date.time
#ibwc2$date.time[9500:9700]
ind.end1 = grep(as.POSIXct("2017-01-20 01:59:58"),ibwc.4$date.time)
event1.q.cms.ibwc = ibwc.Q.cms.adjusted[1:ind.end1] #new subset for the first E1 event
event1.date.time.ibwc = ibwc.4$date.time[1:ind.end1]
peakq.event1.ibwc = max(event1.q.cms.ibwc, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
total.q.mm.1.ibwc = calculate.total.Q.mm(event1.q.cms.ibwc, event1.date.time.ibwc)
runoff.coeff.event1.ibwc = total.q.mm.1.ibwc/totalp.event1.ibwc

#IBWC Event 2: 2017-01-20 02:30 to 1/22/2017
#calc total rainfall, total q, peak q for this new subset
totalp.event2.ibwc = precip_20170120 #1-20, no rain on 1-21
precip00$date.time
ibwc.4$date.time
#ibwc2$date.time[9500:9700]
ind.end2 = grep(as.POSIXct("2017-01-21 23:00:29"),ibwc.4$date.time)
event2.q.cms.ibwc = ibwc.Q.cms.adjusted[(ind.end1+1):ind.end2] #new subset for the first E1 event
event2.date.time.ibwc = ibwc.4$date.time[(ind.end1+1):ind.end2]
peakq.event2.ibwc = max(event2.q.cms.ibwc, na.rm = TRUE) 
total.q.mm.2.ibwc = calculate.total.Q.mm(event2.q.cms.ibwc, event2.date.time.ibwc)
runoff.coeff.event2.ibwc = total.q.mm.2.ibwc/totalp.event2.ibwc

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from IBWC only (only including E1, E2, not including rest of revent)
date.time2 = c(event1.date.time.ibwc, event2.date.time.ibwc)
q.cms = c(event1.q.cms.ibwc, event2.q.cms.ibwc)
event1 = rep("E1",times=length(event1.q.cms.ibwc))
event2 = rep("E2",times=length(event2.q.cms.ibwc)) 
Event = c(event1, event2)
stage.m = rep("NA",times=length(q.cms)) 
data = cbind(date.time2, q.cms, stage.m, Event) #note that this is just the PT data, this doesn't include IBWC rating curve data (E2, E3 use IBWC rating curve data)

#OUTPUT discharge timeseries for A rating only!
#E1
source.E1 = rep("IBWC", times=length(event1.q.cms.ibwc))
E1.data = data.frame(cbind(as.character(event1.date.time.ibwc), event1.q.cms.ibwc, source.E1, event1))
names(E1.data) <- c("date.time", "q.cms", "source", "event")
#E2
source.E2 = rep("IBWC", times=length(event2.q.cms.ibwc))
event2.ibwc = rep("E2",times=length(event2.q.cms.ibwc)) 
E2.data = data.frame(cbind(as.character(event2.date.time.ibwc), event2.q.cms.ibwc, source.E2, event2.ibwc))
names(E2.data) <- c("date.time", "q.cms", "source", "event")
#combining the good datasets together into one dataframe
q.data.all = rbind(E1.data, E2.data)

#output summary for observed data (with "A" rating, use as observed)
date = c("2017-01-19","2017-01-20")
event = c("E1", "E2")
source = c("IBWC", "IBWC") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1.ibwc,peakq.event2.ibwc)
total.q.obs.mm = c(total.q.mm.1.ibwc,total.q.mm.2.ibwc)
total.precip.mm = c(totalp.event1.ibwc, totalp.event2.ibwc)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv = paste(dir, "/", "summary_20170119_observed_q.csv", sep="")
write.csv(obs.summary, file=fpathcsv, row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(NA,NA)
IBWC.peak.q.obs.cms = c(peakq.event1.ibwc,peakq.event2.ibwc)
PT.total.q.obs.mm = c(NA,NA)
IBWC.total.q.obs.mm = c(total.q.mm.1.ibwc,total.q.mm.2.ibwc)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv2 = paste(dir, "/", "summary_20170119_observed_q_PT_IBWC.csv", sep="")
write.csv(obs.summary.PT.IBWC, file=fpathcsv2, row.names=F)



#4/27/2018: NEED TO UPDATE PART BELOW, only had 1 event previously, now have 2.  

##############################################################################################################################

