#Storm Event 9: 02172017
#Figure xx in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#E1 USE IBWC; PT did not function correctly during this event!

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
dir.concepts.main = "C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016" #for CONCEPTS output summary from Main

###############################################################################################################
#Read in barometric and PT data, do atm correction
#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_02172017_KT.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = data.frame(cbind(obs0, date.time, date, time))

#4. read in the barologger at outlet
baro0 = read.csv(file = "baro_02172017_KT.csv", header=TRUE, skip=10)
date.time0 = paste(as.character(baro0$Date), as.character(baro0$Time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
baro = data.frame(cbind(baro0, date.time, date, time))
#3. same date.time stamp, don't need to interpolate

plot(obs$date.time, obs$PT, type="l")
lines(baro$date.time, baro$LEVEL, type="l",col="red") #need to adjust the baro data to match the pt baseflow

#Adjustment of Barometric data for 2/17/2017 Event
#barometric data ajusted based on friday (2 2/17/2017) til 1300 mean (baseflow day, right before peak)
ind = grep(as.POSIXct("2017-02-17 13:00:00 PST"),baro$date.time) #the row that this date falls on for approx.tje2
friday.baro = baro[1:ind,] #the subset of baseflow for baro approx data
av.baro = mean(friday.baro$LEVEL) #/(0.09806649999980076*1000) ) #from mb to m
#observed mean for Friday til 3
ind = grep(as.POSIXct("2017-02-17 13:00:00 PST"),obs$date.time)
friday.obs = obs[1:ind,]
av.obs = mean(friday.obs$PT) 
#adjusted data: subtract the diff in the means to the barometric data
diff.mean.baro = av.baro - av.obs
baro.adj = (baro$LEVEL - diff.mean.baro) #/(0.09806649999980076*1000)
stage.baro.adj.m = (obs$PT- baro.adj)/(0.09806649999980076*1000)
stage.baro.adj.m[stage.baro.adj.m<0] <- 0

plot(obs$date.time, obs$PT, type="l")
lines(baro$date.time, baro$LEVEL, lty=2,col="red") #need to adjust the baro data to match the pt baseflow
lines(baro$date.time, baro.adj, type="l",col="red") #need to adjust the baro data to match the pt baseflow

plot(obs$date.time, obs$PT, type="l")
abline(v=as.POSIXct("2017-02-17 13:00", format="%Y-%m-%d %H:%M"), lty=2) #visualize where baseflow is to use as adjustment, use average from start to line
abline(v=as.POSIXct("2017-02-18 10:00", format="%Y-%m-%d %H:%M"), lty=2) #visualize where baseflow is to use as adjustment, use average from start to line

#plot the pressure from the pt and other barometric sources
plot(obs$date.time, obs$PT/(0.09806649999980076*1000), type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)" ,xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(baro$date.time, baro$LEVEL/(0.09806649999980076*1000), lty=2,col="green") #need to adjust the baro data to match the pt baseflow
lines(baro$date.time, baro.adj/(0.09806649999980076*1000), type="l",col="green") #need to adjust the baro data to match the pt baseflow
legend("bottomright", c("PT","TJE ATM Adjust","TJE ATM","Baro adj","Baro"), col=c("blue","red","red","green","green"), lty=c(1,1,2,1,2))
#also tried adjusting based on Sunday, but did not work well, see PT_script_atmpressure_waterlevel.R to see sunday

###############################################################################################################

#CALCULATE THE ADJUSTED STAGE using baro corrected and Q
stage.m = (obs$PT/(0.09806649999980076*1000)) - (baro.adj/(0.09806649999980076*1000))
stage.m[stage.m<0]<- 0
#plot the stage
plot(obs$date.time, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#calculate Q
path.name.calcQ.mannings = paste(dir, "/", "function_calc_Q_mannings.R", sep="")
source(path.name.calcQ.mannings)
q.cms = calculateQ.mannings(stage.m, 0.013) #calc q based on PT stage using 0.013 n

#calculate peak q and total q in mm for whole storm 
peakq.cms = max(q.cms, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 
mcm = (total.q.mm/1000*10231811.9)/1000000 #formula to calculate MCM

#plot Q timeseries 
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

###############################################################################################################

#IBWC data vs obs_PT
ibwc = read.table(file = "onerain_Goat_Canyon_6680_Stream_Level_6682_52016_22017.txt", skip=0, sep=",", header=TRUE) #sheet from TJE for barometric data
date.time = strptime(ibwc$Reading, "%Y-%m-%d %H:%M:%S") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc.stage.m= ibwc$Value*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$Value-1283.6 #orig rating curve
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847
ibwc2 = data.frame(cbind(ibwc, date.time, date, time, ibwc.stage.m, ibwc.q.cms))

#IBWC rating curve new 0.013
#split stage data into rising and falling limbs
sub.ibwc = ibwc2[1:231,]
peak = max(sub.ibwc$ibwc.stage.m)
ind.peak = grep(peak, sub.ibwc$ibwc.stage.m)
  sub.ibwc [ind.peak,] #just to check got right index for the peak
ibwc.falling.0.013 = sub.ibwc[1:ind.peak,]
ibwc.rising.0.013 = sub.ibwc[(ind.peak+1):length(sub.ibwc$date.time),] #ibwc data is in descending order date
#rating curve for 0.013 IBWC stage --> llcw Q
q.cms.rating.rising.0.013 = 19.60762*ibwc.rising.0.013$ibwc.stage.m -17.76322
q.cms.rating.falling.0.013 = 21.20415*ibwc.falling.0.013$ibwc.stage.m -20.76996
  max(q.cms.rating.rising.0.013)
  max(q.cms.rating.falling.0.013)
  
q.cms.rating.0.013 = c(q.cms.rating.falling.0.013,q.cms.rating.rising.0.013)
q.cms.rating.0.013[q.cms.rating.0.013<0]<- 0

#UPDATE THIS PART!
#visual Q calc from visual stage reading
visual.stage= c(0.5,0.1,0.1)
visual.q.calc = calculateQ.mannings(visual.stage, 0.013)
visual.stage.date.time = c(as.POSIXct("2017-02-17 20:35", format="%Y-%m-%d %H:%M"), as.POSIXct("2017-02-18 11:20", format="%Y-%m-%d %H:%M"), as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"))

#plot discharge ibwc rating curve 0.013 and visual stage-Q
plot(sub.ibwc$date.time, q.cms.rating.0.013, type="l", ylim=c(0,15), ylab="Discharge (cms)", xlab="Date", main="2/18/2017")
points(visual.stage.date.time, visual.q.calc,  col="red")
legend("topright", c("Visual Stage-Q 0.013","IBWC Rating 0.013"), lty=c(NA,1), pch=c(1,NA), col=c("red","black"))

plot(obs$date.time,stage.m, type="l", ylim=c(0,1.5), xlab="Date", ylab="Stage (m)", main="Stage adjusted TJE ATM", col="blue")
abline(v=as.POSIXct("2017-02-17 20:35", format="%Y-%m-%d %H:%M"), lty=2) #visualize where baseflow is to use as adjustment, use average from start to line
points(as.POSIXct("2017-02-17 20:35", format="%Y-%m-%d %H:%M"), 0.5, cex=2)
points(as.POSIXct("2017-02-18 11:20", format="%Y-%m-%d %H:%M"), 0.1, cex=2)
points(as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"), 0.1, cex=2)
lines(obs$date.time, stage.baro.adj.m, col="green")
lines(ibwc2$date.time,ibwc2$ibwc.stage.m, lty=2)
lines(ibwc2$date.time,ibwc2$ibwc.stage.m-0.45)
legend("topright", c("Stage TJE", "Stage Baro", "Visual Stage","IBWC", "IBWC Adj"), lty=c(1,1,NA,2,1), pch=c(NA,NA,1,NA,NA), col=c("blue","green","black","black","black"))


#stage values from adjusted PT
ibwc.stage.visual.approx = approx(ibwc2$date.time,ibwc.stage.m, visual.stage.date.time) 
ibwc.q.visual.approx = approx(ibwc2$date.time,ibwc.q.cms, visual.stage.date.time) 
PT.stage.visual.approx = approx(obs$date.time, stage.m, visual.stage.date.time) 
PT.q.visual.approx = approx(obs$date.time, q.cms, visual.stage.date.time) 


pt.stage.data = data.frame(cbind(as.character(obs$date.time), stage.m,stage.baro.adj.m))
ind1 = grep("2017-02-17 20:35:00",pt.stage.data[,1])
stage1.tje = as.numeric(as.character(pt.stage.data[ind1,2]))
stage1.baro = as.numeric(as.character(pt.stage.data[ind1,3]))
ind2 = grep("2017-02-18 11:20:00",pt.stage.data[,1])
stage2.tje = as.numeric(as.character(pt.stage.data[ind2,2]))
stage2.baro = as.numeric(as.character(pt.stage.data[ind2,3]))
ind3 = grep("2017-02-18 13:00:00",pt.stage.data[,1])
stage3.tje = as.numeric(as.character(pt.stage.data[ind3,2]))
stage3.baro = as.numeric(as.character(pt.stage.data[ind3,3]))


#Q plot
plot(sub.ibwc$date.time, q.cms.rating.0.013, type="l", ylim=c(0,15), ylab="Discharge (cms)", xlab="Date", main="2/18/2017")
points(visual.stage.date.time, visual.q.calc,  col="red")
legend("topright", c("Visual Stage-Q","IBWC Rating"), lty=c(NA,1), pch=c(1,NA), col=c("red","black"))
lines(obs$date.time, q.cms, type="l",col="blue")
legend("topright", c("Visual Stage-Q","IBWC", "PT"), lty=c(NA,1,1), pch=c(1,NA,NA), col=c("red","black","blue"))

plot(sub.ibwc$date.time, q.cms.rating.0.013, type="l", ylim=c(0,40), ylab="Discharge (cms)", xlab="Date", main="2/18/2017")
points(visual.stage.date.time, visual.q.calc,  col="red")
legend("topright", c("Visual Stage-Q 0.013","IBWC Rating 0.013"), lty=c(NA,1), pch=c(1,NA), col=c("red","black"))
lines(obs$date.time, q.cms, type="l",col="blue")
legend("topright", c("Visual Stage-Q 0.013","IBWC Rating 0.013", "PT 0.013"), lty=c(NA,1,1), pch=c(1,NA,NA), col=c("red","black","blue"))
lines(ibwc2$date.time, ibwc2$ibwc.q.cms, type="l", lty=2, xlab="Date", ylab="Discharge (cms)")
legend("topright", c("Visual Stage-Q 0.013","IBWC Rating 0.013", "PT 0.013", "IBWC Rating - Orig"), lty=c(NA,1,1,2), pch=c(1,NA,NA,NA), col=c("red","black","blue","black"))

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_02172017_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
#precip = na.omit(precip00)

plot(precip00$date.time, precip00$Cumulative.rain.mm, type="l")

###############################################################################################################

#FIGRUE 2
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip00$date.time, precip00$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085))
legend("topleft", "E1.IBWC", bty="n", cex=1.25, inset=c(0.15,-.05))
abline(v=as.POSIXct("2017-02-17 08:00", format="%Y-%m-%d %H:%M"), lty=1) #start E1
abline(v=as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"), lty=1) #end E1
par(mar = c(0.1, 4.1, 0.25, 2.1))
plot(obs$date.time, obs$PT/(0.09806649999980076*1000), type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", ylim=c(9.7,10.8), xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
lines(baro$date.time, baro$LEVEL/(0.09806649999980076*1000)) #need to adjust the baro data to match the pt baseflow
lines(baro$date.time, baro.adj/(0.09806649999980076*1000), type="l") #need to adjust the baro data to match the pt baseflow
legend("topleft", c("Barologger","PT"), lty=1, col=c("black","blue"), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2017-02-17 08:00", format="%Y-%m-%d %H:%M"), lty=1) #start E1
abline(v=as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"), lty=1) #end E1
par(mar = c(0, 4.1, 0, 2.1))
plot(sub.ibwc$date.time, sub.ibwc$ibwc.stage.m, type="l", lty=2, xaxt = 'n',   ylab = "Stage (m)", xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
lines(obs$date.time, stage.m)
#lines(obs$date.time, stage.m, col="blue")
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085))
#legend("topleft", c("IBWC","PT"), lty=c(2,1),  col=c("black","black"), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", c("IBWC","PT","Visual"), lty=c(2,1,NA), pch=c(NA,NA,1), col=c("black","black","red"), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
points(visual.stage.date.time, visual.stage,  col="red")
abline(v=as.POSIXct("2017-02-17 08:00", format="%Y-%m-%d %H:%M"), lty=1) #start E1
abline(v=as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"), lty=1) #end E1
par(mar = c(4, 4.1, 0, 2.1))
plot(sub.ibwc$date.time, q.cms.rating.0.013, type ="l", lty=2, ylim=c(0,15), xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
points(visual.stage.date.time, visual.q.calc,  col="red")
lines(obs$date.time, q.cms, type="l",col="black")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
#legend("topleft", c("IBWC","PT"), lty=c(2,1),  col=c("black","black"), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", c("IBWC","PT","Visual"), lty=c(2,1,NA), pch=c(NA,NA,1), col=c("black","black","red"), inset=c(.05,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085))
abline(v=as.POSIXct("2017-02-17 08:00", format="%Y-%m-%d %H:%M"), lty=1) #start E1
abline(v=as.POSIXct("2017-02-18 13:00", format="%Y-%m-%d %H:%M"), lty=1) #end E1

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#Splitting the storm into multiple smaller events since AGNPS only takes a daily total rainfall
#daily total precip for 2017-01-17
precip0 = precip00[precip00$date == "2017-02-17",]
precip_20170217 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2017-01-18
precip1 = precip00[precip00$date == "2017-02-18",]
precip_20170218.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20170218 = precip_20170218.cum - precip_20170217
#daily total precip for 2017-01-19
precip2 = precip00[precip00$date == "2017-02-19",]
precip_20170219.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2/28/14 is the last value for that day
precip_20170219 = precip_20170219.cum - precip_20170218.cum
#daily total precip for 2017-01-20 , last day of rainfall for E2
precip3 = precip00[precip00$date == "2017-02-22",]
precip_20170222.cum = precip3$Cumulative.rain.mm[length(precip3$date)] #total precip for 2/28/14 is the last value for that day
precip_20170222 = precip_20170222.cum - precip_20170219.cum

precip00
#precip start date.time = 2017-02-17 17:50:00, precip end date.tiime for event 2017-02-18 13:00:00 PST

#subset E1 event! (only 1 event but need to cut it off)

#Event 1: start 2017-02-17 08:00:00 to 2017-02-18 13:00:00 PST (will not use PT for E1)
#calc total rainfall, total q, peak q for this new subse
totalp.event1 = precip1$Cumulative.rain.mm[precip1$time == "02:58:00"]
ind.end1 = grep(as.POSIXct("2017-02-18 13:00:00"),obs$date.time)
event1.q.cms = q.cms[1:ind.end1]
event1.date.time = obs$date.time[1:ind.end1]
peakq.event1 = max(event1.q.cms, na.rm = TRUE) 
total.q.mm.1 = calculate.total.Q.mm(event1.q.cms, event1.date.time)
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="02/18/2017", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2016-01-05 13:03:00") #start rainfall at 1/5/2016 13:03

#IBWC Event 1: start 2017-02-17 01:02:15 to 2017-02-18 12:59:50
#calc total rainfall, total q, peak q for this new subset
totalp.event1.ibwc = totalp.event1
sub.ibwc$date.time
ind.start1 = grep(as.POSIXct("2017-02-17 01:02:15"),sub.ibwc$date.time)
ind.end1 = grep(as.POSIXct("2017-02-18 12:59:50"),sub.ibwc$date.time)
event1.q.cms.ibwc = q.cms.rating.0.013[ind.start1:ind.end1] #new subset for the first E1 event
event1.date.time.ibwc = sub.ibwc$date.time[ind.start1:ind.end1]
peakq.event1.ibwc = max(event1.q.cms.ibwc, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
total.q.mm.1.ibwc = calculate.total.Q.mm(event1.q.cms.ibwc, event1.date.time.ibwc)
runoff.coeff.event1.ibwc = total.q.mm.1.ibwc/totalp.event1.ibwc

#leftover rainfall from 2/18
leftover.p.02182017 = precip_20170218.cum - precip1$Cumulative.rain.mm[precip1$time == "02:58:00"]

##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from PT only (this isn't the usable q timeseries)
date.time2 = data.frame(obs$date.time)
event1 = rep("E1",times=length(event1.q.cms))
noevent = rep("NA",times=(length(q.cms)-length(event1)))
Event = c(event1, noevent)
data = cbind(date.time2, q.cms, stage.m, Event) #note that this is just the PT data, this doesn't include IBWC rating curve data (E2, E3 use IBWC rating curve data)

#OUTPUT discharge timeseries for A rating only!
#E1 IBWC will use!
source.E1 = rep("IBWC", times=length(event1.q.cms.ibwc))
event1.ibwc = rep("E1", times=length(event1.q.cms.ibwc))
q.data.all = data.frame(cbind(as.character(event1.date.time.ibwc), event1.q.cms.ibwc, source.E1, event1.ibwc))
names(q.data.all) <- c("date.time", "q.cms", "source", "event")

#output summary for observed data (with "A" rating, use as observed)
date = c("2017-02-17")
event = c("E1")
source = c("IBWC") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.event1.ibwc)
total.q.obs.mm = c(total.q.mm.1.ibwc)
total.precip.mm = c(totalp.event1.ibwc)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv = paste(dir, "/", "summary_20170218_observed_q.csv", sep="")
write.csv(obs.summary, file=fpathcsv, row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(peakq.event1)
IBWC.peak.q.obs.cms = c(peakq.event1.ibwc)
PT.total.q.obs.mm = c(total.q.mm.1)
IBWC.total.q.obs.mm = c(total.q.mm.1.ibwc)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv2 = paste(dir, "/", "summary_20170218_observed_q_PT_IBWC.csv", sep="")
write.csv(obs.summary.PT.IBWC, file=fpathcsv2, row.names=F)



#4/27/2018: NEED TO UPDATE PART BELOW, only had 2 events previously, now have 3.  Also, using IBWC data for E2, E3, so need to use diff data for comparison (data above)

##############################################################################################################################
