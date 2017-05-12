#Storm Event 3: 5/15/2015 
#Figure 4 in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated, no IBWC data for this event
#1 storm event (E1)
#SSC sample collected

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
dir.concepts.main = "C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016" #for CONCEPTS output summary from Main

###############################################################################################################

#Read in barometric and PT data, do atm correction
#to read in PT data, read in barometric data, subtract to get water depth (sections 1-4)

#1. No barometric data from TJE during this time period/will not use SAN, see old script for SAN

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_May15_2015.csv", header=TRUE)
date.time0 = paste(as.character(obs0$date), as.character(obs0$time), sep=" ")
date.time = strptime(date.time0,"%m/%d/%Y %H:%M:%S")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs = cbind(obs0, date.time, date, time)

#step 3 using the TJE naval barometric data (WILL USE TJE NAVAL)
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

#ORIGINAL Adjustment of Barometric data for 5/2015 Event
#TJE Naval data ajusted based on Friday 2015-05-15 til noon mean (baseflow day)
date = as.Date(approx.tje.naval$x)
approx.tje.naval2 = cbind(approx.tje.naval, date)
ind = grep(as.POSIXct("2015-05-15 12:00:00 PST"),approx.tje.naval2$x)
friday.tje.naval = approx.tje.naval2[1:ind,]
av.tje.naval = mean(friday.tje.naval$y) 
#observed mean for Friday til noon
ind = grep(as.POSIXct("2015-05-15 12:00:00 PST"),obs$date.time)
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

###############################################################################################################

#Updated Atmospheric Adjustment using TJE Naval (KT 4/20/2017), interpolation at the breaks
#Adjust the TJE naval atm pressure to match better! 4/20/2017
lines(approx.tje.naval$x, tje.naval.adj-0.01, type = "l", col="green")
lines(approx.tje.naval$x, tje.naval.adj-0.095, type = "l", col="green")
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
abline(v=as.POSIXct("2015-05-15 00:00", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-05-15 15:50", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct("2015-05-15 19:20", format="%Y-%m-%d %H:%M"), lty=2)
abline(v=as.POSIXct(approx.tje.naval$x[length(approx.tje.naval$x)], format="%Y-%m-%d %H:%M"), lty=2) #add break with tje.naval.adj+0.01
lines(approx.tje.naval$x, tje.naval.adj-0.01, type = "l", col="green")
lines(approx.tje.naval$x, tje.naval.adj-0.095, type = "l", col="green")
#breaks
break.dates = c(as.POSIXct("2015-05-15 00:00:00 PST"), as.POSIXct("2015-05-15 15:50:00 PST"), as.POSIXct("2015-05-15 19:20:00 PST"), as.POSIXct(approx.tje.naval$x[length(approx.tje.naval$x)]))
corr = c(diff.mean.tje.naval+0.01, diff.mean.tje.naval+0.01, diff.mean.tje.naval+0.095, diff.mean.tje.naval+0.095)
atm.corr.interp = approx(break.dates, corr, obs$date.time)
adj2.atm.corr.tje.naval  = pressure.tje.naval.m.approx - atm.corr.interp$y
#Updated plot
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n")
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green") #matches the beginning best

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_May15_2015_precip.csv", header=TRUE)
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
plot(approx.tje.naval$x, stage.m, type="l",  xlab = "Date", ylab = "Stage (m)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-05-16 00:00:00 PDT")))
axis.POSIXct(side = 1, approx.tje.naval$x, format = "%Y-%m-%d")

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
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2015-05-16 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

###############################################################################################################

#Panel Plots --> Use 2nd one!
#1. ORIGINAL FIGURE 4
#Overall Panel Plots of everything
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1], "%Y-%m-%d %H:%M:%S"),as.POSIXct(obs$date.time[length(obs$date.time)]))) 
legend("topleft", "A)", bty="n", cex=1.5, inset=c(-.09,-.15))
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
legend("topleft", c("SAN","TJE Naval","PT"),  col=c("brown","green","blue"),lwd=1,lty=c(1,1,1), inset=c(.1,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.5, inset=c(-.09,-.15))
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)")
legend("topleft", "C)", bty="n", cex=1.5, inset=c(-.09,-.2))
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n')
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.5, inset=c(-.09,-.4))


#UPDATED FIGRUE 4 --> USE THIS PLOT FOR REPORT
#UPDATED Overall Panel Plots of everything 4/20/2017
layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.05,0.05,0.04,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(obs$date.time[1], "%Y-%m-%d %H:%M:%S"),as.POSIXct("2015-05-16 00:00:00 PST"))) 
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085))
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", xaxt = 'n', ylab = "Pressure (m)", xlim = c(as.POSIXct(obs$date.time[1], "%Y-%m-%d %H:%M:%S"),as.POSIXct("2015-05-16 00:00:00 PST")))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, adj2.atm.corr.tje.naval, type = "l", col="green")
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"),lwd=1, inset=c(.1,-.04), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.085))
par(mar = c(0, 4.1, 0, 2.1))
plot(approx.tje.naval$x, stage.m,  type="l",  xaxt = 'n',   ylab = "Stage (m)", xlim = c(as.POSIXct(obs$date.time[1], "%Y-%m-%d %H:%M:%S"),as.POSIXct("2015-05-16 00:00:00 PST")))
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.085))
par(mar = c(4, 4.1, 0, 2.1))
plot(obs$date.time, q.cms, type ="l", xlab = "Date",ylab = "Discharge (cms)", xaxt = 'n', xlim = c(as.POSIXct(obs$date.time[1], "%Y-%m-%d %H:%M:%S"),as.POSIXct("2015-05-16 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
legend("topleft", "D)", bty="n", cex=1.25, inset=c(-.03,-.085))


#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#Events summary total precip, peak Q, and total Q for each E1-E3

#Only one event for this storm!
#daily total precip for 2015-05-14
precip0 = precip[precip$date == "2015-05-14",]
precip_20150914 = precip0$Cumulative.rain.mm[length(precip0$date)] 
#daily total precip for 2015-05-15
precip1 = precip[precip$date == "2015-05-15",]
precip_20150915 = precip1$Cumulative.rain.mm[length(precip1$date)] - precip_20150914

#calculate peak q and total q in mm for whole storm
totalp.event1 = precip_20150915
peakq.cms = max(q.cms, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 
mcm = (total.q.mm/1000*10231811.9)/1000000 #formula to calculate MCM
#zoom plot of event to look for oscillations
plot(obs$date.time, q.cms, type="l", main="05/15/15", xlab="Time", ylab="Discharge (cms)")
peak.date.time.ind = which(q.cms==peakq.cms)
peak.date.time = obs$date.time[peak.date.time.ind]
#calculate time to peak
time.2.peak.e1 = peak.date.time - as.POSIXct("2015-05-15 10:30:00") #start rainfall at 5/15/2015 10:30

#discharge plot for event1 done by plotting event1.q.cms and event1.date.time
#plot(precip1$date.time, precip1$Cumulative.rain.mm, type="l")

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
#write.csv(data, file="F:/TJ/Validation_data/Google_drive/processed_data/20150515_discharge_timeseries.csv",row.names=F)

#output summary for observed data
date = c("2015-05-15")
event = c("E1")
source = c("PT") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.cms)
total.q.obs.mm = c(total.q.mm)
total.precip.mm = c(totalp.event1)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv = paste(dir, "/", "summary_20150515_observed_q.csv", sep="")
write.csv(obs.summary, file=fpathcsv, row.names=F)

#output summary for both PT and IBWC data (No IBWC, will just put NA)! Use date, event, source, 
PT.peak.q.obs.cms = peak.q.obs.cms
IBWC.peak.q.obs.cms = c(NA)
PT.total.q.obs.mm = total.q.obs.mm
IBWC.total.q.obs.mm = c(NA)
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv2 = paste(dir, "/", "summary_20150515_observed_q_PT_IBWC.csv", sep="")
write.csv(obs.summary.PT.IBWC, file=fpathcsv2, row.names=F)


#4/27/2018: NEED TO UPDATE PART BELOW, only had 2 events previously, now have 3.  Also, using IBWC data for E2, E3, so need to use diff data for comparison (data above)

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
                #"RADIUS.m",
                ##"CONVEYANCE","F.SLOPE","HEAD","FROUDE","BEDSHEAR","SILTDIS","SANDDIS","GRAVELDIS","TOTALDIS","SILTYLD","SANDYLD","GRAVELYLD","TOTALYLD","CUMBED","THALWEG","CUMLAT","SAFETYL","SAFETYR_APCOHL_APCOHR_POREL_PORER","MATRICL","MATRICR","WBLKL","WBLKR","WWATERL","WWATERR","HYDPRL","HYDPRR","PHREAL","PHREAR","BANKTOPL","BANKTOPR")
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

#CONCEPTS hydrograph in comparison with Observed: 05/15/15
#sub = main2[main2$month.day.year=="05/15/2015",]
#peakq.concepts1 = max(sub$q.adjusted.cms)
#peak.depth = max(sub$DEPTH.m)
#peak.concepts.ind = which(sub$q.adjusted.cms==peakq.concepts1)
#peak.concepts.date.time1 = sub$date.time[peak.concepts.ind]
#time.2.peak.concepts.e1 = peak.concepts.date.time1 - sub$date.time[1] #time to peak concepts

#plot(obs$date.time, q.cms, type="l", main="05/15/15", xlab="Time", ylab="Discharge (cms)")
#lines(sub$date.time, sub$q.adjusted.cms, type="l", xlab = "Time (hrs)", ylab = "Discharge (cms)", main = "2/28/14 E1", col="blue")
#legend("topright", c("Observed","CONCEPTS"),  col=c("black","blue"),lwd=1, cex=0.75,bty = "n" ) #bty="n" means no box




