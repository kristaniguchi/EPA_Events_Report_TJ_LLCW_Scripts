#Storm Event 7: 4/2016 
#PT didn't work for this storm, IBWC Q is zero for this storm!
#Figure 2.17 in Events Report for EPA
#rained from 2016-03-05 to 2015-03-11

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#1. No barometric data from TJE during this time period, use TJE naval

#2. read in the PT_level data
obs0 = read.csv(file = "llc_observed_data_April_2016.csv", header=TRUE)
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

approx.tje.naval = data.frame(approx(tje.naval$date.time, pressure.tje.naval.m, obs$date.time)) #pressure in mb
pressure.tje.naval.m.approx = approx.tje.naval$y 

#Adjustment of Barometric data for 4/9/2016 
#TJE Naval data ajusted based on 2016-04-09  mean (baseflow day)
ind1 = grep(as.POSIXct("2016-04-09  00:00:00 PST"),approx.tje.naval$x)
ind2 = grep(as.POSIXct("2016-04-10  00:00:00 PST"),approx.tje.naval$x)
friday.tje.naval = approx.tje.naval[ind1[1]:ind2[1],]
av.tje.naval = mean(friday.tje.naval$y) 

#observed mean for 2016-03-05 
ind1 = grep(as.POSIXct("2016-04-09  00:00:00 PST"),obs$date.time)
ind2 = grep(as.POSIXct("2016-04-10  00:00:00 PST"),approx.tje.naval$x)
friday.obs = obs[ind1[1]:ind2[1],]
av.obs = mean(friday.obs$PT)
#adjusted data: subtract the diff in the means to the barometric data

#adjusted data: subtract the diff in the means to the barometric data
diff.mean.tje.naval = av.tje.naval - av.obs
tje.naval.adj = approx.tje.naval$y - diff.mean.tje.naval 

#plot the pressure from the pt and other barometric sources just to see
plot(obs$date.time, obs$PT, type="l", col="blue",  xlab = "Date", ylab = "Pressure (m)",xaxt = "n", xlim = c(as.POSIXct(obs$date.time[1], "%m/%d/%Y %H:%M:%S"),as.POSIXct("2016-04-14 00:00:00", "%m/%d/%Y %H:%M:%S")))#, ylim = c(10.2,10.45)) 
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green") 
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_April2016_precip.csv", header=TRUE)
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
peakq.cms = max(q.cms, na.rm = TRUE) 
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_total_Q_mm.R ') #functions are saved in script directory
total.q.mm = calculate.total.Q.mm(q.cms, obs$date.time) 
mcm = (total.q.mm/1000*10231811.9)/1000000 #formula to calculate MCM

###############################################################################################################

#plot Q timeseries 
par(oma=c(2,2,2,2))
plot(obs$date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n",xlim = c(as.POSIXct(obs$date.time[1]),as.POSIXct("2016-03-09 00:00:00 PST")))
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

###############################################################################################################

#ADD in IBWC Data
ibwc = read.csv(file = "IBWC_GC_Bubbler2_2014_72016.csv", skip=2, sep=",", header=TRUE) #sheet from TJE for barometric data
date.time = strptime(ibwc$Date, "%m/%d/%Y %H:%M") #had lower case s instead of S
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc.stage.m= ibwc$Data..ft.*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$Data..ft.-1283.6
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847
ibwc2 = data.frame(cbind(ibwc, date.time, date, time, ibwc.stage.m, ibwc.q.cms))

#Rising and Falling limb subset
ind.start = grep(as.POSIXct(as.character(precip$date[1]), format="%Y-%m-%d"), ibwc2$date)
ind.end = grep(as.POSIXct(as.character(precip$date[length(precip00$date.time)]), format="%Y-%m-%d"), ibwc2$date)
ibwc3 = ibwc2[ind.start[1]: ind.end[length(ind.end)],]
plot(ibwc3$date.time, ibwc3$ibwc.stage.m, type="l")

ind.peak1 = grep(max(ibwc3$Data..ft.), ibwc3$Data..ft.) #index of the max, which is also the #of values that are on the rising limb
rising1 = rep("rising", ind.peak1) 
falling1 = rep("falling", (length(ibwc3$Data..ft.) - ind.peak1))
rising.falling = c(rising1, falling1)
ibwc.4 = cbind(ibwc3, rising.falling)

#Calculate IBWC Q with updated rising and falling rating curves
#rising: Q = 25.26374*stage - 21.61612
rising.sub1 = ibwc.4[1:ind.peak1,]
ibwc.Q.cms.adj.rating.rising1 = 19.60762*rising.sub1$ibwc.stage.m -17.76322
ibwc.Q.cms.adj.rating.rising1[ibwc.Q.cms.adj.rating.rising1<0]<-0
#falling rating: Q = 30.29165*stage - 29.67138
falling.sub1 = ibwc.4[(ind.peak1+1):length(ibwc.4$Date),]
ibwc.Q.cms.adj.rating.falling1 = 21.20415*falling.sub1$ibwc.stage.m -20.76996
ibwc.Q.cms.adj.rating.falling1[ibwc.Q.cms.adj.rating.falling1<0]<-0

#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted = c(ibwc.Q.cms.adj.rating.rising1,ibwc.Q.cms.adj.rating.falling1)

#Plot the updated rising/falling rating curve
plot(ibwc.4$date.time, ibwc.Q.cms.adjusted, type="l")

###############################################################################################################

#FIGRUE 2.17 
#Overall Panel Plots of everything
layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.05,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip$date.time, precip$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)", xlim = c(as.POSIXct(precip$date.time[1]),as.POSIXct("2016-04-12 00:00:00 PST"))) 
legend("topleft", "A)", bty="n", cex=1, inset=c(-.03,-.05)) #inset first value is L-R, second is up-down
par(mar = c(0, 4.1, 0, 2.1))
plot(obs$date.time, obs$PT, type="l", col="blue", ylab = "Pressure (m)", xaxt = 'n', ylim=c(10,10.55), xlim = c(as.POSIXct(precip$date.time[1]),as.POSIXct("2016-04-12 00:00:00 PST")))
lines(approx.tje.naval$x, pressure.tje.naval.m.approx,type="l", col="green")
lines(approx.tje.naval$x, tje.naval.adj, type = "l", col="green")
legend("topleft", c("TJE Naval","PT"),  col=c("green","blue"),lwd=1, lty=1,inset=c(.08,0), cex=0.75,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1, inset=c(-.03,-.05)) #inset first value is L-R, second is up-down
axis.POSIXct(side = 1, obs$date.time, format = "%Y-%m-%d")

#missed the rainfall event, didn't include other 2 plots

#put layout of graphs back to one graph
par(mfrow=c(1,1),xpd=FALSE)

###############################################################################################################
#Summary tables --> empty but that's okay

#output summary for observed data (with "A" rating, use as observed)
date = c("2016-04-09")
event = c(NA)
source = c(NA) #which datasource was used (all A ratings)
peak.q.obs.cms = NA
total.q.obs.mm = NA
total.precip.mm = NA
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary, file="summary_20160409_observed_q.csv", row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = NA
IBWC.peak.q.obs.cms = NA
PT.total.q.obs.mm = NA
IBWC.total.q.obs.mm = NA
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary.PT.IBWC, file="summary_20160409_observed_q_PT_IBWC.csv", row.names=F)

