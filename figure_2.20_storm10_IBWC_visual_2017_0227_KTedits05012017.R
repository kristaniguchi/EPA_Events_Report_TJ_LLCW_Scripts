#Storm event 10: 2/27/2017
#ibwc and llc outlet comparison (stage,velocity, discharge)
#Figure 2.20 in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#1 event: IBWC and Field Camera data --> use field camera and 0.013 n
#IWBC Rating curve developed from this EVENT

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Run R script from IBWC rating curve script(has observed data that needs to be loaded onto here)
source('../EPA_Events_Report_TJ_LLCW_Scripts/figure_2.04_ibwc_ratingcurve.R') #functions are saved in script directory

###############################################################################################################

#read in precip data for the very top plot in figure 2
precip0 = read.csv(file = "llc_observed_data_02272017_KT_precip.csv", header=TRUE)
date.time = strptime(precip0$Date.time,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
precip00 = cbind(precip0, date.time, date, time)
#precip = na.omit(precip00)

plot(precip00$date.time, precip00$Cumulative.rain.mm, type="l")
###############################################################################################################
#Figure 2.20
#IBWC Overall Panel Plots of everything
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.03,0.05,0.05), respect = FALSE)
par(mar = c(0, 4.1, 0, 2.1)) #set margins for bottom, L, top, R
plot(precip00$date.time, precip00$Cumulative.rain.mm, type="l", xaxt = 'n', ylab="Cum. Rain (mm)")
legend("topleft", "A)", bty="n", cex=1.25, inset=c(-.03,-.085))
#legend("topleft", "A)", bty="n", cex=1.5) #, inset=c(-.09,-.15))  
par(mar = c(0, 4.1, 0, 2.1))
 llc$stage
plot(ibwc$date.time,ibwc.stage.m, ylim=c(0,1.7), lty=2,  type="l",xaxt = 'n',   ylab = "Stage (m)",xlim = c(as.POSIXct(precip00$date.time[1]),as.POSIXct(precip00$date.time[length(precip00$date.time)])))
lines(llc$date.time, llc$stage)
legend("topleft", c("IBWC","Camera","No PT"),  col="black",lwd=1,  lty=c(2,1,NA),inset=c(.08,0), cex=1,bty = "n" ) #bty="n" means no box
legend("topleft", "B)", bty="n", cex=1.25, inset=c(-.03,-.055))
#legend("topleft", "C)", bty="n", cex=1.5) #, inset=c(-.09,-.2))
par(mar = c(2, 4.1, 0, 2.1))
plot(llc$date.time, Q.llc.0.013, type="l", xlab="Date", ylab="Discharge cms", ylim=c(0,20), xaxt = 'n',xlim = c(as.POSIXct(precip00$date.time[1]),as.POSIXct(precip00$date.time[length(precip00$date.time)])))
lines(ibwc.date.time, ibwc.Q.cms.adjusted, lty=2,  col="black")
axis.POSIXct(side = 1, precip00$date.time, format = "%Y-%m-%d")
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)
legend("topleft", c("IBWC","Camera","No PT", "Obs Vel - Q"),  col="black",lwd=c(1,1,NA,NA), pch=c(NA,NA,NA,16), lty=c(2,1,NA,NA),inset=c(.08,0), cex=1,bty = "n" ) #bty="n" means no box
legend("topleft", "C)", bty="n", cex=1.25, inset=c(-.03,-.065))

par(mfrow=c(1,1),xpd=FALSE)
dev.off() #to reset back to one plot

###############################################################################################################

#CAMERA total P, total q, peakq
#calculate peak q and total q in mm for whole storm 1
total.p.mm.camera = precip0$Cumulative.rain.mm[length(precip0$Cumulative.rain.mm)]
peakq.cms.camera = max(Q.llc.0.013, na.rm = TRUE) 
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_total_Q_mm.R ') #functions are saved in script directory
total.q.mm.camera = calculate.total.Q.mm(Q.llc.0.013, llc$date.time)

#CAMERA total P, total q, peakq
#calculate peak q and total q in mm for whole storm 1
total.p.mm.ibwc = precip0$Cumulative.rain.mm[length(precip0$Cumulative.rain.mm)]
peakq.cms.ibwc = max(ibwc.Q.cms.adjusted, na.rm = TRUE) 
total.q.mm.ibwc = calculate.total.Q.mm(ibwc.Q.cms.adjusted, ibwc.date.time)

#Daily precip, will only treat this as one storm event, but calc daily precip separately
#daily total precip for 2017-02-26
precip0 = precip00[precip00$date == "2017-02-26",]
precip_20170226 = precip0$Cumulative.rain.mm[length(precip0$date)] #total precip for 2/28/14 is the last value for that day
#daily total precip for 2017-02-27
precip1 = precip00[precip00$date == "2017-02-27",]
precip_20170227.cum = precip1$Cumulative.rain.mm[length(precip1$date)] #total precip for 2/28/14 is the last value for that day
precip_20170227 = precip_20170227.cum - precip_20170226
#daily total precip for 2017-02-28
precip2 = precip00[precip00$date == "2017-02-28",]
precip_20170228.cum = precip2$Cumulative.rain.mm[length(precip2$date)] #total precip for 2/28/14 is the last value for that day
precip_20170228 = precip_20170228.cum  - precip_20170227.cum 


##############################################################################################################################

#Summary tables, export Q data, etc.
#OUTPUT discharge and stage from CAMERA only (this is the usable q timeseries)
date.time2 = data.frame(llc$date.time)
Event = rep("E1",times=length(Q.llc.0.013))
stage.m = llc$stage
data = cbind(date.time2, Q.llc.0.013, stage.m, Event) #note that this is just the PT data, this doesn't include IBWC rating curve data (E2, E3 use IBWC rating curve data)

#OUTPUT discharge timeseries for A rating only!
#E1 Camera will use!
source.E1 = rep("CAMERA", times=length(Q.llc.0.013))
event1.ibwc = rep("E1", times=length(Q.llc.0.013))
q.data.all = data.frame(cbind(date.time2, Q.llc.0.013, source.E1, event1.ibwc))
names(q.data.all) <- c("date.time", "q.cms", "source", "event")

#output summary for observed data (with "A" rating, use as observed)
date = c("2017-02-27")
event = c("E1")
source = c("CAMERA") #which datasource was used (all A ratings)
peak.q.obs.cms = c(peakq.cms.camera)
total.q.obs.mm = c(total.q.mm.camera)
total.precip.mm = c(total.p.mm.camera)
obs.summary = cbind(date, total.precip.mm, peak.q.obs.cms, total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary, file="summary_20170227_observed_q.csv", row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(peakq.cms.camera)
IBWC.peak.q.obs.cms = c(peakq.cms.ibwc)
PT.total.q.obs.mm = c(total.q.mm.camera)
IBWC.total.q.obs.mm = c(total.q.mm.ibwc)
total.precip.mm = total.p.mm.camera
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
write.csv(obs.summary.PT.IBWC, file="summary_20170227_observed_q_PT_IBWC.csv", row.names=F)



##############################################################################################################################

