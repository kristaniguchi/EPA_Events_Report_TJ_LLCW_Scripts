#Figure 2.4
#IBWC Updated Rating Curve
#Using storm 10: 2/27/2017 event: camera discharge vs. IBWC bubbler stage

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Read in IBWC and field camera data

#IBWC
##to read in ibwc data, 
ibwc0 = read.csv(file="ibwc_stage.csv", header=TRUE) 
date.time = strptime(ibwc0$date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
ibwc = cbind(ibwc0, date.time, date, time)

#Field Camera
#to read in stage data from llc outlet FIELD CAMERA!
llc0 = read.csv(file = "llc_stage.csv", header=TRUE) #sheet from stage from camera
date.time = strptime(llc0$date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
llc = cbind(llc0, date.time, date, time)

###############################################################################################################

#interpolate the ibwc data to get the same time stamp as llc_stage data (camera)
approx.ibwc = data.frame(approx(ibwc$date.time, as.numeric(as.character(ibwc$stage)), llc$date.time)) 
approx.llc = data.frame(approx(llc$date.time, as.numeric(as.character(llc$stage)), ibwc$date.time)) 
diff_ibwc_llc= ibwc$stage - approx.llc$y 

#####plot stage timeseries for llc and ibwc
plot(llc$date.time,llc$stage, xlab="Time", ylab="Stage (m)",type="l", ylim=c(0,2))
lines(ibwc$date.time,ibwc$stage, lty=2)
legend("topright", c("LLCW Camera","IBWC Bubbler"), lty=c(1,2))
lines(ibwc$date.time,diff_ibwc_llc, col="red")

#Plot IBWC stage vs. llc camera stage
plot(ibwc$stage,approx.llc$y,xlab="IBWC Stage (m)", ylab="LLCW Stage (m)", main="2/27/2017 Stage (m)") #use this plot!

###############################################################################################################

#Split data into rising and falling limb
rising.falling.ibwc = data.frame(cbind(approx.llc, ibwc)) #combine ibwc and camera llc data into one dataframe
names(rising.falling.ibwc) <- c("date.time","approx.llc.stage.m","date","ibwc.stage.m","ibwc.stage.ft","notes","date.time2","date","time")

rising.ibwc = rising.falling.ibwc[rising.falling.ibwc$notes=="rising",]
falling.ibwc = rising.falling.ibwc[rising.falling.ibwc$notes=="falling",]

#PLOT rising/falling limb with LLCW camera stage vs. IBWC stage
#color code rising and falling limb
plot(ibwc$stage,approx.llc$y,ylab="LLCW Camera Stage (m)", xlab="IBWC Stage (m)", main="2/27/2017") #use this plot!
points(rising.ibwc$ibwc.stage.m, rising.ibwc$approx.llc.stage.m, ylab="LLC Stage (m)", xlab="IBWC Stage (m)", main="Stage(m)", col="blue") #use this plot!
points(falling.ibwc$ibwc.stage.m, falling.ibwc$approx.llc.stage.m, ylab="LLC Stage (m)", xlab="IBWC Stage (m)", pch=16,main="Stage(m)", col="red") #use this plot!
legend("topleft", c("Rising Limb","Falling Limb"), pch=c(1,16), col=c("blue","red"), cex=0.8)

###############################################################################################################
#Calculate LLCW camera discharge using manning's equation and manning's n of 0.013
#Calculate IBWC discharge using original rating curve (just for comparison)

#discharge llc, n 0.013
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_Q_mannings.R') #functions are saved in script directory
Q.llc.0.013 = calculateQ.mannings(llc$stage, 0.013)

#Original IBWC RATING CURVE: discharge ibwc using orig rating curve for comparison to camera discharge
ibwc.stage.m= ibwc$stage.ft*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$stage.ft-1283.6
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847

###############################################################################################################
#Observed velocity and discharge at camera location (same as PT location)

##to read observed velocity in llc outlet velocity_llc_02272017.csv, calc Q, and back calculate manning's n
obs.vel.llc0 = read.csv(file="velocity_llc_02272017.csv", header=TRUE) #observed velocity at outlet
date.time = strptime(obs.vel.llc0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs.vel.adj = obs.vel.llc0$v*0.85 #adjust velocity measurements from surface by factor of 0.85
obs.vel.llc = cbind(obs.vel.llc0, obs.vel.adj, date.time, date, time)
stage.vel.llc.m = data.frame(approx(llc$date.time, as.numeric(as.character(llc$stage)), obs.vel.llc$date.time)) 

#Calculate hydraulic radius and flow area for observed discharge calculation
source('../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_hyd_radius.R') #functions are saved in script directory
R.obs = data.frame(calculateR(stage.vel.llc.m$y )) #to get hyd radius and flow area m2
obs.Q.llc = obs.vel.adj* R.obs$area.m2 #q=va (manning's equation)

#back calculate manning's n from observed v manning's = V=( R^(2/3) * 0.011^(1/2) )/n; n=( R^(2/3) * 0.011^(1/2) )/V
n = ( R.obs$hyd.radius^(2/3) * 0.01575^(1/2) )/obs.vel.adj #new slope = 0.01575
mean.n = mean(n) #mean manning's n from obs vel and stage, mean n too low, will stick with previous back calculation of 0.013

###############################################################################################################

#####plot discharge llc-ibwc timeseries
plot(llc$date.time, Q.llc.0.013, type="l", xlab="Time", main="02/27/2017",ylab="Discharge (cms)", ylim=c(0,55))
lines(ibwc$date.time, ibwc.q.cms, lty=2)
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)
legend("topright", c("LLCW Camera","IBWC Bubbler","Observed"), lty=c(1,2,NA), pch=c(NA,NA,16))

##aprox discharge for LLC at the time that we IBWC stage!
approx.llc_q_ibwc = data.frame(approx(llc$date.time, as.numeric(as.character(Q.llc.0.013)), ibwc$date.time)) 

###############################################################################################################

#Rating curve IBWC stage vs. LLC dishcarge!
plot(ibwc.stage.m, approx.llc_q_ibwc[,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p")
points(ibwc.stage.m[5:14], approx.llc_q_ibwc[5:14,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p", col="blue")
points(ibwc.stage.m[15:53], approx.llc_q_ibwc[15:53,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p", col="red", pch=16)
legend("topleft", c("Rising Limb","Falling Limb"), pch=c(1,16), col=c("blue","red"), cex=0.8)

#add in orig rating curve onto plot
#discharge ibwc using orig rating curve
ibwc.stage.ft.temp = c(0.4/0.3048, 2/0.3048)
ibwc.stage.m.temp= ibwc.stage.ft.temp*0.3048 #feet to meters conv
ibwc.q.cfs.temp= 588.24*ibwc.stage.ft.temp-1283.6
ibwc.q.cfs.temp[ibwc.q.cfs.temp<0]<- 0
ibwc.q.cms.temp = ibwc.q.cfs.temp*0.028316847
lines(ibwc.stage.m.temp,ibwc.q.cms.temp, lty=2 )
legend("topleft", c("Rising Limb","Falling Limb","IBWC Rating Curve"), pch=c(1,16,NA), col=c("blue","red","black"), lty=c(NA,NA,2),cex=0.8)

#Split rising and falling limb!
rising.data = data.frame(cbind(ibwc.stage.m[5:14], approx.llc_q_ibwc[5:14,2]))  #don't inclulde first 4 lower values because they're outliers
names(rising.data) <- c("ibwc.stage.m.rising", "approx.llc_q_ibwc_rising")
falling.data = data.frame(cbind(ibwc.stage.m[15:53], approx.llc_q_ibwc[15:53,2]))
names(falling.data) <- c("ibwc.stage.m.falling", "approx.llc_q_ibwc_falling")

#regression equation for rating curves rising and falling limb! 0.013
###RISING
rating.curve.rising = lm(approx.llc_q_ibwc_rising ~ ibwc.stage.m.rising, rising.data)
summary(rating.curve.rising)
b = as.numeric(rating.curve.rising$coefficients[1])
a = as.numeric(rating.curve.rising$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b      #rising: Q = 19.60762*stage -17.76322
rating.curve.llc.rising = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.rising$x.num, rating.curve.llc.rising$y.num, col="blue")
#Recalculate IBWC Q with new rating curve for rising limbs!
ibwc.Q.cms.adj.rating.rising = rising.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.rising[ibwc.Q.cms.adj.rating.rising<0]<-0
###FALLING 0.013
rating.curve.falling = lm(approx.llc_q_ibwc_falling ~ ibwc.stage.m.falling, falling.data)
summary(rating.curve.falling)
b = as.numeric(rating.curve.falling$coefficients[1])
a = as.numeric(rating.curve.falling$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b     #falling rating: Q = 21.20415*stage -20.76996
rating.curve.llc.falling = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.falling$x.num, rating.curve.llc.falling$y.num, col="red")
#Recalculate IBWC Q with new rating curve for falling limbs!
ibwc.Q.cms.adj.rating.falling = falling.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.falling[ibwc.Q.cms.adj.rating.falling<0]<-0
#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted = c(ibwc.Q.cms.adj.rating.rising,ibwc.Q.cms.adj.rating.falling)
ibwc.date.time = c(rising.ibwc$date.time, falling.ibwc$date.time)

###############################################################################################################
#Figure 2.4

#Plot of ibwc stage v. camera discharge, sep by rising and falling limb
plot(ibwc.stage.m, approx.llc_q_ibwc[,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p")
points(ibwc.stage.m[5:14], approx.llc_q_ibwc[5:14,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p", col="blue")
points(ibwc.stage.m[15:53], approx.llc_q_ibwc[15:53,2], main="Rating Curve", xlab="IBWC Stage (m)", ylab="LLCW Discharge (cms)", type="p", col="red", pch=16)
lines(rating.curve.llc.falling$x.num, rating.curve.llc.falling$y.num, col="red")
lines(rating.curve.llc.rising$x.num, rating.curve.llc.rising$y.num, col="blue")
legend("topleft", c("Rising Limb","Falling Limb"), pch=c(1,16), col=c("blue","red"), cex=0.8)

###############################################################################################################

#PLOT OF DISCHARGE TIMESERIES including new rating curve
plot(llc$date.time, Q.llc.0.013, type="l", main="02/27/2017", xlab="Time", ylab="Discharge cms", ylim=c(0,55))
lines(ibwc$date.time, ibwc.q.cms, lty=2)
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)
#adjusted rating curve
lines(ibwc.date.time, ibwc.Q.cms.adjusted, lty=2, lwd=2, col="black")
legend("topright", c("LLCW Camera","IBWC Bubbler- Orig. Rating","Observed Q","IBWC Bubbler- Adj. Rating"), cex=.75,lty=c(1,2,NA,2),lwd=c(1,1,NA,2), pch=c(NA,NA,16,NA), col=c("black", "black","black","black"))

