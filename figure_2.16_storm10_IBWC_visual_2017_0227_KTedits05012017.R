#Storm event 10: 2/27/2017
#ibwc and llc outlet comparison (stage,velocity, discharge)
#Figure xx in Events Report for EPA
#Script written by Kristine Taniguchi, SDSU (kristaniguchi@gmail.com)
#KT updated using IBWC stage and rating curve, manning's n 0.013 --> use PT for this event
#1 event: IBWC and Field Camera data --> use field camera and 0.013 n
#IWBC Rating curve developed from this EVENT

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
dir.concepts.main = "C:/Users/Kris/Documents/GitHub/Dissertation/AGNPS/Napo_4_4_16/cc/Main_flow_05182016" #for CONCEPTS output summary from Main

###############################################################################################################

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

#####plot stage llc-ibwc
plot(llc$date.time,llc$stage, xlab="Time", ylab="Stage (m)",type="l", ylim=c(0,2))
lines(ibwc$date.time,ibwc$stage, lty=2)
legend("topright", c("LLCW Camera","IBWC Bubbler"), lty=c(1,2))
#legend("topleft", c("SAN","TJE Naval","PT"),  col=c("black","green","blue"),lwd=1, lty=c(2,1,1),inset=c(.08,0), cex=0.75,bty = "n" ) #bty="n" means no box

#3. interpolate the ibwc data to get the same time stamp as llc_stage data
approx.ibwc = data.frame(approx(ibwc$date.time, as.numeric(as.character(ibwc$stage)), llc$date.time)) 
approx.llc = data.frame(approx(llc$date.time, as.numeric(as.character(llc$stage)), ibwc$date.time)) 
diff_ibwc_llc= ibwc$stage - approx.llc$y 
lines(ibwc$date.time,diff_ibwc_llc, col="red")

plot(approx.llc$y, ibwc$stage,xlab="LLC", ylab="IBWC")
plot(approx.llc$y, ibwc$stage,log="xy",xlab="LLC", ylab="IBWC")

#the other way to aproxx
plot(ibwc$stage,approx.llc$y,xlab="IBWC Stage (m)", ylab="LLCW Stage (m)", main="2/27/2017 Stage (m)") #use this plot!
  plot(ibwc$stage,approx.llc$y,log="xy",xlab="IBWC Bubbler", ylab="LLC")

#color code rising and falling limb
rising.falling.ibwc = data.frame(cbind(approx.llc, ibwc))
names(rising.falling.ibwc) <- c("date.time","approx.llc.stage.m","date","ibwc.stage.m","ibwc.stage.ft","notes","date.time2","date","time")

rising.ibwc = rising.falling.ibwc[rising.falling.ibwc$notes=="rising",]
falling.ibwc = rising.falling.ibwc[rising.falling.ibwc$notes=="falling",]

#PLOT rising/falling limb with LLC stage (approx) vs. IBWC stage
plot(ibwc$stage,approx.llc$y,ylab="LLCW Stage (m)", xlab="IBWC Stage (m)", main="2/27/2017 Stage(m)") #use this plot!
points(rising.ibwc$ibwc.stage.m, rising.ibwc$approx.llc.stage.m, ylab="LLC Stage (m)", xlab="IBWC Stage (m)", main="Stage(m)", col="blue") #use this plot!
points(falling.ibwc$ibwc.stage.m, falling.ibwc$approx.llc.stage.m, ylab="LLC Stage (m)", xlab="IBWC Stage (m)", pch=16,main="Stage(m)", col="red") #use this plot!
legend("topleft", c("Rising Limb","Falling Limb"), pch=c(1,16), col=c("blue","red"), cex=0.8)


#discharge llc, n 0.013
path.name.calcQ.mannings = paste(dir, "/", "function_calc_Q_mannings.R", sep="")
source(path.name.calcQ.mannings)
Q.llc.0.013 = calculateQ.mannings(llc$stage, 0.013)
llc$date.time[70]
  #Napo's calculate Q without the function
  #R=(4.96*llc$stage)/(4.96+(2*llc$stage))
  #V=(R^0.666*0.011^0.5)/0.02
  #A=4.96*llc$stage
  #Q.llc.napo=V*A
  #diff.kt.napo.q = Q.llc -Q.llc.napo

#discharge llc, n 0.011 and 0.013
Q.llc.0.015 = calculateQ.mannings(llc$stage, 0.015)
Q.llc.0.017 = calculateQ.mannings(llc$stage, 0.017)

#OLD IBWC RATING CURVE: discharge ibwc using orig rating curve
ibwc.stage.m= ibwc$stage.ft*0.3048 #feet to meters conv
ibwc.q.cfs= 588.24*ibwc$stage.ft-1283.6
ibwc.q.cfs[ibwc.q.cfs<0]<- 0
ibwc.q.cms = ibwc.q.cfs*0.028316847

##to read observed velocity in llc outlet velocity_llc_02272017.csv, calc Q, and back calculate manning's n
obs.vel.llc0 = read.csv(file="velocity_llc_02272017.csv", header=TRUE) #observed velocity at outlet
date.time = strptime(obs.vel.llc0$Date,"%m/%d/%Y %H:%M")
time = format(date.time,"%H:%M:%S")
date = format(date.time,"%Y-%m-%d" )
obs.vel.adj = obs.vel.llc0$v*0.85 #adjust velocity measurements from surface by factor of 0.85
obs.vel.llc = cbind(obs.vel.llc0, obs.vel.adj, date.time, date, time)
stage.vel.llc.m = data.frame(approx(llc$date.time, as.numeric(as.character(llc$stage)), obs.vel.llc$date.time)) 

path.name.calc.hydradius = paste(dir, "/", "function_calc_hyd_radius.R", sep="")
source(path.name.calc.hydradius)
R.obs = data.frame(calculateR(stage.vel.llc.m$y )) #to get hyd radius and flow area m2
obs.Q.llc = obs.vel.adj* R.obs$area.m2 #q=va
#back calculate manning's n from observed v manning's = V=( R^(2/3) * 0.011^(1/2) )/n; n=( R^(2/3) * 0.011^(1/2) )/V
n = ( R.obs$hyd.radius^(2/3) * 0.01575^(1/2) )/obs.vel.adj #new slope = 0.01575
mean.n = mean(n) #mean manning's n from obs vel and stage

#plus 5 cm to the stage 
stage.vel.llc.m.plus5cm = stage.vel.llc.m$y + 0.05
R.obs.plus5cm = data.frame(calculateR(stage.vel.llc.m.plus5cm )) #to get hyd radius and flow area m2
obs.Q.llc.plus5cm = obs.vel.adj* R.obs.plus5cm$area.m2 #q=va
#back calculate manning's n from observed v manning's = V=( R^(2/3) * 0.011^(1/2) )/n; n=( R^(2/3) * 0.011^(1/2) )/V
n.plus5cm = ( R.obs.plus5cm$hyd.radius^(2/3) * 0.01575^(1/2) )/obs.vel.adj #new slope = 0.01575
mean.n.plus5cm = mean(n.plus5cm) #mean manning's n from obs vel and stage+5cm --> 0.012

#minus 5 cm to stage
stage.vel.llc.m.minus5cm = stage.vel.llc.m$y - 0.05
R.obs.minus5cm = data.frame(calculateR(stage.vel.llc.m.minus5cm )) #to get hyd radius and flow area m2
obs.Q.llc.minus5cm = obs.vel.adj* R.obs.minus5cm$area.m2 #q=va
#back calculate manning's n from observed v manning's = V=( R^(2/3) * 0.011^(1/2) )/n; n=( R^(2/3) * 0.011^(1/2) )/V
n.minus5cm = ( R.obs.minus5cm$hyd.radius^(2/3) * 0.01575^(1/2) )/obs.vel.adj #new slope = 0.01575
mean.n.minus5cm = mean(n.minus5cm, na.rm = TRUE) #mean manning's n from obs vel and stage-5cm --> 0.007


#Napo's old Q calcs
#obs.Q = read.csv(file="obs_Q.csv", header=TRUE) 
#date.time = strptime(obs.Q$date,"%m/%d/%Y %H:%M")
#time = format(date.time,"%H:%M:%S")
#date = format(date.time,"%Y-%m-%d" )
#obs_Q = cbind(obs.Q, date.time, date, time)


#####plot discharge llc-ibwc timeseries
plot(llc$date.time, Q.llc.0.013, type="l", xlab="Time", main="02/27/2017",ylab="Discharge (cms)", ylim=c(0,55))
lines(ibwc$date.time, ibwc.q.cms, lty=2)
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)
legend("topright", c("LLCW Camera","IBWC Bubbler","Observed"), lty=c(1,2,NA), pch=c(NA,NA,16))

#####plot discharge llc-ibwc timeseries
plot(llc$date.time, Q.llc.0.013, type="l", xlab="Time", main="02/27/2017",ylab="Discharge (cms)", ylim=c(0,20))
#lines(ibwc$date.time, ibwc.q.cms, lty=2) #orig rating curve
lines(llc$date.time, Q.llc.0.015, col="blue")
lines(llc$date.time, Q.llc.0.017, col="red")
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)
legend("topright", c("Camera 0.013","Camera 0.015","Camera 0.017", "Observed"), col=c("black","blue","red","black"), lty=c(1,1,1,NA), pch=c(NA,NA,NA,16))

##aprox discharge for LLC at the time that we IBWC stage!
approx.llc_q_ibwc = data.frame(approx(llc$date.time, as.numeric(as.character(Q.llc.0.013)), ibwc$date.time)) 

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
#legend("topleft", c("LLCW Rising Limb","ILLCW Falling Limb","IBWC Rating Curve"), pch=c(1,16,NA), col=c("blue","red","black"), lty=c(NA,NA,2),cex=0.8)

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



#regression equation for rating curves rising and falling limb! 0.015
##aprox discharge for LLC at the time that we IBWC stage!
approx.llc_q_ibwc.0.015 = data.frame(approx(llc$date.time, as.numeric(as.character(Q.llc.0.015)), ibwc$date.time)) 
#Split rising and falling limb!
rising.data.0.015 = data.frame(cbind(ibwc.stage.m[5:14], approx.llc_q_ibwc.0.015[5:14,2]))  #don't inclulde first 3 lower values because they're outliers
names(rising.data.0.015) <- c("ibwc.stage.m.rising", "approx.llc_q_ibwc_rising.0.015")
falling.data.0.015 = data.frame(cbind(ibwc.stage.m[15:53], approx.llc_q_ibwc.0.015[15:53,2]))
names(falling.data.0.015) <- c("ibwc.stage.m.falling", "approx.llc_q_ibwc_falling.0.015")
###RISING
rating.curve.rising.0.015 = lm(approx.llc_q_ibwc_rising.0.015 ~ ibwc.stage.m.rising, rising.data.0.015)
summary(rating.curve.rising.0.015)
b = as.numeric(rating.curve.rising.0.015$coefficients[1])
a = as.numeric(rating.curve.rising.0.015$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b      #rising: Q = 16.99327*stage -15.39479
rating.curve.llc.rising.0.015 = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.rising.0.015$x.num, rating.curve.llc.rising.0.015$y.num, col="blue",lty=2)
#Recalculate IBWC Q with new rating curve for rising limbs!
ibwc.Q.cms.adj.rating.rising.0.015 = rising.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.rising.0.015[ibwc.Q.cms.adj.rating.rising.0.015<0]<-0
###FALLING 0.015
rating.curve.falling.0.015 = lm(approx.llc_q_ibwc_falling.0.015 ~ ibwc.stage.m.falling, falling.data.0.015)
summary(rating.curve.falling.0.015)
b = as.numeric(rating.curve.falling.0.015$coefficients[1])
a = as.numeric(rating.curve.falling.0.015$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b     #falling rating: Q = 18.37693*stage -18.00064
rating.curve.llc.falling.0.015 = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.falling.0.015$x.num, rating.curve.llc.falling.0.015$y.num, col="red", lty=2)
#Recalculate IBWC Q with new rating curve for falling limbs!
ibwc.Q.cms.adj.rating.falling.0.015 = falling.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.falling.0.015[ibwc.Q.cms.adj.rating.falling.0.015<0]<-0
#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted.0.015 = c(ibwc.Q.cms.adj.rating.rising.0.015,ibwc.Q.cms.adj.rating.falling.0.015)


#regression equation for rating curves rising and falling limb! 0.017
##aprox discharge for LLC at the time that we IBWC stage!
approx.llc_q_ibwc.0.017 = data.frame(approx(llc$date.time, as.numeric(as.character(Q.llc.0.017)), ibwc$date.time)) 
#Split rising and falling limb!
rising.data.0.017 = data.frame(cbind(ibwc.stage.m[5:14], approx.llc_q_ibwc.0.017[5:14,2]))  #don't inclulde first 4 lower values because they're outliers
names(rising.data.0.017) <- c("ibwc.stage.m.rising", "approx.llc_q_ibwc_rising.0.017")
falling.data.0.017 = data.frame(cbind(ibwc.stage.m[15:53], approx.llc_q_ibwc.0.017[15:53,2]))
names(falling.data.0.017) <- c("ibwc.stage.m.falling", "approx.llc_q_ibwc_falling.0.017")
###RISING
rating.curve.rising.0.017 = lm(approx.llc_q_ibwc_rising.0.017 ~ ibwc.stage.m.rising, rising.data.0.017)
summary(rating.curve.rising.0.017)
b = as.numeric(rating.curve.rising.0.017$coefficients[1])
a = as.numeric(rating.curve.rising.0.017$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b      #rising: Q = 14.99406*stage -13.58364
rating.curve.llc.rising.0.017 = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.rising.0.017$x.num, rating.curve.llc.rising.0.017$y.num, col="blue", lty=3)
#Recalculate IBWC Q with new rating curve for rising limbs!
ibwc.Q.cms.adj.rating.rising.0.017 = rising.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.rising.0.017[ibwc.Q.cms.adj.rating.rising.0.017<0]<-0
###FALLING 0.017
rating.curve.falling.0.017 = lm(approx.llc_q_ibwc_falling.0.017 ~ ibwc.stage.m.falling, falling.data.0.017)
summary(rating.curve.falling.0.017)
b = as.numeric(rating.curve.falling.0.017$coefficients[1])
a = as.numeric(rating.curve.falling.0.017$coefficients[2])
x.num = c(0.5, 2)
y.num =  a*x.num +   b     #falling rating: Q = 16.21494*stage -15.88291
rating.curve.llc.falling.0.017 = data.frame(cbind(x.num, y.num))
lines(rating.curve.llc.falling.0.017$x.num, rating.curve.llc.falling.0.017$y.num, col="red")
#Recalculate IBWC Q with new rating curve for falling limbs!
ibwc.Q.cms.adj.rating.falling.0.017 = falling.ibwc$ibwc.stage.m*a+b
ibwc.Q.cms.adj.rating.falling.0.017[ibwc.Q.cms.adj.rating.falling.0.017<0]<-0
#Cbind rising and falling IBWC discharge new
ibwc.Q.cms.adjusted.0.017 = c(ibwc.Q.cms.adj.rating.rising.0.017,ibwc.Q.cms.adj.rating.falling.0.017)
ibwc.date.time = c(rising.ibwc$date.time, falling.ibwc$date.time)

#PLOT OF DISCHARGE TIMESERIES including new rating curve
plot(llc$date.time, Q.llc.0.013, type="l", main="02/27/2017", xlab="Time", ylab="Discharge cms", ylim=c(0,55))
lines(ibwc$date.time, ibwc.q.cms, lty=2)
#lines(ibwc$date.time, ibwc.Q.cms.adjusted, lty=2, lwd=2)
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)

legend("topright", c("LLCW Camera","IBWC Bubbler- Orig. Rating","Observed Q","IBWC Bubbler- Adj. Rating"), cex=.75,lty=c(1,2,NA,2),lwd=c(1,1,NA,2), pch=c(NA,NA,16,NA), col=c("black", "black","black","black"))
points(obs.vel.llc$date.time ,obs.Q.llc.minus5cm, pch=16, cex=.75, col="green")
points(obs.vel.llc$date.time ,obs.Q.llc.plus5cm, pch=16, cex=.75, col="red")
arrows(obs.vel.llc$date.time[1] ,obs.Q.llc.minus5cm[1],obs.vel.llc$date.time[1] ,obs.Q.llc.plus5cm[1],length=0)
arrows(obs.vel.llc$date.time[2] ,obs.Q.llc.minus5cm[2],obs.vel.llc$date.time[2] ,obs.Q.llc.plus5cm[2],length=0)
arrows(obs.vel.llc$date.time[3] ,obs.Q.llc.minus5cm[3],obs.vel.llc$date.time[3] ,obs.Q.llc.plus5cm[3],length=0)
arrows(obs.vel.llc$date.time[4] ,obs.Q.llc.minus5cm[4],obs.vel.llc$date.time[4] ,obs.Q.llc.plus5cm[4],length=0)
arrows(obs.vel.llc$date.time[5] ,obs.Q.llc.minus5cm[5],obs.vel.llc$date.time[5] ,obs.Q.llc.plus5cm[5],length=0)
arrows(obs.vel.llc$date.time[6] ,obs.Q.llc.minus5cm[6],obs.vel.llc$date.time[6] ,obs.Q.llc.plus5cm[6],length=0)
legend("topright", c("LLCW Camera","IBWC Bubbler- Orig. Rating","Observed Q","IBWC Bubbler- Adj. Rating","Observed Q - stage +5cm ", "Observed Q - stage -5cm "), cex=.75,lty=c(1,2,NA,2,NA,NA),lwd=c(1,1,NA,2,NA,NA), pch=c(NA,NA,16,NA,16,16), col=c("black", "black","black","red","red","green"))

#approx.llc_q_ibwc
lines(llc$date.time, Q.llc.0.015, col="orange")
lines(llc$date.time, Q.llc.0.017, col="blue")
legend("topright", c("LLCW Camera - 0.013","LLCW Camera - 0.015","LLCW Camera - 0.017","IBWC Bubbler- Orig. Rating","Observed Q","Observed Q - stage +5cm ", "Observed Q - stage -5cm ","IBWC Bubbler- Adj. Rating"), cex=.75,lty=c(1,1,1,2,NA,NA,NA,2),lwd=c(1,1,1,1,NA,NA,NA,2), pch=c(NA,NA,NA,NA,16,16,16,NA), col=c("black", "orange", "blue","black","black","red","green","black"))
#adjusted rating curves 
lines(ibwc.date.time, ibwc.Q.cms.adjusted, lty=2, lwd=2, col="black")
lines(ibwc.date.time, ibwc.Q.cms.adjusted.0.015, lty=2, lwd=2, col="orange")
lines(ibwc.date.time, ibwc.Q.cms.adjusted.0.017, lty=2, lwd=2, col="blue")
legend("topright", c("LLCW Camera - 0.012","IBWC Bubbler- Orig. Rating","Observed Q","Observed Q - stage +5cm ", "Observed Q - stage -5cm ","IBWC Bubbler- Adj. Rating 0.012","IBWC Bubbler- Adj. Rating 0.011","IBWC Bubbler- Adj. Rating 0.013"), cex=.75,lty=c(1,2,NA,NA,NA,2,2,2),lwd=c(1,1,NA,NA,NA,2,2,2), pch=c(NA,NA,16,16,16,NA,NA,NA), col=c("black", "black","black","red","green","black","orange", "blue"))


#zoom
plot(llc$date.time, Q.llc.0.013, type="l", main="02/27/2017", xlab="Time", ylab="Discharge cms", ylim=c(0,20))
lines(ibwc$date.time, ibwc.q.cms, lty=2)
#lines(ibwc$date.time, ibwc.Q.cms.adjusted, lty=2, lwd=2)
points(obs.vel.llc$date.time ,obs.Q.llc, pch=16, cex=.75)

legend("topright", c("LLCW Camera","IBWC Bubbler- Orig. Rating","Observed Q","IBWC Bubbler- Adj. Rating"), cex=.75,lty=c(1,2,NA,2),lwd=c(1,1,NA,2), pch=c(NA,NA,16,NA), col=c("black", "black","black","black"))
points(obs.vel.llc$date.time ,obs.Q.llc.minus5cm, pch=16, cex=.75, col="green")
points(obs.vel.llc$date.time ,obs.Q.llc.plus5cm, pch=16, cex=.75, col="red")
arrows(obs.vel.llc$date.time[1] ,obs.Q.llc.minus5cm[1],obs.vel.llc$date.time[1] ,obs.Q.llc.plus5cm[1],length=0)
arrows(obs.vel.llc$date.time[2] ,obs.Q.llc.minus5cm[2],obs.vel.llc$date.time[2] ,obs.Q.llc.plus5cm[2],length=0)
arrows(obs.vel.llc$date.time[3] ,obs.Q.llc.minus5cm[3],obs.vel.llc$date.time[3] ,obs.Q.llc.plus5cm[3],length=0)
arrows(obs.vel.llc$date.time[4] ,obs.Q.llc.minus5cm[4],obs.vel.llc$date.time[4] ,obs.Q.llc.plus5cm[4],length=0)
arrows(obs.vel.llc$date.time[5] ,obs.Q.llc.minus5cm[5],obs.vel.llc$date.time[5] ,obs.Q.llc.plus5cm[5],length=0)
arrows(obs.vel.llc$date.time[6] ,obs.Q.llc.minus5cm[6],obs.vel.llc$date.time[6] ,obs.Q.llc.plus5cm[6],length=0)
legend("topright", c("LLCW Camera","IBWC Bubbler- Orig. Rating","Observed Q","IBWC Bubbler- Adj. Rating","Observed Q - stage +5cm ", "Observed Q - stage -5cm "), cex=.75,lty=c(1,2,NA,2,NA,NA),lwd=c(1,1,NA,2,NA,NA), pch=c(NA,NA,16,NA,16,16), col=c("black", "black","black","red","red","green"))
#lines(llc$date.time, Q.llc.0.011, col="orange")
lines(llc$date.time, Q.llc.0.013, col="blue")
legend("topright", c("LLCW Camera - 0.012","LLCW Camera - 0.011","LLCW Camera - 0.013","IBWC Bubbler- Orig. Rating","Observed Q","Observed Q - stage +5cm ", "Observed Q - stage -5cm ","IBWC Bubbler- Adj. Rating"), cex=.75,lty=c(1,1,1,2,NA,NA,NA,2),lwd=c(1,1,1,1,NA,NA,NA,2), pch=c(NA,NA,NA,NA,16,16,16,NA), col=c("black", "orange", "blue","black","black","red","green","black"))
#lines(ibwc$date.time, ibwc.Q.cms.adjusted.0.011, lty=2, lwd=2, col="orange")
#lines(ibwc$date.time, ibwc.Q.cms.adjusted.0.013, lty=2, lwd=2, col="blue")
legend("topright", c("LLCW Camera - 0.012","IBWC Bubbler- Orig. Rating","Observed Q","Observed Q - stage +5cm ", "Observed Q - stage -5cm ","IBWC Bubbler- Adj. Rating 0.012","IBWC Bubbler- Adj. Rating 0.011","IBWC Bubbler- Adj. Rating 0.013"), cex=.75,lty=c(1,2,NA,NA,NA,2,2,2),lwd=c(1,1,NA,NA,NA,2,2,2), pch=c(NA,NA,16,16,16,NA,NA,NA), col=c("black", "black","black","red","green","black","orange", "blue"))

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

##aprox discharge for ibwc at the time that we have observations
#approx.ibwc_obs = data.frame(approx(ibwc$date.time, as.numeric(as.character(ibwc.q.cms)), obs_Q$date.time)) 
#approx.llc_obs = data.frame(approx(llc$date.time, as.numeric(as.character(Q.llc)), obs_Q$date.time)) 

plot(approx.ibwc$y, llc$stage,log="xy")
plot(llc$stage,approx.ibwc$y)
plot(llc$stage,approx.ibwc$y,log="xy")

#CAMERA total P, total q, peakq
#calculate peak q and total q in mm for whole storm 1
total.p.mm.camera = precip0$Cumulative.rain.mm[length(precip0$Cumulative.rain.mm)]
peakq.cms.camera = max(Q.llc.0.013, na.rm = TRUE) 
path.name.calctotalQ = paste(dir, "/", "function_calc_total_Q_mm.R", sep="")
source(path.name.calctotalQ)
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
fpathcsv = paste(dir, "/", "summary_20170227_observed_q.csv", sep="")
write.csv(obs.summary, file=fpathcsv, row.names=F)

#output summary for both PT and IBWC data! Use date, event, source, 
PT.peak.q.obs.cms = c(peakq.cms.camera)
IBWC.peak.q.obs.cms = c(peakq.cms.ibwc)
PT.total.q.obs.mm = c(total.q.mm.camera)
IBWC.total.q.obs.mm = c(total.q.mm.ibwc)
total.precip.mm = total.p.mm.camera
obs.summary.PT.IBWC = cbind(date, total.precip.mm, PT.peak.q.obs.cms, IBWC.peak.q.obs.cms, PT.total.q.obs.mm, IBWC.total.q.obs.mm, event, source) #may want to add time to peak column
fpathcsv2 = paste(dir, "/", "summary_20170227_observed_q_PT_IBWC.csv", sep="")
write.csv(obs.summary.PT.IBWC, file=fpathcsv2, row.names=F)



#4/27/2018: NEED TO UPDATE PART BELOW, only had 2 events previously, now have 3.  Also, using IBWC data for E2, E3, so need to use diff data for comparison (data above)

##############################################################################################################################

