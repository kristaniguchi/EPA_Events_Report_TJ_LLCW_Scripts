#Figure 3.2 in EPA Events Report
#SSC data from storm 1: 3/1/2014
#Summary data for Table 3.1 and 3.3
#Napoleon Gudino (CICESE) wrote original SSC calculations part of script, Kris Taniguchi (SDSU) reformatted and updated script

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Run the events report script for storm 1 (figure_2.02_PT_2014_03_01_KTedits04172017.R) to get the appropriate Q, stage, etc.
source("../EPA_Events_Report_TJ_LLCW_Scripts/figure_2.02_storm1_PT_2014_03_01_KTedits04172017.R")

#Dataframe q.data.all is the usable observed data (timeseries of all the good data)
names(q.data.all)
date.time = as.POSIXct(q.data.all$date.time) #date.time from obs data
q.cms = as.numeric(as.character(q.data.all[,2])) #q.data.all[,2] saved as factor, change to numeric

#SSC Data --> by Napo
# Load SSC data
fname.sed = "ssc_2014-16.csv"
x.ssc = read.csv(fname.sed)
dates.ssc = strptime(x.ssc$Date,format="%m/%d/%Y, %I:%M:%S %p")

#plot Q timeseries with SSC
par(oma=c(2,2,2,2),mar = c(4, 4, 0, 2))
xlimits = c(as.POSIXct(as.POSIXct("2014-02-28 12:00:00 PST")),as.POSIXct("2014-03-01 10:00:00 PST"))
xtics = seq(xlimits[1],xlimits[2],by=3600)
xlabels = format(xtics,"%m-%d %H:%M")
plot(date.time, q.cms, type ="l", xlab = "", ylab = "Discharge (cms)", xaxt = "n",xlim=xlimits)
axis(side = 1, date.time,at=xtics,labels=xlabels,las=2)
par(new=T)
plot(dates.ssc,x.ssc$g.l,yaxt="n",xlim=xlimits,xlab="",ylab="",pch=20,cex=2)
axis(side=4) 
mtext("SSC g/l",side = 4, line = 3)

###############################################################################################################
#find discharge at time that SSC was collected

#find time of SSC samples #sample1
ftime = date.time
target.time = as.POSIXct("2014-02-28 17:20:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time
index = which(difftime==min(abs(difftime))) #absolute min time diff is when sample was taken (0 is at same time)
match.data = q.cms[index] #discharge at that time
q.cms[(index-10):(index+10)]
date.time[index]
q.data.all[index,] #to make sure E2 for that date.time

#find time of SSC samples #sample2, E3
ftime2 = date.time
target.time2 = as.POSIXct("2014-03-01 07:40:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
approx.q2 = approx(date.time, q.cms, target.time2) #interpolate the discharge at specified time
match.data[2] = approx.q2$y #second discharge measurement

###############################################################################################################

#Summary data for Table 3.1:
match.data #the discharge at time of SSC collection
ssc.date.time = c(target.time, target.time2) #the date.time of SSC collecction
ssc = x.ssc$g.l[1:2]
EMC = mean(ssc)
storm = "Storm 1"
Event = c("E2", "E3")

table.3.1.export = data.frame(cbind(as.character(ssc.date.time), ssc, match.data, Event))
names(table.3.1.export) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
###############################################################################################################

#Table 3.3 Caculations
#SSC1: "2014-02-28 17:20:00 PST" during E2: 3/1/2014; SSC2: "2014-03-01 07:40:00 PST" during E3: 
#Volume weighted mean (VWM) = sum(C1*Q1, C2*Q2, Cn*Qn) / sum(Q1, Q2, Qn)
VWM = ssc*match.data/match.data #in this case, only 1 SSC ssample for each event, VWM= SSC
EMC = ssc #only  one sample per event, event mean concentration = SSC

#Load = total q (m3) * VWM (g/L) * 1000L/1 m3 * 1e-6 tonne/g 
  #samples taken during E2 and E3, do not need E1 data for total.q.m3  
total.q.m3 = total.q.obs.mm[2:3]/1000*10230000 #convert to m, multiply by 10.23 km2 wtshd area or 10230000 m2
load.g = VWM*total.q.m3*1000 #1000L = 1m3
load.ton = load.g * 1e-6 #1 gram = 1e-6 ton
peak.q.obs.cms[2:3] #peak q from E2 and E3

#for table 3.3:
date = obs.summary[2:3,1] #second and third event
event = obs.summary[2:3,5] #second and third event
date.event = paste(date, event, sep=" ")
table.3.3.export = data.frame(cbind(date.event, peak.q.obs.cms[2:3], total.q.obs.mm[2:3], total.q.m3, load.ton, VWM, EMC)) 
names(table.3.3.export) <- c("event.date", "peak.q.cms", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")
