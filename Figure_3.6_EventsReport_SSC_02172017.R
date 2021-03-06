#Figure 3.6 in EPA Events Report
#SSC data from storm 9: 02/17/2017
#Summary data for Table 3.1 and 3.3
#Kris Taniguchi, SDSU (kristaniguchi@gmail.com) reformatted and updated script

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Run the events report script for storm 9 (figure_2.19_storm9_IBWC_visual_2017_02_KTedits05012017.R) to get the appropriate Q, stage, etc.
source("../EPA_Events_Report_TJ_LLCW_Scripts/figure_2.19_storm9_IBWC_visual_2017_02_KTedits05012017.R")

#Dataframe q.data.all is the usable observed data (timeseries of all the good data)
names(q.data.all)
date.time = as.POSIXct(q.data.all$date.time) #date.time from obs data
q.cms = as.numeric(as.character(q.data.all[,2])) #q.data.all[,2] saved as factor, change to numeric

#SSC Data 
# Load SSC data
fname.sed = "ssc_2014-17.csv" #updated ssc csv with the 02/2017 data
x.ssc = read.csv(fname.sed)
dates.ssc = strptime(x.ssc$Date,format="%m/%d/%Y, %I:%M:%S %p")

#plot Q timeseries with SSC
par(oma=c(2,2,2,2),mar = c(4, 4, 0, 2))
xlimits = c(as.POSIXct(as.POSIXct("2017-02-17 00:00:00 PST")),as.POSIXct("2017-02-18 12:00:00 PST"))
xtics = seq(xlimits[1],xlimits[2],by=3600*2)
xlabels = format(xtics,"%m-%d %H:%M")
plot(date.time, q.cms, type ="l", xlab = "", ylab = "Discharge (cms)", xaxt = "n",xlim=xlimits)
axis(side = 1, date.time,at=xtics,labels=xlabels,las=2)
par(new=T)
plot(dates.ssc,x.ssc$g.l,yaxt="n",xlim=xlimits,xlab="",ylab="",pch=20,cex=2)
axis(side=4) 
mtext("SSC g/l",side = 4, line = 3)

###############################################################################################################
#find discharge at time that SSC was collected

#find time of SSC samples #sample1, E1
ftime = date.time
target.time = as.POSIXct("2017-02-17 16:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time
index = which(difftime==min(abs(difftime))) #absolute min time diff is when sample was taken (0 is at same time)
match.data = q.cms[index] #discharge at that time
q.cms[(index-10):(index+10)]
date.time[index]
q.data.all[index,]

#find time of SSC samples #sample2, E1
ftime2 = date.time
target.time2 = as.POSIXct("2017-02-17 17:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
approx.q2 = approx(date.time, q.cms, target.time2) #interpolate the discharge at specified time
match.data[2] = approx.q2$y #second discharge measurement

#find time of SSC samples #sample3, E1
ftime3 = date.time
target.time3 = as.POSIXct("2017-02-17 20:30:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
approx.q3 = approx(date.time, q.cms, target.time3) #interpolate the discharge at specified time
match.data[3] = approx.q3$y #second discharge measurement

###############################################################################################################

#Summary data for Table 3.1:
match.data #the discharge at time of SSC collection
ssc.date.time = c(target.time, target.time2, target.time3) #the date.time of SSC collecction
ssc = x.ssc$g.l[14:16]
EMC = mean(ssc)
storm = "Storm 9"
Event = c("E1", "E1","E1")

#two values have Q<0.07, set Q as minimum value that corresponds to 2cm stage
source("../EPA_Events_Report_TJ_LLCW_Scripts/function_calc_Q_mannings.R") #function to calculate Q based on stage and manning's n
q.2cm = calculateQ.mannings(0.02, 0.013) #2cm depth, 0.013 manning's n
#replace match.data discharge at 0 with q.2cm
match.data[match.data < q.2cm] <- q.2cm

table.3.1.export = data.frame(cbind(as.character(ssc.date.time), ssc, match.data, Event))
names(table.3.1.export) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")

###############################################################################################################
#Table 3.3 Caculations
#Volume weighted mean (VWM) = sum(C1*Q1, C2*Q2, Cn*Qn) / sum(Q1, Q2, Qn)
VWM = sum(ssc*match.data)/sum(match.data) 
EMC = mean(ssc) #Event mean concentration

#Load = total q (m3) * VWM (g/L) * 1000L/1 m3 * 1e-6 tonne/g 
#samples taken during E1 
total.q.m3 = total.q.obs.mm/1000*10230000 #convert to m, multiply by 10.23 km2 wtshd area or 10230000 m2
load.g = VWM*total.q.m3*1000 #1000L = 1m3
load.ton = load.g * 1e-6 #1 gram = 1e-6 ton
peak.q.obs.cms

#for table 3.3:
date = obs.summary[,1] 
event = obs.summary[,5] 
date.event = paste(date, event, sep=" ")
table.3.3.export = data.frame(cbind(date.event, peak.q.obs.cms, total.q.obs.mm, total.q.m3, load.ton, VWM, EMC)) 
names(table.3.3.export) <- c("event.date", "peak.q.cms", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")


