#Figure 3.5 in EPA Events Report
#SSC data from storm 6: 03-06-2016
#Summary data for Table 3.1 and 3.3
#Napoleon Gudino (CICESE) wrote original SSC calculations part of script, Kris Taniguchi (SDSU) reformatted and updated script

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################

#Run the events report script for storm 1 (figure_2.15_storm6_PT_2016_03b_KTedits04172017.R) to get the appropriate Q, stage, etc.
source("../EPA_Events_Report_TJ_LLCW_Scripts/figure_2.15_storm6_PT_2016_03b_KTedits04172017.R")

#Dataframe q.data.all is the usable observed data (timeseries of all the good data)
names(q.data.all)
date.time = as.POSIXct(q.data.all$date.time) #date.time from obs data
q.cms = as.numeric(as.character(q.data.all[,2])) #q.data.all[,2] saved as factor, change to numeric

#SSC Data --> by Napo
# Load SSC data
fname.sed = "ssc_2014-16.csv"
x.ssc = read.csv(fname.sed)
dates.ssc = strptime(x.ssc$Date,format="%m/%d/%Y, %I:%M:%S %p")

#plot Q timeseries 
par(oma=c(2,2,2,2))
xlimits = c(as.POSIXct(as.POSIXct("2016-03-06 00:00:00 PST")),as.POSIXct("2016-03-08 10:07:00 PST"))
xtics = seq(xlimits[1],xlimits[2],by=3600)
xlabels = format(xtics,"%m-%d %H:%M")
plot(date.time, q.cms, type ="l", xlab = "Date", ylab = "Discharge (cms)", xaxt = "n",xlim =xlimits)
axis.POSIXct(side = 1, date.time, format = "%Y-%m-%d")
par(new=T)
plot(dates.ssc,x.ssc$g.l,yaxt="n",xlim=xlimits,xlab="",ylab="",pch=20,cex=2)
axis(side=4)
mtext("SSC g/l",side = 4, line = 3)

#tabla con valores de Q                                                         << ==== NAPO ====== >>>
write.csv(data.frame(date.time, q.cms), file = "q_data_march2016.csv",row.names=FALSE, na="")

###############################################################################################################

#find time of SSC samples #sample1, E1
ftime = date.time
target.time = as.POSIXct("2016-03-06 09:40:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time
index = which(difftime==min(abs(difftime)))
match.data = q.cms [index]
q.cms [index]
date.time[index]
q.data.all[index,] #E1

#find time of SSC samples #sample2, E1
target.time2 = as.POSIXct("2016-03-06 12:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time2
index2 = which(difftime==min(abs(difftime)))
match.data = c(match.data, q.cms[index2])
q.cms [index2]
date.time[index2]
q.data.all[index2,] #E1

#find time of SSC samples #sample3
target.time3 = as.POSIXct("2016-03-06 15:45:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time3
index3 = which(difftime==min(abs(difftime)))
match.data =  c(match.data, q.cms[index3])
q.cms[index3]
obs$date.time[index3]
q.data.all[index3,] #E1

#find time of SSC samples #sample4, E2
target.time4 = as.POSIXct("2016-03-07 06:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time4
index4 = which(difftime==min(abs(difftime)))
match.data =  c(match.data, q.cms[index4])
q.cms [index4]
obs$date.time[index4]
q.data.all[index4,] #E2

#find time of SSC samples #sample5, E2
target.time5 = as.POSIXct("2016-03-07 07:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time5
index5 = which(difftime==min(abs(difftime)))
match.data0 = approx(date.time, q.cms, target.time5) #interpolate the discharge at specified time
match.data = c(match.data, match.data0$y)
q.data.all[index5,] #E2

#find time of SSC samples #sample6, E2
target.time6 = as.POSIXct("2016-03-07 12:00:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time6
match.data0 = approx(date.time, q.cms, target.time6) #interpolate the discharge at specified time
match.data = c(match.data, match.data0$y)

###############################################################################################################

#Summary data for Table 3.1:
match.data  #the discharge at time of SSC collection
ssc.date.time = c(target.time, target.time2, target.time3, target.time4, target.time5, target.time6) #the date.time of SSC collecction
ssc = x.ssc$g.l[8:13]
EMC = mean(ssc)
storm = "Storm 6"
Event = c("E1", "E1","E1","E2","E2","E2")

table.3.1.export = data.frame(cbind(as.character(ssc.date.time), ssc, match.data, Event))
names(table.3.1.export) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")

###############################################################################################################
#Table 3.3 Caculations
#Volume weighted mean (VWM) = sum(C1*Q1, C2*Q2, Cn*Qn) / sum(Q1, Q2, Qn)
VWM.e1 = sum(ssc[1:3]*match.data[1:3])/sum(match.data[1:3]) #3 SSC samples for E1 event
VWM.e2 = sum(ssc[4:6]*match.data[4:6])/sum(match.data[4:6]) #3 SSC samples for E2 event
VWM = c(VWM.e1, mean(ssc[4:6])) #because discharge during E2 samples were zero --> E2 used EMC

#Event mean concentration = mean(SSC)
EMC.e1 = mean(ssc[1:3])
EMC.e2 = mean(ssc[4:6])
EMC = c(EMC.e1, EMC.e2)

#Load = total q (m3) * VWM (g/L) * 1000L/1 m3 * 1e-6 tonne/g 
#samples taken during E2 and E3, do not need E1 data for total.q.m3  
total.q.m3 = total.q.obs.mm/1000*10230000 #convert to m, multiply by 10.23 km2 wtshd area or 10230000 m2
load.g = VWM*total.q.m3*1000 #1000L = 1m3
load.ton = load.g * 1e-6 #1 gram = 1e-6 ton

#for table 3.3:
date = obs.summary[,1] 
event = obs.summary[,5] #second and third event
date.event = paste(date, event, sep=" ")
table.3.3.export = data.frame(cbind(date.event, total.q.obs.mm, total.q.m3, load.ton, VWM, EMC)) 
names(table.3.3.export) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM","EMC")
