#Figure 3.3 in EPA Events Report
#SSC data from storm 2: 3/1/2015
#Summary data for Table 3.1 and 3.3
#Napoleon Gudino (CICESE) wrote original SSC calculations part of script, Kris Taniguchi (SDSU) reformatted and updated script

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)
evt.script = "figure_2.03_storm2_PT_2015_03_01_KTedits04202017.R" #events report script
evt.script.dir = paste(dir, "/GitHub/", evt.script, sep= "") #path for event script


###############################################################################################################

#Run the events report script for storm 1 (figure_2.03_storm2_PT_2015_03_01_KTedits04202017.R) to get the appropriate Q, stage, etc.
source(evt.script.dir)

#SSC Data --> by Napo
# Load SSC data
fname.sed = "ssc_2014-16.csv"
x.ssc = read.csv(fname.sed)
dates.ssc = strptime(x.ssc$Date,format="%m/%d/%Y, %I:%M:%S %p")

#Dataframe q.data.all is the usable observed data (timeseries of all the good data), PT data
names(q.data.all)
date.time = as.POSIXct(q.data.all$date.time) #date.time from obs data
q.cms = as.numeric(as.character(q.data.all[,2])) #q.data.all[,2] saved as factor, change to numeric


#plot Q timeseries taken from the previous script
par(oma=c(2,2,2,2),mar = c(4, 4, 0, 2))
xlimits = c(as.POSIXct(as.POSIXct("2015-03-01 00:00:00 PST")),as.POSIXct("2015-03-03 00:00:00 PST"))
xtics = seq(xlimits[1],xlimits[2],by=3600*2) #every 2 minutes
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
target.time = as.POSIXct("2015-03-01 07:20:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime = ftime-target.time
index = which(difftime==min(abs(difftime)))
match.data = q.cms[index]
q.cms[(index):(index+10)] 
obs$date.time[index]
q.data.all[index,] #E1

#find time of SSC samples #sample2, E1
target.time2 = as.POSIXct("2015-03-01 12:40:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime2 = ftime-target.time2
index2 = which(difftime2==min(abs(difftime2)))
match.data = c(match.data, q.cms[index2])
q.cms[(index2):(index2+10)] 
date.time[index2]
q.data.all[index2,] #E1

#find time of SSC samples #sample3, E1
target.time3 = as.POSIXct("2015-03-01 17:45:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime3 = ftime-target.time3
index3 = which(difftime3==min(abs(difftime3)))
match.data = c(match.data, q.cms[index3])
q.cms[(index3):(index3+10)] 
date.time[index3]
q.data.all[index3,] #E1

#find time of SSC samples #sample4, E2
target.time4 = as.POSIXct("2015-03-02 08:40:00",format="%Y-%m-%d %H:%M:%S")  # date and time # of the SSC sample
difftime4 = ftime-target.time4
index4 = which(difftime4==min(abs(difftime4)))
match.data = c(match.data, q.cms[index4])
q.cms[(index4):(index4+10)] 
date.time[index4]
q.data.all[index4,] #E2

###############################################################################################################

#Summary data for Table 3.1:
match.data #the discharge at time of SSC collection
ssc.date.time = c(target.time, target.time2, target.time3, target.time4)  #the date.time of SSC collecction
ssc = x.ssc$g.l[3:6]
EMC = mean(ssc)
storm = "Storm 2"
Event = c("E1", "E1", "E1","E2") 

table.3.1.export = data.frame(cbind(as.character(ssc.date.time), ssc, match.data, Event))
names(table.3.1.export) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")

###############################################################################################################
#Table 3.3 Caculations
#Volume weighted mean (VWM) = sum(C1*Q1, C2*Q2, Cn*Qn) / sum(Q1, Q2, Qn)
VWM.e1 = sum(ssc[1:3]*match.data[1:3])/sum(match.data[1:3]) #3 SSC samples for E1 event
VWM.e2 = ssc[4]*match.data[4]/match.data[4] #in this case, only 1 SSC ssample for E2, VWM= SSC
VWM = c(VWM.e1, VWM.e2)

#Event mean concentration (EMC) = mean(ssc) if only one sample for that event, EMC = SSC
EMC.e1 = mean(ssc[1:3])
EMC.e2 = ssc[4] #only one sample taken for E2
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
names(table.3.3.export) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")
