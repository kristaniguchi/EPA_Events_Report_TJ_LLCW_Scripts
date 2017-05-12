#To generate Table 3.1 and 3.3 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#table generated from each SSC script, pull summary data from each script and put into table

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################

#list files in the directory
script.dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017/GitHub_SSC/"
flist = list.files(script.dir)

###############################################################################################################

#Loop to run each R events script and pull out the appropriate tables: obs.summary.PT.IBWC and obs.summary

table.3.1.out = data.frame(matrix(nrow=1, ncol=4)) #empty dataframe to put the values in from obs.summary.PT.IBWC
names(table.3.1.out) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
table.3.3.out =  data.frame(matrix(nrow=1, ncol=6))
names(table.3.3.out) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")
Storm.out = NA

for (i in 1:length(flist)) {
  source(paste(script.dir,flist[i], sep = ""))
  #append the obs.summary.PT.IBWC to the summary.out dataframe from each storm
  EMC.out = c("EMC", EMC, "","")
  table.3.1.out = rbind(table.3.1.out, table.3.1.export, EMC.out)
  table.3.3.out = rbind(table.3.3.out, table.3.3.export) #only the usable data (A rating)
  
  Storm.out = c(Storm.out, storm)
}

###############################################################################################################

#New summary tables
table.3.1 = table.3.1.out[2:length(table.3.1.out$Date),]
names(table.3.1) <- c("Date", "SSC (g/L)", "Q (cms)", "Event")
table.3.3 = table.3.3.out[2:length(table.3.3.out$event.date),]
names(table.3.3) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")

Storm.out = Storm.out[2:length(Storm.out)]

###############################################################################################################

#Export new tables into .csv
fname.3.1 = "summary_table.3.1.csv"
write.csv(table.3.1, file=fname.3.1, row.names=F)

fname.3.3 = "summary_table.3.3.csv"
write.csv(table.3.3 , file=fname.3.3, row.names=F)

#see table.3.1_cal_format.R for html formatting of table in report