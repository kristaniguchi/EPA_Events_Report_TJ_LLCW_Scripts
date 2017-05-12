#To generate Table 2.2 and 2.3 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#table generated from each event script, pull summary data from each script and put into table
#table1 contains both ibwc and PT summary data, table2 contains the usable observed data (A ratings)

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################

#list files in the directory
script.dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017/GitHub/"
flist = list.files(script.dir)

###############################################################################################################

#Loop to run each R events script and pull out the appropriate tables: obs.summary.PT.IBWC and obs.summary

summary.out.PT.IBWC = data.frame(matrix(nrow=1, ncol=8)) #empty dataframe to put the values in from obs.summary.PT.IBWC
  names(summary.out.PT.IBWC) <- c("date", "total.precip.mm", "PT.peak.q.obs.cms", "IBWC.peak.q.obs.cms", "PT.total.q.obs.mm", "IBWC.total.q.obs.mm", "event", "source")
summary.out.gooddataonly =  data.frame(matrix(nrow=1, ncol=6))
  names(summary.out.gooddataonly) <- c("date", "total.precip.mm", "peak.q.obs.cms", "total.q.obs.mm", "event", "source")

for (i in 1:length(flist)) {
  source(paste(script.dir,flist[i], sep = ""))
  #append the obs.summary.PT.IBWC to the summary.out dataframe from each storm
  summary.out.PT.IBWC = rbind(summary.out.PT.IBWC, obs.summary.PT.IBWC)
  summary.out.gooddataonly = rbind(summary.out.gooddataonly, obs.summary) #only the usable data (A rating)
}

###############################################################################################################

#calculate runoff coeff for IBWC and PT datasets
runoff.coeff.ibwc = as.numeric(summary.out.PT.IBWC$IBWC.total.q.obs.mm)/as.numeric(summary.out.PT.IBWC$total.precip.mm)
runoff.coeff.PT = as.numeric(summary.out.PT.IBWC$PT.total.q.obs.mm)/as.numeric(summary.out.PT.IBWC$total.precip.mm)
#calculate the runoff coeff for the good data (usable observed A rating data)
runoff.coeff.obs = as.numeric(summary.out.gooddataonly$total.q.obs.mm)/as.numeric(summary.out.gooddataonly$total.precip.mm)

###############################################################################################################

#New summary tables
summary.PT.IBWC = cbind(summary.out.PT.IBWC[2:length(summary.out.PT.IBWC$date),], runoff.coeff.PT[2:length(runoff.coeff.PT)], runoff.coeff.ibwc[2:length(runoff.coeff.ibwc)])
  names(summary.PT.IBWC) <- c("date", "total.precip.mm", "PT.peak.q.obs.cms", "IBWC.peak.q.obs.cms", "PT.total.q.obs.mm", "IBWC.total.q.obs.mm", "event", "source","runoff.coeff.PT","runoff.coeff.ibwc")
summary.obs.data = cbind(summary.out.gooddataonly[2:length(summary.out.gooddataonly$date),], runoff.coeff.obs[2:length(runoff.coeff.obs)])
  names(summary.obs.data) <- c("date", "total.precip.mm", "peak.q.obs.cms", "total.q.obs.mm", "event", "source", "runoff.coeff")

###############################################################################################################

#Export new tables into .csv
fname.pt.ibwc = "summary_table_PT_IBWC_allevents.csv"
write.csv(summary.PT.IBWC, file=fname.pt.ibwc, row.names=F)

fname.obs = "summary_table_obs_allevents.csv"
write.csv(summary.obs.data , file=fname.obs, row.names=F)

#see table.2.1_cal_format.R for html formatting of table in report