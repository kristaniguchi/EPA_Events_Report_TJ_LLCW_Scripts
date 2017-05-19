#To generate Table 2.2 and 2.3 in EPA Events Report
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#table generated from each event script, pull summary data from each script and put into table
#table1 contains both ibwc and PT summary data, table2 contains the usable observed data (A ratings)

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder
script.dir= '../EPA_Events_Report_TJ_LLCW_Scripts/' #directory where scripts are saved

###############################################################################################################

#list all the storm event files except       "figure_2.17_storm7_PT_2016_04_KTedits04172017.R" (no observations)
flist = c("figure_2.02_storm1_PT_2014_03_01_KTedits04172017.R", 
      "figure_2.07_storm2_PT_2015_03_01_KTedits04202017.R", 
      "figure_2.09_storm3_PT_2015_05_15_KTedits04202017.R",
      "figure_2.11_storm4_PT_2015_09_15_KTedits04202017.R" ,        
      "figure_2.13_storm5_PT_2016_01_04_KTedits04172017.R" ,
      "figure_2.15_storm6_PT_2016_03b_KTedits04172017.R" ,          
      "figure_2.18_storm8_IBWC_Visual_2017_01_KTedits04172017.R",  
      "figure_2.19_storm9_IBWC_visual_2017_02_KTedits05012017.R",
      "figure_2.20_storm10_IBWC_visual_2017_0227_KTedits05012017.R")

###############################################################################################################

#Loop to run each R events script and pull out the summary tables: obs.summary.PT.IBWC and obs.summary

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