#Figure 3.9 in EPA Events Report, SSC plots
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#2 Panel plot of EMC vs. total Q and Load vs. total Q

dir = "F:/TJ/R/TJ/events_report/Napo_PT_Script_data_used_in_script_02232017" #update this directory
setwd(dir)

###############################################################################################################
#Read in the tables generated from Table2.1_cal_generate.R and all of the events scripts
table.3.3.ssc = read.csv("summary_table.3.3.csv") #table with both IBWC and PT data

#set value columns to numeric, create new dataframe to run regression
table.3.3.ssc[,2] <- as.numeric(as.character(table.3.3.ssc[,2]))
table.3.3.ssc[,3] <- as.numeric(as.character(table.3.3.ssc[,3]))
table.3.3.ssc[,4] <- as.numeric(as.character(table.3.3.ssc[,4]))
table.3.3.ssc[,5] <- as.numeric(as.character(table.3.3.ssc[,5]))
table.3.3.ssc[,6] <- as.numeric(as.character(table.3.3.ssc[,6]))
  names(table.3.3.ssc) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")  #set column names       

###############################################################################################################
#Regression equation for Q-Load relationship
q.load.model = lm(log10(load.ton) ~ log10(total.q.mm), table.3.3.ssc) #log-log
summary(q.load.model)
b = as.numeric(q.load.model$coefficients[1]) #y-int
a = as.numeric(q.load.model$coefficients[2]) #slope
x.num = c(0.1, 50)
y.num =  (10^b)*(x.num^a)         #y = 10^b*x^a , y = 51.47*x^1.52
reg.q.load = data.frame(cbind(x.num, y.num))

#Regression equation for Q-Event mean ssc (EMC) relationship
q.ssc.model = lm(log10(EMC) ~ log10(total.q.mm), table.3.3.ssc) #log-log
summary(q.ssc.model)
b = as.numeric(q.ssc.model$coefficients[1]) #y-int
a = as.numeric(q.ssc.model$coefficients[2]) #slope
x.num = c(0.1, 50)
y.num =  (10^b)*(x.num^a)         #y = 10^b*x^a , y = 4.77*x^0.45
reg.q.EMC = data.frame(cbind(x.num, y.num))



###############################################################################################################

#Figure 3.7
#2 Panel plots for SSC
layout(matrix(1:2, ncol = 1), widths = 2, respect = FALSE)
par(mar = c(4, 4.1, 0, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$EMC), log="xy", pch=16, cex = 1.5, ylab="Event-mean SSC (g/L)", xlab = "Total Q (mm)")
lines(reg.q.EMC$x.num, reg.q.EMC$y.num)
  text.eq1 = expression("y = 4.77"~ x^{0.45}) #equation text to be added to plot
  text(20, 10, text.eq1) #add equation to 
par(mar = c(4, 4.1, 1, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$load.ton), log="xy", pch=16, cex = 1.5,ylab="Suspended Sediment Load (tons)", xlab = "Total Q (mm)")
lines(reg.q.load$x.num, reg.q.load$y.num)
  text.eq2 = expression("y = 51.47"~ x^{1.52}) #equation text to be added to plot
text(20, 2000, text.eq2)

