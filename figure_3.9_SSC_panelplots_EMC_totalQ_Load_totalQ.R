#Figure 3.9 in EPA Events Report, SSC plots
#Script written by Kris Taniguchi, SDSU (kristaniguchi@gmail.com)
#3 Panel plot of raw SSC vs. instantaneous Q (from table 3.1), EMC vs. total Q and Load vs. total Q (from table 3.3)

#Set working directory to the data folder, script directory will be used if sourcing functions
getwd() #the directory where the script is saved
setwd('../EPA_Events_Report_TJ_LLCW_Data') #set working directory as the data folder, which is one folder back in it's own folder

###############################################################################################################
#Read in the tables generated from Table 3.1 (for first plot)
table.3.1.ssc.all = read.csv("summary_table.3.1.csv") #table with both IBWC and PT data
#exclude all rows that have EMC
table.3.1.ssc = table.3.1.ssc.all[-c(3,8,10,17,21,23),]

#set value columns to numeric, create new dataframe to run regression
table.3.1.ssc[,2] <- as.numeric(as.character(table.3.1.ssc[,2]))
table.3.1.ssc[,3] <- as.numeric(as.character(table.3.1.ssc[,3]))
names(table.3.1.ssc) <- c("Date","SSC.g.L", "Q.cms", "Event")  #set column names       

#Read in the tables generated from Table 3.3 (for second and third plot)
table.3.3.ssc = read.csv("summary_table.3.3.csv") #table with both IBWC and PT data

#set value columns to numeric, create new dataframe to run regression
table.3.3.ssc[,2] <- as.numeric(as.character(table.3.3.ssc[,2]))
table.3.3.ssc[,3] <- as.numeric(as.character(table.3.3.ssc[,3]))
table.3.3.ssc[,4] <- as.numeric(as.character(table.3.3.ssc[,4]))
table.3.3.ssc[,5] <- as.numeric(as.character(table.3.3.ssc[,5]))
table.3.3.ssc[,6] <- as.numeric(as.character(table.3.3.ssc[,6]))
  names(table.3.3.ssc) <- c("event.date", "total.q.mm", "total.q.m3", "load.ton", "VWM", "EMC")  #set column names       

###############################################################################################################
#Regression equation for raw ssc and instantaneous q relationship
rawq.ssc.model = lm(SSC.g.L ~ Q.cms, table.3.1.ssc) 
summary(rawq.ssc.model)
b = as.numeric(rawq.ssc.model$coefficients[1]) #y-int
a = as.numeric(rawq.ssc.model$coefficients[2]) #slope
x.num = c(0.1, 50)
y.num =  a*x.num+b         
reg.q.rawSSC = data.frame(cbind(x.num, y.num))
  
#Regression equation for Q-Event mean ssc (EMC) relationship
q.ssc.model = lm(log10(EMC) ~ log10(total.q.mm), table.3.3.ssc) #log-log
summary(q.ssc.model)
b = as.numeric(q.ssc.model$coefficients[1]) #y-int
a = as.numeric(q.ssc.model$coefficients[2]) #slope
x.num = c(0.1, 50)
y.num =  (10^b)*(x.num^a)         #y = 10^b*x^a , y = 4.77*x^0.45
reg.q.EMC = data.frame(cbind(x.num, y.num))
  
#Regression equation for Q-Load relationship
q.load.model = lm(log10(load.ton) ~ log10(total.q.mm), table.3.3.ssc) #log-log
summary(q.load.model)
b = as.numeric(q.load.model$coefficients[1]) #y-int
a = as.numeric(q.load.model$coefficients[2]) #slope
x.num = c(0.1, 50)
y.num =  (10^b)*(x.num^a)         #y = 10^b*x^a , y = 51.47*x^1.52
reg.q.load = data.frame(cbind(x.num, y.num))

###############################################################################################################

#Figure 3.7
#3 Panel plots for SSC
layout(matrix(1:3, ncol = 1), widths = 2, respect = FALSE)
par(mar = c(4, 4.1, 0, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.1.ssc$Q.cms), as.numeric(table.3.1.ssc$SSC.g.L),  pch=16, cex = 1.5, ylab="Grab Sample SSC (g/L)", xlab = "Instantaneous Q (cms)")
par(mar = c(4, 4.1, 1, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$EMC), log="xy", pch=16, cex = 1.5, ylab="Event-mean SSC (g/L)", xlab = "Total Q (mm)")
lines(reg.q.EMC$x.num, reg.q.EMC$y.num)
  text.eq1 = expression("y = 4.77"~ x^{0.45}) #equation text to be added to plot
  text(20, 10, text.eq1) #add equation to 
par(mar = c(4, 4.1, 1, 0.1)) #set margins for bottom, L, top, R
plot(as.numeric(table.3.3.ssc$total.q.mm), as.numeric(table.3.3.ssc$load.ton), log="xy", pch=16, cex = 1.5,ylab="Suspended Sediment Load (tons)", xlab = "Total Q (mm)")
lines(reg.q.load$x.num, reg.q.load$y.num)
  text.eq2 = expression("y = 51.47"~ x^{1.52}) #equation text to be added to plot
text(20, 2000, text.eq2)

