############################################################################# 
# Supplemental R Code 02
# R code used to estimate quantify C-Q, P-Q trends for O’Donnell & Hotchkiss
# SOURCE: O’Donnell, B. & E.R. Hotchkiss. Coupling Concentration- and Process-Discharge Relationships Integrates Water Chemistry and Metabolism in Streams. Submitted to Water Resources Research, Feb 2019.
# Code by: BO
# Last update: 2019-02-01 by ERH
############################################################################

############################################################################
## [01] LOAD PACKAGES & DATA #####################################

#load packages
library(ggplot2)
library(segmented)
library(lubridate)
library(chron)
library(grid)
library(gridExtra)

#########################

#load data 
# includes quality–checked metabolism estimates from streamMetabolizer
# see “ODonnellHotchkiss_SuppRCode01_Metab” for metab model specs
#datavar <- read.csv("/Users/Supplemental Data_QualityChecked_12.31.18.csv", fileEncoding="UTF-8-BOM")
datavar <- read.csv("C:/Users/dwh18/Downloads/wrcr24261-sup-0002-2019wr025025-ds01 (1).csv")

#########################

#convert time to posix 
datavar$date <- chron::chron(dates=as.character(datavar$date))

##/ END [01] LOAD PACKAGES & DATA #####################################
############################################################################

##################e##########################################################
## [02] ANALYSIS OF WATER TEMP & Q #####################################

## TEMP (C)

# log-transform data
datavar$loggedtemp <- log(datavar$temp.Median)
datavar$loggeddischarge <- log(datavar$discharge.median)

attach(datavar)

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedtemp))+geom_point()

# linear model
linearModelVar <- lm(loggedtemp ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
tempdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 
#prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)
temp.seg <- segmented(linearModelVar, 
                      seg.Z =~loggeddischarge, 
                      psi = medianpsi) 
##segmented seems to do most of the work; there is another similar package called breakpoint
summary(temp.seg)

#obtain breakpoints
temp.seg$psi
tempestbp <-summary.segmented(temp.seg)$psi [1,2]
tempbpstderror <- summary.segmented(temp.seg)$psi [1,3]

#get slopes
slope(temp.seg)

#get fitted data
my.fitted <- fitted(temp.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedtemp=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= temp.seg$psi[2])
low <- subset(datavar, loggeddischarge < temp.seg$psi[2])

# anova
templowaov <-aov(low$loggedtemp ~ low$loggeddischarge, data = low)
summary(templowaov)
temphighaov <-aov(high$loggedtemp ~ high$loggeddischarge, data = high)
summary(temphighaov)

#########################

## Make the CQ plot
color <- rep(0, length(datavar[,1]))

datavar$month <- month(datavar$date)

datavar$season <- NA
datavar$season <- ifelse(datavar$month == 1,"winter" , datavar$season)
datavar$season <- ifelse(datavar$month == 2,"winter" , datavar$season)
datavar$season <- ifelse(datavar$month == 3,"spring" , datavar$season)
datavar$season <- ifelse(datavar$month == 4,"spring" , datavar$season)
datavar$season <- ifelse(datavar$month == 5,"spring" , datavar$season)
datavar$season <- ifelse(datavar$month == 6,"summer" , datavar$season)
datavar$season <- ifelse(datavar$month == 7,"summer" , datavar$season)
datavar$season <- ifelse(datavar$month == 8,"summer" , datavar$season)
datavar$season <- ifelse(datavar$month == 9,"fall" , datavar$season)
datavar$season <- ifelse(datavar$month == 10,"fall" , datavar$season)
datavar$season <- ifelse(datavar$month == 11,"fall" , datavar$season)
datavar$season <- ifelse(datavar$month == 12,"winter" , datavar$season)

color[datavar$month==3] <- "#cccccc"
color[datavar$month==4] <- "#cccccc"
color[datavar$month==5] <- "#cccccc"
color[datavar$month==6] <- "#969696"
color[datavar$month==7] <- "#969696"
color[datavar$month==8] <- "#969696"
color[datavar$month==9] <- "#636363"
color[datavar$month==10] <- "#636363"
color[datavar$month==11] <- "#636363"
color[datavar$month==12] <- "#252525"
color[datavar$month==1] <- "#252525"
color[datavar$month==2] <- "#252525"

# plot it
temppch <- ifelse(datavar[,18]=="winter", 8, ifelse(datavar[,18]=="spring", 17, 
                                                    ifelse(datavar[,18] == "fall", 15,
                                                           ifelse(datavar[,18] == "summer", 16, 1))) )

ggplot(my.model, aes(x=loggeddischarge, y=loggedtemp)) + geom_line()

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedtemp))+
  geom_point(col=color, shape = temppch)

breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedtemp), colour = "red", size = 1.25)

# add vertical lines to indicate break points
tempmy.lines <- temp.seg$psi[, 2]

tempplot <- breakpointplot+geom_vline(xintercept=tempmy.lines,linetype="dashed", col ="red")+ xlab(expression(Discharge~(m^3/s)))+ylab(expression(Temperature~(degree~C)))+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(0, 1, 2, 3), labels =round(c(exp(0), exp(1), exp(2), exp(3)),1))
tempplot

## / END [02] ANALYSIS OF WATER TEMP & Q #####################################
############################################################################

############################################################################
## [03] ANALYSIS OF PH & Q ###################################################

## PH
# log-transform data
# pH is already log-transformed. Discharge has been log-transformed above

#linear model
linearModelVar <- lm(pH.Median ~ loggeddischarge, datavar)
#display linear model
linearModelVar


originalplot <-ggplot(datavar, aes(x=discharge.median,y=pH.Median))+ geom_point() 
originalplot


# Davies test for significant breakpoint
phdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)
ph.seg <- segmented(linearModelVar, 
                    seg.Z =~loggeddischarge, 
                    psi = medianpsi) 
summary(ph.seg)

phestbp <-summary.segmented(ph.seg)$psi [1,2]

phbpstderror <- summary.segmented(ph.seg)$psi [1,3]

#obtain breakpoints
ph.seg$psi

#get slopes
slope(ph.seg)

#get fitted data
my.fitted <- fitted(ph.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, pH.Median=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= ph.seg$psi[2])
low <- subset(datavar, loggeddischarge < ph.seg$psi[2])

# anova
phlowaov <-aov(low$pH.Median ~ low$loggeddischarge, data = low)
summary(phlowaov)

phhighaov <-aov(high$pH.Median ~ high$loggeddischarge, data = high)
summary(phhighaov)


#########################
## Make the CQ plot

#plot it
originalplot <-ggplot(datavar, aes(x=loggeddischarge,y=pH.Median))+geom_point(col=color, shape=temppch) 
ggplot(my.model, aes(x=loggeddischarge, y=pH.Median)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = pH.Median), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
pHmy.lines <- ph.seg$psi[, 2]

phplot <- breakpointplot+geom_vline(xintercept=pHmy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab("pH")+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))
phplot


## / END [03] ANALYSIS OF PH & Q #################################################
############################################################################

############################################################################
## [04] ANALYSIS OF DISSOLVED O2 & Q #############################################

## DISSOLVED OXYGEN (mg/L)
datavar$loggedoxygen <- log(oxygen.Median)


#########################
# log-transform data. Discharge has been log-transformed above

originalplot <-ggplot(datavar, aes(x=discharge.median,y=oxygen.Median))+ geom_point() 
originalplot

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedoxygen))+geom_point()

# linear model
linearModelVar <- lm(loggedoxygen ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
dodavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
do.seg <- segmented(linearModelVar, 
                    seg.Z =~loggeddischarge, 
                    psi = medianpsi) 
summary(do.seg)

#obtain breakpoints
do.seg$psi

doestbp <-summary.segmented(do.seg)$psi [1,2]

dobpstderror <- summary.segmented(do.seg)$psi [1,3]

#get slopes
slope(do.seg)

#get fitted data
my.fitted <- fitted(do.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedoxygen=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= do.seg$psi[2])
low <- subset(datavar, loggeddischarge < do.seg$psi[2])

# anova
dolowaov <-aov(low$loggedoxygen ~ low$loggeddischarge, data = low)
summary(dolowaov)

dohighaov <-aov(high$loggedoxygen ~ high$loggeddischarge, data = high)
summary(dohighaov)


#########################
## Make the CQ plot

#plot it
originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedoxygen))+
  geom_point(col=color, shape = temppch)
ggplot(my.model, aes(x=loggeddischarge, y=loggedoxygen)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedoxygen), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
oxygenmy.lines <- do.seg$psi[, 2]

oxyplot <- breakpointplot+geom_vline(xintercept=oxygenmy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab("Dissolved Oxygen (mg/L)")+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(1.8, 2.0, 2.2, 2.4), labels =round(c(exp(1.8), exp(2.0), exp(2.2), exp(2.4)),1))
oxyplot


## / END [04] ANALYSIS OF DISSOLVED O2 & Q #######################################
############################################################################

############################################################################
## [05] ANALYSIS OF CONDUCTIVITY & Q ############################################

## CONDUCTIVITY
# log-transform data. Discharge has been log-transformed above
datavar$loggedConductivity <- log(Conductivity.Median)


#########################

originalplot <-ggplot(datavar, aes(x=discharge.median,y=Conductivity.Median))+ geom_point() 
originalplot

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedConductivity))+geom_point()

# linear model
linearModelVar <- lm(loggedConductivity ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
conddavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)

cond.seg <- segmented(linearModelVar, 
                      seg.Z =~loggeddischarge, 
                      psi = medianpsi) 
summary(cond.seg)

#obtain breakpoints
cond.seg$psi
condestbp <-summary.segmented(cond.seg)$psi [1,2]

condbpstderror <- summary.segmented(cond.seg)$psi [1,3]

#get slopes
slope(cond.seg)

#get fitted data
my.fitted <- fitted(cond.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedConductivity=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= cond.seg$psi[2])
low <- subset(datavar, loggeddischarge < cond.seg$psi[2])

#anova
condlowaov <-aov(low$loggedConductivity ~ low$loggeddischarge, data = low)
summary(condlowaov)

condhighaov <-aov(high$loggedConductivity ~ high$loggeddischarge, data = high)
summary(condhighaov)


#########################

## Make the CQ plot

#plot it
originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedConductivity))+
  geom_point(col=color, shape = temppch)
ggplot(my.model, aes(x=loggeddischarge, y=loggedConductivity)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedConductivity), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
Conductivitymy.lines <- cond.seg$psi[, 2]

condplot <- breakpointplot+geom_vline(xintercept=Conductivitymy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab("Conductivity (ms/cm)")+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(-1.5, -1.0, -0.5, 0.0), labels =round(c(exp(-1.5), exp(-1.0), exp(-0.5), exp(0.0)),1))
condplot


## / END [05] ANALYSIS OF CONDUCTIVITY & Q #######################################
############################################################################

############################################################################
## [06] ANALYSIS OF TURBIDITY & Q ############################################

## TURBIDITY
# log-transform data. Discharge has been log-transformed above
datavar$loggedturbidity <- log(turbidity.Median)

originalplot <-ggplot(datavar, aes(x=discharge.median,y=turbidity.Median))+ geom_point() 
originalplot

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedturbidity))+geom_point()

# linear model
linearModelVar <- lm(loggedturbidity ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
turbdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)

turb.seg <- segmented(linearModelVar, 
                      seg.Z =~loggeddischarge, 
                      psi = medianpsi) 
summary(turb.seg)

#obtain breakpoints
turb.seg$psi

turbestbp <-summary.segmented(turb.seg)$psi [1,2]

turbbpstderror <- summary.segmented(turb.seg)$psi [1,3]

#get slopes
slope(turb.seg)

#get fitted data
my.fitted <- fitted(turb.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedturbidity=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)

## segment according to the est bp from segmented package
high <- subset(datavar, loggeddischarge >= turb.seg$psi[2])
low <- subset(datavar, loggeddischarge < turb.seg$psi[2])

#anova
turblowaov <-aov(low$loggedturbidity ~ low$loggeddischarge, data = low)
summary(turblowaov)

turbhighaov <-aov(high$loggedturbidity ~ high$loggeddischarge, data = high)
summary(turbhighaov)


#########################

## Make the CQ plot

#plot it
originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedturbidity))+
  geom_point(col=color, shape = temppch)
ggplot(my.model, aes(x=loggeddischarge, y=loggedturbidity)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedturbidity), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
turbiditymy.lines <- turb.seg$psi[, 2]

turbplot <- breakpointplot+geom_vline(xintercept=turbiditymy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab("Turbidity (NTU)")+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position="top")+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels =round(c(exp(0), exp(2), exp(4), exp(6)),1))
turbplot


## / END [06] ANALYSIS OF TURBIDITY & Q #######################################
############################################################################

############################################################################
## [07] MULTI-PANEL PLOT OF ALL C-Q ANALYSES #####################################

## FINAL PLOT
par(mfrow=c(2,3))
par(mar=c(5,7,3.5,1)+.1, mgp = c(3.75, 1.5, 0))
library(cowplot)
grid.arrange(turbplot, tempplot,oxyplot,phplot, condplot, ncol=3)

## /END [07] MULTI-PANEL PLOT OF ALL C-Q ANALYSES ################################
############################################################################

############################################################################
## [08] ANALYSIS OF GPP & Q #####################################

## GPP

# log-transform data. Discharge has been log-transformed above
datavar$loggedGPP<- log(datavar$GPP_mean)

originalplot <-ggplot(datavar, aes(x=discharge.median,y=GPP_mean))+ geom_point() 
originalplot

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedGPP))+geom_point()

# linear model
linearModelVar <- lm(loggedGPP ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
GPPdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#####################################
##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)
gpp.seg <- segmented(linearModelVar, 
                     seg.Z =~loggeddischarge, 
                     psi = medianpsi) 
summary(gpp.seg)

#obtain breakpoints
gpp.seg$psi
GPPestbp <-summary.segmented(gpp.seg)$psi [1,2]
GPPbpstderror <- summary.segmented(gpp.seg)$psi [1,3]

#get slopes
slope(gpp.seg)

#get fitted data
my.fitted <- fitted(gpp.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedGPP=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= gpp.seg$psi[2])
low <- subset(datavar, loggeddischarge < gpp.seg$psi[2])

# anova
gpplowaov <-aov(low$loggedGPP ~ low$loggeddischarge, data = low)
summary(gpplowaov)
gpphighaov <-aov(high$loggedGPP ~ high$loggeddischarge, data = high)
summary(gpphighaov)

################
## Make the PQ plot

#plot it

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedGPP))+
  geom_point(col=color, shape = temppch, size = 4)
ggplot(my.model, aes(x=loggeddischarge, y=loggedGPP)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedGPP), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
GPPmy.lines <- gpp.seg$psi[, 2]

gpp_plot <- breakpointplot+geom_vline(xintercept=GPPmy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab(expression("GPP"~~(g~O[2]~~m^-2~~d^-1)))+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position="top", axis.text=element_text(size=30),
        axis.title = element_text(size=30))+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(-6, -4, -2, 0, 2), labels =round(c(exp(-6), exp(-4), exp(-2), exp(0), exp(2)),2))

gpp_plot

## / END [08] ANALYSIS OF GPP & Q #######################################
############################################################################

############################################################################
## [09] ANALYSIS OF ER & Q #####################################

##  ER

# log-transform data. Discharge has been log-transformed above
datavar$ER <- abs(datavar$ER_mean)
datavar$loggedER<- log(datavar$ER)

originalplot <-ggplot(datavar, aes(x=discharge.median,y=ER))+ geom_point() 
originalplot
originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedER))+geom_point()

#linear model
linearModelVar <- lm(loggedER ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
ERdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#####################################
##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)
er.seg <- segmented(linearModelVar, 
                    seg.Z =~loggeddischarge, 
                    psi = medianpsi) #visual breakpoint estimation from plot above
summary(er.seg)

#obtain breakpoints
er.seg$psi

#get fitted data
my.fitted <- fitted(er.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedER=my.fitted)

#########################
## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= er.seg$psi[2])
low <- subset(datavar, loggeddischarge < er.seg$psi[2])

# anova
erlowaov <-aov(low$loggedER ~ low$loggeddischarge, data = low)
summary(erlowaov)
erhighaov <-aov(high$loggedER ~ high$loggeddischarge, data = high)
summary(erhighaov)

####################################

## Make the PQ plot

# plot it

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y =loggedER))+
  geom_point(col=color, shape = temppch, size = 4)
ggplot(my.model, aes(x=loggeddischarge, y=loggedER)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedER), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
ERmy.lines <- er.seg$psi[, 2]

er_plot <- breakpointplot+geom_vline(xintercept=ERmy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab(expression(paste("|",ER,"|"~(g~O[2]~~m^-2~~d^-1))))+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.text=element_text(size=30),
        axis.title = element_text(size=30))+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(1.0, 1.5, 2.0, 2.5, 3.0), labels =round(c(exp(1.0), exp(1.5), exp(2.0), exp(2.5), exp(3.0)),1))

er_plot

## / END [09] ANALYSIS OF ER & Q #######################################
############################################################################

############################################################################
## [10] ANALYSIS OF NEP & Q #####################################

##NEP

# log-transform data. Discharge has been log-transformed above
datavar$NE <-  datavar$GPP_mean + datavar$ER_mean 
datavar$NEP <- datavar$NE+16 #Transformed by +16 here so can be log-transformed,scale adjusted to reflect untransformed below
datavar$loggedNEP<- log(datavar$NEP)

originalplot <-ggplot(datavar, aes(x=discharge.median,y=NEP))+ geom_point() 
originalplot

originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedNEP))+geom_point()

#linear model
linearModelVar <- lm(loggedNEP ~ loggeddischarge, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
NEPdavies <- davies.test(linearModelVar,seg.Z =~loggeddischarge,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)

#####################################
##BREAKPOINT ANALYSES USING 'SEGMENTED' 

# prior estimates of breakpoints (psi)
medianpsi <- median(datavar$loggeddischarge)
nep.seg <- segmented(linearModelVar, 
                     seg.Z =~loggeddischarge, 
                     psi = medianpsi) #visual breakpoint estimation from plot above
summary(nep.seg)

#obtain breakpoints
nep.seg$psi
NEPestbp <-summary.segmented(nep.seg)$psi [1,2]
NEPbpstderror <- summary.segmented(nep.seg)$psi [1,3]

#get slopes
slope(nep.seg)

#get fitted data
my.fitted <- fitted(nep.seg)
my.model <- data.frame(discharge=datavar$loggeddischarge, loggedNEP=my.fitted)

#########################
## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, loggeddischarge >= nep.seg$psi[2])
low <- subset(datavar, loggeddischarge < nep.seg$psi[2])

# anova
neplowaov <-aov(low$loggedNEP ~ low$loggeddischarge, data = low)
summary(neplowaov)
nephighaov <-aov(high$loggedNEP ~ high$loggeddischarge, data = high)
summary(nephighaov)

#########################
## Make the PQ plot

#plot it
originalplot <- ggplot(datavar, aes(x=loggeddischarge, y= loggedNEP))+
  geom_point(col=color, shape = temppch, size =4)
ggplot(my.model, aes(x=loggeddischarge, y=loggedNEP)) + geom_line()
breakpointplot=originalplot + geom_line(data = my.model, aes(x = loggeddischarge, y = loggedNEP), colour = "red", size = 1.25)#+coord_cartesian((xlim = c(0,100)))

#add vertical lines to indicate break points
NEPmy.lines <- nep.seg$psi[, 2]

nep_plot <- breakpointplot+geom_vline(xintercept=NEPmy.lines,linetype="dashed", col ="red")+
  xlab(expression(Discharge~(m^3/s)))+ylab(expression("NEP"~(g~O[2]~~m^-2~~d^-1)))+
  theme(panel.border =  element_rect(color = "black", fill = NA, size=1),
        panel.background=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position="top", axis.text=element_text(size=30),
        axis.title = element_text(size=30))+
  scale_x_continuous(breaks = c(-3, -2, -1), labels =round(c(exp(-3), exp(-2), exp(-1)),2))+
  scale_y_continuous(breaks = c(0, 1, 2, 3), labels =round(c((exp(0)-16), (exp(1)-16), (exp(2)-16), (exp(3)-16)),1))

nep_plot

## / END [10] ANALYSIS OF NEP & Q #######################################
############################################################################

############################################################################
## [11] PLOTS OF P-Q ANALYSES #####################################

## plot of GPP-Q with turbidity-Q inset
vp <- viewport(width = 0.4, height = 0.4, x = 0.4, y = 0.4)
print(gpp_plot)
print(turbplot, vp = vp)

## plot of ER-Q
er_plot


## plot of NEP-Q with pH-Q inset
vp <- viewport(width = 0.4, height = 0.4, x = 0.77, y = 0.4)
print(nep_plot)
print(phplot, vp = vp)

## / END [11] PLOTS OF P-Q ANALYSES #######################################
############################################################################

