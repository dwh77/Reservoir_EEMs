############################################################################# 
# Supplemental R Code 02
# R code used to estimate quantify C-Q, P-Q trends for O’Donnell & Hotchkiss
# SOURCE: O’Donnell, B. & E.R. Hotchkiss. Coupling Concentration- and Process-Discharge Relationships Integrates Water Chemistry and Metabolism in Streams. Submitted to Water Resources Research, Feb 2019.
# Code by: BO
# Last update: 2019-02-01 by ERH
############################################################################

## [01] LOAD PACKAGES & DATA #####################################

#load packages
library(ggplot2)
library(segmented)
library(lubridate)
library(chron)
library(grid)
library(gridExtra)


datavar <- chemjoin |> 
  filter(!is.na(HIX_mean)) 

################HIX_mean#########################

#convert time to posix 
datavar$Date <- as.character(datavar$Date)


## [02] ANALYSIS OF WATER TEMP & Q #####################################


ggplot(datavar, aes(x=Distance, y= HIX_mean))+geom_point()

# linear model
linearModelVar <- lm(HIX_mean ~ Distance, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
tempdavies <- davies.test(linearModelVar,seg.Z =~Distance,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)
tempdavies

#########################

##BREAKPOINT ANALYSES USING 'SEGMENTED' 
#prior estimates of breakpoints (psi)
temp.seg <- segmented(linearModelVar, 
                      seg.Z =~Distance, 
                      npsi = 1) ##segmented seems to do most of the work; there is another similar package called breakpoint
summary(temp.seg)

#obtain breakpoints
temp.seg$psi
tempestbp <-summary.segmented(temp.seg)$psi [1,2]
tempbpstderror <- summary.segmented(temp.seg)$psi [1,3]

#get slopes
slope(temp.seg)

#get fitted data
my.fitted <- fitted(temp.seg)
my.model <- data.frame(Distance=datavar$Distance, HIX_mean=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, Distance >= temp.seg$psi[2])
low <- subset(datavar, Distance < temp.seg$psi[2])

# anova
templowaov <-aov(low$HIX_mean ~ low$Distance, data = low)
summary(templowaov)
temphighaov <-aov(low$HIX_mean ~ low$Distance, data = high)
summary(temphighaov)

#########################

# add vertical lines to indicate break points
tempmy.lines <- temp.seg$psi[, 2]


# plot it

ggplot(my.model, aes(x=Distance, y=HIX_mean)) + geom_line()

ggplot(datavar, aes(x=Distance, y= HIX_mean, color = Depth_m))+
  geom_point()+
  geom_line(data = my.model, aes(x = Distance, y = HIX_mean), colour = "red", size = 1.25)+
  geom_vline(xintercept=tempmy.lines,linetype="dashed", col ="red")+
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



################  DOC   #########################

## [02] ANALYSIS OF WATER TEMP & Q #####################################


ggplot(datavar, aes(x=Distance, y= DOC_mean))+geom_point()

# linear model
linearModelVar <- lm(DOC_mean ~ Distance, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
tempdavies <- davies.test(linearModelVar,seg.Z =~Distance,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)
tempdavies

##### segmented ####
temp.seg <- segmented(linearModelVar, 
                      seg.Z =~Distance, 
                      npsi = 1,
                      psi = median(datavar$Distance, na.rm = T)) ##segmented seems to do most of the work; there is another similar package called breakpoint
summary(temp.seg)

#obtain breakpoints
temp.seg$psi
tempestbp <-summary.segmented(temp.seg)$psi [1,2]
tempbpstderror <- summary.segmented(temp.seg)$psi [1,3]

#get slopes
slope(temp.seg)

#get fitted data
my.fitted <- fitted(temp.seg)
my.model <- data.frame(Distance=datavar$Distance, DOC_mean=my.fitted)

#########################

## anova test of independence to see if the slope = 0 (i.e., chemostasis)
## segment according to the est bp from segmented package

# subset above/below breakpoint
high <- subset(datavar, Distance >= temp.seg$psi[2])
low <- subset(datavar, Distance < temp.seg$psi[2])

# anova
templowaov <-aov(low$HIX_mean ~ low$Distance, data = low)
summary(templowaov)
temphighaov <-aov(low$HIX_mean ~ low$Distance, data = high)
summary(temphighaov)

#########################

# add vertical lines to indicate break points
tempmy.lines <- temp.seg$psi[, 2]


# plot it

ggplot(my.model, aes(x=Distance, y=DOC_mean)) + geom_line()

ggplot(datavar, aes(x=Distance, y= DOC_mean, color = Depth_m))+
  geom_point()+
  geom_line(data = my.model, aes(x = Distance, y = DOC_mean), colour = "red", size = 1.25)+
  geom_vline(xintercept=tempmy.lines,linetype="dashed", col ="red")+
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



