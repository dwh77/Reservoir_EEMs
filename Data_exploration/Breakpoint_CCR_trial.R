############################################################################# 
# Supplemental R Code 02
# R code used to estimate quantify C-Q, P-Q trends for O’Donnell & Hotchkiss
# SOURCE: O’Donnell, B. & E.R. Hotchkiss. Coupling Concentration- and Process-Discharge Relationships Integrates Water Chemistry and Metabolism in Streams. Submitted to Water Resources Research, Feb 2019.
# Code by: BO
# Last update: 2019-02-01 by ERH
############################################################################
library(ggpubr)

#### DWH just looking at boxplots by site ####

#normality: https://www.sthda.com/english/wiki/normality-test-in-r
ggdensity(eems_summary$BIX_mean, xlab = "BIX", main = "Density plot of BIX in CCR")
shapiro.test(eems_summary$BIX_mean) # p-values < 0.05 means data is not normally distributed
ggdensity(solubles_summary$DOC_mean, xlab = "DOC (mg/L)", main = "Density plot of DOC in CCR")
shapiro.test(solubles_summary$DOC_mean) # p-values < 0.05 means data is not normally distributed

eemboxplot <- eems_summary |> 
  # filter(Depth_m != 1.5,
  #        Depth_m != 9) |> 
  mutate(Site_Class = ifelse(Site_Class == "Pool", "Backwater", Site_Class)) |> 
  mutate(Site_Type = ifelse(Site %in% c(50,88), "Pelagic", NA),
         Site_Type = ifelse(Site %in% c(90,92), "Cove", Site_Type),
         Site_Type = ifelse(Site %in% c(96,98), "Backwater", Site_Type),
         Site_Type = ifelse(Site %in% c(100,101), "Stream", Site_Type)
  ) 

eemboxplot |>   group_by(Site_Class) %>%
  summarise(Count = n()) 

eemboxplot |>   group_by(Site_Type) %>%
  summarise(Count = n()) 


#kruskal and dunn test
kruskal_bix <- kruskal.test(BIX_mean ~ Site_Type, data = eemboxplot)
kruskal_bix
dunn_bix <- FSA::dunnTest(BIX_mean ~ Site_Type, data = eemboxplot)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")
 
left_join(eemboxplot, dunn_bix_letters_list, by = c("Site_Type" = "Group")) %>%
  ggplot(aes(x = factor(Site_Type, level = level_order), y = BIX_mean))+   
  geom_boxplot()+ #, outlier.alpha = 0
  #geom_violin()+
  geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 0.94, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Type, label = Letter), y = 0.92, size = 5)+
  theme_bw()

  
#for doc
docboxplot <- solubles_summary |> 
  mutate(Site_Type = ifelse(Site %in% c(50,88), "Pelagic", NA),  Site_Type = ifelse(Site %in% c(90,92), "Cove", Site_Type),
         Site_Type = ifelse(Site %in% c(96,98), "Backwater", Site_Type), Site_Type = ifelse(Site %in% c(100,101), "Stream", Site_Type)) 

#kruskal and dunn test
kruskal_bix <- kruskal.test(DOC_mean ~ Site_Type, data = docboxplot)
dunn_bix <- FSA::dunnTest(DOC_mean ~ Site_Type, data = docboxplot)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")

left_join(docboxplot, dunn_bix_letters_list, by = c("Site_Type" = "Group")) %>%
  ggplot(aes(x = factor(Site_Type, level = level_order), y = DOC_mean))+   
  geom_boxplot()+   geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 4.8, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Type, label = Letter), y = 4.6, size = 5)+  theme_bw()


### by site class (where P1, P2, and C2 will change) for proof of concept
kruskal_bix <- kruskal.test(BIX_mean ~ Site_Class, data = eemboxplot)
dunn_bix <- FSA::dunnTest(BIX_mean ~ Site_Class, data = eemboxplot)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")

left_join(eemboxplot, dunn_bix_letters_list, by = c("Site_Class" = "Group")) %>%
  ggplot(aes(x = factor(Site_Class, level = level_order), y = BIX_mean))+   
  geom_boxplot()+ #, outlier.alpha = 0
  #geom_violin()+
  geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 0.94, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Class, label = Letter), y = 0.92, size = 5)+
  theme_bw()
  


#### DWH start from scratch style based on example from https://rpubs.com/MarkusLoew/12164 ####
library(segmented)
library(tidyverse)

#set up data and base plot
df <- eems_summary

#df <- df |> filter(Depth_m == 0.1)

p <- ggplot(df, aes(x = Distance, y = BIX_mean))+
  geom_point()

p

#fit normal linear regression 
my.lm <- lm(BIX_mean ~ Distance, data = df)
summary(my.lm)

my.coef <- coef(my.lm)

p + geom_abline(intercept = my.coef[1], 
                slope = my.coef[2], 
                aes(colour = "overall"))


#Run breakpoint
my.seg <- segmented(my.lm,
                    seg.Z = ~ Distance,
                    psi = 2 #using 1,2,3,4 gives just one break for BIX; using the line below gives multiple numbers
                    #psi = list(Distance = c(200, 500)) #This will force breaks for number of options ie. run c(100,250,500,750) instead
                    )

summary(my.seg)


my.seg$psi
slope(my.seg)

my.fitted <- fitted(my.seg)

my.model <- data.frame(Distance = df$Distance, BIX_fit = my.fitted)

ggplot(my.model, aes(x = Distance, y = BIX_fit))+
  geom_line()

my.lines <- my.seg$psi[ , 2]

p+geom_line(data = my.model, aes(x = Distance, y = BIX_fit), color = "red" )+
  #geom_smooth()+
  geom_vline(xintercept = my.lines, linetype = "dashed")



### DWH fitting actual regression lines instead of model fit as red lines
my.slopes <- coef(my.seg)

b0 <- coef(my.seg)[[1]]
b1 <- coef(my.seg)[[2]]

# Important:
# the coefficients are the differences in slope in comparison to the previous slope
c1 <- coef(my.seg)[[2]] + coef(my.seg)[[3]]
break1 <- my.seg$psi[[3]]

#Solve for c0 (intercept of second segment):
c0 <- b0 + b1 * break1 - c1 * break1


p+  geom_vline(xintercept = my.lines, linetype = "dashed")+ #breakpoint
  geom_abline(intercept = b0, slope = b1, 
              aes(colour = "first part"), show_guide = TRUE)+ #line before first break
  geom_abline(intercept = c0, slope = c1, 
              aes(colour = "second part"), show_guide = TRUE)



### isotopes
df <- iso_plotting

p <- ggplot(df, aes(x = Distance, y = d18O_VSMOW))+
  geom_point()
p

#fit normal linear regression 
my.lm <- lm(d18O_VSMOW ~ Distance, data = df)
summary(my.lm)

my.coef <- coef(my.lm)

p + geom_abline(intercept = my.coef[1], 
                slope = my.coef[2], 
                aes(colour = "overall"))


#Run breakpoint
my.seg <- segmented(my.lm,  seg.Z = ~ Distance,       psi = 2 )

my.seg$psi

my.fitted <- fitted(my.seg)
my.model <- data.frame(Distance = df$Distance, Iso_fit = my.fitted)

ggplot(my.model, aes(x = Distance, y = Iso_fit))+
  geom_line()

my.lines <- my.seg$psi[ , 2]

p+geom_line(data = my.model, aes(x = Distance, y = Iso_fit), color = "red" )+
  #geom_smooth()+
  geom_vline(xintercept = my.lines, linetype = "dashed")




#### updating examples from Brynn's paper ####


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



