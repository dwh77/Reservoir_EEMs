#### Breakpoint and ANOVA across sites 
## DWH editing code from O’Donnell et al. 2019 for breakpoint analysis
## adding own code for kruskall-wallis across sites

## packages 
library(ggpubr) # for ggdensity
library(segmented)
library(tidyverse)

#### set up data ----
chem <- full_join(eems_summary, solubles_summary, by = c("Site", "Date", "Depth_m")) |> 
  full_join(iso_plotting, by = c("Site", "Date", "Depth_m")) |> 
  mutate(Site_Class = ifelse(Site_Class == "Pool", "Backwater", Site_Class)) |> 
  mutate(Site_Type = ifelse(Site %in% c(50,88), "Pelagic", NA),
         Site_Type = ifelse(Site %in% c(90,92), "Cove", Site_Type),
         Site_Type = ifelse(Site %in% c(96,98), "Backwater", Site_Type),
         Site_Type = ifelse(Site %in% c(100,101), "Stream", Site_Type)
  ) 

#make nice versions of each variable over distance plot
doc_clean <- chem |> 
  ggplot(aes(x = Distance, y = DOC_mean, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + #shape = 21,
  # geom_smooth()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)", y = "DOC (mg/L)", fill = "Depth (m)",
       shape = "Site Type")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+ 
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15))

bix_clean <- chem |> 
  ggplot(aes(x = Distance, y = BIX_mean, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + #shape = 21,
  # geom_smooth()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)", y = "BIX", fill = "Depth (m)",
       shape = "Site Type")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+ 
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15))

hix_clean <- chem |> 
  ggplot(aes(x = Distance, y = HIX_mean, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + #shape = 21,
  # geom_smooth()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)", y = "HIX", fill = "Depth (m)",
       shape = "Site Type")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+ 
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15))


d2H_clean <- chem |> 
  ggplot(aes(x = Distance, y = d2H_VSMOW, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + #shape = 21,
  # geom_smooth()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)", y = "d2H (VSMOW)", fill = "Depth (m)",
       shape = "Site Type")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+ 
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15))

#### normality
  ##https://www.sthda.com/english/wiki/normality-test-in-r

ggdensity(chem$BIX_mean, xlab = "BIX", main = "Density plot of BIX in CCR")
shapiro.test(chem$BIX_mean) # p-values < 0.05 means data is not normally distributed
ggdensity(chem$DOC_mean, xlab = "DOC (mg/L)", main = "Density plot of DOC in CCR")
shapiro.test(chem$DOC_mean) 
ggdensity(chem$d2H_VSMOW, xlab = "dH2", main = "Density plot of dH2 in CCR")
shapiro.test(chem$d2H_VSMOW) 


#### Boxplots by sites ####

##how many samples are different site types depending on grouping
#changing sites
chem |>   group_by(Site_Class) |> 
  summarise(Count = n()) 
#fixed
chem |>   group_by(Site_Type) |> 
  summarise(Count = n()) 


#kruskal and dunn test
kruskal_bix <- kruskal.test(BIX_mean ~ Site_Type, data = chem)
kruskal_bix
dunn_bix <- FSA::dunnTest(BIX_mean ~ Site_Type, data = chem)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")
 
left_join(chem, dunn_bix_letters_list, by = c("Site_Type" = "Group")) %>%
  ggplot(aes(x = factor(Site_Type, level = level_order), y = BIX_mean))+   
  geom_boxplot()+ #, outlier.alpha = 0
  #geom_violin()+
  geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 0.94, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Type, label = Letter), y = 0.92, size = 5)+
  theme_bw()

  
#for doc

#kruskal and dunn test
kruskal_bix <- kruskal.test(DOC_mean ~ Site_Type, data = chem)
dunn_bix <- FSA::dunnTest(DOC_mean ~ Site_Type, data = chem)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")

left_join(chem, dunn_bix_letters_list, by = c("Site_Type" = "Group")) %>%
  ggplot(aes(x = factor(Site_Type, level = level_order), y = DOC_mean))+   
  geom_boxplot()+   geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 4.8, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Type, label = Letter), y = 4.6, size = 5)+  theme_bw()


### by site class (where P1, P2, and C2 will change) for proof of concept
kruskal_bix <- kruskal.test(BIX_mean ~ Site_Class, data = chem)
dunn_bix <- FSA::dunnTest(BIX_mean ~ Site_Class, data = chem)
dunn_bix_letters <- dunn_bix$res
dunn_bix_letters_list <- rcompanion::cldList(comparison = dunn_bix_letters$Comparison, p.value = dunn_bix_letters$P.adj, threshold = 0.05)

level_order <- c("Stream", "Backwater", "Cove", "Pelagic")

left_join(chem, dunn_bix_letters_list, by = c("Site_Class" = "Group")) %>%
  ggplot(aes(x = factor(Site_Class, level = level_order), y = BIX_mean))+   
  geom_boxplot()+ #, outlier.alpha = 0
  #geom_violin()+
  geom_jitter(width = 0.2)+
  stat_compare_means(method = "kruskal.test", label.y = 0.94, label.x = 1.3, size = 4)+ 
  geom_text(aes(x = Site_Class, label = Letter), y = 0.92, size = 5)+
  theme_bw()
  


#### DWH breakpoint ----

#### BIX
df <- chem

#fit normal linear regression 
my.lm <- lm(BIX_mean ~ Distance, data = df)
summary(my.lm)

my.coef <- coef(my.lm)


#Run breakpoint
my.seg <- segmented(my.lm,  seg.Z = ~ Distance,  psi = 2 )

summary(my.seg)


my.seg$psi
slope(my.seg)

my.fitted <- fitted(my.seg)

my.model <- data.frame(Distance = df$Distance, BIX_fit = my.fitted)


my.lines <- my.seg$psi[ , 2]

bix_clean+
  #geom_smooth()+
  geom_vline(xintercept = my.lines, linetype = "dashed") +
  geom_line(data = my.model, aes(x = Distance, y = BIX_fit), inherit.aes = FALSE ) #inheret aes fixes error w/ fill setting aesthetic above


#### DWH start from scratch style based on example from https://rpubs.com/MarkusLoew/12164 ####
###this was original DWH breakpoint testing 

#set up data and base plot
df <- chem
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




#### updating examples from Brynn's paper ----


## [01] LOAD PACKAGES & DATA 

#load packages
library(ggplot2)
library(segmented)
library(lubridate)
library(chron)
library(grid)
library(gridExtra)


datavar <- chemjoin |> 
  filter(!is.na(HIX_mean)) 

################HIX_mean

#convert time to posix 
datavar$Date <- as.character(datavar$Date)


## [02] ANALYSIS OF WATER TEMP & Q 


ggplot(datavar, aes(x=Distance, y= HIX_mean))+geom_point()

# linear model
linearModelVar <- lm(HIX_mean ~ Distance, datavar)
#display linear model
linearModelVar

# Davies test for significant breakpoint
tempdavies <- davies.test(linearModelVar,seg.Z =~Distance,k=10,alternative=c("two.sided","less","greater"), values=NULL,dispersion=NULL)
tempdavies


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



# add vertical lines to indicate break points
tempmy.lines <- temp.seg$psi[, 2]


# plot it

ggplot(my.model, aes(x=Distance, y=HIX_mean)) + geom_line()

ggplot(datavar, aes(x=Distance, y= HIX_mean, color = Depth_m))+
  geom_point()+
  geom_line(data = my.model, aes(x = Distance, y = HIX_mean), colour = "red", size = 1.25)+
  geom_vline(xintercept=tempmy.lines,linetype="dashed", col ="red")+
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



