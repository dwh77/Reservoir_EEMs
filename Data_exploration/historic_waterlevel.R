## CCR Historic Hydrology
library(tidyverse)

#### Read in WVWA data ####
wvwa <- read_csv("./Data_exploration/WVWA_data/CCR_water_levels_clean_1987_july2025.csv")

ccr <- wvwa |> 
  pivot_longer(-c(1:2), names_to = "Year", values_to = "Level_ft") |> 
  mutate(Date = WVWA_Date) |> 
  separate(Date, into = paste0("part", 1:2), sep = " ", fill = "right") |> 
  mutate(part2 = substr(part2, 1, 2),
         part2 = str_remove_all(part2, "-"),
         Date_ymd = ymd(paste(Year, part1, part2, sep = "-"))) |> 
  select(WVWA_Date, Date_ymd, Julian_date, Year, Level_ft) |> 
  rename(Date = Date_ymd) 


#### Heatmaps and timeseries of wvwa water level ####
ccr |> 
  filter(Year >= 2015) |> 
  ggplot(aes(x = Julian_date, y = Year, fill = round(Level_ft)))+
  geom_tile()+
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = median(ccr$Level_ft, na.rm = T), guide = "colourbar") +
  # scale_fill_gradient(low="blue", high="red")+
  labs(fill = "Water Level (ft)", 
       y = "Year", x = "Julian Day of Year")

ccr |> 
  filter(Year >= 2015) |> 
  ggplot(aes(x = Julian_date, y = Level_ft, color = Year))+
  geom_line()+
  theme_bw()


ccr |> 
  filter(Year >= 2015) |> 
  ggplot(aes(x = Date, y = Level_ft))+ # color = Year
  geom_line(size = 1.3)+
  geom_rect(aes(xmin = ymd("2024-05-01"), xmax = ymd("2025-04-01"), ymin = -Inf, ymax = Inf), 
            alpha = 0.01, fill = "gray", color = NA )+ #color gets rid of border
  theme_bw()


#### CCR water year starting on May 1st ####

ccr_wateryear <- ccr |> 
  mutate(water_year = if_else(month(Date) >= 5, year(Date), year(Date) - 1)) |> ## fix year to start on may 1; all prior dates go to following year 
  mutate(water_year_Fakedate = ymd(paste(2030, month(Date), day(Date), sep = "-"))) |>  # Dummy year column
  mutate(Days_since_1may = Julian_date - 122,
         Days_since_1may = ifelse(Days_since_1may < 0, Days_since_1may +365, Days_since_1may))

median(ccr_wateryear$Level_ft, na.rm = T)

z <- filter(ccr_wateryear, water_year > 2015, water_year < 2025)
median(z$Level_ft, na.rm = T)
 
ccr_wateryear |>  
  filter(water_year > 2015, water_year <2025) |>
  ggplot(aes(x = Days_since_1may, y = as.factor(water_year), fill = round(Level_ft))) +
  geom_tile() +
  #scale_fill_gradient(low="blue", high="red")+
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = median(ccr_wateryear$Level_ft, na.rm = T), guide = "colourbar")+
  labs(x = "Days since 1 May", y = "Water Year", fill = "Water Level") 


ccr_wateryear |> 
  filter(water_year > 2015, water_year <2025) |>
  ggplot(aes(x = Days_since_1may, y = Level_ft, color = as.factor(water_year)))+
  geom_line()+
  theme_bw()





#### Density plots and stats by Year ####

## density plots of water level by year
ccr_wateryear %>% 
  filter(water_year >= 2015, water_year<2025) |> 
  ggplot(aes(x = Level_ft, color = factor(water_year), fill = factor(water_year)))+
  geom_density(alpha = 0.5)

## yearly stats per water year
yearmean <- ccr_wateryear |> 
  filter(water_year > 1986, water_year < 2025) |> 
  #filter(water_year >= 2015, water_year < 2025) |> 
  group_by(water_year) |> 
  summarise(min = min(Level_ft, na.rm = T),
            median = median(Level_ft, na.rm = T),
            mean = mean(Level_ft, na.rm = T),
            max = max(Level_ft, na.rm = T)
  ) 

summary(yearmean)

yearmean |> filter(water_year == 2024)


## probabilities of yearly stats for 2024
quantile(yearmean$mean, probs = seq(.1, .9, by = .1))


cdf <- ecdf(yearmean$mean)
cdf(1166)
allx <- cdf(yearmean$mean)
allx

yearmean |> 
  # filter(water_year >= 2015) |> 
  ggplot(aes(x = mean))+
  geom_density()+
  geom_point(aes(y = 0.1, color = factor(water_year)))+
  # facet_wrap(~name, scales = "free_y")+
  geom_vline(xintercept = 1166)+
  labs(x = "Water level (ft)", title = "1987-2024 Mean yearly water level")



#### ROA precip ----
#temps are in F; precip is in inches
roa <- read.csv("C:/Users/dwh18/OneDrive/Desktop/CCR_rhessys_data/NOAA_ROA/4043554_NOAA_ROAdaily_1jan1948_2jun2025.csv")

#daily rain ts
roa |> 
  ggplot(aes(x = ymd(DATE), y = PRCP))+
  geom_point()

#Yearly rain by actual year 
roa |> 
  select(DATE, PRCP) |> 
  mutate(Year = year(DATE)) |> 
  group_by(Year) |> 
  summarize(Yearly_Precip_in = sum(PRCP)) |> 
  ggplot(aes(x = Year, y = Yearly_Precip_in))+
  geom_point()+
  geom_line()


## ROA water year
roa_wateryear <- roa |> 
  rename(Date = DATE) |> 
  select(Date, PRCP) |> 
  mutate(Julian_date = yday(Date)) |> 
  mutate(water_year = if_else(month(Date) >= 5, year(Date), year(Date) - 1)) |> ## fix year to start on may 1; all prior dates go to following year 
  mutate(water_year_Fakedate = ymd(paste(2030, month(Date), day(Date), sep = "-"))) |>  # Dummy year column
  mutate(Days_since_1may = Julian_date - 122,
         Days_since_1may = ifelse(Days_since_1may < 0, Days_since_1may +365, Days_since_1may))


roa_wateryear |> 
  filter(water_year > 1947, water_year <2025) |>
  group_by(water_year) |> 
  summarize(Yearly_Precip_in = sum(PRCP)) |> 
  ggplot(aes(x = water_year, y = Yearly_Precip_in))+
  geom_point()+
  geom_line()


roa_wateryear |>  
  #filter(water_year > 2015, water_year <2025) |>
  ggplot(aes(x = Days_since_1may, y = as.factor(water_year), fill = round(PRCP))) +
  geom_tile() +
  #scale_fill_gradient(low="blue", high="red")+
  scale_fill_gradient2(low = "red",  high ="blue",
                       #midpoint = mean(roa_wateryear$PRCP, na.rm = T),
                       guide = "colourbar")+
  labs(x = "Days since 1 May", y = "Water Year", fill = "Daily Precipitation") 










#### OLD ####

############# NOAA ROA ############################
roa1 <- read.csv("./Data_exploration/NOAA_ROA/3958298_NOAA_ROA_20240101_20250310.csv")
roa2 <- read.csv("./Data_exploration/NOAA_ROA/3958385_NOAA_ROA_20140101_20231231.csv")

roa <- rbind(roa1, roa2) |> 
  filter(REPORT_TYPE == "SOD  ") |> 
  dplyr::select(DATE, DailyAverageDryBulbTemperature, DailyPrecipitation) |> 
  filter(!DailyPrecipitation == "Ts") |> 
  mutate(DailyPrecipitation = ifelse(DailyPrecipitation == "T", 100, DailyPrecipitation)) |> 
  mutate(DailyPrecipitation = ifelse(DailyPrecipitation == 100, 0, DailyPrecipitation)) |> 
  mutate(Precip = as.numeric(DailyPrecipitation),
         Temp = as.numeric(DailyAverageDryBulbTemperature))

roa |> select(1,4,5) |> pivot_longer(-1) |> 
  mutate(Date = as.Date(DATE)) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)


roa |> select(1,4,5) |> pivot_longer(-1) |> 
  mutate(Date = as.Date(DATE)) |> 
  mutate(year = year(Date)) |> 
  group_by(year, name) |> 
  summarise(min = min(value, na.rm = T),
            mean = mean(value, na.rm = T),
            median = median(value, na.rm = T),
            max = max(value, na.rm = T)) |> 
  rename(Var = name) |> 
  ggplot(aes(x = year, y = median))+
  geom_point()+
  geom_errorbar(aes(ymin = min, ymax = max))+
  geom_point(aes(y = mean), shape = 2)+
  scale_y_log10()+
  facet_wrap(~Var, scales = "free_y", ncol = 1)





############ WVWA dam water level #########################

# wvwa <- read.csv("C:/Users/dwh18/Downloads/CCR_water_levels_clean_1987_2023.csv")
# 
# ccr_dam_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1069/2/ea78dd541e089687af1f4c4b550bc9ca")
# ccr_dam_git <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/refs/heads/ccre-dam-data-qaqc/ccre-waterquality_L1.csv")
# ccrdam <- rbind(ccr_dam_edi, ccr_dam_git)




dam <- ccrdam |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(dam_depth_m = mean(LvlDepth_m_13, na.rm = T)) |> 
  filter(dam_depth_m > 14) |> 
  mutate(Year = year(Date),
         Julian_date = yday(Date))


wvwa_a <- wvwa |> 
  pivot_longer(-c(1:2), names_to = "Year")

wvwa_a$Year <- substring(wvwa_a$Year, 2) 
wvwa_a$Year <- as.numeric(wvwa_a$Year)
  


z <- full_join(wvwa_a, dam, by = c("Year", "Julian_date"))
head(z)

z |> 
  ggplot(aes(x = dam_depth_m, y = value))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = dam_depth_m, y = value, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()


summary(lm(value~dam_depth_m, data = z))
lm(value~dam_depth_m, data = z)


y <- z |> 
  mutate(Level_lm = (3.376 * dam_depth_m) + 1105.004)

y |> 
  ggplot(aes(x = Level_lm, y = value))+
  geom_point()


x <- y |> 
  rename(WVWA_level = value) |> 
  mutate(Level_ft = ifelse(is.na(WVWA_level), Level_lm, WVWA_level))

days <- seq(4,366, by = 4)
days <- c(days, 361, 362, 363, 365, 366)

days <- unique(wvwa$Julian_date)

#heatmap of waterlevel over years
x %>% 
  filter(Year >= 2015) |> 
  filter(Julian_date %in% c(days)) |> 
  filter(!is.na(Level_ft)) |> 
  ggplot(aes(x = Julian_date, y = Year, fill = round(Level_ft)))+
  geom_tile()+
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = median(x$Level_ft, na.rm = T),  guide = "colourbar") +
  # scale_fill_gradient(low="blue", high="red")+
  labs(fill = "Water Level (ft)", 
       y = "Year", x = "Julian Day of Year")

#density plots of water lvel by year
x %>% 
  # filter(Year > 2010) |> 
  filter(Julian_date %in% c(days)) |> 
  filter(!is.na(Level_ft)) |> 
  ggplot(aes(x = Level_ft, color = factor(Year), fill = factor(Year)))+
  geom_density(alpha = 0.5)



#summarize level by year and make plot 
yearmean <- x |> 
  filter(Year < 2025) |> 
  # filter(Year >= 2015) |> 
  filter(Julian_date %in% c(days)) |> 
  filter(!is.na(Level_ft)) |> 
  group_by(Year) |> 
  summarise(min = min(Level_ft, na.rm = T),
            median = median(Level_ft, na.rm = T),
            mean = mean(Level_ft, na.rm = T),
            max = max(Level_ft, na.rm = T)
            ) 

 summary(yearmean)

 yearmean |> filter(Year == 2024)
 
 quantile(yearmean$mean, probs = seq(.1, .9, by = .1))
 

cdf <- ecdf(yearmean$mean)
cdf(1167)
allx <- cdf(yearmean$mean)
allx
 
yearmean |> 
  # pivot_longer(-1) |> 
  ggplot(aes(x = mean))+
  geom_density()+
  geom_point(aes(y = 0.1, color = factor(Year)))+
  # facet_wrap(~name, scales = "free_y")+
  geom_vline(xintercept = 1167)+
  labs(x = "Water level (ft)", title = "1987-2024 Mean yearly water level")











