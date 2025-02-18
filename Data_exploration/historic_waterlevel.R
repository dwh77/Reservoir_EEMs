## CCR water level exploration
library(tidyverse)
library(ggpmisc) #stat poly line


# wvwa <- read.csv("C:/Users/dwh18/Downloads/CCR_water_levels_clean_1987_2023.csv")
# 
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











