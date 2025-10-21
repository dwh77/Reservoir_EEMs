## Isotopes 

#pakcages
library(tidyverse)
library(ggpmisc)
library(viridis)
#
#### naming  -----------------------
# Site_code_number <- data.frame(Site_code = c("CS1", "CS2", "CP1", "CP2", "CC1", "CC2", "CC3", "CC4", "C50"),
#                                Site_number = c(101, 100, 98, 96, 94, 92, 90, 88, 50))
# 
# iso_qaqc <- read_csv("./Water_Isotopes/Isotopes_allruns_data.csv")
# 
# iso_names <- iso_qaqc |> 
#   select(SampleName) |> 
#   mutate(Name = SampleName) |> 
#   # mutate(Name = sapply(Name, add_underscore)) |>  # add underscore between depth and rep
#   separate(Name, into = paste0("part", 1:3), sep = " ", fill = "right") |> #break Name into four columns
#   rename(Site_code = part1,
#          Date = part2,
#          Depth_m = part3) |>  #give columns meaningful names
#   mutate(Depth_m = str_sub(Depth_m, 1, str_length(Depth_m)-1)) |>  #remove m from depth column
#   mutate(Date = dmy(Date),
#          Reservoir = "CCR") |> 
#   left_join(Site_code_number, by = "Site_code") |> 
#   select(Reservoir, Site_code, Site_number, Depth_m, Date, SampleName) 
# 
# #bring names to sample data 
# iso_named <- left_join(iso_names, iso_qaqc, by = c("SampleName")) |> 
#   arrange(Date, Site_number, Depth_m )
#
# #write csv w/ names
#write.csv(iso_named, "./Water_Isotopes/isotopes_named.csv", row.names = F)





#### Look at data -------------
distances <- read.csv("C:/Users/dwh18/Downloads/2024 spatial sampling - Distances.csv") |> 
  mutate(Date = mdy(Date)) |> 
  dplyr::select(-c(13,14)) |> 
  pivot_longer(-c(1,11,12), names_to = "Site_code", values_to = "Distance_ft") |> 
  filter(!Site_code == "C1") 


site_class <- read.csv("C:/Users/dwh18/Downloads/2024 spatial sampling - Sites_Class.csv") |> 
  dplyr::select(-X, -Key) |> 
  mutate(Date = mdy(Date)) |> 
  pivot_longer(-c(1), names_to = "Site_code", values_to = "Site_Class") |> 
  mutate(Site_Class = ifelse(Site_Class %in% c("S", "S "), "Stream", Site_Class),
         Site_Class = ifelse(Site_Class %in% c("P", "P "), "Pool", Site_Class),
         Site_Class = ifelse(Site_Class == "C", "Cove", Site_Class),
         Site_Class = ifelse(Site_Class == "L", "Pelagic", Site_Class))



## ISO data
iso24 <- read.csv("./Water_Isotopes/isotopes_named.csv")

iso_rain <- iso24 |> 
  rename(Site = Site_number) |> 
  filter(Site == 51) |> 
  dplyr::select(Reservoir, Site, Site_code, Depth_m, Date, d18O_VSMOW, d2H_VSMOW) |> 
  pivot_longer(-c(1:5)) 
  

iso <- iso24 |> 
  rename(Site = Site_number) |> 
  filter(Site != 94,
         Site != 51) |> 
  mutate(Date = mdy(Date),
         Site_code = ifelse(Site == 101, "S1", NA),
         Site_code = ifelse(Site == 100, "S2", Site_code),
         Site_code = ifelse(Site == 98, "P1", Site_code),
         Site_code = ifelse(Site == 96, "P2", Site_code),
         Site_code = ifelse(Site == 94, "C1", Site_code),
         Site_code = ifelse(Site == 92, "C2", Site_code),
         Site_code = ifelse(Site == 90, "C3", Site_code),
         Site_code = ifelse(Site == 88, "C4", Site_code),
         Site_code = ifelse(Site == 50, "C50", Site_code)) 


## ISO plotting 
iso_plotting <- left_join(iso, distances, by = c("Site_code", "Date")) |> 
  left_join(site_class, by = c("Site_code", "Date")) |> 
  mutate(Distance = Distance_ft*0.3048,
         Dry_start = Dry_start*0.3048,
         Dry_end = Dry_end*0.3048) |>
  dplyr::select(Reservoir, Site, Site_code, Site_Class, Depth_m, Date, Distance, Dry_start, Dry_end, d18O_VSMOW, d2H_VSMOW)

 #plot across dates
iso_plotting |> 
  pivot_longer(-c(1:9)) |> 
  ggplot(aes(x = Distance, y = value, fill = Depth_m
  ))+
  geom_point(shape = 21, size = 3) + 
  geom_point(data = iso_rain, mapping = aes(x = 1250, y = value, color = Date), size = 2, shape = 17)+
  facet_wrap(~name, scales = "free_y", nrow = 2)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)",  fill = "Depth (m)", color = "Rain")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+  
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15, 20))

#by date
iso_plotting |> 
  pivot_longer(-c(1:9)) |> 
  ggplot(aes(x = Distance, y = value, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + 
  facet_grid(name~Date, scales = "free_y")+
  geom_rect(aes(xmin = Dry_start, xmax = Dry_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.1, fill = "gray", color = NA )+ #color gets rid of border
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)",  fill = "Depth (m)")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+  
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15, 20))



##18O ~ 2H
iso_plotting |> 
  ggplot(aes(x = d18O_VSMOW, y = d2H_VSMOW, fill = as.factor(Site_Class)))+
  geom_point(shape = 21, size = 3) + 
  # stat_poly_line(method = "lm", linewidth = 2, aes(group = 1))+
  # stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
  #              aes(x = d18O_VSMOW, y = d2H_VSMOW, group = 1,
  #                  label=paste(..eq.label.., ..adj.rr.label.., ..p.value.label..,sep="~~~"),size=3))+
  ## add GMWL and equation 
  geom_abline(slope = 8, intercept = 10, color = "black", linetype = "dashed", size = 1.5) +
  # annotate("text", x = min(iso_plotting$d18O_VSMOW), y = -26,
  #          label = "y = 8x + 10", size = 5, color = "black", hjust = 0)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## D excess
iso_plotting |> 
  mutate(D_excess = d2H_VSMOW - 8*d18O_VSMOW) |> 
  ggplot(aes(x =Date, y = D_excess, fill = Depth_m))+
  geom_point(shape = 21, size = 4) + 
  facet_wrap(~Site)+
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15, 20))+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#just 0.1m
iso_plotting |> 
  mutate(Site_code_new = ifelse(Site == 101, "S1", NA),
         Site_code_new = ifelse(Site == 100, "S2", Site_code_new),
         Site_code_new = ifelse(Site == 98, "B1", Site_code_new),
         Site_code_new = ifelse(Site == 96, "B2", Site_code_new),
         Site_code_new = ifelse(Site == 92, "C1", Site_code_new),
         Site_code_new = ifelse(Site == 90, "C2", Site_code_new),
         Site_code_new = ifelse(Site == 88, "P1", Site_code_new),
         Site_code_new = ifelse(Site == 50, "P2", Site_code_new),
  ) |> 
  filter(Depth_m == 0.1) |> 
  mutate(D_excess = d2H_VSMOW - 8*d18O_VSMOW) |> 
  ggplot(aes(x =Date, y = D_excess, color = as.factor(Site_code_new)))+
  geom_point(size = 2) + 
  geom_line(size = 1.3)+
  annotate("text", x = ymd("2024-06-01"), y = 18,
           label = "d excess = dD - 8*d18O", size = 5, color = "black", hjust = 0)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
#2 H just 0.1m
iso_plotting |> 
  mutate(Site_code_new = ifelse(Site == 101, "S1", NA),
         Site_code_new = ifelse(Site == 100, "S2", Site_code_new),
         Site_code_new = ifelse(Site == 98, "B1", Site_code_new),
         Site_code_new = ifelse(Site == 96, "B2", Site_code_new),
         Site_code_new = ifelse(Site == 92, "C1", Site_code_new),
         Site_code_new = ifelse(Site == 90, "C2", Site_code_new),
         Site_code_new = ifelse(Site == 88, "P1", Site_code_new),
         Site_code_new = ifelse(Site == 50, "P2", Site_code_new),
         ) |> 
  filter(Depth_m == 0.1) |> 
  ggplot(aes(x =Date, y = d2H_VSMOW, color = as.factor(Site_code_new)))+
  geom_point(size = 2) + 
  geom_line(size = 1.3)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())



## D excess sites by depth 

iso_levels_sites <- c("S1", "S2", "B1", "B2", "C1", "C2", "P1", "P2")

iso_levels <- c("S1_0.1", "S2_0.1", "B1_0.1", "B1_BOT", "B2_0.1", "B2_BOT", "C1_0.1", "C1_BOT",
                "C2_0.1", "C2_1.5", "C2_BOT",
                "P1_0.1", "P1_1.5", "P1_6", "P1_9", "P1_BOT",
                "P2_0.1", "P2_1.5", "P2_6", "P2_9", "P2_BOT")

iso_levels2 <- c("S1_0.1", "S2_0.1", "B1_0.1",  "B2_0.1",  "C1_0.1",  "C2_0.1", "P1_0.1", "P2_0.1",
                 "C2_1.5", "P1_1.5", "P2_1.5", "B1_BOT",  "B2_BOT", "C1_BOT", "C2_BOT",
                  "P1_6", "P1_9", "P1_BOT","P2_6", "P2_9", "P2_BOT")




iso_plotting |> 
  mutate(D_excess = d2H_VSMOW - 8*d18O_VSMOW) |> 
  #set up naming to make sites by depth and sitee
  mutate(Depth_new = ifelse(Site %in% c(98,96,92) & Depth_m > 0.1, "BOT", Depth_m),
         Depth_new = ifelse(Site == 90 & Depth_m > 1.5, "BOT", Depth_new),
         Depth_new = ifelse(Site %in% c(88,50) & Depth_m > 9, "BOT", Depth_new)
         ) |> 
  mutate(Site_code_new = ifelse(Site == 101, "S1", NA),
         Site_code_new = ifelse(Site == 100, "S2", Site_code_new),
         Site_code_new = ifelse(Site == 98, "B1", Site_code_new),
         Site_code_new = ifelse(Site == 96, "B2", Site_code_new),
         Site_code_new = ifelse(Site == 92, "C1", Site_code_new),
         Site_code_new = ifelse(Site == 90, "C2", Site_code_new),
         Site_code_new = ifelse(Site == 88, "P1", Site_code_new),
         Site_code_new = ifelse(Site == 50, "P2", Site_code_new),
  ) |> 
  mutate(Site_CODE = paste(Site_code_new, Depth_new, sep = "_")) |> 
 #make plot
  filter(Depth_new %in% c("0.1", "BOT")) |> 
  # filter(Site %in% c(101,100,98,96)) |> 
  mutate(TOP_BOT = ifelse(Depth_new %in% c("0.1"), "TOP", "BOT")) |> 
  ggplot(aes(x =Date, y = D_excess, 
             #color = factor(Site_CODE, levels = iso_levels),
             color = factor(Site_code_new, levels = iso_levels_sites),
             linetype = factor(TOP_BOT, levels = c("TOP", "BOT"))
             ))+
  geom_point(size = 3) +  geom_line(size = 1.3)+
  labs(x = "Date", y = "Deuterium excess", color = "Site_Depth", linetype = "Depth")+
  annotate("text", x = ymd("2024-06-01"), y = 18,
           label = "D excess = dD - 8*d18O", size = 5, color = "black", hjust = 0)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+ # Arrange legend in 2 rows, filling by row
  scale_color_viridis_d(option = "H") #a-h are color options



