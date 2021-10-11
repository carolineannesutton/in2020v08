

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readr)
library(dplyr)

catchcomp <- read.csv("Catch_Comp.csv") %>%
   filter(Depth_Stratum== "Epi" | Depth_Stratum== "Meso" | Depth_Stratum== "Lmeso")
catchcomp$Depth_Stratum <- factor(as.factor(catchcomp$Depth_Stratum), levels=c("Lmeso","Meso","Epi"))

catchcomp <- catchcomp %>% 
  dplyr::mutate(Species_facet = case_when(Taxa_PLAOS == "Crustacean" ~ "Crustacean",
                                       Taxa_PLAOS == "Cephalopod" ~ "Cephalopod",
                                       Taxa_PLAOS == "Chaetognath" ~ "Gelatinous",
                                       Taxa_PLAOS == "F" ~ "Fish",
                                       Taxa_PLAOS == "Krill" ~ "Crustacean",
                                       Taxa_PLAOS == "Medusa" ~ "Gelatinous",
                                       Taxa_PLAOS == "Siphonophore" ~ "Gelatinous", 
                                       Taxa_PLAOS == "Pyrosome" ~ "Gelatinous",
                                       Taxa_PLAOS == "Salp" ~ "Gelatinous",
                                       Taxa_PLAOS == "FGB" ~ "Fish",
                                       Taxa_PLAOS == "Prawn" ~ "Crustacean",
                                       Taxa_PLAOS == "Other" ~ "Other",
                                       Taxa_PLAOS == "Ctenophore" ~ "Gelatinous",
                                       Taxa_PLAOS == "FnGB" ~ "Fish",
                                       Taxa_PLAOS == "Siph_physonect" ~ "Gelatinous"))


#calculate standard deviation?

# catchcomp20 <- catchcomp20 %>% 
#   group_by(Taxa_Groups) %>% 
#   mutate(sd= sd(Weight_Std_gm3_gps)) %>% 
#   ungroup()

day_values <- catchcomp %>% 
  filter(daylight== "Day")

night_values <- catchcomp %>% 
  filter(daylight== "Night")
night_values <- night_values %>%
  mutate_if(is.numeric, funs(. * -1))



library(rcartocolor)
# bold <- carto_pal("Bold", n= 15)
# bold
# cbp1 <- c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A","#E68310", "#008695", "#CF1C90", "#A5AA99" )

catchcomp_plot <- ggplot(catchcomp, aes(x= Weight_Std_gm3_gps, y= Depth_Stratum, fill= Taxa_PLAOS))  +
  geom_rect(xmax= -7, xmin= 0, ymin=-Inf, ymax=Inf, fill= "lightgray", alpha= 0.7) +
  geom_bar(data= day_values, stat= "identity")+ geom_bar(data= night_values, stat= "identity") +
  theme_classic()  + facet_wrap(~Site) + labs(y="Depth Stratum", x="Biomass") +
  facet_wrap(~Species_facet, scales = "free") + geom_vline(xintercept= 0) 
plot(catchcomp_plot)
# coord_cartesian(xlim= c(-0.1, 0.1))
# scale_fill_manual(values = cbp1) +

facet