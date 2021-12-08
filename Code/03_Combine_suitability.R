### combine for overall suitbaility
library(tidyr)
library(tidyverse)


## upload all species suitability data
## time stats


# Willow ------------------------------------------------------------------


getwd()
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New")

willow_current <- read.csv("output_data//Willow/Current/02_Willow_Current.csv")
willow_watercons <- read.csv("output_data//Willow/Water_Conservation/02_Willow_WaterCons.csv")

willow <- rbind(willow_current, willow_watercons)
head(willow)

willow_ref <- read.csv("output_data/Willow/Reference/02_Willow_Reference.csv")
head(willow_ref)

## current
time_stats <- willow %>% 
  select(-X) %>%
  rename(TimePeriod = season, TimePercentage = PercentTime) %>%
  distinct()

## spring
time_stats_seas <- time_stats %>%
  filter(!TimePeriod %in% c("winter", "summer")) %>%
  distinct()
head(time_stats_seas)


## spring - binary 1,0 1 = inundated, 0 = not inundated

head(time_stats)
unique(time_stats_seas$TimePeriod)


## calculate suitability

time_stats_seas$TimePercentage[is.na(time_stats_seas$TimePercentage)] <- 0

time_stats_seas <- time_stats_seas %>%
  mutate(Suitability_Class = NA)

probs <- seq(1, dim(time_stats_seas)[1], 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$TimePercentage[p] > 0) {
    paste("High")
  } else  if(time_stats_seas$TimePercentage[p] == 0){
    paste("Low")
  } 
  
}

head(time_stats_seas)
time_stats_spring <- time_stats_seas

## winter and summer = same as below


## change time period to seasonal 

time_stats_seas <- time_stats %>%
  filter(TimePeriod %in% c("winter", "summer")) %>%
  distinct()
head(time_stats_seas)

## calculate suitability

time_stats_seas$TimePercentage[is.na(time_stats_seas$TimePercentage)] <- 0

time_stats_seas <- time_stats_seas %>%
  mutate(Suitability_Class = NA)
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_seas)[1], 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$TimePercentage[p] >= 75) {
    paste("High")
  } else  if(time_stats_seas$TimePercentage[p] >= 25 & time_stats_seas$TimePercentage[p] <= 75 ){
    paste("Partial")
  } else  if(time_stats_seas$TimePercentage[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}


head(time_stats_seas)


## join back together and save
time_stats_all <-rbind(time_stats_seas, time_stats_spring)
write.csv(time_stats_all, "output_data/Results/03_Willow_time_stats_combined_current_water_conservation.csv")




# Chub --------------------------------------------------------------------


ts0 <- list.files("output_data/Chub", pattern="time_stats")
ts1 <- Filter(function(x) grepl("Existing", x), ts0)
ts2 <- Filter(function(x) grepl("Recalibration", x), ts0)
ts3 <- Filter(function(x) grepl("Water_Conservation", x), ts0)
ts0
ts <- c(ts1,ts2, ts3)
ts
time_statsx <- NULL
j=1
## combine chub and willow time stats
for(j in 1: length(ts)) {
  
  
  time_stats <- read.csv(file=paste("output_data/Chub/", ts[j], sep=""))
  head(time_stats)
  
  ######## juvenile
  time_stats <- time_stats %>% 
    select(-X) %>%
    # filter(Probability_Threshold == ann_stats) %>%
    rename(TimePeriod = season, TimePercentage = value) %>%
    # mutate(Species = "Willow", Life_Stage = "Seedling", Hydraulic = "Depth", Node = paste(stringx[3])) %>%
    distinct()
  
  time_statsx <- rbind(time_statsx, time_stats)
  
}

head(time_statsx)


## change time period to seasonal 

time_stats_seas <- time_statsx %>%
  filter(TimePeriod == "critical") %>%
  distinct()
head(time_stats_seas)

## calculate suitability

time_stats_seas$TimePercentage[is.na(time_stats_seas$TimePercentage)] <- 0

time_stats_seas <- time_stats_seas %>%
  mutate(Suitability_Class = NA)
# group_by(Node, position, Species, Life_Stage, water_year) %>%

probs <- seq(1, dim(time_stats_seas)[1], 1)  

for(p in 1: length(probs)) {
  
  time_stats_seas$Suitability_Class[p] = if(time_stats_seas$TimePercentage[p] >= 75) {
    paste("High")
  } else  if(time_stats_seas$TimePercentage[p] >= 25 & time_stats_seas$TimePercentage[p] <= 75 ){
    paste("Partial")
  } else  if(time_stats_seas$TimePercentage[p] < 25){
    paste("Low")
  } else {
    paste("Partial")
  }
  
}


head(time_stats_seas)


## join back together and save
time_stats_all <-time_stats_seas
write.csv(time_stats_all, "output_data/Results/03_Chub_time_stats_combined_current_water_conservation.csv")

# Suitability Over POR -------------------------------------------------------------

chub <- read.csv("output_data/Results/03_Chub_time_stats_combined_current_water_conservation.csv")
head(chub)
names(chub)

## upload slices - use only main channel for chub

slices <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/XS_plots_updated_axis_labels/lookup_reach_manningsn_channeltype_splits_10192020.csv")
head(slices)

slices <- slices %>% 
  select(Reach.ID, main.channel.slice) %>%
  rename(Node = Reach.ID) %>%
  mutate(position = paste0("slice", main.channel.slice)) %>%
  mutate(code = paste(Node, "_", position, sep="")) 

codes <- unique(slices$code)
codes

## filter to only MC slice 
time_stats <- chub  %>%
  select(Species, Life_Stage, Node,Hydraulic, water_year,TimePeriod, position,
         Suitability_Class, Probability_Threshold, Scenario) %>%
  mutate(code = paste(Node, "_", position, sep="")) %>%
  filter(code %in% codes) %>% 
  distinct()

SuitClassOverYears <- time_stats %>%
  group_by(Species, Life_Stage, Hydraulic, TimePeriod, position, Node, Probability_Threshold, code, Scenario) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1)) ## takes most dominant suitability - this may need changing

SuitClassOverYears

write.csv(SuitClassOverYears, "output_data/Results/03_chub_suitability_combined_years_all_probs_current.csv")



willow <- read.csv("output_data/Results/03_Willow_time_stats_combined_current_water_conservation.csv")
head(willow)
names(willow)


## does not have slices associated
time_stats <- willow  %>%
  select(Species, LifeStage, Node, Metric, water_year,TimePeriod,
         Suitability_Class, SeasonalComponent, Scenario) %>%
  distinct()

SuitClassOverYears <- time_stats %>%
  group_by(Species, LifeStage, Node, Metric, TimePeriod,
            SeasonalComponent, Scenario) %>%
  summarise(Overall_Class = tail(names(sort(table(Suitability_Class))), 1)) ## takes most dominant suitability - this may need changing

SuitClassOverYears

write.csv(SuitClassOverYears, "output_data/Results/03_willow_suitability_combined_years_all_probs_current.csv")


