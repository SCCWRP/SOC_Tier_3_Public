## suitability % at Aliso

library(tidyverse)
library(tidyr)

## standard error

std_error <- function(x) sd(x)/sqrt(length(x))

# Willow ------------------------------------------------------------------

## existing
willow <- read.csv("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/Willow/Current/02_Willow_Current.csv")
head(willow)

MeanPerc <- willow %>%
  group_by(season, Node, SeasonalComponent, Metric) %>%
  summarise(MeanPerc = mean(PercentTime), StdErr = std_error(PercentTime))

head(MeanPerc)

write.csv(MeanPerc,"/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/Willow/Current/07_Willow_Mean_Percent_existing_XS.csv" )


## also need % of years > 0 for spring

willow2 <- read.csv("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/Results/03_Willow_time_stats_combined_current_water_conservation.csv")
head(willow2)

willowC <- willow2 %>%
  filter(Node == "J01-020", TimePeriod == "spring", Scenario == "201105_Aliso_Recalibration_Update") %>%
  summarise(Total = sum(Suitability_Class == "High"), TotalPerc = Total/length(Suitability_Class)*100)
  
willowC
dim(willowC)

# Total TotalPerc
# 1    21  80.76923

## restored

willowR <- read.csv("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/Willow/Current/07_Willow_Aliso_restored.csv")
willowR

MeanPerc <- willowR %>%
  group_by(season, Node, SeasonalComponent, Metric) %>%
  summarise(MeanPerc = mean(PercentTime), StdErr = std_error(PercentTime))

MeanPerc

willowS <- willowR %>%
  filter(season == "spring") %>%
  # group_by(season, Node, SeasonalComponent, Metric) %>%
  summarise(Total = sum(PercentTime>0), TotalPerc = Total/26*100)

# Total TotalPerc
# 1    21  80.76923
# Chub --------------------------------------------------------------------

chub <- read.csv("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/Results/03_Chub_time_stats_combined_current.csv")
head(chub)

MeanPerc <- chub %>%
  filter(Node == "J01-020", position == "slice3") %>% # take only aliso and chub slice
  group_by( TimePeriod, Probability_Threshold, Hydraulic) %>%
  summarise(MeanPerc = mean(TimePercentage), StdErr = std_error(TimePercentage))

head(MeanPerc)

# 5 critical   Medium                Depth         1.92  0.305
# 6 critical   Medium                Velocity     95.5   0.607