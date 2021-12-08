### combine flow limits with Q calculation
### idea is to use calculation to define upper and lower limits
## add best slice info

getwd()

library(tidyverse)
library(tidyr)

## empty df
LimitsCalcDatax <- NULL
r=1

## list limits data
## upload and format Q limits and calculation
L0 <- list.files(path = "output_data/Chub", pattern="_Q_limits_")
L1 <- Filter(function(x) grepl("Existing", x), L0)
L2 <- Filter(function(x) grepl("Recalibration", x), L0)

L <- c(L1,L2)
L
### list Q calculations

C0 <- list.files(path = "output_data/Chub", pattern="_Q_calculation")
C1 <- Filter(function(x) grepl("Existing", x), C0)
C2 <- Filter(function(x) grepl("Recalibration", x), C0)

C <- c(C1, C2)
C
r = 100
for (r in 1: length(L)) {
  
  LimitData <- read.csv(file=paste("output_data/Chub/", L[r], sep=""))
  LimitData
  ## define node name
  LimitName <- str_split(L[r], "_", 3)[[1]]
  LimitName <- LimitName[2]
  
  ## define species, life stage, hydraulic
  QName <- str_split(L[r], "_", 6)[[1]]
  SpeciesName <- QName[3]
  LifeStageName <- QName[4]
  HydraulicName <- QName[5]
  
  if (HydraulicName == "depth") {
    HydraulicName <-  "Depth"
  } else if (HydraulicName == "shear") {
    HydraulicName <- "Shear"
  } else {
    
  }
  
  ## extract matching file - limits file matches Q calulation file
  QC <- Filter(function(x) grepl(paste(LimitName, "_", SpeciesName, "_", LifeStageName, "_", HydraulicName, sep=""), x), C)
  
  # LimitData
  ## upload calculation
  
  CalcData <- read.csv(file=paste("output_data/Chub/", QC, sep=""))
  # CalcData
  ## make data same format so can join
  
  slices <- LimitData %>%
    select(-X, -c(Type, Species, Life_Stage, Hydraulic, Node, Scenario))
  # slices
  ## number of slices as changes per node
  nslice <- paste("V",  length(names(slices)), sep="")
  # LimitDatax
  ## reformat and change names - change willow to Thresh
  
  if(LimitData$Species == "Willow" && LimitData$Life_Stage == "Adult") {
    
    LimitDatax <- LimitData %>%
      select(-X) %>%
      group_by(Type) %>%
      pivot_longer(V1:paste(nslice), names_to = "Position", values_to = "QTarget") %>%
      mutate(Position = gsub("V", "slice", Position),
             Type = gsub("Q_limitLow", "Thresh", Type)) 
    
  } else {
    
    LimitDatax <- LimitData %>%
      select(-X) %>%
      group_by(Type) %>%
      pivot_longer(V1:paste(nslice), names_to = "Position", values_to = "QTarget") %>%
      mutate(Position = gsub("V", "slice", Position),
             Type = gsub("Q_limit", "", Type)) 
    
  }
  
  
  
  ## extract hydaulic limit values in own df
  Hlimitsx <- LimitDatax %>%
    filter(Type %in% c("Hydraulic_limitLow", 
                       "Hydraulic_limitMed", "Hydraulic_limitHigh")) %>%
    mutate(Type = gsub("Hydraulic_limit", "", Type)) %>%
    rename(HTarget = QTarget, ProbabilityThreshold = Type)
  
  ## remove hydraulic limits from df
  LimitDatax <- LimitDatax %>%
    filter(!Type %in% c("Hydraulic_limitLow", 
                        "Hydraulic_limitMed", "Hydraulic_limitHigh")) %>%
    rename(ProbabilityThreshold = Type)
  
  ## combine back together
  limits <- bind_cols(LimitDatax, Hlimitsx[, "HTarget"])
  limits
  CalcData
  
  ## format differently for willow adult as only has threshold
  if(CalcData$Species == "Willow" && CalcData$Life_Stage == "Adult") {
    
    ## format  calculation
    CalcDatax <- CalcData %>%
      separate(Position, into = c("HydUnit", "Position"), sep="_") %>%
      group_by(Position) %>%
      pivot_longer(Thresh, names_to = "ProbabilityThreshold", values_to = "Calculation") %>%
      select(-X, -HydUnit, -Species, -Life_Stage, -Hydraulic, -Node, -Scenario) %>%
      mutate(ProbabilityThreshold = gsub("Medium", "Med", ProbabilityThreshold))
    
  } else {
    
    ## format  calculation
    CalcDatax <- CalcData %>%
      separate(Position, into = c("HydUnit", "Position"), sep="_") %>%
      group_by(Position) %>%
      pivot_longer(Low:High, names_to = "ProbabilityThreshold", values_to = "Calculation") %>%
      select(-X, -HydUnit, -Species, -Life_Stage, -Hydraulic, -Node, -Scenario) %>%
      mutate(ProbabilityThreshold = gsub("Medium", "Med", ProbabilityThreshold))
    
  }
  
  
  
  ## combine limits and calculation
  
  # LimitsCalcData
  LimitsCalcData <- left_join(limits, CalcDatax, by= c("ProbabilityThreshold", "Position"))
  
  
  LimitsCalcDatax <- rbind(LimitsCalcDatax, LimitsCalcData)
  
}

dim(LimitsCalcDatax)
names(LimitsCalcDatax)
# ?round

LimitsCalcDatax$QTarget <- round(LimitsCalcDatax$QTarget, digits = 2)
LimitsCalcDatax$QTarget
head(LimitsCalcDatax)

# LimitsCalcDatax <- na.omit(LimitsCalcDatax)

write.csv(LimitsCalcDatax, "output_data/Results/04_Chub_limits_calc_combined_current.csv")


## filter to main channel slices

SuitabilityPerSlice <- read.csv("output_data/results/S1_suitability_per_slice_median_current.csv")
LimitsCalcDatax <- read.csv("flow_targets/S2_all_limits_calc_combined_current.csv")

SuitabilityPerSlice <- SuitabilityPerSlice %>%
  select(-X, -X.1) %>%
  rename(ProbabilityThreshold = Probability_Threshold, Position = position) %>%
  # filter(ProbabilityThreshold == "Medium") %>% ### best slice based on med threshold
  mutate(ProbabilityThreshold = gsub("Medium", "Med", ProbabilityThreshold)) %>%
  distinct()

LimitsCalcDatax <- LimitsCalcDatax %>%
  select( -X) 


names(SuitabilityPerSlice)
names(LimitsCalcDatax)

all_data <- left_join(LimitsCalcDatax, SuitabilityPerSlice, 
                      by=c("Species", "Life_Stage", "Hydraulic", "Node",
                           "ProbabilityThreshold", "Position"))

head(all_data)

slices <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/XS_plots/XS_plots_updated_axis_labels/lookup_reach_manningsn_channeltype_splits_10192020.csv")
head(slices)

slices <- slices %>% 
  select(Reach.ID, main.channel.slice) %>%
  rename(Node = Reach.ID) %>%
  mutate(position = paste0("slice", main.channel.slice)) %>%
  mutate(code = paste(Node, "_", position, sep="")) 

codes <- unique(slices$code)
codes

all_data <- LimitsCalcDatax  %>%
  mutate(code = paste(Node, "_", Position, sep="")) %>%
  filter(code %in% codes) %>% 
  distinct()

head(all_data)

write_csv(all_data, "output_data/Results/04_chub_limits_calcs_best_slice.csv")

### add upper/lower thresholds

head(all_data)
# ?gsub
all_data$Calculation <- gsub("newx1Low", "limit",all_data$Calculation)
all_data$Calculation <- gsub("newx1Med", "limit",all_data$Calculation)
all_data$Calculation <- gsub("newx1High", "limit",all_data$Calculation)


calcs <- unique(sort(all_data$Calculation))
calcs


## do for all calcs
## remove hydraulic limit and add back in


names(all_data)

## "Q < 0"  
all_data1 <- all_data %>%
  filter(Calculation == calcs[1]) %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste("Upper"), Flag = paste("No Limit"))

## "Q <= limit[1] & Q >= limit[2]"
all_data2 <- all_data %>%
  filter(Calculation == calcs[2]) %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste(c("Upper", "Lower")), Flag = paste("N"))

##  "Q >= limit"
all_data3 <- all_data %>%
  filter(Calculation == calcs[3]) %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste("Lower"), Flag = paste("N"))

## "Q >= limit[1] & Q <= limit[2]"
all_data4 <- all_data %>%
  filter(Calculation == calcs[4]) %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste(c("Lower", "Upper")), Flag = paste("N"))

## "Q >= min_limit & Q <= limit"
all_data5 <- all_data %>%
  filter(Calculation == calcs[5]) %>%
  group_by(Species, Life_Stage, Hydraulic, Node, ProbabilityThreshold, Position) %>%
  mutate(LimitBound = paste( "Upper"), Flag = paste("With Min Limit"))


all_data_Q <- bind_rows(all_data1, all_data2, all_data3, all_data4, all_data5)


write.csv(all_data_Q, "output_data/Results/04_chub_limits_calcs_best_slice_bounds.csv")


### subset to best slices - select best slices based on med probability


all_data_Q

## make same code for main dataframer

data <- all_data_Q %>%
  select( -HTarget) %>%
  na.omit(data) %>%
  distinct() %>%
  group_by(ProbabilityThreshold, Species, Life_Stage, Hydraulic, Node) %>%
  pivot_wider(names_from = LimitBound, values_from = QTarget) %>%
  mutate(Code = paste(Species, "_",Life_Stage, "_", Node, "_", Position, sep="")) %>%
  ungroup()%>%
  select(ProbabilityThreshold:Node, Position, Lower:Upper)

names(data)  

write.csv(data, "output_data/Results/04_chub_flow_targets_best_slices_only.csv")  




