### combine all results and condense

library(tidyverse)
library(tidyr)


# Chub --------------------------------------------------------------------


## upload data

data <- read.csv("output_data/Results/03_chub_suitability_combined_years_all_probs_current.csv")
head(data)

unique(data$Scenario)

full_df <- data %>%
  mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  filter(Probability_Threshold == "Medium") %>% 
  mutate(Scenario = case_when(Scenario %in% c("201105_Aliso_Recalibration_Update", 
                                                "210422_San_Juan_Existing_Conditions", 
                                  "201118_Oso,_Small_Creeks_Existing_Conditions") ~ "Current", 
                                Scenario %in%  c("201111_Aliso_Water_Conservation_Scenario", 
                                                 "210422_San_Juan_Water_Conservation",
                                                 "201119_Oso,_Small_Creeks_Water_Conservation_Scenario") 
                                ~ "Water Conservation")) %>%
  distinct()


### if one hydraulic varibale per species and per position is low/partial, position is low

full_df <- full_df %>%
  mutate(sp_code = paste(Species, "_", Life_Stage,"_", Scenario, sep="")) %>%
  mutate(ClassOrder = Overall_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))


## subset by speices, then pos_code, then hydraulic
## rule: if one hydraulic variable is low, then node position is low
pos <- unique(full_df$pos_code)
pos
sp_dfx <- NULL
s=2
p=17


for(p in 1: length(pos)) {
  
  ## filters to node and postion
  pos_df <- full_df %>%
    filter(pos_code == pos[p])
  pos_df
  ## define species $ lifstage at node/position
  species <- unique(pos_df$sp_code)
  species
  for(s in 1:length(species)) {
    ## filter to 1 species lifestage
    sp_df <- pos_df %>%
      filter(sp_code == species[s])
    sp_df
    if (length(unique(sp_df$Overall_Class)) >1) {
      ## if more than 1 suitbaility class then take the lowest one
      MinClass <- filter(sp_df, ClassOrder ==min(sp_df$ClassOrder))
      sp_df$Check_Class <- MinClass$Overall_Class[1]
      ## if only 1 class then take that class
    } else if (length(unique(sp_df$Overall_Class)) == 1){
      sp_df$Check_Class <-  paste(sp_df$Overall_Class[1])
    }
    
    sp_dfx <- rbind(sp_dfx, sp_df)
    
    
  }
}

sp_dfx

sp_dfx ## data frame has overall class per node, position, species, life stage and hydraulic variable


hp_dfx <- sp_dfx %>%
  select(Node, Species, Life_Stage, position, sp_code, Check_Class) %>%
  distinct()

unique(hp_dfx$sp_code)


### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <- hp_dfx %>%
  mutate(ClassOrder = Check_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Check_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
node


node_dfx <- NULL
s
p=5

for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])
  
  
  nd <- unique(sp_df$Node)
  for(p in 1: length(nd)) {
    
    node_df <- sp_df %>%
      filter(Node == nd[p])
    
    node_df
    if(length(unique(node_df$Check_Class)) == 1) {
      node_df$Node_Class <- node_df$Check_Class
    } else if (length(unique(node_df$Check_Class)) > 1){
      ## if more than 1 suitbaility class then take the highest one
      MaxClass <- filter(node_df, ClassOrder ==max(node_df$ClassOrder))
      node_df$Node_Class <- MaxClass$Check_Class[1]
    }
    
    node_dfx <- rbind(node_dfx, node_df)
    
  }
}


head(node_dfx)

df <- node_dfx %>%
  select(Node, Species, Life_Stage, sp_code, Node_Class) %>%
  distinct()

df


df_wide <- df %>%
  pivot_wider(id_cols = Node, names_from = sp_code, values_from = Node_Class)

df_wide

write.csv(df_wide, "output_data/Results/05_chubs_suit_class_wide_updated_current_50_prob.csv")


getwd()


# Willow ------------------------------------------------------------------

data <- read.csv("output_data/Results/03_willow_suitability_combined_years_all_probs_current.csv")
head(data)

unique(data$Scenario)

full_df <- data %>%
  # mutate(pos_code = paste(Node, "_", position, sep="")) %>%
  # filter(Probability_Threshold == "Medium") %>% 
  mutate(Scenario = case_when(Scenario %in% c("201105_Aliso_Recalibration_Update", 
                                              "210422_San_Juan_Existing_Conditions", 
                                              "201118_Oso,_Small_Creeks_Existing_Conditions") ~ "Current", 
                              Scenario %in%  c("201111_Aliso_Water_Conservation_Scenario", 
                                               "210422_San_Juan_Water_Conservation",
                                               "201119_Oso,_Small_Creeks_Water_Conservation_Scenario") 
                              ~ "Water Conservation")) %>%
  distinct()

head(full_df)

df <- full_df %>%
  select(Node, Species, LifeStage, Metric, Overall_Class, Scenario) %>%
  distinct()

head(df)

df_wide <- df %>%
  pivot_wider(id_cols = c(Node, LifeStage, Metric), names_from = Scenario, values_from = Overall_Class)

df_wide

write.csv(df_wide, "output_data/Results/05_willow_suit_class_wide_updated_current_watercons.csv")


getwd()
