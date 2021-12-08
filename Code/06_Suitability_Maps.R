### suitability maps 

library(dplyr) 
library(tidyverse)
library(tidyr)

### upload spatial stuff

library(spDataLarge)
library(viridis)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      

library(geosphere)
library(rgeos)

#UPDATE
getwd()
#set output directory
out.dir <- "figures/"
#out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option2_strict_prob_shorter_time/")

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("input_data/Agg_Boundaries_v14.shp", quiet = T)
basins


#reach polylines
reaches <- st_read('input_data/reaches_forSCCWRP.shp', quiet = T)

#lookuptable to convert subbasin codes for model output subbasin names - doesn't exist!!!
subbasin_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)


# Current and water conservation ------------------------------------------

chub <- read.csv("output_data/Results/05_chubs_suit_class_wide_updated_current_50_prob.csv")
willow <- read.csv("output_data/Results/05_willow_suit_class_wide_updated_current_watercons.csv")

head(willow)
head(chub)

willow <- willow %>%
  select(-X) %>%
  pivot_longer(Current:Water.Conservation, names_to = "Scenario", values_to = "Suitability") %>%
  mutate(Metric = case_when(Metric == "DS_Mag_50" ~ "Dry Season Baseflow",
                            Metric == "Wet_BFL_Mag_10" ~ "Wet Season Baseflow",
                            Metric == "SP_Mag" ~ "Spring Recession")) %>%
  mutate(Scenario = case_when(Scenario == "Current" ~ "Cur", Scenario == "Water.Conservation"~ "WC"))

chub <- chub %>%
  select(-X) %>%
  rename(Current = Chub_Adult_Current, Water.Conservation = Chub_Adult_Water.Conservation) %>%
  pivot_longer(Current:Water.Conservation, names_to = "Scenario", values_to = "Suitability")


# Willow --------------------------------------------------------------------

## number iof basins in each category

willow_nums <- willow %>%
  group_by(Metric, LifeStage, Scenario, Suitability) %>%
  summarise(NBasins = length(Node)) %>%
  pivot_wider(names_from = Scenario, values_from = NBasins)

head(willow_nums)

write_csv(willow_nums, "output_data/Results/06_willow_subbasins_counts.csv")


##### loop through scenarios and metrics
sc = 1
sp=1
## define  scenarios
Scen <- unique(willow$Scenario)


for(sc in 1:length(Scen)) {
  
  scen_data <- willow %>%
    filter(Scenario == Scen[sc])
  
  metrics <- unique(scen_data$Metric)
  
  for(sp in 1:length(metrics)) {
    
    metric_data <- scen_data %>%
      filter(Metric == metrics[sp])
    
    head(metric_data)
    
    ## join with Basins
    subset.join <- metric_data %>% 
      rename(New_Name = Node) %>%
      full_join(basins, by = c('New_Name')) 
    
    
    subset.join <- na.omit(subset.join)
    head(subset.join)
    
    ## define categories
    #colors for suitability categories
    colors <- c("#0571b0", "#ca0020", "darkorange", "white")
    suitability <- c("High",  "Low","Partial", NA)
    categories <- c("Yes", "No", "Partially", "Not evaluated")
    
    lookup <- data.frame(cbind(colors, suitability, categories))
    
    #subset lookup categories and tables
    lookup.sub <- lookup[lookup$suitability %in% unique(subset.join$Suitability),]
    
    title <- paste0( "Suitability: ", metrics[sp], " (", Scen[sc], ")")
    title
    
    
    #plot
    #Set up base map 
    study <- ggplot(basins) +
      geom_sf(color = "#969696", fill="white", size = 0.1) +
      labs(title=title,  x ="", y = "")  +
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.1),
            plot.title = element_text(size=20)) 
    
    study
    names(subset.join)
    
    
    #synthesis map
    syn.plot <- study + geom_sf(data = subset.join, aes(fill=Suitability, geometry = geometry), size = 0.2) +
      scale_fill_manual(name = "Flow conditions suitable?", labels = lookup.sub$categories, values=lookup.sub$colors) +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.1) 
    # facet_grid(cols=vars(Species), rows = vars(ScenType), labeller = labeller(Species = species.labs, ScenType = scen.labs))
    
    syn.plot
    
    # out.filename
    out.filename <- paste0(out.dir, metrics[sp], "_Willow_suitability_map_", Scen[sc], ".jpg")
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    # out.filename
    
  }
  
}



# Chub --------------------------------------------------------------------

##### loop through scenarios and metrics
sc = 1
sp=1
## define  scenarios
Scen <- unique(chub$Scenario)


for(sc in 1:length(Scen)) {
  
  scen_data <- chub %>%
    filter(Scenario == Scen[sc])
  

    ## join with Basins
    subset.join <- scen_data %>% 
      rename(New_Name = Node) %>%
      full_join(basins, by = c('New_Name')) 
    
    
    subset.join <- na.omit(subset.join)
    head(subset.join)
    
    ## define categories
    #colors for suitability categories
    colors <- c("#0571b0", "#ca0020", "darkorange", "white")
    suitability <- c("High",  "Low","Partial", NA)
    categories <- c("Yes", "No", "Partially", "Not evaluated")
    
    lookup <- data.frame(cbind(colors, suitability, categories))
    
    #subset lookup categories and tables
    lookup.sub <- lookup[lookup$suitability %in% unique(subset.join$Suitability),]
    
    title <- paste0( "Suitability: ", Scen[sc])
    title
    
    
    #plot
    #Set up base map 
    study <- ggplot(basins) +
      geom_sf(color = "#969696", fill="white", size = 0.1) +
      labs(title=title,  x ="", y = "")  +
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.1),
            plot.title = element_text(size=20)) 
    
    study
    names(subset.join)
    
    
    #synthesis map
    syn.plot <- study + geom_sf(data = subset.join, aes(fill=Suitability, geometry = geometry), size = 0.2) +
      scale_fill_manual(name = "Flow conditions suitable?", labels = lookup.sub$categories, values=lookup.sub$colors) +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.1) 
    # facet_grid(cols=vars(Species), rows = vars(ScenType), labeller = labeller(Species = species.labs, ScenType = scen.labs))
    
    syn.plot
    
    # out.filename
    out.filename <- paste0(out.dir, "Chub_suitability_map_", Scen[sc], ".jpg")
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    # out.filename
    
  }
  



# change map Chub ------------------------------------------------------

head(chub2)

chub$Suitability <- gsub("Low", 1, chub$Suitability)
chub$Suitability <- gsub("Partial", 2, chub$Suitability)
chub$Suitability <- gsub("High", 3, chub$Suitability)

chub2 <- chub %>%
  pivot_wider(names_from = Scenario, values_from = Suitability) %>%
  mutate(Change = ifelse(Current == Water.Conservation, "none", "change")) %>%
  mutate(Change = ifelse(Current < Water.Conservation, "Improvement", Change)) %>%
  mutate(Change = ifelse(Current > Water.Conservation, "Degradation", Change))


## define  scenarios
Scen <- unique(chub2$Scenario)
t=1
sp=1

    
    ## join with Basins
    subset.join <- chub2 %>% 
      rename(New_Name = Node) %>%
      full_join(basins, by = c('New_Name')) 
    
    
    subset.join <- na.omit(subset.join)
    head(subset.join)
    
    
    ## define categories
    #colors for suitability categories
    colors <- c("#0571b0", "#ca0020", "gray" ,"white")
    change <- c("Improvement", "Degradation", "none", NA)
    categories <- c("Improvement", "Degradation", "No Change",  "Not evaluated")
    
    
    lookup <- data.frame(cbind(colors, change, categories))
    
    #subset lookup categories and tables
    lookup.sub <- lookup[lookup$change %in% unique(subset.join$Change),]
    
    title <- paste0( "Change: Chub")
    title
    
    
    #plot
    #Set up base map 
    study <- ggplot(basins) +
      geom_sf(color = "#969696", fill="white", size = 0.1) +
      labs(title=title,  x ="", y = "")  +
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.1),
            plot.title = element_text(size=20)) 
    
    
    # #synthesis map
    
    syn.plot <- study + geom_sf(data = subset.join, aes(fill=Change, geometry = geometry), size = 0.2) +
      scale_fill_manual(name = "Change in Suitability?", labels = lookup.sub$categories, values=lookup.sub$colors) +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.1) 
    # facet_grid(rows=vars(Time), cols = vars(Scenario), labeller = labeller(Time = time.labs, Scenario = scen.labs))
    
    
    
    syn.plot
    
    # out.filename
    out.filename <- paste0(out.dir,  "chub_change_map.jpg")
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)




# Change Map Willow -------------------------------------------------------

    head(willow2)
    
    willow$Suitability <- gsub("Low", 1, willow$Suitability)
    willow$Suitability <- gsub("Partial", 2, willow$Suitability)
    willow$Suitability <- gsub("High", 3, willow$Suitability)
    
    willow2 <- willow %>%
      pivot_wider(names_from = Scenario, values_from = Suitability) %>%
      mutate(Change = ifelse(Current == Water.Conservation, "none", "change")) %>%
      mutate(Change = ifelse(Current < Water.Conservation, "Improvement", Change)) %>%
      mutate(Change = ifelse(Current > Water.Conservation, "Degradation", Change))
    
    
    ## define  scenarios
    metric <- unique(willow2$Metric)
    t=1
    sp=1
    
    for(sc in 1:length(metric)) {
      
      metric_data <- willow2 %>%
        filter(Metric == metric[sc])
      metric_data

        
        ## join with Basins
        subset.join <- metric_data %>% 
          rename(New_Name = Node) %>%
          full_join(basins, by = c('New_Name')) 
        
        
        subset.join <- na.omit(subset.join)
        head(subset.join)
        
        
        ## define categories
        #colors for suitability categories
        colors <- c("#0571b0", "#ca0020", "gray" ,"white")
        change <- c("Improvement", "Degradation", "none", NA)
        categories <- c("Improvement", "Degradation", "No Change",  "Not evaluated")
        
        
        lookup <- data.frame(cbind(colors, change, categories))
        
        #subset lookup categories and tables
        lookup.sub <- lookup[lookup$change %in% unique(subset.join$Change),]
        
        title <- paste0( "Change: ", metric[sc])
        title

        
        
        #plot
        #Set up base map 
        study <- ggplot(basins) +
          geom_sf(color = "#969696", fill="white", size = 0.1) +
          labs(title=title,  x ="", y = "")  +
          theme(panel.background = element_rect(fill = "white"),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                panel.grid = element_line(color = "white", size = 0.1),
                plot.title = element_text(size=20)) 
        
        
        # #synthesis map
        
        syn.plot <- study + geom_sf(data = subset.join, aes(fill=Change, geometry = geometry), size = 0.2) +
          scale_fill_manual(name = "Change in Suitability?", labels = lookup.sub$categories, values=lookup.sub$colors) +
          geom_sf(data = reaches, color = "#67a9cf", size = 0.1) 
        # facet_grid(rows=vars(Time), cols = vars(Scenario), labeller = labeller(Time = time.labs, Scenario = scen.labs))
        
        
        
        syn.plot
        
        # out.filename
        out.filename <- paste0(out.dir, "Willow_change_map_", metric[sc], ".jpg")
        ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
        
      }
      
    
    
    

