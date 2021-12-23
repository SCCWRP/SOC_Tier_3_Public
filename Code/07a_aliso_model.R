## Aliso analysis

## move code to other iter 3 repo




library(tidyverse)
library(tidyr)
library(lubridate) # work with dates
library(data.table)

getwd()


outdir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/output_data/"

## upload flow ranges 

ranges <- read.csv("input_data/flow_ranges_alisoSTP_12152021_restoredXSgeom.csv")
ranges <- ranges %>%
  filter(Species1 == "Willow", channel_geom == "Restored")
ranges

## define sites
sites <- unique(ranges$Reach)
sites
## upload hydraulic data
setwd("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics")
indir <- "/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/"
f <- list.files() ## list scenario files


# Current -----------------------------------------------------------------


f <- f[c(1)] ## take only existing conditions
f
## empty DF for time stats
time_statsx <- NULL
c=1
n=2
  
  h <- list.files(f[c], pattern = "hydraulic") 
  h
  ScenName <- paste(f[c])
  
  
    
    hydraul <- read.csv(file=paste(indir, f[c],"/", h[n], sep="")) ## aliso node
    head(hydraul)
    
    
    ## define nodename
    NodeName <- str_split(h[n], "_", 3)[[1]]
    NodeName <- NodeName[1]
    NodeName
    ## change names and add datenum
    hyd_dep <- hydraul %>%
      rename(Q = q.cms) %>%
      mutate(date_num = seq(1,length(date), 1))
    
    ## format date time
    hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                                 format = "%m/%d/%Y",
                                 tz = "GMT")
    ## add water year
    hyd_dep <- hyd_dep %>%
      mutate(month = month(DateTime)) %>%
      mutate(year = year(DateTime)) %>%
      mutate(day = day(DateTime)) %>%
      mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
    
    hyd_dep <- hyd_dep %>%
      select(DateTime, Q, date_num, month, day, water_year)
    
    ## define seasons
    winter <- c(1:3,11:12) ## winter months
    summer <- c(6:10) ## summer months
    spring <- c(4:5) ## spring
    
    ## add to df
    all_datax <- hyd_dep %>%
      mutate(season = case_when(month %in% winter ~ "winter", 
                                month %in% summer ~ "summer",
                                month %in% spring ~ "spring")) %>%
      mutate(Node = NodeName, Scenario = ScenName)
    
    
    
    ## extract node from flow ranges
    node_range <- ranges %>%
      filter(Reach == NodeName)
    
    if(dim(node_range)[1] == 0) {
      
      metrics <- c(NA)
    } else {
      
      ## define metrics
      metrics <- unique(node_range$metric)
      
    }
    
    metrics
    
    for(m in 1:length(metrics)) {
      
      
      one_range <- node_range %>%
        filter(metric == metrics[m])
      one_range
      
      LS <- one_range$LifeStage
      sp <- one_range$Species1
      sc <- one_range$Seasonal_Component
      
      lowQ <- one_range$Lower.Limit
      highQ <- one_range$Upper.Limit
      highQ
      ## sometime no limit so make limit either very high or zero
      
      try(if(is.na(highQ)) {
        highQ <- 6000
      } else {
        highQ <- one_range$Upper.Limit
      }, silent = T)
      
      try(if(is.na(lowQ)) {
        lowQ <- 0
      } else {
        lowQ <- one_range$Lower.Limit
      }, silent = T)
      
      ## subset to relevant season
      
      if(is.na(metrics[1])) {
        
        time_stats <- all_datax %>%
          dplyr::group_by(water_year) %>%
          dplyr::mutate(PercentTime = NA) %>%
          distinct(water_year, PercentTime, season) %>%
          mutate(Node = NodeName, Species = NA, LifeStage = NA, SeasonalComponent = NA, Metric = NA, Scenario = ScenName)
        time_statsx <- rbind(time_statsx, time_stats)
        
      } else if(metrics[m] == "Wet_BFL_Mag_10") {
        
        new_data <- all_datax %>%
          filter(season == "winter")
        
        ###### calculate amount of time
        
        time_stats <- new_data %>%
          dplyr::group_by(water_year) %>%
          dplyr::mutate(PercentTime = sum(Q > lowQ & Q < highQ)/length(DateTime)*100) %>%
          distinct(water_year, PercentTime, season) %>%
          mutate(Node = NodeName, Species = sp, LifeStage = LS, SeasonalComponent = sc, Metric = metrics[m], Scenario = ScenName)
        
        time_statsx <- rbind(time_statsx, time_stats)
        
      } else if(metrics[m] == "DS_Mag_50") {
        new_data <- all_datax %>%
          filter(season == "summer")
        
        ###### calculate amount of time
        
        time_stats <- new_data %>%
          dplyr::group_by(water_year) %>%
          dplyr::mutate(PercentTime = sum(Q > lowQ & Q < highQ)/length(DateTime)*100) %>%
          distinct(water_year, PercentTime, season) %>%
          mutate(Node = NodeName, Species = sp, LifeStage = LS, SeasonalComponent = sc, Metric = metrics[m], Scenario = ScenName)
        
        time_statsx <- rbind(time_statsx, time_stats)
        
      } else if(metrics[m] == "SP_Mag") {
        new_data <- all_datax %>%
          filter(season == "spring")
        
        ###### calculate amount of time
        sum(new_data$Q > lowQ & new_data$Q < highQ)
        
        time_stats <- new_data %>%
          dplyr::group_by(water_year) %>%
          dplyr::mutate(PercentTime = sum(Q > lowQ & Q < highQ)/length(DateTime)*100) %>%
          distinct(water_year, PercentTime, season) %>%
          mutate(Node = NodeName, Species = sp, LifeStage = LS, SeasonalComponent = sc, Metric = metrics[m], Scenario = ScenName)
        time_stats
        time_statsx <- rbind(time_statsx, time_stats)
        
      } 
      
    }   


time_statsx <- na.omit( time_statsx) ## remove missing subbasins
time_statsx


## save
write.csv(time_statsx, paste0(outdir,"Willow/Current/07_Willow_Aliso_restored.csv"))
