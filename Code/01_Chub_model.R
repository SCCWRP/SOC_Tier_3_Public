### chub application
## depth and velocity
## Katie Irving

## packages

library(tidyverse)
library(tidyr)
library(lubridate) # work with dates
library(data.table)
getwd()



setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New")
outdir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New/"

## upload depth model
load(file = "models_functions/chub_depth_mod.RData")

## upload velocity data
fitdata <- read.csv("output_data/00_chub_adult_velocity_prob_curve_data.csv")


## root function
load(file="models_functions/root_interpolation_function.Rdata")

## define root equation
load(file="models_functions/expression_Q_limit_function.RData")

min_limit <- 0.01 ## define min limit for depth. can be changed
## upload hydraulic data
setwd("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics")
indir <- "/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_hydraulics/"
f <- list.files() ## list climate scenario files
f <- f[c(1:2, 5:11)]
f
c=2
n=1
h
for(c in 1: length(f)) {
  
  h <- list.files(f[c], pattern = "hydraulic") 
  ScenName <- paste(f[c])
  ScenName
  for(n in 1: length(h)) {
    
    hydraul <- read.csv(file=paste(indir, f[c],"/", h[n], sep=""))
    # head(hydraul)
    
    head(hydraul)
    
    ## define nodename
    NodeName <- str_split(h[n], "_", 3)[[1]]
    NodeName <- NodeName[1]
    
    ## change names and add datenum
    
    hyd_dep <- hydraul %>%
      rename(Q = q.cms) %>%
      mutate(date_num = seq(1,length(date), 1))
    
    ## format date time
    hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                                 format = "%m/%d/%Y",
                                 tz = "GMT")
    
    hyd_dep <- hyd_dep %>%
      mutate(month = month(DateTime)) %>%
      mutate(year = year(DateTime)) %>%
      mutate(day = day(DateTime)) %>%
      mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
    
    # head(hyd_dep)
    # ## melt channel position data
    
    hyd_dep <- hyd_dep %>%
      select(DateTime, Q, date_num, month, day, water_year, contains("max.depth"))
    
    # transform m to cm
    hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
    hyd_dep <- hyd_dep %>% 
      mutate(Depth_cm = as.numeric(as.character(value*100)))
    head(hyd_dep)
    
    
    ## predict values
    
    all_data <- hyd_dep %>%
      mutate(prob_fit = predict(chub_depth_mod, newdata = hyd_dep, type="response")) %>%
      mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) %>%## predicts negative percentages - cut off at 0 for quick fix
      mutate(prob_fit = ifelse(Depth_cm == 0, 0, prob_fit)) ## when depth is 0, prob is 0
    
    ## chub critical season is all year - for probs split into seasons
    # non_critical <- c(1:3,10:12) ## winter months
    critical <- c(1:12) ## summer months
    
    
    all_datax <- all_data %>%
      mutate(season = ifelse(month %in% critical, "critical", "non_critical") ) %>%
      separate(variable, into = c("Hydraulic", "Position"), sep="_", remove = F) %>%
      mutate(Node = NodeName, Scenario = ScenName)
    
    write.csv(all_datax, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Depth_prob_stats_all_", ScenName, ".csv", sep=""))
    
    prob_stats <- all_datax %>%
      # filter(season == "critical") %>%
      group_by(water_year,variable, Hydraulic, Position, season) %>%
      summarise(MeanProbability = mean(prob_fit), SDProbability = sd(prob_fit)) %>%
      mutate(Species = "Chub", LifeStage = "Adult", Node = NodeName, Scenario = ScenName)
    
    write.csv(prob_stats, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Depth_prob_stats_annual_", ScenName, ".csv", sep=""))
    
    
    head(all_data)
    ## define positions
    positions <- unique(all_data$variable)
    # positions 
    ## Q Limits
    limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
    limits$Type<-c("Q_limitLow", "Q_limitLow","Q_limitMed", "Q_limitMed", "Q_limitHigh", "Q_limitHigh")
    
    H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
    H_limits$Type<-c("Hydraulic_limitLow","Hydraulic_limitLow", "Hydraulic_limitMed", 
                     "Hydraulic_limitMed", "Hydraulic_limitHigh", "Hydraulic_limitHigh")
    
    ## calculation
    Q_Calc <- as.data.frame(matrix(nrow=length(positions), ncol=3 ))
    
    names(Q_Calc) <- c("Low", "Medium", "High")
    
    time_statsx <- NULL
    days_data <- NULL
    
    p=1
    for(p in 1:length(positions)) {
      
      # probability as a function of discharge -----------------------------------
      
      new_data <- all_data %>% 
        filter(variable  == positions[p])
      head(new_data)
      new_data <- na.omit(new_data)
      ## define position
      PositionName <- str_split(positions[p], "_", 3)[[1]]
      PositionName <- PositionName[2]
      
      # head(new_data)
      # PositionName
      peak <- new_data %>%
        filter(prob_fit == max(na.omit(prob_fit))) #%>%
      range(new_data$Depth_cm)
      range(new_data$prob_fit)
      # write.csv(new_data, "output_data/02_example_node_for_figures.csv")
      # peakQ
      peakQ  <- max(peak$Q)
      min_limit <- filter(new_data, Q > 0)
      min_limit <- min(min_limit$Q)
      
      ## define limits for time stats
      
      # newx1Low
      if(min(new_data$prob_fit) > 0.25) {
        newx1Low <- min(new_data$Q)
        hy_lim1Low <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else {
        newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.250)
        hy_lim1Low <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.250)
      }
      
      
      if(length(newx1Low)>2) {
        newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
        hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
      }
      
      ## find roots for each probability - medium
      
      if(min(new_data$prob_fit) > 0.5) {
        newx1Med <- min(new_data$Q)
        hy_lim1Med <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else {
        newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
        hy_lim1Med <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.50)
      }
      
      if(length(newx1Med)>2) {
        newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
        hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
      }
      
      ## find roots for each probability - High
      
      if(min(new_data$prob_fit) > 0.75) {
        newx1High<- min(new_data$Q)
        hy_lim1High <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.75) {
        newx1High <- NA
        hy_lim1High <- NA
      } else {
        newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.750)
        hy_lim1High <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.750)
      }
      
      if(length(newx1High)>2) {
        newx1High <- c(newx1High[1], newx1High[length(newx1High)])
        hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
      }
      
      
      # create year_month column       
      new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
      
      # dataframe for stats -----------------------------------------------------
      
      ## define critical period or season for seedling as all year is critical
      new_datax <- new_datax %>%
        mutate(season = ifelse(month %in% critical, "critical", "non_critical" ))
      
      
      # time stats - mid channel ------------------------------------------------
      
      ## Main channel curves
      threshLow <- expression_Q(newx1Low, peakQ) 
      threshLow <-as.expression(do.call("substitute", list(threshLow[[1]], list(limit = as.name("newx1Low")))))
      # threshLow
      threshMed <- expression_Q(newx1Med, peakQ) 
      threshMed <-as.expression(do.call("substitute", list(threshMed[[1]], list(limit = as.name("newx1Med")))))
      # threshMed
      threshHigh <- expression_Q(newx1High, peakQ) 
      threshHigh <-as.expression(do.call("substitute", list(threshHigh[[1]], list(limit = as.name("newx1High")))))
      
      Q_Calc[p,] <- c(paste(threshLow), paste(threshMed), paste(threshHigh))
      ###### calculate amount of time
      
      time_stats <- new_datax %>%
        dplyr::group_by(water_year, season) %>%
        dplyr::mutate(Low = sum(eval(threshLow))/length(DateTime)*100) %>%
        dplyr::mutate(Medium = sum(eval(threshMed))/length(DateTime)*100) %>%
        dplyr::mutate(High = sum(eval(threshHigh))/length(DateTime)*100) %>%
        distinct(water_year, Medium, High, Low) %>%
        mutate(position= paste(PositionName), Node = NodeName)
      # time_statsx
      
      time_statsx <- rbind(time_statsx, time_stats)
      # startsWith(NodeName, "J", trim=T)
      ### count hours per day
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(eval(threshLow))) %>%
        mutate(Low = if_else(eval(threshLow), row_number(), 0L)) %>%
        ungroup() %>%
        group_by(month, day, water_year, ID02 = data.table::rleid(eval(threshMed))) %>%
        mutate(Medium = if_else(eval(threshMed), row_number(), 0L)) %>%
        ungroup %>%
        group_by(month, day, water_year, ID03 = data.table::rleid(eval(threshHigh))) %>%
        mutate(High = if_else(eval(threshHigh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
      # new_datax
      
      days_data <- rbind(days_data, new_datax)
      
      ## define limits for flow targets
      
      if(min(new_data$prob_fit) > 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else if(max(new_data$prob_fit) < 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else {
        newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.250)
        hy_lim1Low <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.250)
      }
      
      newx1Low
      
      if(length(newx1Low)>2) {
        newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
        hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
      }
      
      ## find roots for each probability - medium
      
      if(min(new_data$prob_fit) > 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else if(max(new_data$prob_fit) < 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else {
        newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
        hy_lim1Med <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.50)
      }
      
      if(length(newx1Med)>2) {
        newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
        hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
      }
      
      ## find roots for each probability - High
      
      if(min(new_data$prob_fit) > 0.75) {
        newx1High<- NA
        hy_lim1High <- NA
      } else if(max(new_data$prob_fit) < 0.75) {
        newx1High <- NA
        hy_lim1High <- NA
      } else {
        newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.750)
        hy_lim1High <- RootLinearInterpolant(new_data$Depth_cm, new_data$prob_fit, 0.750)
      }
      
      if(length(newx1High)>2) {
        newx1High <- c(newx1High[1], newx1High[length(newx1High)])
        hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
      }
      
      ## MAKE DF OF Q LIMITS
      # c(newx1Low, newx1Med, newx1High)
      limits[,p] <- c(newx1Low[1],newx1Low[2], newx1Med[1],newx1Med[2], newx1High[1], newx1High[2])
      H_limits[,p] <- c(hy_lim1Low[1],hy_lim1Low[2],hy_lim1Med[1], hy_lim1Med[2], hy_lim1High[1], hy_lim1High[2])
      # limits
      
    } ## end 2nd loop
    
    Q_Calc$Position <- positions
    # Q_Calc
    Q_Calc <- Q_Calc %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName, Scenario = ScenName)
    
    write.csv(Q_Calc, paste(outdir, "output_data/Chub/01_",NodeName,"_Chub_Adult_Depth_Q_calculation_", ScenName, ".csv", sep=""))
    
    
    limits <- rbind(limits, H_limits)
    # limits
    limits <- limits %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName, Scenario = ScenName)
    
    
    write.csv(limits, paste(outdir, "output_data/Chub/01_",NodeName,"_Chub_Adult_Depth_Q_limits_", ScenName, ".csv", sep=""))
    
    ## percentage time
    melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
    melt_time <- melt_time %>% 
      rename( Probability_Threshold = variable) %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Node = NodeName, Scenario = ScenName)
    
    write.csv(melt_time, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Depth_time_stats_", ScenName, ".csv", sep=""))
    
    ### days per month
    head(days_data)
    days_data <- select(days_data, -c(variable, value, Depth_cm, prob_fit, date_num, month_year, season)) #c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
    
    melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
    melt_data <- rename(melt_data, Probability_Threshold = variable, 
                        consec_hours = value)
    
    
    ## count how many full days i.e. 24 hours
    total_days01 <- melt_data %>% 
      filter(Probability_Threshold == "Low") %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_low = sum(n_days_low))
    
    
    total_days02 <- melt_data %>% 
      filter(Probability_Threshold == "Medium") %>% 
      group_by(ID02, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month02 <- total_days02 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_medium = sum(n_days_medium))
    
    # total_days_per_month02
    
    total_days03 <- melt_data %>% 
      filter(Probability_Threshold == "High") %>% 
      group_by(ID03, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month03 <- total_days03 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_high = sum(n_days_high))
    
    ## combine all thresholds
    total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
    
    
    # # create year_month column       
    total_days <- ungroup(total_days) %>%
      unite(month_year, water_year:month, sep="-", remove=F) %>%
      mutate(Node= paste(NodeName)) #%>%
    
    
    ## change names of columns
    total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
    
    total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
    
    total_days <- total_days %>%
      mutate(season = ifelse(month %in% critical, "critical", "non_critical") )
    
    melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
    melt_days <- melt_days %>%
      rename(Probability_Threshold = variable, n_days = value) %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Depth", Scenario = ScenName)
    
    ## save df
    write.csv(melt_days, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Depth_total_days_long_", ScenName, ".csv", sep="") )
    
    cat(paste("Finished Node", NodeName))
    
  } ## end 1st loop
  
}

# Velocity ----------------------------------------------------------------
# rm(all_data)
## function to scale probability
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


for(c in 1: length(f)) {
  h
  h <- list.files(f[c], pattern = "hydraulic") 
  ScenName <- paste(f[c])
  ScenName
  for(n in 1: length(h)) {
    
    hydraul <- read.csv(file=paste(indir, f[c],"/", h[n], sep=""))
    
    
    ## define nodename
    NodeName <- str_split(h[n], "_", 3)[[1]]
    NodeName <- NodeName[1]
    NodeName
    hyd_dep <- hydraul %>%
      rename(Q = q.cms) %>%
      mutate(date_num = seq(1,length(date), 1))
    
    ## format date time
    hyd_dep$DateTime<-as.POSIXct(hyd_dep$date,
                                 format = "%m/%d/%Y",
                                 tz = "GMT")
    
    hyd_dep <- hyd_dep %>%
      mutate(month = month(DateTime)) %>%
      mutate(year = year(DateTime)) %>%
      mutate(day = day(DateTime)) %>%
      mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))
    
    
    # # ## melt channel position data

    
    hyd_vel <- hyd_dep %>%
      select(DateTime, Q, date_num, month, day, water_year, contains("vel"))

    
    hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
    hyd_vel <- hyd_vel %>% rename(vel_m = value)
    
    ### get depth df for min limit
    hyd_dep <- hyd_dep %>%
      select(DateTime, Q, date_num, month, day, water_year, contains("av.depth"))
    
    # transform m to cm
    hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q",  "date_num", "month", "day", "water_year"))
    hyd_dep <- hyd_dep %>%
      mutate(depth_cm = as.numeric(as.character(value*100)))
    
    hyd_dep <- select(hyd_dep, date_num, depth_cm)
    
    ## join depth data to vel df
    hyd_vel <- left_join(hyd_vel, hyd_dep, by="date_num")
    
    
    ## change NAs to 0 in concrete overbanks
    hyd_vel[is.na(hyd_vel)] <- 0
    
    ## use smooth spline to predict on new data set
    new_values <-smooth.spline(fitdata$velocity_fit, fitdata$prob_fit)
    
    ## probabilities and scale
    all_data <- hyd_vel %>%
      group_by(variable) %>%
      mutate(prob_fit = predict(new_values, vel_m)$y) %>%
      mutate(prob_fit = ifelse(prob_fit <0, 0, prob_fit)) %>%
      mutate(prob_fit  = range01(prob_fit))
    
    ## chub critical season is all year - for probs split into seasons
    # non_critical <- c(1:3,10:12) ## winter months
    critical <- c(1:12) ## summer months
    
    all_datax <- all_data %>%
      mutate(season = ifelse(month %in% critical, "critical", "non_critical") ) %>%
      separate(variable, into = c("Hydraulic", "Position"), sep="_", remove = F) %>%
      mutate(Node = NodeName, Scenario = ScenName)
    
    write.csv(all_datax, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Velocity_prob_stats_all_", ScenName, ".csv", sep=""))
    
    
    prob_stats <- all_datax %>%
      # filter(season == "critical") %>%
      group_by(water_year,variable, Hydraulic, Position, season) %>%
      summarise(MeanProbability = mean(prob_fit), SDProbability = sd(prob_fit)) %>%
      mutate(Species = "Chub", LifeStage = "Adult", Node = NodeName, Scenario = ScenName)
    
    write.csv(prob_stats, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Velocity_prob_stats_annual_", ScenName, ".csv", sep=""))
    
    
    
    ## define positions
    positions <- unique(all_data$variable)
    
    ## Q Limits
    limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
    limits$Type<-c("Q_limitLow", "Q_limitLow","Q_limitMed", "Q_limitMed", "Q_limitHigh", "Q_limitHigh")
    
    H_limits <- as.data.frame(matrix(ncol=length(positions), nrow=6)) 
    H_limits$Type<-c("Hydraulic_limitLow","Hydraulic_limitLow", "Hydraulic_limitMed", 
                     "Hydraulic_limitMed", "Hydraulic_limitHigh", "Hydraulic_limitHigh")
    
    ## calculation
    Q_Calc <- as.data.frame(matrix(nrow=length(positions), ncol=3 ))
    
    names(Q_Calc) <- c("Low", "Medium", "High")
    
    time_statsx <- NULL
    days_data <- NULL
    
    p=2
    for(p in 1:length(positions)) {
      
      # probability as a function of discharge -----------------------------------
      
      new_data <- all_data %>% 
        filter(variable  == positions[p])
      
      new_data$prob_fit[is.na(new_data$prob_fit)] <- 0
      new_data <- na.omit(new_data)
      
      ## define position
      PositionName <- str_split(positions[p], "_", 3)[[1]]
      PositionName <- PositionName[2]
      
      # head(new_data)
      # PositionName
      peak <- new_data %>%
        filter(prob_fit == max(na.omit(prob_fit))) #%>%
      # range(new_data$Depth_cm)
      # range(new_data$prob_fit)
      # write.csv(new_data, "output_data/02_example_node_for_figures.csv")
      # peakQ
      peakQ  <- max(peak$Q)
      min_limit <- filter(new_data, Q > 0)
      min_limit <- min(min_limit$Q)
      #vel_m
      ## find roots for each probability - Low
      
      
      ## define limits for time stats
      
      # newx1Low
      if(min(new_data$prob_fit) > 0.25) {
        newx1Low <- min(new_data$Q)
        hy_lim1Low <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else {
        newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.250)
        hy_lim1Low <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.250)
      }
      
      newx1Low
      
      if(length(newx1Low)>2) {
        newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
        hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
      }
      
      ## find roots for each probability - medium
      
      if(min(new_data$prob_fit) > 0.5) {
        newx1Med <- min(new_data$Q)
        hy_lim1Med <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else {
        newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
        hy_lim1Med <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.50)
      }
      
      if(length(newx1Med)>2) {
        newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
        hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
      }
      
      ## find roots for each probability - High
      
      if(min(new_data$prob_fit) > 0.75) {
        newx1High<- min(new_data$Q)
        hy_lim1High <- min(new_data$Q)
      } else if(max(new_data$prob_fit) < 0.75) {
        newx1High <- NA
        hy_lim1High <- NA
      } else {
        newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.750)
        hy_lim1High <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.750)
      }
      
      if(length(newx1High)>2) {
        newx1High <- c(newx1High[1], newx1High[length(newx1High)])
        hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
      }
      
      
      # create year_month column       
      new_datax <- new_data %>% unite(month_year, water_year:month, sep="-", remove=F) 
      
      # dataframe for stats -----------------------------------------------------
      
      ## define critical period or season for seedling as all year is critical
      new_datax <- new_datax %>%
        mutate(season = ifelse(month %in% critical, "critical", "non_critical" ))
      
      
      # time stats - mid channel ------------------------------------------------
      
      ## Main channel curves
      threshLow <- expression_Q(newx1Low, peakQ) 
      threshLow <-as.expression(do.call("substitute", list(threshLow[[1]], list(limit = as.name("newx1Low")))))
      # threshLow
      threshMed <- expression_Q(newx1Med, peakQ) 
      threshMed <-as.expression(do.call("substitute", list(threshMed[[1]], list(limit = as.name("newx1Med")))))
      # threshMed
      threshHigh <- expression_Q(newx1High, peakQ) 
      threshHigh <-as.expression(do.call("substitute", list(threshHigh[[1]], list(limit = as.name("newx1High")))))
      
      Q_Calc[p,] <- c(paste(threshLow), paste(threshMed), paste(threshHigh))
      ###### calculate amount of time
      
      time_stats <- new_datax %>%
        dplyr::group_by(water_year, season) %>%
        dplyr::mutate(Low = sum(eval(threshLow))/length(DateTime)*100) %>%
        dplyr::mutate(Medium = sum(eval(threshMed))/length(DateTime)*100) %>%
        dplyr::mutate(High = sum(eval(threshHigh))/length(DateTime)*100) %>%
        distinct(water_year, Medium, High, Low) %>%
        mutate(position= paste(PositionName), Node = NodeName)
      # time_statsx
      
      time_statsx <- rbind(time_statsx, time_stats)
      # startsWith(NodeName, "J", trim=T)
      ### count hours per day
      
      new_datax <- new_datax %>%
        ungroup() %>%
        group_by(month, day, water_year, ID01 = data.table::rleid(eval(threshLow))) %>%
        mutate(Low = if_else(eval(threshLow), row_number(), 0L)) %>%
        ungroup() %>%
        group_by(month, day, water_year, ID02 = data.table::rleid(eval(threshMed))) %>%
        mutate(Medium = if_else(eval(threshMed), row_number(), 0L)) %>%
        ungroup %>%
        group_by(month, day, water_year, ID03 = data.table::rleid(eval(threshHigh))) %>%
        mutate(High = if_else(eval(threshHigh), row_number(), 0L)) %>%
        mutate(position= paste(PositionName), Node = NodeName)
      # new_datax
      
      days_data <- rbind(days_data, new_datax)
      
      ## define limits for flow targets
      
      if(min(new_data$prob_fit) > 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else if(max(new_data$prob_fit) < 0.25) {
        newx1Low <- NA
        hy_lim1Low <- NA
      } else {
        newx1Low <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.250)
        hy_lim1Low <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.250)
      }
      
      newx1Low
      
      if(length(newx1Low)>2) {
        newx1Low <- c(newx1Low[1], newx1Low[length(newx1Low)])
        hy_lim1Low<- c(hy_lim1Low[1], hy_lim1Low[length(hy_lim1Low)])
      }
      
      ## find roots for each probability - medium
      
      if(min(new_data$prob_fit) > 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else if(max(new_data$prob_fit) < 0.5) {
        newx1Med <- NA
        hy_lim1Med <- NA
      } else {
        newx1Med <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.50)
        hy_lim1Med <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.50)
      }
      
      if(length(newx1Med)>2) {
        newx1Med <- c(newx1Med[1], newx1Med[length(newx1Med)])
        hy_lim1Med<- c(hy_lim1Med[1], hy_lim1Med[length(hy_lim1Med)])
      }
      
      ## find roots for each probability - High
      
      if(min(new_data$prob_fit) > 0.75) {
        newx1High<- NA
        hy_lim1High <- NA
      } else if(max(new_data$prob_fit) < 0.75) {
        newx1High <- NA
        hy_lim1High <- NA
      } else {
        newx1High <- RootLinearInterpolant(new_data$Q, new_data$prob_fit, 0.750)
        hy_lim1High <- RootLinearInterpolant(new_data$vel_m, new_data$prob_fit, 0.750)
      }
      
      if(length(newx1High)>2) {
        newx1High <- c(newx1High[1], newx1High[length(newx1High)])
        hy_lim1High<- c(hy_lim1High[1], hy_lim1High[length(hy_lim1High)])
      }
      
      ## MAKE DF OF Q LIMITS
      # c(newx1Low, newx1Med, newx1High)
      limits[,p] <- c(newx1Low[1],newx1Low[2], newx1Med[1],newx1Med[2], newx1High[1], newx1High[2])
      H_limits[,p] <- c(hy_lim1Low[1],hy_lim1Low[2],hy_lim1Med[1], hy_lim1Med[2], hy_lim1High[1], hy_lim1High[2])
      # limits
      
    } ## end 2nd loop
    
    Q_Calc$Position <- positions
    
    Q_Calc <- Q_Calc %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName, Scenario = ScenName)
    
    write.csv(Q_Calc, paste(outdir, "output_data/Chub/01_",NodeName,"_Chub_Adult_Velocity_Q_calculation_", ScenName, ".csv", sep=""))
    
    
    limits <- rbind(limits, H_limits)
    limits
    ## note that 0.1 upper/lower limit is max/min Q to adhere to 0.1 bound
    limits <- limits %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName, Scenario = ScenName)
    
    
    write.csv(limits, paste(outdir, "output_data/Chub/01_",NodeName,"_Chub_Adult_Velocity_Q_limits_", ScenName, ".csv", sep=""))
    ## percentage time
    melt_time<-reshape2::melt(time_statsx, id=c("season", "position", "water_year", "Node"))
    melt_time <- melt_time %>% 
      rename( Probability_Threshold = variable) %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Node = NodeName, Scenario = ScenName)
    
    write.csv(melt_time, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Velocity_time_stats_", ScenName, ".csv", sep=""))
    
    ### days per month
    head(days_data)
    days_data <- select(days_data, -c(variable, depth_cm, vel_m, prob_fit, date_num, month_year, season)) #c(Q, month, water_year, day, ID01, Mid, position, DateTime, Node) )# all probs
    
    melt_data<-reshape2::melt(days_data, id=c("ID01", "ID02", "ID03", "day", "month", "water_year", "Q", "position", "Node", "DateTime"))
    melt_data <- rename(melt_data, Probability_Threshold = variable, 
                        consec_hours = value)
    
    
    ## count how many full days i.e. 24 hours
    total_days01 <- melt_data %>% 
      filter(Probability_Threshold == "Low") %>% 
      group_by(ID01, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    ## count the number of days in each month
    total_days_per_month01 <- total_days01 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_low = sum(n_days_low))
    
    
    total_days02 <- melt_data %>% 
      filter(Probability_Threshold == "Medium") %>% 
      group_by(ID02, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month02 <- total_days02 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_medium = sum(n_days_medium))
    
    # total_days_per_month02
    
    total_days03 <- melt_data %>% 
      filter(Probability_Threshold == "High") %>% 
      group_by(ID03, day, month, water_year, position) %>%
      summarise(n_hours = max(consec_hours))  %>%
      mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%
    
    total_days_per_month03 <- total_days03 %>%
      group_by(month, water_year, position) %>%
      summarise(days_per_month_high = sum(n_days_high))
    
    ## combine all thresholds
    total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
    
    
    # # create year_month column       
    total_days <- ungroup(total_days) %>%
      unite(month_year, water_year:month, sep="-", remove=F) %>%
      mutate(Node= paste(NodeName)) #%>%
    
    
    ## change names of columns
    total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)
    
    total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
    
    total_days <- total_days %>%
      mutate(season = ifelse(month %in% critical, "critical", "non_critical") )
    
    
    melt_days<-reshape2::melt(total_days, id=c("month_year", "water_year", "month", "season", "position", "Node"))
    melt_days <- melt_days %>%
      rename(Probability_Threshold = variable, n_days = value) %>%
      mutate(Species ="Chub", Life_Stage = "Adult", Hydraulic = "Velocity", Scenario = ScenName)
    
    ## save df
    write.csv(melt_days, paste(outdir, "output_data/Chub/01_", NodeName, "_Chub_Adult_Velocity_total_days_long_", ScenName, ".csv", sep="") )
    
    cat(paste("Finished Node", NodeName))
    # rm(list = ls())
    
  } ## end 1st loop
  
}
