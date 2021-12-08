## adding extra chub data and new model

getwd()

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)
        

setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_Tier_3_New")

# upload data

micro_avail <- read.csv("input_data/SAR 2015 Microhabitat Availability.csv")
micro_use <- read.csv("input_data/SAR 2015 Microhabitat Use.csv")
fish_data <- read.csv("input_data/SAR 2015 Reach Fish Data.csv")

dim(chub_data) ## n with NV = 1368, length without NV = 393
chub_data <- fish_data %>%
  filter(Common.name == "Arroyo Chub") 


colnames(micro_avail)[8:13] <- c("Transect", "ch_width_m", "Depth_cm", "Velocity_0.6_ms","Velocity_0.2_ms", "Velocity_0.8_ms")
micro_avail <- micro_avail[,-c(4, 19,20)]

## make coord code
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude, sep="")
# unique(micro_avail$coord_code) ## 17
# head(micro_avail)


names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
head(micro_use)
micro_use$coord_code <- paste(micro_use$Latitude, "_", micro_use$Longitude, sep="")
unique(micro_use$coord_code) # 11 sites in total
dim(micro_use)

unique(micro_avail$coord_code) %in% unique(micro_use$coord_code) 

#  make sure species name is homogenous
# micro_usex
micro_use$Species <- gsub("Santa Ana sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Santa Ana Sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Arroyo Chub", "ArroyoChub", micro_use$Species)
micro_use$Species <- gsub("Yellow bullhead", "Yellowbullhead", micro_use$Species)
micro_use$Species <- gsub("Western Misquitofish", "WesternMisquitofish", micro_use$Species)
fish <- unique(na.omit(micro_use$Species))
fish


micro_suck <- subset(micro_use, Species %in% fish)
dim(micro_suck) # 334 27

unique(micro_suck$coord_code) 
unique(micro_avail$coord_code) 

### create df pf presence/absence (abundance/absence)
## format each df - availability and use
head(micro_avail)
head(micro_suck)
names(micro_avail)
micro_availx <- micro_avail %>%
  select(Section, Latitude, Longitude, Date,  Depth_cm, Velocity_0.6_ms:Velocity_0.8_ms, coord_code) %>%
  mutate("SantaAnaSucker" = 0, "ArroyoChub" = 0, "Yellowbullhead" = 0, "WesternMisquitofish" = 0)

names(micro_suck)
micro_suckx <- micro_suck %>%
  pivot_wider(names_from = "Species", values_from = "Number") %>%
  select(Section, Latitude, Longitude, Date, Depth_cm, Velocity_0.6_ms:Velocity_0.8_ms, SantaAnaSucker:WesternMisquitofish, coord_code)

## change all NAs to 0

micro_suckx$SantaAnaSucker[is.na(micro_suckx$SantaAnaSucker)] <- 0
micro_suckx$ArroyoChub[is.na(micro_suckx$ArroyoChub)] <- 0
micro_suckx$Yellowbullhead[is.na(micro_suckx$Yellowbullhead)] <- 0
micro_suckx$WesternMisquitofish[is.na(micro_suckx$WesternMisquitofish)] <- 0

## find locations not in use df

presentlocations <- which(unique(micro_availx$coord_code) %in% unique(micro_suckx$coord_code))
removetheselocations <- unique(micro_suckx$coord_code)
removetheselocations
micro_avail_abs <- micro_availx %>%
  filter(!coord_code %in% removetheselocations)

## check
unique(micro_avail_abs$coord_code) %in% unique(micro_suckx$coord_code)
unique(micro_suckx$coord_code) %in% unique(micro_avail_abs$coord_code) ## no matches

## combine dfs
head(micro_avail_abs)
head(micro_suckx)

names(micro_avail_abs)
names(micro_suckx)

str(micro_avail_abs)
str(micro_suckx)

## format 
micro_avail_abs$Velocity_0.2_ms <- as.numeric(micro_avail_abs$Velocity_0.2_ms)
micro_avail_abs$Velocity_0.8_ms <- as.numeric(micro_avail_abs$Velocity_0.8_ms)

micro_suckx$SantaAnaSucker <- as.numeric(micro_suckx$SantaAnaSucker)
micro_suckx$ArroyoChub <- as.numeric(micro_suckx$ArroyoChub)
micro_suckx$Yellowbullhead <- as.numeric(micro_suckx$Yellowbullhead)
micro_suckx$WesternMisquitofish <- as.numeric(micro_suckx$WesternMisquitofish)

data_pa <- bind_rows(micro_avail_abs, micro_suckx)

#  as all adults, only micro_suck needed

# save 

write.csv(data_pa, "output_data/00_all_species_pres_abs_wulff_2015.csv")

#  SAR 2016 

# upload data

micro_avail <- read.csv("input_data/SAR 2016 Microhabitat Availability Data.csv")
micro_use <- read.csv("input_data/SAR 2016 Microhabitat Use Data.csv")
fish_data <- read.csv("input_data/SAR 2016 Reach Fish Data_v3_2.05.2019.csv")
reach_data <- read.csv("input_data/SAR 2016 Reach Habitat Data.csv")

head(fish_data)
dim(chub_data) ## n with NV = 778, length without NV = 393
chub_data <- fish_data %>%
  filter(Common.Name == "Arroyo Chub") %>%
  mutate(Total.length = as.numeric(Total.Length..mm.), Fork.length = as.numeric(Fork.Length..mm.))
head(micro_avail)

colnames(micro_avail)[7:9] <- c("ch_width_m", "Depth_cm", "Velocity_0.6_ms")
# micro_avail <- micro_avail[,-c(4, 19,20)]
names(micro_avail)
names(micro_use)
## make coord code
micro_avail$coord_code <- paste(micro_avail$Latitude, "_", micro_avail$Longitude,  sep="")
unique(micro_avail$coord_code) ## 40
head(micro_avail)

names(micro_use) [14:20] <- c("length_mm", "Distance_fish_abv_bottom_cm", "Velocity_at_fish_ms", "Depth_cm", "Velocity_0.6_ms", "Velocity_0.2_ms", "Velocity_0.8_ms")
names(micro_use)
micro_use$coord_code_e <- paste(micro_use$Ending.Latitude, "_", micro_use$Ending.Longitude, sep="")
# "34.03493_-117.35665" 
# "34.03561_-117.35677" 
# "34.03776_-117.35596" ## 33

micro_use$coord_code_s <- paste(micro_use$Starting.Latitude, "_", micro_use$Starting.Longitude, sep="")
#"34.03299_-117.3561" 
# "34.03493_-117.35665" 
#"34.03662_-117.35625" number 22
unique(micro_use$coord_code) # 3 sites in total
dim(micro_use)

unique(micro_avail$coord_code) %in% unique(micro_use$coord_code_e) 
unique(micro_avail$coord_code) %in% unique(micro_use$coord_code_s) 
av_codes <- paste(unique(micro_avail$coord_code)[22], unique(micro_avail$coord_code)[33])
av_codes
## find locations to check if they are the same

av_test <- micro_avail %>%
  filter(coord_code %in% c("34.03776_-117.35596", "34.03662_-117.35625"))

use_test <- micro_use %>%
  filter(coord_code_e == "34.03776_-117.35596", 
         coord_code_s == "34.03662_-117.35625")

### start and end point relates to one location in availability. just use presence for this df

#  make sure species name is homogenous
# micro_usex
# micro_use$Species <- gsub("Santa Ana sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Santa Ana Sucker", "SantaAnaSucker", micro_use$Species)
micro_use$Species <- gsub("Arroyo Chub", "ArroyoChub", micro_use$Species)
# micro_use$Species <- gsub("Yellow bullhead", "Yellowbullhead", micro_use$Species)
# micro_use$Species <- gsub("Western Misquitofish", "WesternMisquitofish", micro_use$Species)
fish <- unique(na.omit(micro_use$Species))
fish


micro_suck <- subset(micro_use, Species %in% fish)
dim(micro_suck) # 144 27

unique(micro_suck$coord_code) 
unique(micro_avail$coord_code) 

### create df pf presence/absence (abundance/absence)
## format each df - availability and use

micro_availx <- micro_avail %>%
  select(Section, Latitude, Longitude, Date,  Depth_cm, Velocity_0.6_ms, coord_code) %>%
  mutate("SantaAnaSucker" = 0, "ArroyoChub" = 0)

names(micro_suck)
micro_suckx <- micro_suck %>%
  pivot_wider(names_from = "Species", values_from = "Count") %>%
  select(Section, Starting.Latitude, Starting.Longitude, Date, Depth_cm, Velocity_0.6_ms, 
         SantaAnaSucker:ArroyoChub, coord_code_s) %>%
  rename(Latitude = Starting.Latitude, Longitude = Starting.Longitude, coord_code = coord_code_s)

## change all NAs to 0

micro_suckx$SantaAnaSucker[is.na(micro_suckx$SantaAnaSucker)] <- 0
micro_suckx$ArroyoChub[is.na(micro_suckx$ArroyoChub)] <- 0


## find locations not in use df

presentlocations <- which(unique(micro_availx$coord_code) %in% unique(micro_suckx$coord_code))
removetheselocations <- unique(micro_suckx$coord_code)
removetheselocations
micro_avail_abs <- micro_availx %>%
  filter(!coord_code %in% removetheselocations)

## check
unique(micro_avail_abs$coord_code) %in% unique(micro_suckx$coord_code)
unique(micro_suckx$coord_code) %in% unique(micro_avail_abs$coord_code) ## no matches

## combine dfs


data_pa <- bind_rows(micro_avail_abs, micro_suckx)
# save 

write.csv(data_pa, "output_data/00_all_species_pres_abs_wulff_2016.csv")

## upload data and merge

depth2015 <- read.csv("output_data/00_all_species_pres_abs_wulff_2015.csv")
depth2016 <- read.csv("output_data/00_all_species_pres_abs_wulff_2016.csv")

names(depth2015)
names(depth2016)
head(depth2015)
head(depth2016)

depth2015 <- depth2015 %>%
  pivot_longer(SantaAnaSucker:WesternMisquitofish, names_to = "Species", values_to = "Abundance") %>%
  select(Section:Velocity_0.6_ms, coord_code:Abundance) %>%
  mutate(Year = 2015, Source = "Wulff_etal")


depth2016 <- depth2016 %>%
  pivot_longer(SantaAnaSucker:ArroyoChub, names_to = "Species", values_to = "Abundance") %>%
  select(-X) %>%
  mutate(Year = 2016, Source = "Wulff_etal")

str(depth2015)
str(depth2016)

depth2016 <- depth2016 %>%
  mutate(Section = as.character(Section), Latitude = as.character(Latitude), Longitude = as.character(Longitude),
         Depth_cm = as.numeric(Depth_cm))

depth <- bind_rows(depth2016, depth2015)

write.csv(depth, "output_data/00_Wulff_ALL_depth_vel_abundance.csv")


# Model -------------------------------------------------------------------

library(tidyverse)

data <- read.csv("output_data/00_Wulff_ALL_depth_vel_abundance.csv")
head(data)

dataSum <- data %>%
  group_by(coord_code, Date, Species, Year) %>%
  mutate(TotalAbundance = sum(Abundance)) %>%
  group_by(Depth_cm) %>%
  mutate(RelAbundance = (Abundance/TotalAbundance)*100)

dataSum$RelAbundance[is.na(dataSum$RelAbundance)] <- 0


depth_chub <- dataSum %>% filter(Species == "ArroyoChub") 

## make presence absence column

chub <- depth_chub %>%
  ungroup() %>%
  mutate(presence_absence = ifelse(Abundance == 0,0,1)) %>%
  # filter(Year == 2015) %>% ## remove 2016 to check
  mutate(Depth_m = Depth_cm/100) ## change to m

##### model to use

chub_depth_mod <- glm(presence_absence~Depth_cm, family=binomial(link="logit"), data=chub)
xdepth <- seq(0, 120, 0.01)
ydepth <- predict(chub_depth_mod, list(Depth_cm = xdepth),type="response")
getwd()

png("figures/00_chub_Adult_depth_Prob_curve.png", width = 700, height = 700)
par(mar=c(5,7,4,2,))
plot(chub$Depth_cm, chub$presence_absence, cex.main = 2.5, cex.axis=2.5, cex.lab=2.5, pch = 16, xlab = "Depth (cm)", ylab = "Probability of Occurrence")
par(new=TRUE)
#plot the line with no axes or labels
plot(xdepth, ydepth, axes=FALSE, ylab = "", type='l', col='red', main = "",
     xlab="")

dev.off()
anova(chub_depth_mod, test="Chi")

save(chub_depth_mod, file="models_functions/chub_depth_mod.RData")

n#### velocity
head(chub)
vel_freq <- rename(chub, Velocity = Velocity_0.6_ms)
vel_freq <- na.omit(vel_freq)
## probability curve
vel_freq$Scaled_Vel <-scale(vel_freq$Velocity, scale=T, center=T)
scaled_x <- vel_freq$Scaled_Vel
h <- hist(scaled_x, plot=F)
xfit<-seq(min(scaled_x),max(scaled_x),length=1000)
yfit<-dnorm(xfit,mean=mean(scaled_x),sd=sd(scaled_x))

## x axis with raw velocity values
xfit_r <- seq(min(vel_freq$Velocity), max(vel_freq$Velocity), length=1000)
head(vel_freq)


## plot curve with raw depth axis
png("figures/00_chub_Adult_velocity_Prob_curve.png", width = 700, height = 700)

par(mar=c(5,6,4,2))
plot(xfit_r, yfit, axes=FALSE, xlab='', ylab='', type='l', col='', main = "" )
axis(1, at=pretty(xfit_r), cex.axis=2)
par(new=TRUE)
#plot the line with no axes or labels
plot(xfit, yfit, axes=FALSE, xlab='Velocity (m/s)', ylab='', type='l', col='red', main = "",
     cex.main = 2.5, cex.axis=2.5, cex.lab=2.5)
title(ylab='Probability Distribution', line = 3.5, cex.lab = 2)
## add 1sd shift
par(new=TRUE)

#add these now with axis

axis(2, at=pretty(range(yfit)), cex.axis=2)
dev.off()
## data frame with probabilities and depth

fitdata <- data.frame(matrix(ncol=2, nrow=length(yfit)))
fitdata[,1] <- xfit_r
fitdata[,2] <- yfit
colnames(fitdata) <- c("velocity_fit", "prob_fit")

write.csv(fitdata, "output_data/00_chub_adult_velocity_prob_curve_data.csv")




