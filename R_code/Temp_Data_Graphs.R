
# Purpose: This script creates figures relating to predicted changes in
#   temperature for the San Francisco Estuary (Bay-Delta). It uses
#   data from downscaled climate models (Wulff et al. 2021). Some graphs
#   are intended to mimic those in Brown et al. (2016), which looked at the
#   effects of temperature on Delta Smelt. Here, we update these figures and
#   create original figures which are formatted to apply more directly to the
#   effects of climate change on Longfin Smelt. Figures that appeared in the
#   Species Status Assessment (SSA) for the Bay-Delta DPS of Longfin Smelt
#   are labeled as such in the code.

# Authors:
#   Vanessa D. Tobias, USFWS
#   Craig Anderson, USFWS

## Data source: 
# Wulff, M.L., Brown, L.R., Huntsman, B.M., Knowles, N., and Wagner, W., 2021,
# Data used in projected air and water temperatures for selected regions of the 
# upper San Francisco Estuary and Yolo Bypass under 20 scenarios of climate 
# change, U.S. Geological Survey data release https://doi.org/10.5066/P9CXGU44
## Citation:
# Brown, L.R., Komoroske, L.M., Wagner, R.W., Morgan-King, T., May, J.T., 
#   Connon, R.E. and Fangue, N.A. 2016. Coupled downscaled climate models 
#   and ecophysiological metrics forecast habitat compression for an endangered 
#   estuarine fish. PloS one, 11(1), p.e0146724.

#### Site Codes:
# DEC, Sacramento River at Decker Island
# DWS, Sacramento Deepwater Shipping Channel
# JPT, San Joaquin River at Jersey Point
# LCS, Lower Cache Slough
# LIB, Liberty Island at breach 
#       (This site was not used in the paper. The hydrodynamics at the 
#        breach can be weird and there is a deep hole
#        there so I didn't consider it representative of 
#        Liberty Island - Larry Brown)
# LIW, Liberty Island a little north of the breach.
# MIN, Miner Slough
# UCS, Upper Cache Slough
# MAL, Mallard Island
# MRZ, Martinez

# 1. Setup ####
## Load Packages ####
library(readxl)
library(tidyverse)
library(lubridate)
library(plotrix)
library(RColorBrewer)

## Functions ####
countOver <- function(x, number){
  sum(x >= number)
}

countUnder <- function(x, number){
  sum(x < number)
}

# 2. Read Data ####
Tavg_RCP45. <- read_excel("./Data_original/Tavg_RCP45.xlsx")
Tavg_RCP85. <- read_excel("./Data_original/Tavg_RCP85.xlsx")

# 3. Count days over thresholds of interest ####
## -- RCP 4.5
tempEx_RCP45. <- Tavg_RCP45. %>%
  group_by(year) %>% 
            # -- Count days over 22°C
  summarise("DEC22" = countOver(DEC, 22),
            "DWS22" = countOver(DWS, 22),
            "JPT22" = countOver(JPT, 22),
            "LCS22" = countOver(LCS, 22),
            "LIB22" = countOver(LIB, 22), 
            "LIW22" = countOver(LIW, 22), 
            "MIN22" = countOver(MIN, 22), 
            "UCS22" = countOver(UCS, 22),
            "MAL22" = countOver(MAL, 22),
            "MRZ22" = countOver(MRZ, 22),
            
            # -- Count days over 20°C
            "DEC20" = countOver(DEC, 20),
            "DWS20" = countOver(DWS, 20),
            "JPT20" = countOver(JPT, 20),
            "LCS20" = countOver(LCS, 20),
            "LIB20" = countOver(LIB, 20), 
            "LIW20" = countOver(LIW, 20), 
            "MIN20" = countOver(MIN, 20), 
            "UCS20" = countOver(UCS, 20),
            "MAL20" = countOver(MAL, 20),
            "MRZ20" = countOver(MRZ, 20),
            
            # -- Count days under 14°C
            "DEC14" = countUnder(DEC, 14),
            "DWS14" = countUnder(DWS, 14),
            "JPT14" = countUnder(JPT, 14),
            "LCS14" = countUnder(LCS, 14),
            "LIB14" = countUnder(LIB, 14), 
            "LIW14" = countUnder(LIW, 14), 
            "MIN14" = countUnder(MIN, 14), 
            "UCS14" = countUnder(UCS, 14),
            "MAL14" = countUnder(MAL, 14),
            "MRZ14" = countUnder(MRZ, 14),
            
            # -- Count days under 12°C
            "DEC12" = countUnder(DEC, 12),
            "DWS12" = countUnder(DWS, 12),
            "JPT12" = countUnder(JPT, 12),
            "LCS12" = countUnder(LCS, 12),
            "LIB12" = countUnder(LIB, 12), 
            "LIW12" = countUnder(LIW, 12), 
            "MIN12" = countUnder(MIN, 12), 
            "UCS12" = countUnder(UCS, 12),
            "MAL12" = countUnder(MAL, 12),
            "MRZ12" = countUnder(MRZ, 12))
ungroup(Tavg_RCP45.)


# -- RCP 8.5
tempEx_RCP85. <- Tavg_RCP85. %>% 
  group_by(year) %>% 
            # -- Count days over 22°C
  summarise("DEC22" = countOver(DEC, 22),
            "DWS22" = countOver(DWS, 22),
            "JPT22" = countOver(JPT, 22),
            "LCS22" = countOver(LCS, 22),
            "LIB22" = countOver(LIB, 22), 
            "LIW22" = countOver(LIW, 22), 
            "MIN22" = countOver(MIN, 22), 
            "UCS22" = countOver(UCS, 22),
            "MAL22" = countOver(MAL, 22),
            "MRZ22" = countOver(MRZ, 22),
            
            # -- Count days over 20°C
            "DEC20" = countOver(DEC, 20),
            "DWS20" = countOver(DWS, 20),
            "JPT20" = countOver(JPT, 20),
            "LCS20" = countOver(LCS, 20),
            "LIB20" = countOver(LIB, 20), 
            "LIW20" = countOver(LIW, 20), 
            "MIN20" = countOver(MIN, 20), 
            "UCS20" = countOver(UCS, 20),
            "MAL20" = countOver(MAL, 20),
            "MRZ20" = countOver(MRZ, 20),
            
            # -- Count days under 14°C
            "DEC14" = countUnder(DEC, 14),
            "DWS14" = countUnder(DWS, 14),
            "JPT14" = countUnder(JPT, 14),
            "LCS14" = countUnder(LCS, 14),
            "LIB14" = countUnder(LIB, 14), 
            "LIW14" = countUnder(LIW, 14), 
            "MIN14" = countUnder(MIN, 14), 
            "UCS14" = countUnder(UCS, 14),
            "MAL14" = countUnder(MAL, 14),
            "MRZ14" = countUnder(MRZ, 14),
            
            # -- Count days under 12°C
            "DEC12" = countUnder(DEC, 12),
            "DWS12" = countUnder(DWS, 12),
            "JPT12" = countUnder(JPT, 12),
            "LCS12" = countUnder(LCS, 12),
            "LIB12" = countUnder(LIB, 12), 
            "LIW12" = countUnder(LIW, 12), 
            "MIN12" = countUnder(MIN, 12), 
            "UCS12" = countUnder(UCS, 12),
            "MAL12" = countUnder(MAL, 12),
            "MRZ12" = countUnder(MRZ, 12))
ungroup(Tavg_RCP85.)

## Combine data frames of counts for RCP 4.5 and RCP 8.5 ####
tempExRCP <- merge(tempEx_RCP45., tempEx_RCP85.,
                by = "year",
                suffixes = c("_RCP45", "_RCP85"))

## Make a vector for decades ####
# dataset starts with 2011, so decades start on the 1's
tempExRCP$decade <- "2010s" # 2011:2020
tempExRCP$decade[which(tempExRCP$year > 2020)] <- "2020s" # 2021:2030
tempExRCP$decade[which(tempExRCP$year > 2030)] <- "2030s"
tempExRCP$decade[which(tempExRCP$year > 2040)] <- "2040s"
tempExRCP$decade[which(tempExRCP$year > 2050)] <- "2050s"
tempExRCP$decade[which(tempExRCP$year > 2060)] <- "2060s"
tempExRCP$decade[which(tempExRCP$year > 2070)] <- "2070s"
tempExRCP$decade[which(tempExRCP$year > 2080)] <- "2080s"
tempExRCP$decade[which(tempExRCP$year > 2090)] <- "2090s"

# Make decade a factor:
tempExRCP$decade <- as.factor(tempExRCP$decade)


# 4. Calculate summary stats ####
# -- RCP 4.5
tempStat_RCP45. <- Tavg_RCP45. %>% 
  group_by(year) %>% 
  summarise("DECmean" = mean(DEC),
            "DWSmean" = mean(DWS),
            "JPTmean" = mean(JPT),
            "LCSmean" = mean(LCS),
            "LIBmean" = mean(LIB), 
            "LIWmean" = mean(LIW), 
            "MINmean" = mean(MIN), 
            "UCSmean" = mean(UCS),
            "MALmean" = mean(MAL),
            "MRZmean" = mean(MRZ),
            
            "DECmin" = min(DEC),
            "DWSmin" = min(DWS),
            "JPTmin" = min(JPT),
            "LCSmin" = min(LCS),
            "LIBmin" = min(LIB), 
            "LIWmin" = min(LIW), 
            "MINmin" = min(MIN), 
            "UCSmin" = min(UCS),
            "MALmin" = min(MAL),
            "MRZmin" = min(MRZ),

            "DECmax" = max(DEC),
            "DWSmax" = max(DWS),
            "JPTmax" = max(JPT),
            "LCSmax" = max(LCS),
            "LIBmax" = max(LIB), 
            "LIWmax" = max(LIW), 
            "MINmax" = max(MIN), 
            "UCSmax" = max(UCS),
            "MALmax" = max(MAL),
            "MRZmax" = max(MRZ))
ungroup(Tavg_RCP45.)

# -- RCP 8.5
tempStat_RCP85. <- Tavg_RCP85. %>% 
  group_by(year) %>% 
  summarise("DECmean" = mean(DEC),
            "DWSmean" = mean(DWS),
            "JPTmean" = mean(JPT),
            "LCSmean" = mean(LCS),
            "LIBmean" = mean(LIB), 
            "LIWmean" = mean(LIW), 
            "MINmean" = mean(MIN), 
            "UCSmean" = mean(UCS),
            "MALmean" = mean(MAL),
            "MRZmean" = mean(MRZ),
            
            "DECmin" = min(DEC),
            "DWSmin" = min(DWS),
            "JPTmin" = min(JPT),
            "LCSmin" = min(LCS),
            "LIBmin" = min(LIB), 
            "LIWmin" = min(LIW), 
            "MINmin" = min(MIN), 
            "UCSmin" = min(UCS),
            "MALmin" = min(MAL),
            "MRZmin" = min(MRZ),
            
            "DECmax" = max(DEC),
            "DWSmax" = max(DWS),
            "JPTmax" = max(JPT),
            "LCSmax" = max(LCS),
            "LIBmax" = max(LIB), 
            "LIWmax" = max(LIW), 
            "MINmax" = max(MIN), 
            "UCSmax" = max(UCS),
            "MALmax" = max(MAL),
            "MRZmax" = max(MRZ))
ungroup(Tavg_RCP85.)

# Combine data frames of stats for RCP 4.5 and RCP 8.5
tempStatRCP <- merge(tempStat_RCP45., tempStat_RCP85.,
                by = "year",
                #suffixes = c("_gfdla2", "_gfdlb1"))
                suffixes = c("_RCP45", "_RCP85"))


## Make a vector for decades ####
# dataset starts with 2011, so decades start on the 1's

tempStatRCP$decade <- "2010s" # 2011:2020
tempStatRCP$decade[which(tempStatRCP$year > 2020)] <- "2020s" # 2021:2030
tempStatRCP$decade[which(tempStatRCP$year > 2030)] <- "2030s"
tempStatRCP$decade[which(tempStatRCP$year > 2040)] <- "2040s"
tempStatRCP$decade[which(tempStatRCP$year > 2050)] <- "2050s"
tempStatRCP$decade[which(tempStatRCP$year > 2060)] <- "2060s"
tempStatRCP$decade[which(tempStatRCP$year > 2070)] <- "2070s"
tempStatRCP$decade[which(tempStatRCP$year > 2080)] <- "2080s"
tempStatRCP$decade[which(tempStatRCP$year > 2090)] <- "2090s"

# Make decade a factor:
tempStatRCP$decade <- as.factor(tempStatRCP$decade)

## Calculate decadal summaries ####
# If you use a different dataset, double check column numbers
tempExDecadeRCP <- data.frame(decade = unique(tempExRCP$decade))
for(i in 2:81){  
  tempExDecadeRCP[, i] <- as.vector(tapply(X = tempExRCP[,i],
                                           INDEX = tempExRCP$decade,
                                           FUN = median))
  names(tempExDecadeRCP)[i] <- paste0(names(tempExRCP[i]), "_med")
}

for(i in 2:81){
  tempExDecadeRCP[, i+80] <- as.vector(tapply(X = tempExRCP[,i],
                                              INDEX = tempExRCP$decade,
                                              FUN = min))
  names(tempExDecadeRCP)[i+80] <- paste0(names(tempExRCP[i]), "_min")
}

for(i in 2:81){
  tempExDecadeRCP[, i+160] <- as.vector(tapply(X = tempExRCP[,i],
                                           INDEX = tempExRCP$decade,
                                           FUN = max))
  names(tempExDecadeRCP)[i+160] <- paste0(names(tempExRCP[i]), "_max")
}

tempStatDecadeRCP <- data.frame(decade = unique(tempStatRCP$decade))
for(i in 2:61){    
  tempStatDecadeRCP[, i] <- as.vector(tapply(X = tempStatRCP[,i],
                                        INDEX = tempStatRCP$decade,
                                        FUN = mean))
  names(tempStatDecadeRCP)[i] <- paste0(names(tempStatRCP[i]), "_mean")
}

for(i in 2:61){    
  tempStatDecadeRCP[, i+60] <- as.vector(tapply(X = tempStatRCP[,i],
                                            INDEX = tempStatRCP$decade,
                                            FUN = min))
  names(tempStatDecadeRCP)[i+60] <- paste0(names(tempStatRCP[i]), "_min")
}

for(i in 2:61){    
  tempStatDecadeRCP[, i+120] <- as.vector(tapply(X = tempStatRCP[,i],
                                            INDEX = tempStatRCP$decade,
                                            FUN = max))
  names(tempStatDecadeRCP)[i+120] <- paste0(names(tempStatRCP[i]), "_max")
}

## Write decadal summary tables ####
write.csv(tempStatDecadeRCP, "./Data_Derived/tempStatDecadeRCP.csv")
write.csv(tempExDecadeRCP, "./Data_Derived//tempExDecadeRCP.csv")

# 5. Figures ####

## Medians with min & max ####  
# These figures in the SSA were composites of graphs for five locations
#   superimposed on a map of the upper San Francisco Estuary (Suisun Bay
#   and the Delta). The following code creates the individual figures for
#   each location. The code for the background map can be found here:
#   ./R_code/Delta_Map_Figure.R

## Fig 4.6 Days over 22°C ####

png("./Figures/Decker_22_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         DEC22_RCP85_med,
         ui = DEC22_RCP85_max,
         li = DEC22_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days > 22°C",
         main = "")
  title(main = "Decker Island", 
        adj = 0.05, line = -1.3, 
        font.main = 1, 
        cex.main = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         DEC22_RCP45_med,
         ui = DEC22_RCP45_max,
         li = DEC22_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/DWS_22_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         DWS22_RCP85_med,
         ui = DWS22_RCP85_max,
         li = DWS22_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days > 22°C",
         main = "")
  title(main = "Deepwater Ship Channel", adj = 0.05, line = -1.3, font.main = 1, cex.main = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         DWS22_RCP45_med,
         ui = DWS22_RCP45_max,
         li = DWS22_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Mallard_22_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         MAL22_RCP85_med,
         ui = MAL22_RCP85_max,
         li = MAL22_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days > 22°C",
         main = "")
  title(main = "Mallard Island", 
        adj = 0.05, 
        line = -1.3, 
        font.main = 1, 
        cex.main = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         MAL22_RCP45_med,
         ui = MAL22_RCP45_max,
         li = MAL22_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")

  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Martinez_22_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         MRZ22_RCP85_med,
         ui = MRZ22_RCP85_max,
         li = MRZ22_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days > 22°C",
         main = "")
  title(main = "Martinez", adj = 0.05, line = -1.3, font.main = 1, cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         MRZ22_RCP45_med,
         ui = MRZ22_RCP45_max,
         li = MRZ22_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Jersey_22_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         JPT22_RCP85_med,
         ui = JPT22_RCP85_max,
         li = JPT22_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days > 22°C",
         main = "")
  title(main = "Jersey Point", 
        adj = 0.05, 
        line = -1.3, 
        font.main = 1, 
        cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         JPT22_RCP45_med,
         ui = JPT22_RCP45_max,
         li = JPT22_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

## Fig 4.11 Days under 14°C ####
png("./Figures/Decker_14_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         DEC14_RCP85_med,
         ui = DEC14_RCP85_max,
         li = DEC14_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days < 14°C",
         main = "")
  title(main = "Decker Island",
        adj = 0.05, 
        line = -1.3, 
        font.main = 1, 
        cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         DEC14_RCP45_med,
         ui = DEC14_RCP45_max,
         li = DEC14_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/DWS_14_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         DWS14_RCP85_med,
         ui = DWS14_RCP85_max,
         li = DWS14_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days < 14°C",
         main = "")
  title(main = "Deepwater Ship Channel", adj = 0.05, line = -1.3, font.main = 1, cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         DWS14_RCP45_med,
         ui = DWS14_RCP45_max,
         li = DWS14_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Mallard_14_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         MAL14_RCP85_med,
         ui = MAL14_RCP85_max,
         li = MAL14_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days < 14°C",
         main = "")
  title(main = "Mallard Island", adj = 0.05, line = -1.3, font.main = 1, cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         MAL14_RCP45_med,
         ui = MAL14_RCP45_max,
         li = MAL14_RCP45_min,
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Martinez_14_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         MRZ14_RCP85_med,
         ui = MRZ14_RCP85_max,
         li = MRZ14_RCP85_min,
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days < 14°C",
         main = "")
  title(main = "Martinez", adj = 0.05, line = -1.3, font.main = 1, cex =1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         #DEC22_pcmb1j_m,
         MRZ14_RCP45_med,
         #ui = DEC22_pcmb1j_m + 1.96*DEC22_pcmb1j_se,
         #li = DEC22_pcmb1j_m - 1.96*DEC22_pcmb1j_se,
         ui = MRZ14_RCP45_max,
         li = MRZ14_RCP45_min,
         
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

png("./Figures/Jersey_14_RCP.png",
    height = 6, width = 8, units = "in", res = 300)
par(cex = 1.25)
with(tempExDecadeRCP, {
  plotCI(as.numeric(decade)-0.1,
         JPT14_RCP85_med,
         ui = JPT14_RCP85_max,
         li = JPT14_RCP85_min,
         
         pch = 21, cex = 1.5, pt.bg = "red",
         lwd = 2,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         cex.axis = 1,
         font.lab = 1,
         cex.lab = 1.1,
         xlab = "",
         ylab = "Days < 14°C",
         main = "")
  title(main = "Jersey Point", 
        adj = 0.05, 
        line = -1.3, 
        font.main = 1, 
        cex = 1.3)
  
  par(new = TRUE)
  plotCI(as.numeric(decade)+0.1,
         #DEC22_pcmb1j_m,
         JPT14_RCP45_med,
         #ui = DEC22_pcmb1j_m + 1.96*DEC22_pcmb1j_se,
         #li = DEC22_pcmb1j_m - 1.96*DEC22_pcmb1j_se,
         ui = JPT14_RCP45_max,
         li = JPT14_RCP45_min,
         
         pch = 21, cex = 1.5, pt.bg = "blue",
         lwd = 2.5,
         xlim = c(1, 9),
         ylim = c(0, 200),
         xaxt = "n",
         xlab = "",
         ylab = "",
         main = "")
  
  axis(side = 1,
       at = 1:9,
       labels = levels(decade),
       tick = FALSE,
       las = 1, cex.axis = 0.85, line = -1)
})
dev.off()

## Line graphs ####
## Fig 4.5 Days over 22°C, RCP 8.5 ####
png("./Figures/RCP85_22.png",
    height = 5, width = 7, units = "in", res = 300)
with(tempExRCP, {
  plot(year, DEC22_RCP85, type = "n",
       ylim = c(0, 250),
       xlab = "Year",
       ylab = "Days Above 22°C",
       main = "")
  title(main = "RCP 8.5", line = -1, font.main = 1)
  lines(year, DEC22_RCP85, col = "black", lwd = 2)
  lines(year, DWS22_RCP85, col = "gray", lwd = 2)
  lines(year, MAL22_RCP85, col = "dodgerblue", lwd = 2)
  lines(year, MRZ22_RCP85, col = "orange", lwd = 2)
  lines(year, JPT22_RCP85, col = "darkorchid", lwd = 2)
})

legend("topleft",
       title = "",
       lty = 1, lwd = 2,
       col = c("black", "gray", "dodgerblue", "orange", "darkorchid"),
       legend = c("Decker Island", "Deepwater Ship Channel", "Mallard Island", "Martinez", "Jersey Point"),
       bty = "n",
       cex = 0.8)
dev.off()

## Fig 4.9 Days under 14°C, RCP 8.5 ####
png("./Figures/RCP85_14.png",
    height = 5, width = 7, units = "in", res = 300)
with(tempExRCP, {
  plot(year, DEC14_RCP85, type = "n",
       ylim = c(20, 200),
       xlab = "Year",
       ylab = "Days Below 14°C",
       main = "")
  title(main = "RCP 8.5", line = -1, font.main = 1)
  lines(year, DEC14_RCP85, col = "black", lwd = 2)
  lines(year, DWS14_RCP85, col = "gray", lwd = 2)
  lines(year, MAL14_RCP85, col = "dodgerblue", lwd = 2)
  lines(year, MRZ14_RCP85, col = "orange", lwd = 2)
  lines(year, JPT14_RCP85, col = "darkorchid", lwd = 2)
})

legend("topright",
       title = "",
       lty = 1, lwd = 2,
       col = c("black", "gray", "dodgerblue", "orange", "darkorchid"),
       legend = c("Decker Island", "Deepwater Ship Channel", "Mallard Island", "Martinez", "Jersey Point"),
       bty = "n",
       cex = 0.8)
dev.off()



## Heat maps of predicted temperatures ####
# https://semba-blog.netlify.app/05/10/2020/heatmaps-in-r-with-ggplot2-and-metr-packages/
#  ^ look at the last set of plots

# Create day of year (doy) variable for graphing
#  Don't use doy() for this. It creates a gap in the graph because
#  of the way leap days are handled in the dataset.
Tavg_RCP45.$doy <- rep(1:365, 89)
Tavg_RCP85.$doy <- rep(1:365, 89)

## Fig 4.10 Heat map of temperatures at Martinez; RCP 8.5 ####
png("./Figures/RCP85_MRZ_Heatmap.png",
    height = 5, width = 7, units = "in", res = 300)
Tavg_RCP85. %>% ggplot(aes(doy, year, z = MRZ)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                    #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Martinez") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))
dev.off()

## Other heatmaps ####

# RCP 4.5
Tavg_RCP45. %>% ggplot(aes(doy, year, z = MRZ)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                    #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Martinez 4.5") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))
  
Tavg_RCP45. %>% ggplot(aes(doy, year, z = DEC)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                       #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Decker Island") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))

# RCP 8.5
Tavg_RCP85. %>% ggplot(aes(doy, year, z = DWS)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                    #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Deepwater Ship Channel") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))

Tavg_RCP85. %>% ggplot(aes(doy, year, z = MAL)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                    #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Mallard Island") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))


Tavg_RCP85. %>% ggplot(aes(doy, year, z = JPT)) +
  geom_contour_filled(breaks = c(5, 12, 14, 16, 18, 20, 22, 31)) +
  scale_fill_manual(values=rev(brewer.pal(7, "RdBu")), 
                    #breaks = c(5, 12, 14, 16, 18, 20, 22, 31),
                    labels= c("up to 12", "12-14", "14-16",
                              "16-18", "18-20", "20-22", "over 22")) +
  scale_y_continuous(limits = c(2010, 2100)) +
  xlab("Day of Year") +
  ylab("Year") +
  ggtitle("Jersey Point") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title="Temperature °C"))

