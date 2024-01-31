
# Purpose: This script creates the map used in the background of some of the
#  climate change temperature graphs in the SSA for the Bay-Delta DPS of
#  Longfin Smelt.

devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)
library(tidyverse)


png("./Figures/Delta_Map.png",
    height = 6, width = 6, units = "in", res = 300)
WW_Delta %>%
ggplot(aes())+
  geom_sf(col = "dodgerblue4", fill = "lightskyblue2")+
  theme_bw() +
  xlim(-122.1, -121) +
  ylim(37.7, 38.7)
dev.off()