



# Load Packages ####

library(readxl)
library(lubridate)

# Prep Data ####

# Data for this project come from the following publication:
# Knowles, N., C. Cronkite-Ratcliff, D.W. Pierce, and D.R. Cayan. 2018.
#   Water Resources Research 54(10):7631-7650 https://doi.org/10.1029/2018WR022852
# Additional information can be found in the USGS Open File report located
#   here: https://pubs.usgs.gov/of/2018/1101/ofr20181101.pdf
# The full dataset can be downloaded directly from
#   https://www.sciencebase.gov/catalog/item/5b170442e4b092d9651fcc93
# The dataset below consists of the outputs from the calsim model runs derived
#   from the data in the sciencebase repository. The dataset below is what was
#   used in the future conditions chapter of the SSA for the Bay-Delta DPS of
#   Longfin Smelt.
dat <- read_excel("./Data_Original/calsim_indices_all.xlsx",
                  skip = 2)

# just keep "index" and the Sac columns
dat <- dat[, c(1, grep("Sac", names(dat)))]
# bummer. that also caught the OctMar vars...
dat <- dat[, -(grep("Oct", names(dat)))]

dat45 <- dat[, c(1, 1 + which(c("rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
                                  "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
                                  "rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
                                  "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
                                  "historical") == "rcp45"))]
names(dat45) <- c("year", c("ACCESS1-0", "CCSM4", "CESM1-BGC", 
                            "CMCC-CMS", "CNRM-CM5", "CanESM2", 
                            "GFDL-CM3", "HadGEM2-CC", "HadGEM2-ES", 
                            "MIROC5"))

dat85 <- dat[, c(1, 1 + which(c("rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
                            "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
                            "rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
                            "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
                            "historical") == "rcp85"))]
names(dat85) <- c("year", c("ACCESS1-0", "CCSM4", "CESM1-BGC", 
                            "CMCC-CMS", "CNRM-CM5", "CanESM2", 
                            "GFDL-CM3", "HadGEM2-CC", "HadGEM2-ES", 
                            "MIROC5"))







dat <- stack(dat[, c(-1)])
# index is actually the year, starting at 1951 to 2099
dat$Index <- 1951:2099
# names(dat) <- c("year",
#                 )
c("rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
  "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
  "rcp45", "rcp85", "rcp45", "rcp85", "rcp45", 
  "rcp85", "rcp45", "rcp85", "rcp45", "rcp85", 
  "historical")
c(rep(c("ACCESS1-0", "CCSM4", "CESM1-BGC", 
      "CMCC-CMS", "CNRM-CM5", "CanESM2", 
      "GFDL-CM3", "HadGEM2-CC", "HadGEM2-ES", 
      "MIROC5"), each = 2), "livneh")											

