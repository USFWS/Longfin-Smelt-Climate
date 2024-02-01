# Purpose: this code calculates and graphs the probability of consecutive
# dry years over time using an ensemble of climate model outputs.


# Data ####
# Source (or run) ./R_code/Hydrology_Data_Prep.R

# Consecutive dry years ####

## RCP 4.5 ####
same45 <- data.frame(dat45[, -c(1)])
tmp45 <- matrix(nrow = length(same45[,1]),
                ncol = length(names(same45)))

# find which values are = dry or critically dry
for(i in 2:length(same45[,1])){
  same45[i,] <- same45[i,] %in% c(4, 5)
  #as.numeric(identical(same45[i, j], same45[i-1, j]))
}
# difference between a cell and the previous cell
for(j in 1:length(names(same45))){
  tmp45[, j] <- c(NA, diff(same45[,j]))
}

# find cells that match (where diff = 0) 
#  & keep the corresponding values in the matrix of dry years
for(i in 1:ncol(same45)){
  same45[,i] <- same45[,i]*(as.numeric(tmp45[,i]==0))
}

dryRun45 <- matrix(nrow = length(same45[,1]),
                   ncol = length(names(same45)))
for(i in (15:(length(same45[,1])-15))){
  dryRun45[i, ] <- colSums(same45[(i-15):(i+15), ])/length(-15:15)
}


## RCP 8.5 ####
same85 <- data.frame(dat85[, -c(1)])
tmp85 <- matrix(nrow = length(same85[,1]),
                ncol = length(names(same85)))

for(i in 2:length(same85[,1])){
  same85[i,] <- same85[i,] %in% c(4, 5)
  #as.numeric(identical(same85[i, j], same85[i-1, j]))
}
for(j in 1:length(names(same85))){
  tmp85[, j] <- c(NA, diff(same85[,j]))
}

for(i in 1:ncol(same85)){
  same85[,i] <- same85[,i]*(as.numeric(tmp85[,i]==0))
}

dryRun85 <- matrix(nrow = length(same85[,1]),
                   ncol = length(names(same85)))
for(i in (15:(length(same85[,1])-15))){
  dryRun85[i, ] <- colSums(same85[(i-15):(i+15), ])/length(-15:15)
}

## Calculate means for each year ####
datMeans <- data.frame(year = 1951:2099,
                       rcp45 = apply(dryRun45, 1, mean),
                       rcp85 = apply(dryRun85, 1, mean))

# Trends ####
datMeans$trend45 <- predict(lm(rcp45 ~ year, data = datMeans),
                            newdata = datMeans)
datMeans$trend85 <- predict(lm(rcp85 ~ year, data = datMeans),
                            newdata = datMeans)

summary(lm(rcp45 ~ year, data = datMeans[which(datMeans$year > 2019),]))
summary(lm(rcp85 ~ year, data = datMeans[which(datMeans$year > 2019),]))

## FIGURE 4.15 IN THE SSA ####
png("./Figures/ConsDryYrs.png",
    height = 5, width = 6, units = "in", res = 300)
plot(0, 0, type = "n",
     xlim = c(1965, 2084),
     ylim = c(0, 0.15),
     xlab = "",
     ylab = "Probability of Consecutive Dry Years",
     #yaxt = "n",
     main = "")
lines(1951:2099,
      apply(dryRun45, 1, mean),
      col = "orange",
      lwd = 3)
lines(1951:2099,
      apply(dryRun85, 1, mean),
      col = "dodgerblue",
      lwd = 3)
lines(datMeans$year, datMeans$trend45,
      lty = 2, col = "orange", lwd = 3)
lines(datMeans$year, datMeans$trend85,
      lty = 2, col = "dodgerblue", lwd = 3)
legend("bottomleft", lwd = 3, col = c("orange", "dodgerblue"),
       legend = c("RCP 4.5", "RCP 8.5"))
dev.off()




