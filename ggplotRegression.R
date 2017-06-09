library(ggplot2)
library(plyr)
library(readxl)

prepareData <- function(path.obs, path.sim, rotation.to.return) {
  require(readxl)
  
  ### Prepare obs data
  # Read data with observed yield and remove unwanted columns and rows
  df.obs <- read_excel(path.obs)
  df.obs <- df.obs[-c(1),-c(3:11, 13, 14, 16, 18, 19)]
  df.obs <- df.obs[!df.obs$`Data-Quality`=="0",]
  
  # Add rotation column to merge with sim results
  df.obs$rotation <- ifelse(df.obs$Crop == "Spring Wheat", "sW-wW", ifelse(df.obs$Crop == "Spring Canola", "sC-wW", ifelse(df.obs$Crop == "Spring Pea", "sP-wW", "")))
  
  # Rename column to match sim and change column class to numeric
  df.obs <- rename(df.obs, c("Sim-Location"="location"))
  df.obs$Yield <- as.numeric(df.obs$Yield)
  
  ### Prepar sim data
  # Read data with simulated yield, only read required columns
  columnClasses = c("Date", rep("NULL", 10), "numeric", rep("NULL", 70), rep("character", 3), "NULL")
  df.sim <- read.table(path.sim, colClasses = columnClasses, sep="\t", header=TRUE)
  
  # Convert units (from/to?) and extract planting year from date
  df.sim$yield <- df.sim$yield * 10000
  df.sim$Year <- as.numeric(substring(df.sim$planting_date,0,4))
  
  ### Merge data
  # Merge sim and obs dataframes by location and  year
  df <- merge(df.obs, df.sim, by=c("location", "Year", "rotation"))
  
  # Rename yield columns for clarity
  df <- rename(df, c("Yield"="yield.obs", "yield"="yield.sim"))
  
  # Split based on crop/rotation
  df.sw <- df[df$rotation == "sW-wW",]
  df.sp <- df[df$rotation == "sP-wW",]
  df.sc <- df[df$rotation == "sC-wW",]
  
  df <- df[df$rotation == rotation.to.return,]

  return(df)
}

ggplotRegression <- function(df) {
  require(ggplot2)
  require(plyr)
  
  fit <- lm(yield.sim ~ yield.obs, data = df)
  print(summary(fit))
  ggplot(df, aes(x=df$yield.obs, y=df$yield.sim)) + 
    geom_point(shape=1) +
    geom_abline(slope=1) +
    stat_smooth(method="lm", formula = y ~ x, size=1) +
    coord_cartesian(xlim=c(0,8000), ylim=c(0,8000)) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

## =======================

path.obs <- "C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CafModelingFlexCropping\\Methods\\Calibration\\variety-trial-data.xlsx"

ggplotRegression(prepareData(path.obs, "Input\\season_all_170608.dat", "sC-wW"))
ggplotRegression(prepareData(path.obs, "Input\\season_all_170608.dat", "sW-wW"))
ggplotRegression(prepareData(path.obs, "Input\\season_all_170608.dat", "sP-wW"))
