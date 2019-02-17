# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "ggplot2", "tseries")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


exports <- read_csv("../../Data/export.csv")
exports <- mutate(exports, `Week Ending` = as.Date(`Week Ending`, "%m/%d/%y"))

ggplot() +
  geom_line(data = exports, aes(x=`Week Ending`, y=`Weekly Exports`, group = 1), color = "black")