# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "tseries")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


fm_inc <- read_csv("../Data/farmincome_wealthstatisticsdata_march2019.csv")

# debt -----------------------------------
income <- filter(fm_inc,State == "US", 
                   str_detect(VariableDescriptionTotal, "Net cash income|Net farm income"),
                   Year >= 2010) %>%
  select(Year, State, VariableDescriptionTotal, Amount) %>%
  spread(VariableDescriptionTotal, Amount)

ggplot(income) +
  geom_line(aes(x = Year, y = `Net cash income`), color = "black") +
  geom_line(aes(x = Year, y = `Net farm income`), color = "blue")
