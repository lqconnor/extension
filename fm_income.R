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
<<<<<<< HEAD
                   str_detect(VariableDescriptionTotal, "Net cash income|Net farm income|receipts value, all commodities"),
=======
                   str_detect(VariableDescriptionTotal, "Net cash income|Net farm income"),
>>>>>>> be85579614f99a3a2f27cf3d1a014adc05d97db9
                   Year >= 2010) %>%
  select(Year, State, VariableDescriptionTotal, Amount) %>%
  spread(VariableDescriptionTotal, Amount)

ggplot(income) +
  geom_line(aes(x = Year, y = `Net cash income`), color = "black") +
<<<<<<< HEAD
  geom_line(aes(x = Year, y = `Net farm income`), color = "blue") +
  geom_line(aes(x = Year, y = `Cash receipts value, all commodities , all`), color = "red")
=======
  geom_line(aes(x = Year, y = `Net farm income`), color = "blue")
>>>>>>> be85579614f99a3a2f27cf3d1a014adc05d97db9
