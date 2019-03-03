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


fm_inc <- read_csv("../../Data/farmincome_wealthstatisticsdata_november2018.csv")

# debt -----------------------------------
debt <- filter(fm_inc,State == "US", 
       str_detect(VariableDescriptionTotal, "[Dd]ebt, all, excl. operator dwelling"),
       VariableDescriptionPart1 == "Farm sector debt",
       Year >= 2004) %>%
rename(gdp_df = ChainType_GDP_Deflator) %>%
mutate(debts = Amount/gdp_df)


# plots ----------------------------
ggplot() +
  geom_line(data = debt, aes(x=Year, y=debts, group = 1), color = "black")
