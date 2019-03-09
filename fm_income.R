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
       str_detect(VariableDescriptionTotal, "Net cash income"),
       #VariableDescriptionPart1 == "Farm sector debt",
       Year >= 2010) %>%
rename(gdp_df = ChainType_GDP_Deflator) %>%
mutate(debts = Amount/gdp_df)


# plots ----------------------------
ggplot() +
  geom_line(data = debt, aes(x=Year, y=debts, group = 1), color = "black")

ggplot() +
  geom_line(data = pdmi, aes(x=year, y=m_pdmi*4e6, group = 1), color = "red")

cor(pdmi$m_pdmi,debt$debts)

run <- lm(log(debt$debts) ~ pdmi$drought)
summary(run)
