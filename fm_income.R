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
       Year >= 2004 & Year <= 2016) %>%
rename(gdp_df = ChainType_GDP_Deflator) %>%
mutate(debts = Amount/gdp_df)

# Modified Palmer Drought Index Data (PMDI) -----------------------------------------------------------------------------------
pmdi_files <- c(2016:2004)

# read files into dataframe list
pdmi <- pmdi_files %>%
  map(~read_csv(str_c("../../Data/palmer/pmdi_", . ,".txt")))

# Take dataframe list and concatenate by row
pdmi <- bind_rows(pdmi) %>%
  select(STATEFP, COUNTYFP, state, year, pdmi) %>%
  group_by(year) %>%
  summarize(m_pdmi = mean(pdmi)) %>%
  mutate(drought = as.numeric(m_pdmi < -0.3))


# plots ----------------------------
ggplot() +
  geom_line(data = debt, aes(x=Year, y=debts, group = 1), color = "black")

ggplot() +
  geom_line(data = pdmi, aes(x=year, y=m_pdmi*4e6, group = 1), color = "red")

cor(pdmi$m_pdmi,debt$debts)

run <- lm(log(debt$debts) ~ pdmi$drought)
summary(run)

# check to see how revert works
