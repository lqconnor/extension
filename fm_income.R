# Pre - Amble -----------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

Sys.setenv(NASSQS_TOKEN = readLines(".secret"))

#Check if packages installed, install missing packages and load all installed packages
pckgs <- c("tidyverse", "rnassqs", "stargazer", "tseries", "ggplot2", "dichromat")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


fm_inc <- read_csv("../../Data/farmincome_wealthstatisticsdata_March2019.csv")

# Cash Receipts ---------------------------------
receipts <- filter(fm_inc,State == "LA", 
               str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
               str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|sorghum|wheat"),
               #VariableDescriptionPart1 == "Farm sector debt",
               VariableDescriptionPart2 == "All",
               Year >= 2014 & Year <= 2017) %>%
  mutate(year = as.factor(Year)) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(debts = Amount/gdp_df)


p <- ggplot(receipts, aes(x = VariableDescriptionPart1, y = Amount)) +
  geom_bar(
    aes(color = year, fill = year),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_fill_brewer(palette="Blues") +
  scale_colour_brewer(palette="Blues")

p

# Expenses --------------------------------------
expns <- filter(fm_inc,State == "US", 
               str_detect(VariableDescriptionTotal, "[Ee]xpenses"),
               #str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|sorghum|wheat"),
               #VariableDescriptionPart1 == "Farm sector debt",
               VariableDescriptionPart2 == "All",
               Year >= 2016) %>%
  mutate(year = as.factor(Year))


# Debt ------------------------------------------
debt <- filter(fm_inc,State == "US", 
               str_detect(VariableDescriptionTotal, "[Dd]ebt, all, excl. operator dwelling"),
               VariableDescriptionPart1 == "Farm sector debt",
               Year >= 2004 & Year <= 2016) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(debts = Amount/gdp_df)

# Farm Income -----------------------------------
income <- filter(fm_inc, str_detect(State, "US|LA"), 
               str_detect(VariableDescriptionTotal, "Net [Ff]arm income$"),
               #VariableDescriptionPart1 == "Farm sector debt",
               Year >= 2012) %>%
  mutate(year = as.factor(Year)) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(debts = Amount/gdp_df)

p <- ggplot(income, aes(x = State, y = log(Amount))) +
  geom_bar(
    aes(color = year, fill = year),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_fill_brewer(palette="Blues") +
  scale_colour_brewer(palette="Blues")

p

ggplot(income, aes(x = year, y = Amount,
                      group = State,
                      colour = State)) +
  geom_line()

ggplot() +
  geom_line(data = debt, aes(x=Year, y=Amount, group = 1), color = "black")
geom_line(data = debt, aes(x=Year, y=Amount, group = 1), color = "black")

# Cash Income -----------------------------------


