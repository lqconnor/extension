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


fm_inc <- read_csv("../Data/farmincome_wealthstatisticsdata_March2019.csv")

# Cash Receipts ---------------------------------
#excludes <- "commodities"
excludes <- "commodities|livestock|value, crops|oil crops|lint|feed|mohair|hogs|chicken|broilers|wool|cottonseed|sheep"

receipts <- filter(fm_inc,State == "LA", 
               str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
               !str_detect(VariableDescriptionTotal, excludes),
               #str_detect(VariableDescriptionTotal, "livestock|value, crops"),
               #str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|sorghum|wheat"),
               #VariableDescriptionPart1 == "Farm sector debt",
               VariableDescriptionPart2 == "All",
               Year == 2010) %>%
  mutate(year = as.factor(Year)) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(debts = Amount/gdp_df)


p <- ggplot(receipts, aes(x = VariableDescriptionPart1, y = Amount)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Blues") +
  scale_colour_brewer(palette="Blues") +
  theme(axis.title.x=element_blank()) +
  labs(y = "US Dollars")

p

# Proportion of farm economy -------------------------
excluded <- "livestock|value, crops|oil crops|lint|feed|mohair|hogs|chicken|broilers|wool|cottonseed|sheep"

nw_rcpt <- filter(fm_inc,State == "US", 
               str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
               !str_detect(VariableDescriptionTotal, excluded),
               #str_detect(VariableDescriptionTotal, "livestock|value, crops"),
               #str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|sorghum|wheat"),
               #VariableDescriptionPart1 == "Farm sector debt",
               VariableDescriptionPart2 == "All",
               Year >= 2005) %>%
  select(Year, State, VariableDescriptionPart1, Amount) %>%
  mutate(year = as.factor(Year)) %>%
  spread(VariableDescriptionPart1, Amount)

ggplot(nw_rcpt) +
  geom_line(aes(x = Year, y = (`Soybeans`/`All Commodities`)*100), color = "black") +
  geom_line(aes(x = Year, y = (`Cane for sugar`/`All Commodities`)*100), color = "red") +
  geom_line(aes(x = Year, y = (`Corn`/`All Commodities`)*100), color = "blue") +
  geom_line(aes(x = Year, y = (`Poultry/Eggs`/`All Commodities`)*100), color = "purple") +
  geom_line(aes(x = Year, y = (`Rice`/`All Commodities`)*100), color = "orange") +
  geom_line(aes(x = Year, y = (`Cotton`/`All Commodities`)*100), color = "brown")


ggplot(nw_rcpt) +
  geom_line(aes(x = Year, y = `Soybeans`), color = "black") +
  geom_line(aes(x = Year, y = `Cane for sugar`), color = "red") +
  geom_line(aes(x = Year, y = `Corn`), color = "blue") +
  geom_line(aes(x = Year, y = `Poultry/Eggs`), color = "purple") +
  geom_line(aes(x = Year, y = `Rice`), color = "orange") +
  geom_line(aes(x = Year, y = `Cotton`), color = "brown")

ggplot(nw_rcpt) +
  geom_line(aes(x = Year, y = `Soybeans`), color = "black") +
  geom_line(aes(x = Year, y = `Cane for sugar`), color = "red") +
  geom_line(aes(x = Year, y = `Corn`), color = "blue") +
  geom_line(aes(x = Year, y = `Poultry/Eggs`), color = "purple") +
  geom_line(aes(x = Year, y = `Rice`), color = "orange") +
  geom_line(aes(x = Year, y = `Cotton`), color = "brown")

# Proportion of farm economy -------------------------
# Livestock and Crops --------------------
all_rcpt <- filter(fm_inc,State == "US", 
                  str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
                  str_detect(VariableDescriptionTotal, ", crops|livestock"),
                  VariableDescriptionPart2 == "All",
                  Year >= 2005) %>%
  select(Year, State, VariableDescriptionPart1, Amount) %>%
  spread(VariableDescriptionPart1, Amount)

ggplot(all_rcpt) +
  geom_line(aes(x = Year, y = `Crops`), color = "black") +
  geom_line(aes(x = Year, y = `Animals and products`), color = "red")

# Proportion of national farm economy -------------------------
US_rcpt <- filter(fm_inc, str_detect(State, "LA|US"), 
                  str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
                  str_detect(VariableDescriptionTotal, "commodities"),
                  VariableDescriptionPart2 == "All",
                  Year >= 2005) %>%
  select(Year, State, VariableDescriptionPart1, Amount) %>%
  mutate(year = as.factor(Year)) %>%
  spread(State, Amount)

ggplot(US_rcpt) +
  geom_line(aes(x = Year, y = (LA/US)*100), color = "black")

ggplot(US_rcpt) +
  geom_line(aes(x = Year, y = US*100), color = "black") +
  geom_line(aes(x = Year, y = LA*100), color = "blue") 

ggplot() +
  geom_line(data = US_rcpt, aes(x = Year, y = log(LA)), color = "black") +
  geom_line(data = US_rcpt, aes(x = Year, y = log(US)/1.3), color = "red") +
  geom_line(data = nw_rcpt, aes(x = Year, y = log(`Corn`)*1.1), color = "blue") +
  geom_line(data = nw_rcpt, aes(x = Year, y = log(`Poultry/Eggs`)*1.1), color = "purple") +
  geom_line(data = nw_rcpt, aes(x = Year, y = log(`Soybeans`)*1.1), color = "brown")
