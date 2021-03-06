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


fm_inc <- read_csv("../Data/farmincome_wealthstatisticsdata_march2019.csv")
 
long_ts <- filter(fm_inc, State == "US", 
         str_detect(VariableDescriptionTotal, "Net farm income$"),
         Year >= 1950) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(Income = Amount/gdp_df) %>%
  mutate(year = as.Date(as.character(Year), format = "%Y"))

# Cash Receipts ---------------------------------
receipts <- filter(fm_inc,State == "US", 
               str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
               str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|sorghum|wheat"),
               #VariableDescriptionPart1 == "Farm sector debt",
               VariableDescriptionPart2 == "All",
               Year >= 2016 & Year <= 2019) %>%
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
  scale_colour_brewer(palette="Blues") +
  theme(axis.title.x=element_blank()) +
  labs(y = "US Dollars")

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
               str_detect(VariableDescriptionTotal, "Net [cC]ash income$"),
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
  scale_colour_brewer(palette="Blues") +
  theme(axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank())

p

# Animal Products Receipts -----------------------------------------------
income <- filter(fm_inc, str_detect(State, "US"), 
                 str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
                 str_detect(VariableDescriptionTotal, "poultry|cattle|hogs|broilers|dairy"),
                 #VariableDescriptionPart1 == "Farm sector debt",
                 Year >= 2016 & Year <= 2019) %>%
  mutate(year = as.factor(Year)) %>%
  rename(gdp_df = ChainType_GDP_Deflator) %>%
  mutate(debts = Amount/gdp_df)

p <- ggplot(income, aes(x = VariableDescriptionPart1, y = Amount)) +
  geom_bar(
    aes(color = year, fill = year),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_fill_brewer(palette="Blues") +
  scale_colour_brewer(palette="Blues") +
  theme(axis.title.x=element_blank()) +
  labs(y = "US Dollars")

p


ggplot(income, aes(x = year, y = Amount,
                      group = State,
                      colour = State)) +
  geom_line()


ggplot() +
  geom_line(data = debt, aes(x=Year, y=Amount, group = 1), color = "black")
geom_line(data = debt, aes(x=Year, y=Amount, group = 1), color = "black")

# Louisiana Decomposition
# Pie Charts -----------------------------------

# Create a basic bar
LA_rcpts <- c(2010, 2017)

rcpt <- function(z){
  filter(fm_inc,State == "LA", 
                     str_detect(VariableDescriptionTotal, "[Rr]eceipts"),
                     str_detect(VariableDescriptionTotal, "corn|soybeans|cotton ,|rice|for sugar|poultry|cattle"),
                     #VariableDescriptionPart1 == "Farm sector debt",
                     VariableDescriptionPart2 == "All",
                     Year == z) %>%
    mutate(totals = sum(Amount),
           prop = (Amount/totals)*100,
           prop = round(prop, 2))
}

LA_rcpts <- map(LA_rcpts, rcpt)

pie = ggplot(LA_rcpts[[2]], aes(x="", y=Amount, fill=VariableDescriptionPart1)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5), color = "white")

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", 	"#008B8B", "#0000CD", "#A52A2A")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Statewide Insurance Plan Use")

# Tidy up the theme
pie + theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
