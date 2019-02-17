# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

pckgs <- c("tidyverse", "rnassqs", "stargazer", "Hmisc")
lapply(pckgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data Import ---------------------------------
feb_18 <- read_csv("../../Data/FarmIncome_WealthStatisticsData_November2018.csv")
wasde <- read_csv("../FSR/Data/psd_grains_pulses.csv")
wasde_all <- read_csv("../FSR/Data/psd_alldata.csv")
plt_acr <- read_csv("../FSR/Data/crop_acres_state.csv")
prices <- read_csv("../FSR/Data/prices.csv")
###########################################################################################
ohio <- filter(feb_18, State == "US")
acres <- filter(plt_acr, str_detect(Period, "YEAR$"), Year >= 2000, State == "US TOTAL")
f_year <- 2000

# Receipts ###############################################################################
receipts <- c("Cash receipts value, corn , all", "Cash receipts value, soybeans , all")
income <- "Net farm income"
cash_grn <- filter(ohio, str_detect(VariableDescriptionTotal, paste(receipts, collapse = "|")),
                   Year == 2016) %>%
  mutate(n_amount = Amount/sum(Amount))
ggplot(data = cash_grn, aes(x = "", y = n_amount, fill = VariableDescriptionTotal)) +
  geom_bar(width = 1, stat = "identity")

###########################################################################################
# Soybeans
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, soybeans , all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Soybean Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

###########################################################################################
# Cotton
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, cotton , all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Cotton Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

###########################################################################################
# Corn
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, corn , all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Corn Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

###########################################################################################
# Wheat
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, wheat , all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Wheat Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

###########################################################################################
# Grain Sorghum
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, sorghum grain, all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Sorghum Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))

###########################################################################################
# Sugar Cane
cash_crn <- filter(ohio, str_detect(VariableDescriptionTotal, "Cash receipts value, cane for sugar , all"),
                   Year >= f_year)
ggplot(data = cash_crn, aes(x = Year, y = log(Amount))) +
  geom_line() + labs(y = "Sugar Cane Receipts") +
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))
