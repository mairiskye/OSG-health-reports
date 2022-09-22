library(readxl)
library(magrittr)

region_lookup <- read.csv("Data/CodeLookUp.csv") %>% 
  select(CouncilName, Region)

historic_classification_data <- read_xlsx("Clean Old Excel Data/Original Historic/historic-classification-original.xlsx", sheet = "time-series", trim_ws = TRUE) %>%
  select(-n)
names(historic_classification_data) <- c("CouncilName", "Measure", "Value", "Date")
historic_classification_data$Measure <- tolower(historic_classification_data$Measure)
historic_classification_data$Value <- round(historic_classification_data$Value * 100,1)

clean_historic <- left_join(historic_classification_data, region_lookup, by = "CouncilName") %>%
  select("CouncilName", "Region", "Date", "Measure", "Value")

clean_historic$DateName <- format(as.Date(clean_historic$Date), "%b-%y")

historic_class_with_averages <- clean_historic %>%
  group_by(Date, Region, Measure) %>%
  summarise(RegionAvg = round(mean(Value),1)) %>%
  left_join(clean_historic, .)
  

write.csv(historic_class_with_averages, "Clean Old Excel Data/Clean Historic/historic-classification-data.csv", row.names = FALSE)
write.csv(historic_class_with_averages, "Data/Time Series/historic-classification-data.csv", row.names = FALSE)
