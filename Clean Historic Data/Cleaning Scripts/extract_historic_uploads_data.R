#A script to extract time-series data from original workbooks used in manual OSG health check analysis
library(readxl)
region_lookup <- read.csv("Data/CodeLookUp.csv") %>%
  select(CouncilName, Region)

original_uploads <- readxl::read_xlsx("Clean Old Excel Data/Original Historic/historic-uploads-original.xlsx", sheet = "time-series") %>%
  rename("Value" = Uploads) %>%
  left_join(., region_lookup, by = "CouncilName")

with_region_averages <- original_uploads %>%
  group_by(Region, Date) %>%
  summarise(RegionAvg = round(mean(Value),1)) %>%
  left_join(original_uploads, ., by = c("Region", "Date"))

with_region_averages$DateName <- format(as.Date(with_region_averages$Date), "%b-%y")

write.csv(with_region_averages, "Clean Old Excel Data/Clean Historic/historic-uploads-data.csv", row.names = FALSE)
write.csv(with_region_averages, "Data/Time Series/historic-uploads-data.csv", row.names = FALSE)
