library(readxl)
library(config)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)

#READ IN DATA ---------------------
#new quarterly data
new_uploads_raw <- read.csv("Cleaning Raw Data/Raw Data/OSG Uploads.csv", skip = 3)
#historic time-series data
uploads_previous <- read.csv("Cleaning Raw Data/Time Series Data/historic-uploads-data.csv")
#----------------------------------

session <- config::get("reporting_month")

#read in council/region look-up file
region_lookup <- read.csv("Cleaning Raw Data/CodeLookUp.csv") %>%
  select(CouncilName, Region)


names(new_uploads_raw)[1] <- "CouncilName"
new_uploads_clean <- new_uploads_raw %>%
  mutate(Date = session,
         FileType = "A and J",
         Value = File.Type.A + File.Type.J) %>%
  select(CouncilName, Date, FileType, Value) %>%
  left_join(., region_lookup, by = "CouncilName")

latest_region_averages <- new_uploads_clean %>%
  group_by(Region) %>%
  summarise(RegionAvg = round(mean(Value),1))

new_uploads_ready <- left_join(new_uploads_clean,latest_region_averages, by = "Region")
new_uploads_ready$DateName <- format(as.Date(new_uploads_ready$Date), "%b-%y")

#add new data to time-series and add 'Region' from lookup file
updated_uploads_time_series <- rbind(new_uploads_ready, uploads_previous) %>%
  distinct()

write.csv(updated_uploads_time_series, paste0("Make Reports/Clean Data/uploads-time-series-", session, ".csv"),row.names = FALSE)
write.csv(updated_uploads_time_series, paste0("Cleaning Raw Data/Time Series Data/uploads-time-series-", session, ".csv"),row.names = FALSE)

          