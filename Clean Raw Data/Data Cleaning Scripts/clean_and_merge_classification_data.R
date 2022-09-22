library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

session <- session <- config::get("reporting_month")

#read in new data
new_class_data <- readxl::read_xlsx("Clean Raw Data/Raw Data/OSG Classifications 2022-08-24.xlsx", trim_ws = TRUE, skip = 1)
#read in old data 
previous_class_data <- read.csv("Clean Raw Data/Time Series Data/historic-classification-data.csv") 

#clean and reformat new data------------------------------------
#read in lookup file to match council codes to council names and regions
region_lookup <- read.csv("Clean Raw Data/CodeLookUp.csv") %>%
  select(-AltName)

new_class_data[new_class_data == "totals"] <- "Scotland"
#remove rows which have only NA
class_data_no_blanks <- new_class_data[rowSums(is.na(new_class_data)) != ncol(new_class_data), ] %>%
  select(-tertiary)

#calculate proportion of each class (primary, secondary missing in raw file)
class_proportions <- class_data_no_blanks %>%
  mutate(secondary = round(secondary/`total open BLPUs (excluding P*)`*100,1)) %>%
  mutate(primary = round(primary/`total open BLPUs (excluding P*)`*100, 1)) %>%
  rename(tertiary = `% tertiary by LA`) %>%
  mutate(tertiary = round(tertiary, 1)) %>%
  select(-`total open BLPUs (excluding P*)`)

#pivot data so it is in long format for easy filtering
clean_classifications <- class_proportions %>%
  pivot_longer(cols = c(primary, secondary, tertiary), names_to = "Measure", values_to = "Value") %>%
  left_join(., region_lookup) %>%
  select(CouncilName, Region, Measure, Value) %>%
  arrange(CouncilName)

#clean_classifications$Measure <- factor(clean_classifications$Measure, ordered = TRUE)
clean_classifications$Date <- session
clean_classifications$DateName <- format(as.Date(clean_classifications$Date), "%b-%y")

classifications_with_averages<- clean_classifications %>%
  group_by(Date, Region, Measure) %>%
  summarise(RegionAvg = round(mean(Value),1)) %>%
  left_join(clean_classifications, .) %>%
  distinct()
  

# merge new data with old-----------------------------------
updated_class_time_series <- rbind(classifications_with_averages, previous_class_data) %>%
  select(CouncilName, Region, Date, DateName, Measure, Value, RegionAvg)


#write data to clean data file folder use this session, and to time series folder for future sessions
write.csv(updated_class_time_series, paste0("Make Reports/Clean Data/classification-time-series-", session,".csv"), row.names = FALSE)
