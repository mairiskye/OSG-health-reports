library(magrittr)
library(dplyr)
library(tidyr)

#READ IN DATA ----------------
errors_raw <- readxl::read_xlsx("Cleaning Raw Data/Raw Data/Edited Warning Records Aug 2022.xlsx")

#------------------------------

#extracts date for this reporting session from config.yml file
session <- config::get("reporting_month")


#clean data -----------------------------
code_lookup <- read.csv("Cleaning Raw Data/CodeLookUp.csv") %>%
  select(-AltName)

#for errors data look for row index which contain strings in column one 
errors_no_blank_rows <- errors_raw[rowSums(is.na(errors_raw)) != ncol(errors_raw), ]
#find and return a vecor of strings found in Sr.No. column
strings <- grep('^[A-Za-z ]+', errors_no_blank_rows$Sr.No., value = TRUE)
# return indices of those rows which contain strings in Sr.No column
indices <- which(errors_no_blank_rows$Sr.No. %in% strings)
#create new column to store error categories
errors_no_blank_rows$category <- ""

#n and last are loop variables to walk through the strings/indices vectors
n <- 1
last <- length(strings)
bottom_row_index <- nrow(errors_no_blank_rows)
errors_with_categories <- errors_no_blank_rows

#populate the category column with the appropriate string 
while (n <= last) {
  category_start <- indices[n]
  category_end <- ifelse(n < last, indices[n+1] - 1, bottom_row_index)
  error_type <- strings[n]
  errors_with_categories$category[category_start : category_end] <- error_type
  n <- n+1
}

#remove category heading rows
errors_with_categories <- errors_with_categories[-indices,] %>%
  rename("LA" = CUSTODIAN_CODE) %>%
  rename("CouncilName" = ORGANISATION_NAME)
errors_with_categories$LA <- as.character(errors_with_categories$LA )
clean_errors <- left_join(errors_with_categories, code_lookup) %>%
  select(-c(Sr.No., LA)) %>%
  mutate(Date = session)

#Aggregate data by error type-----------------------------------
council_error_summary <- clean_errors %>%
  group_by(CouncilName, category) %>%
  summarise(Value = sum(FAIL_COUNT))

error_summary <- pivot_wider(council_error_summary, id_cols = CouncilName, names_from = category, values_from = Value) %>%
  mutate(TotalErrors = sum(Classifications, Levels, Streets, LPI, BLPU, na.rm = TRUE))

#scotland_totals <- summarise_all(error_summary[,2:7], sum, na.rm = TRUE)
#error_summary_complete <- rbind(error_summary, scotland_totals)
#error_summary_complete[33,1] <- "Scotland"

error_summary_long <- error_summary %>% pivot_longer(., 2:7, names_to = "Category", values_to = "Value") %>%
  left_join(., code_lookup[,2:3])
error_summary_long[is.na(error_summary_long)] <- 0

#write to csv-----------------------------------------
write.csv(error_summary_long, paste0("Make Reports/Clean Data/errors-", session, ".csv"), row.names = FALSE)
