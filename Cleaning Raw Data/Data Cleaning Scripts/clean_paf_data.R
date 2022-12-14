library(readxl)
library(config)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)

#READ IN DATA----------
paf_raw <- readxl::read_xls("Cleaning Raw Data/Raw Data/PAF_STATS_JUN.xls") 
#----------------------

session <- config::get("reporting_month")

paf_raw <- paf_raw %>%
  select(AUTH_CODE, LINK_RATE, CRITERION_LEVEL) %>%
  rename("LA" = AUTH_CODE)

paf_raw$LA <- as.character(paf_raw$LA)

code_lookup <- read.csv("Cleaning Raw Data/CodeLookUp.csv") %>%
  select(-AltName)

clean_paf <- left_join(paf_raw, code_lookup) %>%
  select(CouncilName, Region, LINK_RATE, CRITERION_LEVEL) %>%
  mutate(LinkRate = round(LINK_RATE*100,2)) %>%
  select(-LINK_RATE) %>%
  arrange(CouncilName)

write.csv(clean_paf, paste0("Make Reports/Clean Data/paf-", session,".csv"), row.names = FALSE)
