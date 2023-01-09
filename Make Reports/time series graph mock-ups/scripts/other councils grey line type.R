library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(geomtextpath)
library(grid)

session <- config::get("reporting_month")
class_data <- read.csv(paste0("Make Reports/Clean Data/classification-time-series-", session, ".csv"))

dummy_region <- "North"
dummy_council <- "Highland"

region_averages <- class_data %>%
  filter(Measure == "tertiary") %>%
  group_by(Region, Date, DateName) %>%
  summarise(Value = mean(Value)) %>%
  mutate(CouncilName = paste0(Region, " Region"))

all_data <- class_data%>%
  filter(Measure == "tertiary") %>%
  select(!Measure) %>%
  select(!RegionAvg) %>%
  rbind(region_averages)

region_subset <-all_data %>%
  filter(Region == dummy_region) %>%
  select(CouncilName, Date, DateName, Value, Region) %>% 
  arrange(Date)


line_data <- region_subset
region_subset_levels <- c(dummy_council, paste0(dummy_region, " Region"), other_councils)
line_data$CouncilName <- factor(line_data$CouncilName, levels = region_subset_levels, ordered = TRUE)

other_councils <- line_data %>% filter(!CouncilName %in% c(dummy_council, paste0(dummy_region, " Region"))) %>% pull(CouncilName) %>% unique()
highlighted_values <- c(dummy_council, paste0(dummy_region, " Region"))
council_order <- c("Your council/region", highlighted_values," ", "Other Councils", other_councils)
linetypes <- c("F1", "longdash", "dashed",
               "twodash", "dotdash","dotted", "1F")

colour_and_line_plot <- ggplot(line_data, aes(x=DateName, y=Value, group = CouncilName, linetype = CouncilName, colour = CouncilName)) +
  geom_line(size = 1.2) + 
  scale_color_manual(name = "Councils",
                     labels = region_subset_levels,
                     #values = c("steelblue4", "steelblue3", "snow3", "snow4", "snow3", "snow4","snow3", "snow4")
                     values = c("steelblue4", "steelblue3", "snow3", "snow3", "snow3", "snow3","snow3", "snow3")
  ) +
  scale_linetype_manual(name = "Councils",
                        labels = region_subset_levels,
                        values = c("solid", "solid", linetypes)) +
  theme_minimal() + 
  labs(y = "Percentage", x = "Period") + 
  ggtitle("Teritary Property Proportions Over Time") +
  theme(legend.key.width=unit(3,"cm"))

colour_and_line_plot 