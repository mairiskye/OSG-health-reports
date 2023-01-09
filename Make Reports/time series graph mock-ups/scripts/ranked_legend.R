library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(geomtextpath)
library(grid)
library(viridis)
session <- config::get("reporting_month")
class_data <- read.csv(paste0("Make Reports/Clean Data/classification-time-series-", session, ".csv"))#

dummy_region <- "North"
dummy_council <- "Highland"

date_name_levels <- class_data %>%
  select(Date, DateName) %>%
  unique() %>%
  arrange(Date) %>%
  pull(DateName)

region_averages <- class_data %>%
  filter(Measure == "tertiary") %>%
  group_by(Region, Date, DateName) %>%
  summarise(Value = round(mean(Value),1)) %>%
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

#if we want to order legend by most recent year decreasing
ordered_council_factor <- region_subset %>%
  filter(Date == max(Date)) %>%
  arrange(desc(Value)) %>%
  pull(CouncilName)

plot_data <- region_subset

plot_data$Value <- as.numeric(plot_data$Value)
plot_data$CouncilName <- factor(plot_data$CouncilName, levels = ordered_council_factor, ordered = TRUE)
plot_data$DateName <- factor(plot_data$DateName, levels = date_name_levels, ordered = TRUE)

viridisCols <- scales::viridis_pal()(8)

plot_data <- plot_data %>%
  mutate(council_labels = paste0(CouncilName, " / ", Value))

data_labels <- plot_data %>%
  filter(Date==max(Date)) %>%
  arrange(CouncilName) %>%
  pull(council_labels)

latest <- max(plot_data$DateName)

plot3 <- ggplot(plot_data, aes(x=DateName, y=Value, group = CouncilName, colour = CouncilName,  shape = CouncilName, alpha = CouncilName)) +
  geom_line(size = 1.2) +
  #scale_linetype_manual(values = c("solid",  "longdash"),guide = "none") +
  geom_point(size = 3) +
  #scale_alpha_discrete(range = c(1,0.9), guide = "none") + 
  scale_colour_manual(name = paste0("Council / ",latest, " rankings"),
                      labels = data_labels,
                      values = viridisCols) +
  scale_shape_manual(name = paste0("Council / ",latest, " rankings"),
                     labels = data_labels,
                     values = c(15,17,15,17,15,17,15,17,15,17,15,17)) +
  scale_alpha_manual(name = paste0("Council / ",latest, " rankings"),
                     labels = data_labels,
                     values = c(0.8, 0.8 ,0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
  )+
  theme_minimal() + 
  labs(y = "Percentage", x = "Period") + 
  ggtitle("Teritary Property Proportions Over Time") #+

plot3
