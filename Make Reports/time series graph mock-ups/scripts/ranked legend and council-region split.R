library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtext)

session <- config::get("reporting_month")
class_data <- read.csv(paste0("Make Reports/Clean Data/classification-time-series-", session, ".csv"))#

dummy_region <- "South East"
dummy_council <- "City of Edinburgh"

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
concat_council_value <- region_subset %>%
  filter(Date == max(Date)) %>%
  mutate(council_labels = paste0(CouncilName, " / ", round(Value,1))) %>%
  arrange(desc(Value)) %>%
  select(CouncilName, council_labels)

council_levels <- concat_council_value %>%
  filter(CouncilName != paste0(dummy_region, " Region")) %>%
  pull(council_labels)

region_label <- concat_council_value %>%
  filter(CouncilName == paste0(dummy_region, " Region")) %>%
  pull(council_labels)

selected_council_label <- concat_council_value %>%
  filter(CouncilName == dummy_council) %>%
  pull(council_labels)

plot_1_data <- region_subset %>%
  left_join(concat_council_value, by = "CouncilName")

#bring region and council to top of legend list using factor
council_order <- c("Your region", region_label," ", "Councils", council_levels)


arb_line1 <- c("Your region", NA, "Sep-22", 1, " ", "Your region")
arb_line2 <- c(" ", NA, "Sep-22", 1, " ", " ")
arb_line3 <- c("Councils", NA, "Sep-22", 1, " ", "Councils")
labelled_plot_data <- plot_1_data %>%
  rbind(arb_line1, arb_line2, arb_line3)
labelled_plot_data$Value <- round(as.numeric(labelled_plot_data$Value),1)
labelled_plot_data$council_labels <- factor(labelled_plot_data$council_labels, levels = council_order, ordered = TRUE)
labelled_plot_data$DateName <- factor(labelled_plot_data$DateName, levels = date_name_levels, ordered = TRUE)

viridisCols <- scales::viridis_pal()(8)
viridisTweak <- c("#ffffff", viridisCols[1], "#ffffff", "#ffffff", viridisCols[2:8])

council_order[which(selected_council_label == council_order)] <- paste0("**", selected_council_label, "**")
council_order[which(region_label == council_order)] <- paste0("**", region_label, "**")

colour_and_alpha_and_value_plot <- ggplot(labelled_plot_data, aes(x=DateName, 
                                                                  y=Value, 
                                                                  group = council_labels, 
                                                                  colour = council_labels,  
                                                                  shape = council_labels, 
                                                                  alpha = council_labels)) +
  geom_line(size = 1.2) +
  #scale_linetype_manual(values = c("solid",  "longdash"),guide = "none") +
  geom_point(size = 3) +
  #scale_alpha_discrete(range = c(1,0.9), guide = "none") + 
  scale_colour_manual(name = "Areas",
                      labels = council_order,
                      values = viridisTweak) +
  scale_shape_manual(name = "Areas",
                     labels = council_order,
                     values = rep(c(15, 17),8)) +
  scale_alpha_manual(name = "Areas",
                     labels = council_order,
                     values = c(0,0.8,0,0,rep(0.8,8))
  )+
  theme_minimal() + 
  labs(y = "Percentage", x = "Period") + 
  ggtitle("Teritary Property Proportions Over Time") +
  theme(legend.text = element_markdown())

colour_and_alpha_and_value_plot
