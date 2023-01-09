library(plotly)
library(tidyr)
library(dplyr)
library(ggplot2)

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

#if we want to order legend by most recent year decreasing
ordered_council_factor <- region_subset %>%
  filter(Date == max(Date)) %>%
  arrange(desc(Value)) %>%
  pull(CouncilName)


alpha_data <- region_subset

#bring region and council to top of legend list using factor
other_councils <- alpha_data %>% filter(!CouncilName %in% c(dummy_council, paste0(dummy_region, " Region"))) %>% pull(CouncilName) %>% unique()
highlighted_values <- c(dummy_council, paste0(dummy_region, " Region"))
council_order <- c("Your council/region", highlighted_values," ", "Other Councils", other_councils)


alpha_data <- region_subset
line1 <- c("Your council/region", NA, "Sep-22", 1, " ")
line2 <- c(" ", NA, "Sep-22", 1, " ")
line3 <- c("Other Councils", NA, "Sep-22", 1, " ")
tweak_data <- alpha_data %>%
  rbind(line1, line2, line3)
tweak_data$Value <- as.numeric(tweak_data$Value)
tweak_data$CouncilName <- factor(tweak_data$CouncilName, levels = council_order, ordered = TRUE)
tweak_data$DateName <- factor(tweak_data$DateName, levels = date_name_levels, ordered = TRUE)

viridisCols <- scales::viridis_pal()(10)
viridisTweak <- c("#ffffff", viridisCols[1:2], "#ffffff", "#ffffff", viridisCols[3:10] )


colour_and_alpha_plot <- ggplot(tweak_data, aes(x=DateName, y=Value, group = CouncilName, colour = CouncilName,  shape = CouncilName, alpha = CouncilName)) +
  geom_line(size = 1.2) +
  #scale_linetype_manual(values = c("solid",  "longdash"),guide = "none") +
  geom_point(size = 3) +
  #scale_alpha_discrete(range = c(1,0.9), guide = "none") + 
  scale_colour_manual(name = "Areas",
                      labels = council_order,
                      values = viridisTweak) +
  scale_shape_manual(name = "Areas",
                     labels = council_order,
                     values = rep(c(15, 17),6)) +
  scale_alpha_manual(name = "Areas",
                     labels = council_order,
                     values = c(0,1,1,0,0,0.8,0.8,0.8,0.8,0.8,0.8)
                     )+
  theme_minimal() + 
  labs(y = "Percentage", x = "Period") + 
  ggtitle("Teritary Property Proportions Over Time - grouped and alphabetical legend") #+

colour_and_alpha_plot

#default <- gg_color_h
#blues <- rev(RColorBrewer::brewer.pal(8,"Blues"))
#blue_yel <- rev(rcartocolor::carto_pal(6, "BluYl"))
#ylgnbu <- rev(RColorBrewer::brewer.pal(6,"YlGnBu"))
#rdbu <- ylgnblue <- rev(RColorBrewer::brewer.pal(6,"RdBu"))
#gnbu <- c("#064c7f", rev(RColorBrewer::brewer.pal(6,"GnBu")))
#mint <- rev(rcartocolor::carto_pal(6, "Mint"))
#emrld <- c(rev(rcartocolor::carto_pal(7, "Emrld")), "black")
#highlighted_values_colour <- c("#E6550D", "#A63603")
#palt<- c("darkred", "red3", gnbu)
#paulTolBrightFunc <- khroma::color("bright", reverse = TRUE)
#paulTolBright <- c(paulTolBrightFunc(7),"black")
#bright <- c("#BBBBBB", "#AA3377", "#66CCEE", "#CCBB44", "#228833", "#EE6677", "#4477AA")
