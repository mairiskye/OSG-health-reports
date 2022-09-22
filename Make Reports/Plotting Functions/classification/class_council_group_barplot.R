library(ggplot2)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(ggrepel)

class_council_group_barplot <- function(data, council) {
  
  #filter tertiary data and add region averages
  tertiary_data <- data %>% filter(Measure == "tertiary")
  tertiary_averages <- tertiary_data %>%
    group_by(Region, Date) %>%
    summarise(RegionAvg = round(mean(Value),1)) %>%
    left_join(tertiary_data, .)
  
  #filter by four most recent sessions (whole time series gives an untidy plot)
  sessions <- unique(tertiary_averages$Date) %>%
    sort(decreasing = TRUE)
  selected_sessions <- sessions[1:4]
  
  #subset data according to council and extract plot variables
  region <- tertiary_averages %>% filter(CouncilName == council) %>% pull(Region) %>% unique()
  region_data <- tertiary_averages %>% 
    filter(Region ==region,
           Date %in% selected_sessions)
  h_intercept <- region_data %>% filter(Date == max(Date)) %>% pull(RegionAvg) %>% unique()
  latest_date_label <- region_data %>%
    filter(Date == max(Date)) %>%
    pull(DateName) %>%
    unique()
  
  #constrain x-axis label width
  region_data$CouncilNamesWrapped <- wrap_it(region_data$CouncilName,9)
  
  #order councils by upload values (descending) in most recent year
  #this determines council order on x-axis
  council_order <- region_data %>% 
    filter(Date == max(Date)) %>% 
    arrange(desc(Value))
  
  date_name_order <- region_data %>%
    select(Date, DateName) %>%
    distinct() %>%
    arrange(Date, decreasing = TRUE) %>%
    pull(DateName)
  
  region_data$DateName <- factor(region_data$DateName, levels = date_name_order, ordered = TRUE)
  
  #order whole time-series data by above factor levels
  region_data_ordered <- region_data %>%
    mutate(CouncilNamesWrapped = factor(CouncilNamesWrapped, levels = council_order$CouncilNamesWrapped, ordered = TRUE)) %>%
    arrange(CouncilName)

  plot <- ggplot(region_data_ordered, aes(fill=DateName, y=Value, x = CouncilNamesWrapped)) + 
    geom_col(width = 0.7, position= position_dodge(0.8)) + 
    scale_y_continuous(n.breaks = 6) +
    scale_fill_manual(values =  c( "#C6DBEF", "#9ECAE1", "#4292C6", "#084594")) +
    geom_hline(aes(yintercept = h_intercept, linetype = paste0(region, " Average ", latest_date_label)),  size =0.8) +
    scale_linetype_manual(name = "", values = "longdash", 
                          guide = guide_legend(override.aes = list(color = "black"))) +
   #geom_text(aes(5, h_intercept, label = paste0("Average: ",h_intercept), vjust = -0.2),
    #          show.legend = FALSE) +
    ggtitle(paste0("Tertiary Properties (", region, " Region)"), 
            subtitle = "Proportion of properties recorded as tertiary.") + 
    xlab("") +
    ylab("Properties (%)") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(colour = "#082A75", size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 10),
          axis.title.x = element_text(size=8, colour = "#082A75"),
          axis.text = element_text(colour = "#082A75"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "gray", fill=NA, size=0.5),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line( size=.1, color="gray"),
          panel.grid.minor.y = element_blank(),
          #axis.text.x=element_text(angle = -35, hjust = 0)
    )
  return(plot)
}
