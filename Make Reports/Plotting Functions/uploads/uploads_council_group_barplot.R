library(ggplot2)
library(magrittr)
library(dplyr)

uploads_council_group_barplot <- function(data, council) {

  #filter by four most recent sessions (whole time series gives an untidy plot)
  sessions <- unique(data$Date) %>%
    sort(decreasing = TRUE)
  selected_sessions <- sessions[1:4]
  
  #subset data and extract plot variables according to council/period
  region <- data %>% filter(CouncilName == council) %>% pull(Region) %>% unique()
  region_data <- data %>% filter(Region ==region, Date %in% selected_sessions)
  h_intercept <- region_data %>% filter(Date == max(Date)) %>% pull(RegionAvg) %>% unique()
  latest_date <- region_data %>%
    filter(Date == max(Date)) %>%
    pull(DateName) %>%
    unique()
  
  #constrain x-axis label length
  region_data$CouncilNamesWrapped <- wrap_it(region_data$CouncilName,10)
  
  #order councils by upload values (descending) in most recent year
  #this will be used to determines council order on x-axis
  council_select_order <- region_data %>% 
    filter(Date == max(Date)) %>% 
    arrange(desc(Value))
  
  #as above but for DateName
  date_order <- region_data %>%
    select(Date, DateName) %>%
    distinct() %>%
    arrange(Date)
  
  #order whole time-series data by above factor levels
  region_data_ordered <- region_data %>%
    mutate(CouncilNamesWrapped = factor(CouncilNamesWrapped, levels = council_select_order$CouncilNamesWrapped, ordered = TRUE)) %>%
    mutate(DateName = factor(DateName, levels = date_order$DateName, ordered = TRUE)) %>%
    arrange(CouncilName)
  
  plot <- ggplot(region_data_ordered, aes(fill=DateName, y=Value, x = CouncilNamesWrapped)) + 
    geom_col(width = 0.7, position= position_dodge(0.8)) + 
    scale_y_continuous(n.breaks = 6) +
    scale_fill_manual(values = c( "#C6DBEF", "#9ECAE1", "#4292C6", "#084594")) +
    geom_hline(aes(yintercept = h_intercept, linetype = paste0("Region Average ", latest_date)), size =1, color = "black") +
    scale_linetype_manual(name = "", values = "longdash", 
                          guide = guide_legend(override.aes = list(color = "black"))) +
    #geom_text(aes(1, h_intercept, label = paste0("Average: ",h_intercept), vjust = -0.5),
    #          size = 5,
     #         show.legend = FALSE) +
    ggtitle(paste0("OSG Uploads - ", region, " Region (Includes A and J File Types)")) + 
    xlab("") +
    ylab("Uploads") +
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
