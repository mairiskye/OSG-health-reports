
uploads_regional_summary_barplot <- function(data) {
  
  region_summary <- data %>%
    select(Region, Date, DateName, RegionAvg) %>%
    distinct()

  h_intercept <- data %>% 
    filter(Date == max(Date)) %>% 
    select(Value) %>%
    summarise(NationalAverage = mean(Value)) %>%
    pull(NationalAverage) %>%
    round(., 1)
  
  latest_date <- region_summary %>%
    filter(Date ==max(Date)) %>%
    pull(DateName) %>%
    unique()
  
  #order regions by upload values (descending) in most recent year
  #this determines region order on x-axis
  select_order <- region_summary %>% 
    filter(Date == max(Date)) %>% 
    arrange(desc(RegionAvg))
  
  date_name_order <- region_summary %>%
    select(Date, DateName) %>%
    distinct() %>%
    arrange(Date, decreasing = TRUE) %>%
    pull(DateName)
  
  #order whole time-series data by above factor levels
  region_data_ordered <- region_summary %>%
    mutate(Region = factor(Region, levels = select_order$Region, ordered = TRUE)) %>%
    mutate(DateName = factor(DateName, levels = date_name_order, ordered = TRUE)) %>%
    arrange(Region)
  
  #region_data_ordered$DateName <- format(as.Date(region_data_ordered$Date), "%b-%y")
  
    #create plot
    plot <- ggplot(region_data_ordered, aes(fill=DateName, y=RegionAvg, x = Region)) + 
    geom_col(width = 0.7, position= position_dodge(0.8)) + 
    scale_y_continuous(n.breaks = 6) +
    scale_fill_manual(values =  c( "#C6DBEF", "#9ECAE1", "#4292C6", "#084594")) +
    geom_hline(aes(yintercept = h_intercept, linetype = paste0("National Average ", latest_date)), size =1, color = "black") +
    scale_linetype_manual(name = "", values = "longdash", 
                            guide = guide_legend(override.aes = list(color = "black"))) +
    #geom_text(aes(1, h_intercept, label = paste0("Average: ",h_intercept), vjust = -0.5),
    #          size = 5,
    #          show.legend = FALSE) +
    ggtitle("OSG Uploads - Region Averages",
            subtitle ="Includes A and J File Types") + 
    xlab("") +
    ylab("Average Uploads") +
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

