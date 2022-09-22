
uploads_council_group_recent_barplot <- function(data, council) {

session <- format(as.Date(max(data$Date)), "%b-%y")
region <- data %>% filter(CouncilName ==council) %>% pull(Region) %>% unique()
region_session_subset <- data %>% filter(Date == max(Date), Region == region)
h_intercept <- round(mean(region_session_subset$Value),1)
councils_abbr <- wrap_it(region_session_subset$CouncilName, 7)

plot <- ggplot(region_session_subset, aes(y=Value, x = reorder(councils_abbr, -Value))) + 
  geom_col(width = 0.7, position= position_dodge(0.8), fill = "#084594") + 
  scale_y_continuous(n.breaks = 6) +
  geom_hline(aes(yintercept = h_intercept, linetype = paste0(region," Average ")), size =1, color = "black") +
  scale_linetype_manual(name = "", values = "longdash", 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  #geom_text(aes(6, h_intercept, label = paste0(region, " Average: ",h_intercept), vjust = -0.5),
  #          size = 4,
  #          show.legend = FALSE) +
  ggtitle(paste0("OSG Uploads ", region, " Region (", session, ")"), 
       subtitle = "Includes A and J File Types") +
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
