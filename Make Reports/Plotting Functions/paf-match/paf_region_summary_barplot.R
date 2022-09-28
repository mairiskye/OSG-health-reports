
paf_region_summary_barplot <- function(data) {

region_summary <- data %>%
  group_by(Region) %>%
  summarise(LinkRate = round(mean(LinkRate),1))
h_intercept <- round(mean(data$LinkRate),1)

ggplot(region_summary, aes(y=LinkRate, x = reorder(Region, -LinkRate))) + 
  geom_col(width = 0.5, position= position_dodge(0.8), fill = "#082A75") + 
  scale_y_continuous(n.breaks = 5) +
  coord_cartesian(ylim=c(97,100)) +
  geom_hline(aes(yintercept = h_intercept, linetype = "Average of Regions"), size =0.8, color = "darkgray") + 
  scale_linetype_manual(name = "", values = "longdash", 
                        guide = guide_legend(override.aes = list(color = "darkgray"))) +
  #geom_text(aes(1.2, h_intercept, label = paste0("National Average: ",h_intercept), vjust = -0.5),
  #          size = 5,
  #          show.legend = FALSE) +
  ggtitle(paste0("PAF MATCH - Regional Averages")) + 
  xlab("") +
  ylab("Average Link Rate (%)") +
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

}
