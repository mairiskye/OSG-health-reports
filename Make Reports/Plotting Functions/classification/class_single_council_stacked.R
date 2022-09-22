library(forcats)
library(ggrepel)

#to reproduce single council/Scotland class breakdown comparison (two stacked bars)
class_single_council_stacked <- function(data, council) {
  
  subset <- data %>% filter(CouncilName %in% c(council,"Scotland"),
                            Date == max(Date))
  #subset$alpha <- as.factor(ifelse(subset$CouncilName == "Scotland", 0.5, 1))
  
  stacked_bar <- ggplot(subset, aes(fill= forcats::fct_rev(Measure), y=Value, x=CouncilName, label= Value)) + 
    geom_col(colour = "black", width = 0.3) +
    #scale_alpha_manual(values = c("0.5"=0.6, "1"=1), guide='none') + # add "alpha = factor(alpha)" to ggplot aes()
    geom_text_repel(colour = "darkgray", size = 3, nudge_y = 0.6, nudge_x = -0.32, min.segment.length = 0.3) +
    scale_fill_manual(values = c("#084594", "#4292C6", "#C6DBEF")) +
    scale_y_continuous(n.breaks = 6) +
    ggtitle("Property Classification Proportions") +
    xlab("") +
    ylab("Classification (%)") + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(colour = "#082A75", size = 12),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 10),
          axis.title.x = element_text(size=8, colour = "#082A75"),
          axis.text = element_text(colour = "#082A75"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "gray", fill=NA, size=0.2),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line( size=.1, color="gray"),
          panel.grid.minor.y = element_blank(),
          #axis.text.x=element_text(angle = -35, hjust = 0)
    )
  return(stacked_bar)
}
