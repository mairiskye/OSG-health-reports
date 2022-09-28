library(magrittr)
library(dplyr)
library(ggplot2)

paf_council_group_barplot <- function(data, council) {

data$CRITERION_LEVEL <- factor(data$CRITERION_LEVEL, levels = c("Gold","BNS"), ordered = TRUE)
region <- data %>% filter(CouncilName == council) %>% pull(Region)
region_subset <- data %>% filter(Region == region)
h_intercept <- mean(region_subset$LinkRate) %>% round(., 2)
abbr_councils <- wrap_it(region_subset$CouncilName, 10)

ggplot(region_subset, aes(y=LinkRate, x = reorder(abbr_councils, -LinkRate), fill = CRITERION_LEVEL)) + 
  geom_col(width = 0.5, position= position_dodge(0.8)) + 
  scale_fill_manual(values = c("#082A75", "red")) +
  coord_cartesian(ylim=c(97,100)) +
  scale_y_continuous(n.breaks = 5) +
  geom_hline(aes(yintercept = h_intercept, linetype = paste0(region,"Average")), size =0.8, color = "darkgray") + 
  scale_linetype_manual(name = "", values = "longdash", 
                        guide = guide_legend(override.aes = list(color = "darkgray"))) +
  #geom_text(aes(2, h_intercept, label = paste0(region, " Average: ",h_intercept), vjust = -0.5),
  #          size = 5,
  #          show.legend = FALSE) +
  ggtitle(paste0("PAF MATCH - ", region, " Region")) + 
  xlab("") +
  ylab(" Link Rate (%)") +
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
