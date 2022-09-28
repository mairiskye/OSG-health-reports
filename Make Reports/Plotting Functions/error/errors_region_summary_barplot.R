library(scales)
library(magrittr)
library(dplyr)
library(ggplot2)

errors_region_summary_barplot <- function(data) {

region_summary <- data %>% 
  filter(Category != "TotalErrors") %>% 
  group_by(Region, Category) %>% 
  summarise(Value = sum(Value))

select_order <-  data %>% 
  filter(Category == "TotalErrors") %>%
  group_by(Region, Category) %>% 
  summarise(Value = sum(Value)) %>%
  arrange(desc(Value))

region_summary_ordered <- region_summary %>%
  mutate(Region = factor(Region, levels = select_order$Region, ordered = TRUE)) %>%
  arrange(Region)

colour_blind_friendly <- c("#f3a902", "chartreuse3","blue4", "powderblue", "#D55E00")
h_intercept <- round(mean(select_order$Value),-3)

plot <- ggplot(region_summary_ordered, aes(fill=Category, y=Value, x=Region)) + 
  geom_col(colour = "black", width = 0.5) +
  scale_fill_manual(values = colour_blind_friendly) +
  scale_y_continuous(label = comma, n.breaks = 8) +
  geom_hline(aes(yintercept = h_intercept, linetype = "Average of Regions"), size =0.7, color = "gray30") +
  scale_linetype_manual(name = "", values = "longdash",
                        guide = guide_legend(override.aes = list(color = "gray30"))) +
  #geom_label(aes(4.5, h_intercept, label = paste0("Scotland: ",format(h_intercept, big.mark = ","))),
  #          fill = "white",
  #          #size = 4,
  #          show.legend = FALSE) +
  ggtitle(paste0("All Regions - Total Errors")) +
  xlab("") +
  ylab("Total Errors") + 
  theme(#legend.position = "bottom",
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
