library("scales")

errors_council_group_barplot <- function(data, council) {

region <- data %>% filter(CouncilName == council) %>% pull(Region) %>% unique()
region_subset <- data %>% filter(Region == region) 

region_subset$CouncilNamesWrapped <- wrap_it(region_subset$CouncilName,9)

#order councils by total errors (descending) in most recent year
#this determines council order on x-axis
select_order <- region_subset %>% 
  filter(Category == "TotalErrors") %>%
  arrange(desc(Value))

#order whole time-series data by above factor levels
region_subset_ordered <- region_subset %>%
  filter(Category != "TotalErrors") %>%
  mutate(CouncilNamesWrapped = factor(CouncilNamesWrapped, levels = select_order$CouncilNamesWrapped, ordered = TRUE)) %>%
  arrange(CouncilNamesWrapped, Category)

colour_blind_friendly <- c("#f3a902", "chartreuse3","blue4", "powderblue", "#D55E00")
h_intercept_region <- round(mean(select_order$Value),-3)
h_intercept_scotland <- data %>% 
  filter(Category =="TotalErrors") %>% 
  select(Value) %>% 
  summarise(Value = mean(Value)) %>% 
  pull(Value) %>%
  round(-3)

plot <- ggplot(region_subset_ordered, aes(fill=Category, y=Value, x=CouncilNamesWrapped)) + 
  #geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  geom_col(colour = "black", width = 0.5) +
  #scale_fill_manual(values = colour_blind_friendly) +
  scale_fill_manual(values = c("#DC267F", "#FE6100", "#FFB000","#648FFF", "#785EF0")) +
  scale_y_continuous(labels = comma, n.breaks = 8) +
  geom_hline(aes(yintercept = h_intercept_scotland, linetype = paste0("Scotland Average")), size =0.7, color = "gray80") +
  #geom_label(aes(6, h_intercept_scotland, label = paste0("Scotland: ",format(h_intercept_scotland, big.mark = ","))),
  #           size = 3,
  #          fill = "white",
  #          colour = "ivory4",
  #           show.legend = FALSE) +
  geom_hline(aes(yintercept = h_intercept_region, linetype = paste0(region," Average")), size =0.7, color = "gray30") +
  #geom_label(aes(6, h_intercept_region, label = paste0(region,": ",format(h_intercept_region, big.mark = ","))),
   #         fill = "white",
   #         size = 3,
   #         show.legend = FALSE) +
  scale_linetype_manual(name = "", values = c("longdash", "longdash"),
                        guide = guide_legend(override.aes = list(color = c("gray80", "gray30")))) +
  #guides(linetype=guide_legend(override.aes=list(colour = c( "ivory4", "black"), linetype = c(2, 1)))) +
  labs(title = paste0(region, " Region - Total Errors"), x = "", y = "Total Errors") + 
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
        axis.text.x=element_text(angle = -35, hjust = 0)
  )

return(plot)

}
