get_label_levels <- function(data, name_var, sort_var) {
  
  subset <- data %>%
    select({{sort_var}}, {{name_var}}) %>%
    distinct()
  
  ordered_levels <- subset[rev(order(subset[[sort_var]])),] %>%
    pull({{name_var}})
  
  #data[[name_var]] <- factor(data[[name_var]], levels = ordered_levels, ordered = TRUE)
  return(ordered_levels)
}