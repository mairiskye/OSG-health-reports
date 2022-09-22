order_factor_variables <- function(data, name_var, sort_var) {
  
  subset <- data %>%
    filter(Date == max(Date)) %>%
    select({{sort_var}}, {{name_var}}) %>%
    distinct()
  
   ordered_levels <- subset[rev(order(subset[[sort_var]])),] %>%
    pull({{name_var}})
  
  data[[name_var]] <- factor(data[[name_var]], levels = ordered_levels, ordered = TRUE)
  return(data)
}
