library(ggplot2)
library(ggthemes)

plot_match_proportions <- function(candidate_name, sex, sum_table) {
  match_probs_plot <- sum_table %>% 
    filter(.data[[sex]] == candidate_name) %>%
    ggplot(aes(night, prob)) + 
    geom_point() + geom_line() +
    labs(x = "Matching night", y = "Proportion of combinations") +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
    ggtitle(candidate_name) +
    facet_wrap(~boy, ncol = 5) +
    theme_stata()
  ggsave(paste0("graphics/", tolower(candidate_name), ".png"))
  match_probs_plot
}