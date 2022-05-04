library(ggplot2)
library(ggthemes)


plot_match_probabilities <- function(name_of_girl, sum_table) {
  match_probs_plot <- sum_table %>% 
    filter(girl == name_of_girl) %>%
    ggplot(aes(night, prob)) + 
    geom_point() + geom_line() +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) + 
    ggtitle(name_of_girl) +
    facet_wrap(~boy, ncol = 5) +
    theme_stata()
  ggsave(paste0("graphics/", tolower(name_of_girl), ".png"))
  match_probs_plot
}