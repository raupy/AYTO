library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

add_labels_to_plot <- function(plt, candidate_name) {
  plt + 
    labs(x = "Matching night", y = "Proportion of combinations") +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
    ggtitle(paste0(candidate_name, "'s perfect match development"))
}

filter_for_candidate <- function(candidate_name, sex, sum_table) {
  sum_table %>% 
    filter(.data[[sex]] == candidate_name)
}


plot_match_proportions_grid <- function(candidate_name, sex, sum_table) {
  match_probs_plot <- filter_for_candidate(candidate_name, sex, sum_table) %>%
    ggplot(aes(night, prob)) + 
    geom_point() + geom_line() + 
    facet_wrap(~ boy, ncol = 5) +
    theme_stata()
  
  match_probs_plot <- add_labels_to_plot(match_probs_plot, candidate_name) 
  ggsave(paste0("graphics/", tolower(candidate_name), "_grid.png"))
  match_probs_plot
}

plot_match_proportions_area <- function(candidate_name, sex, sum_table) {
  match_probs_plot <- filter_for_candidate(candidate_name, sex, sum_table) %>%
    ggplot(aes(night, prob, fill = boy)) + 
    geom_area(alpha = 0.6 , size = 0.5, colour = "white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() 
  
  match_probs_plot <- add_labels_to_plot(match_probs_plot, candidate_name)
  ggsave(paste0("graphics/", tolower(candidate_name), "_area.png"))
  match_probs_plot
}

