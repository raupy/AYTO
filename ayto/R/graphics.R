library(ggthemes)
library(viridis)

add_labels_to_plot <- function(plt, candidate_name) {
  plt + 
    labs(x = "Matching Night", y = "Anteil an möglichen Kombinationen") +
    scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
    ggtitle(paste0(candidate_name, "s Perfect-Match-Entwicklung"))
}

filter_for_candidate <- function(candidate_name, sex, sum_table) {
  sum_table %>% 
    filter(.data[[sex]] == candidate_name)
}


plot_match_proportions_grid <- function(candidate_name, sex, sum_table) {
  match_probs_plot <- filter_for_candidate(candidate_name, sex, sum_table) %>%
    mutate(Anteil = round(prob, 4)) %>%
    #rename("Matching Night" = night, "Anteil an möglichen Kombinationen (in Prozent)" = anteil) %>%
    rename(Night = night) %>%
    #ggplot(aes_(quote(Matching Night), quote("Anteil an möglichen Kombinationen (in Prozent)"))) + 
    ggplot(aes(Night, Anteil)) + 
    geom_point() + geom_line() + 
    facet_wrap(~ boy, ncol = 5, scales = "free_x") 
    #theme(strip.background = element_blank(), strip.placement = "outside")#switch = "x")  
  
  match_probs_plot <- add_labels_to_plot(match_probs_plot, candidate_name) 
  match_probs_plot
}

plot_match_proportions_area <- function(candidate_name, sex, sum_table) {
  match_probs_plot <- filter_for_candidate(candidate_name, sex, sum_table) %>%
    mutate(Anteil = round(prob, 4)) %>%
    rename(Boy = boy, Night = night) %>%
    ggplot(aes(Night, Anteil, fill = Boy)) + 
    geom_area(alpha = 0.6 , size = 0.5, colour = "white") +
    scale_fill_viridis(discrete = T) 
  
  match_probs_plot <- add_labels_to_plot(match_probs_plot, candidate_name)
  match_probs_plot
}

#sapply(girls, plot_match_proportions_grid, "girl", summarized_table)