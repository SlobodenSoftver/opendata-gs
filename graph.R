#' A ggplot function to draw bar charts  from the long-format results data
#' 
#' @param data_ data frame or tibble for the data to plot
#' @param question_ numeric or character of length = 1 for which question to plot
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom stringr str_wrap
#' 
#' @examples 
#' library(readr)
#' qal <- read_csv(file = "data/results_long.csv")
#' bar_graph(data_ = qal, question_ = 2, top_ = 4)
#' 
#' @export 

bar_graph <- function(data_, question_, top_ = NULL) {
  
  if (is.numeric(question_)) {
    to_plot <- data_ %>% 
      filter(question_number == question_)
  } else {
    to_plot <- data_ %>% 
      filter(question == question_)
  }
  
  tallied <- to_plot %>% 
    group_by(answer) %>% 
    tally() %>% 
    arrange(desc(n)) 
  
  if (!is.null(top_)) {
    tallied <- tallied %>% 
      slice_head(n = top_)
  }
  
  ggplot(data = tallied,
  mapping = aes(x = answer, y=n)) +
    geom_col(aes(fill = answer), color = "black", size = .3) +
    scale_fill_brewer(palette = "Set3") +
    theme_classic() +
    labs(y = "Вкупно") +
    labs(x = "") +
    labs(title = stringr::str_wrap(unique(to_plot$question), 60)) +
    coord_flip() +
    theme(legend.position = "none")
}
