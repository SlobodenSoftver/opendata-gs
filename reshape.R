# convert to long format 
# question_number, question, answer

library(tidyverse)

qa <- read_csv(file = "data/results.csv")
cols <- names(qa)[-c(1)]
qal <- qa %>% 
  dplyr::select(-Timestamp) %>% 
  tidyr::pivot_longer(
    data = .,
    cols = all_of(cols),
    names_to = "question",
    values_to = "answer"
  ) %>% 
  dplyr::mutate(question_number = as.numeric(factor(question, levels = cols))) %>%
  select(question_number, question, answer) %>% 
  dplyr::arrange(., question_number)

write_csv(x = qal, ,path = "data/results_long.csv")
