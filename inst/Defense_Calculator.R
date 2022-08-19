library(magrittr)
library(defense)

dem <-
  FedData::get_ned(FedData::meve,
                 label = "meve") %>%
  defense::di_calculate_defensibility_index()
