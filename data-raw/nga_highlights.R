library(tidyverse)
library(stringr)

nga_highlights <- clean_collection_data %>%
  filter(onview == "true") %>%
  filter(!is.na(area)) %>%
  mutate(
    label = str_wrap(glue("{artists.0.name}, \"{title}\""), width = 30),
    long_label = glue("{artists.0.name}, \"{title}\" (National Gallery of Art)", width = 30)) %>%
  select(label, long_label, width, height, area) %>%
  arrange(area)

save(nga_highlights, file = "data/nga_highlights.rda")
