library(tidyverse)

# following https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2267325/; figure 2
block_lookup <- tribble(
  ~block_number, ~block_content,
  1, c("yes", "no"),
  2, c("no", "yes"),
  3, c("yes", "yes", "no", "no"),
  4, c("yes", "no", "yes", "no"),
  5, c("yes", "no", "no", "yes"),
  6, c("no", "yes", "yes", "no"),
  7, c("no", "yes", "no", "yes"),
  8, c("no", "no", "yes", "yes")
)

set.seed(983745)
sample_indices <- sample(1:8, 300, replace = TRUE)

blocks <- tibble(block_number = sample_indices) %>% 
  left_join(block_lookup) %>% 
  unnest(block_content)

# check number of T & F
blocks %>% 
  count(block_content)
# equal proportions - success!!

# generate block IDs
base_for_block_id <- tibble(block_number = sample_indices) %>% 
  left_join(mutate(block_lookup, block_length = map_int(block_content, length))) %>% 
  select(block_number, block_length)

block_id <- rep(1:300, times = base_for_block_id$block_length)

blocks_cleaned <- blocks %>% 
  mutate(randomisation_id = seq_along(block_content),
         block_id = block_id) %>% 
  select(randomisation_id, send_email = block_content,
         block_id)

write_csv(blocks_cleaned, "study_randomisation.csv")
