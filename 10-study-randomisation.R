library(tidyverse)

# following https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2267325/; figure 2
block_lookup <- tribble(
  ~block_number, ~block_content,
  1, c(TRUE, FALSE),
  2, c(FALSE, TRUE),
  3, c(TRUE, TRUE, FALSE, FALSE),
  4, c(TRUE, FALSE, TRUE, FALSE),
  5, c(TRUE, FALSE, FALSE, TRUE),
  6, c(FALSE, TRUE, TRUE, FALSE),
  7, c(FALSE, TRUE, FALSE, TRUE),
  8, c(FALSE, FALSE, TRUE, TRUE)
)

# set.seed()
sample_indices <- sample(1:8, 250, replace = TRUE)

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

block_id <- rep(1:250, times = base_for_block_id$block_length)

blocks_cleaned <- blocks %>% 
  mutate(randomisation_id = seq_along(block_content),
         block_id = block_id) %>% 
  select(randomisation_id, send_email = block_content,
         block_id)

write_csv(blocks_cleaned, "sample_randomisation.csv")
