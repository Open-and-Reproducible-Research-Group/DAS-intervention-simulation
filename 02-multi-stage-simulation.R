library(tidyverse)
library(brms)
library(marginaleffects)
library(future)
library(furrr)
library(glue)

bernoulli <- function(n, p) {
  runif(n) < p
}


base_parameters <- tibble(
  journal = LETTERS[1:2] %>% rep(each = 2),
  intervention = rep(c(TRUE, FALSE), 2)
) %>% 
  mutate(
    n = case_when(
      journal == "A" ~ 250,
      journal == "B" ~ 250
    ),
    intercept = case_when(
      journal == "A" ~ .1,
      journal == "B" ~ .1
    ),
    effect = case_when(
      journal == "A" & intervention ~ .05,
      journal == "B" & intervention ~ .05,
      TRUE ~ 0
    ))


varied_sample_size_5perc_increase <- map(
  seq(.2, 3, by = .2), 
  ~mutate(base_parameters, n = n * .x,
          prob = intercept + effect)
)

varied_sample_size_10perc_increase <- map(
  varied_sample_size_5perc_increase,
  ~mutate(., effect = effect * 2,
          prob = intercept + effect)
)

# check whether sample sizes are as expected
get_true_effect <- function(df) {
  df %>% 
    filter(intervention) %>% 
    mutate(journal_effect = (1 - intercept) * effect) %>% 
    summarise(avg_effect = weighted.mean(journal_effect, n)) %>% 
    pull(avg_effect)
}

map_dbl(varied_sample_size_5perc_increase, get_true_effect)
map_dbl(varied_sample_size_10perc_increase, get_true_effect)


# generate random data ------
simulate_data <- function(df) {
  df %>% 
    # stage 1: simulate pre-intervention state of DAS based on intercept
    mutate(pre_intervention = map2(n, intercept, ~bernoulli(.x / 2, .y)),
           true_effect = get_true_effect(df)) %>% 
    unnest(pre_intervention) %>% 
    # stage 2: simulate change due to the intervention. We use a simple assumption:
    # negative change is not possible. Out of those not sharing previously, we 
    # want to see an increase by 10%, which would result on average in 19% sharing
    # (assuming an intercept of .1)
    mutate(non_sharers = n() - sum(pre_intervention),
           post_intervention = case_when(
             !pre_intervention ~ bernoulli(non_sharers, effect),
             TRUE ~ pre_intervention
           ),
           # introduce some random noise. it might be that people change their DAS for 
           # other reasons. It might also be that the intervention has a negative effect
           # in some cases. 
           # However, it is hard to estimate what this change rate might be. Once we have
           # data from the control group, we can plug this in here to get a better 
           # calculation of our post-hoc power.
           post_intervention = if_else(bernoulli(n, .01), !post_intervention, 
                                       post_intervention))
}

dummy_data <- simulate_data(varied_sample_size_5perc_increase[[1]])

# set up base model ------
message("Setting up base model.")
base_model <- brm(post_intervention ~ pre_intervention + intervention,
                  family = brms::bernoulli(), 
                  data = dummy_data, 
                  prior = c(
                    prior(normal(0, 5), class = b)
                  ),
                  chains = 0)


if (!dir.exists("individual_runs")) {
  message("No directory for individual runs - creating 'individual_runs'.")
  dir.create("individual_runs")
} else {
  message("Directory for runs already present, re-using 'individual_runs'.")
}

get_avg_slopes <- function(run_id, parameters, base_model, write_to_disk = FALSE) {
  sim_data <- simulate_data(parameters)
  
  n <- nrow(sim_data)
  true_effect <- unique(sim_data$true_effect)
  
  # capture the warnings
  logs = vector("character")
  log_fun = function(w) logs <<- append(logs, w$message)
  
  fit <- withCallingHandlers(update(base_model, newdata = sim_data, chains = 4,
                                    refresh = 0, silent = 2, 
                                    control = list(adapt_delta = .8)), 
                             warning = log_fun)
  out <- marginaleffects::avg_slopes(fit) %>% 
    as_tibble() %>% 
    mutate(run_id = run_id,
           n = n,
           true_effect = true_effect,
           warnings = paste(logs, collapse = "; ")) %>% 
    select(run_id, n, true_effect, term, contrast, 
           estimate, conf.low, conf.high, warnings)
  
  if (!write_to_disk) {
    out
  } else {
    write_csv(out, glue("individual_runs/effect_{true_effect}_n_{n}_run_{run_id}.csv"))
  }
}

# res <- get_avg_slopes(run_id = 1, varied_sample_size_5perc_increase[[1]], base_model = base_model)


# parallel execution -------
parallel_execution <- function(df, runs) {
  future_walk(
    seq(1, runs, by = 1),
    ~get_avg_slopes(run_id = .x, df, base_model = base_model,
                    write_to_disk = TRUE)
  )
}

message("Starting simulation.")
plan(multicore)
set.seed(65848)
map(varied_sample_size_5perc_increase, ~parallel_execution(.x, 100))
map(varied_sample_size_10perc_increase, ~parallel_execution(.x, 100))

message("Simulation concluded - collecting files.")
# gather all files into one large file
individual_files <- list.files("individual_runs/", full.names = TRUE)

col_spec <- cols(
  run_id = col_double(),
  n = col_double(),
  true_effect = col_double(),
  term = col_character(),
  contrast = col_character(),
  estimate = col_double(),
  conf.low = col_double(),
  conf.high = col_double(),
  warnings = col_character()
)

full_outcome <- map(individual_files, read_csv, col_types = col_spec) %>% 
  list_rbind()

full_outcome %>% 
  write_csv("combined_sim_res.csv")

message("Done writing file to disk.")
