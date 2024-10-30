library(tidyverse)
library(brms)
library(shinystan)
library(bayesplot)
library(marginaleffects)
library(furrr)
library(future)

bernoulli <- function(n, p) {
  runif(n) < p
}


parameters <- tibble(
  journal = LETTERS[1:2] %>% rep(each = 2),
  intervention = rep(c(TRUE, FALSE), 2)
) %>% 
  mutate(
    n = case_when(
      journal == "A" ~ 250000,
      journal == "B" ~ 250000
    ),
    intercept = case_when(
      journal == "A" ~ .1,
      journal == "B" ~ .1
    ),
    effect = case_when(
      journal == "A" & intervention ~ .1,
      journal == "B" & intervention ~ .1,
      TRUE ~ 0
    ))


# true effect
true_effect <- parameters %>% 
  filter(intervention) %>% 
  summarise(avg_effect = weighted.mean(effect, n)) %>% 
  pull(avg_effect)


set.seed(0292349846)
# two stage simulation:
# 1. pre intervention
# 2. post intervention

simulated_data <- parameters %>% 
  # stage 1: simulate pre-intervention state of DAS based on intercept
  mutate(pre_intervention = map2(n, intercept, ~bernoulli(.x / 2, .y))) %>% 
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
         # on some. 
         # However, it is hard to estimate what this change rate might be. Once we have
         # data from the control group, we can plug this in here to get a better 
         # calculation of our post-hoc power.
         post_intervention = if_else(bernoulli(n, .01), !post_intervention, 
                                     post_intervention))

simulated_data %>% 
  group_by(intervention) %>% 
  summarise(mean_pre = mean(pre_intervention), 
            mean_post = mean(post_intervention))

glm(post_intervention ~ pre_intervention + intervention, 
    data = simulated_data, family = binomial()) %>% 
  avg_slopes()

# only look at negatives from pre-intervention
glm(post_intervention ~ intervention, 
    data = filter(simulated_data, !pre_intervention), family = binomial()) %>% 
  avg_slopes()

# analyse the data
fit0 <- brm(post_intervention ~ intervention,
            data = simulated_data, family = brms::bernoulli(),
            prior = c(
              # prior(normal(0, 5), class = b),
              prior(student_t(3, 0, 2.5), class = b)
              # prior(student_t(4, 0, 2.5), class = b)
            ),
            cores = 4)

fit1 <- brm(post_intervention ~ pre_intervention + intervention,
            data = simulated_data, family = brms::bernoulli(),
            prior = c(
              # prior(normal(0, 5), class = b),
              prior(student_t(3, 0, 2.5), class = b)
              # prior(student_t(4, 0, 2.5), class = b)
            ),
            # sample_prior = "only",
            cores = 4)


avg_slopes(fit1)
avg_slopes(fit0)

pp_check(fit0)
pp_check(fit1)


posterior <- as.matrix(fit1)
mcmc_intervals(posterior, prob = 0.8)
mcmc_intervals(posterior, prob = 0.8, regex_pars = "intervention")

launch_shinystan(fit1)
