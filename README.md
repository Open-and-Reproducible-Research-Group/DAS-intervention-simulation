# Simulations for DAS intervention study
The repository contains code and analysis for simulations for the DAS 
intervention study. The purpose of the simulation was twofold:

1. Develop the statistical models and estimation strategies before collecting
actual data.
2. Getting an understanding of the interplay between effect sizes, sample sizes
and attainable precision.

The repo contains the following files:

- `01-sample-size-calc.R`: A script that was used to develop the
simulation approach and the modelling approach.
- `02-multi-stage-simulation.R`: Code used to run the simulations multiple times
to assess coverage, power and bias.
- `03-simulation-analysis.R`: A notebook analysing the data obtained via the 
above file.
- `combined_sim_res.csv`: The output from the simulations from 
`02-multi-stage-simulation.R`.
- `10-study-randomisation.R`: Contains code to generate the random sequence we
used to implement the study. The specific seed used in the study is not 
provided for data protection reasons.

You should be able to install the required packages to run the simulations
by running `renv::restore()`. 
