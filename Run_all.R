run <- c("Basic_functions.R", "Dominate.R", "Naive.R", "Dominate-K.R", "K-1.R", "K-level.R", "Nash.R", "Optimist.R",
         "Pessimist.R", "Altruist.R", "Get_data_functions.R")
lapply(run, source)

### To directly simulate data with level-1 meta-transitions and 3000 unique games, run:
# source("Simulate_search_data.R")