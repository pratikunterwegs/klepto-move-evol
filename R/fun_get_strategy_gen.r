#### functions to get strategy over generations ####

get_strategy_gen = function(filepath) {

  # what to source
  toSource = glue::glue("{filepath}/sourceMe.R")
  source(toSource)

  # get summary
  data = summary()
  data = data.table::data.table(data$agents)

  # how many agents and timesteps
  n_agents = config$agents.N
  n_time = config$T

  # get stealing
  data[, stealing := (n_agents * n_time) - (foraging+ handling)]

  data = data[, list(`pop fitness`, foraging, 
            stealing, handling, conflicts)]

  data[, gen := seq(nrow(data))]
  
  # remove last gen
  data = data[gen < max(gen), ]
  data.table::setnames(data, old = "pop fitness", new = "pop_fitness")
  # melt
  data = data.table::melt(data, id.vars = c("gen", "pop_fitness", "conflicts"))
  
  data[, value := value / sum(value), by = "gen"]

  return(data)

}

