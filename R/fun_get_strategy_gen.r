#### functions to get strategy over generations ####

#' Get strategy counts over generations.
#'
#' @param filepath The filepath to data. Must contain a \code{sourceMe.R}.
#'
#' @return A data.table of strategy counts over generations.
#' @export

get_strategy_gen <- function(filepath,
                             n_agents = 10000,
                             n_time = 400) {

  # what to source
  toSource <- glue::glue("{filepath}/sourceMe.R")
  source(toSource, local = T)

  # get summary
  data <- summary()
  data <- data[["agents"]]
  data <- as.data.frame(data)
  data.table::setDT(data)

  # how many agents and timesteps
  # n_agents <- config$agents.N
  # n_time <- config$T

  assertthat::assert_that(is.data.table(data),
    msg = "strategy_gen: not a data table"
  )

  assertthat::assert_that(nrow(data) > 1,
    msg = "strategy_gen: empty datatable"
  )

  # get stealing
  data$stealing <- (n_agents * n_time) - (data$foraging + data$handling)

  data <- data[, c(
    "pop fitness", "foraging",
    "stealing", "handling", "conflicts"
  )]

  data$gen <- seq(nrow(data))

  # remove last gen
  data <- data[data$gen < max(data$gen), ]
  data.table::setnames(data, old = "pop fitness", new = "pop_fitness")
  # melt
  data <- data.table::melt(data, id.vars = c("gen", "pop_fitness", "conflicts"))

  # data[, value := value / sum(value), by = "gen"]
  data$value <- data$value / (n_agents * n_time)

  return(data)
}
