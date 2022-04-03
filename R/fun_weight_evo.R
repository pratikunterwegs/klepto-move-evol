#### helper functions for kleptomove ####

#' Get generation data.
#'
#' @param filepath Simulation output path.
#' @param which_gen Which generation to get.
#'
#' @return A \code{generation} object.
#' @export
#'
get_generation_data <- function(filepath,
                                which_gen = 991) {

  # get filepath of the function sourceME.r
  toSource <- list.files(
    path = filepath,
    pattern = "sourceMe.R",
    full.names = TRUE
  )

  # source it
  source(toSource, local = T)
  # get summary data
  temp_gen_data <- generation(which_gen)

  temp_gen_data
}

#' Get weight data from \code{generation} output.
#'
#' A function to return scaled weight proportion data from weight values
#' returned by the \code{generation} function. Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#'
#' @param gen_data A list object produced by \code{generation()}.
#' @param digits Rounding digits when scaling preferences.
#' @return Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#' @export
#'
get_weights_prop <- function(gen_data, digits = 2) {

  # get generation data times 20 (why 20?)
  weights <- gen_data[["agents"]][["ann"]]
  n_agents = nrow(weights)

  # these are hardcoded, unfortunately
  weight_move_scaled = weights[, c(2,3,4)]
  weight_strategy_scaled = weights[, c(5,6,7,8)]

  # scale the weights by the sum of absolute values
  weights = cbind(
    weight_move_scaled / rowSums(abs(weight_move_scaled)),
    weight_strategy_scaled / rowSums(abs(weight_strategy_scaled))
  )
  # round the weights to the second decimal place
  weights = round(weights, digits = digits)

  # make data table and melt
  weights = data.table::as.data.table(weights)
  data.table::setnames(weights, glue::glue("wt_{seq(2, 8, 1)}"))

  # melt data
  weights = data.table::melt(
    weights,
    measure.vars = glue::glue("wt_{seq(2, 8, 1)}"),
    value.name = "wt_value",
    variable.name = "wt"
  )

  # summarise proportions
  weights = weights[, list(prop = .N / (n_agents)), by = c("wt", "wt_value")]

  weights
}

#' Get weight proportions across generations.
#'
#' @param generations A sequence of generations.
#' @param config_list The config list object which must have at least
#' @param which_weight Which weights, a numeric vector or \code{NA} for all.
#' @return A data.table of the weight proportions over the generations provided.
#' @export
#'
get_weights_timeline <- function(generations,
                                 config_list = config,
                                 which_weight = seq(2, 8)
                                ) {
  if (all(is.na(which_weight))) {
    seq_weights <- seq(config_list[["agents.ann.weights"]])[-1]
  } else {
    seq_weights <- which_weight
  }
  # get data per generation
  weight_data_gen <- lapply(generations, function(g) {

    # subtract 1 because C++ counting
    G <- generation(g - 1)

    # get weight data as a list
    weight_prop = get_weights_prop(G, digits = 2)
    weight_prop$gen = g

    weight_prop = weight_prop[wt %in% glue::glue("wt_{seq_weights}"), ]
  })

  # bind all generations
  weight_data_gen <- data.table::rbindlist(weight_data_gen)

  weight_data_gen
}

#' Get weight evolution data for a simulation replicate.
#'
#' @param generations Which generations, a numeric vector.
#' @param which_weight Which weights, a numeric vector or \code{NA} for all.
#' @param data_folder Where the \code{sourceMe.R} file should be sourced from.
#'
#' @return A data.table of weight evolution
#' @export
#'
get_sim_weight_evol <- function(data_folder,
                                generations,
                                which_weight
                              ) {

  # source 'source_me.R' in the data folder
  # this isn't how I'd do it but oh well
  source(sprintf("%s/sourceMe.R", data_folder))

  # get generation weights
  weight_data_gen <- get_weights_timeline(generations,
    config_list = config,
    which_weight = which_weight
  )
  # return the ggplot
  return(weight_data_gen)
}

#' Summarise Kleptomove output.
#'
#' A function to get the per-generation per-timestep occupancy per cell.
#'
#' @param data_folder Which data folder to summarise. Data folders should
#' contain the results of ONE replicate of ONE parameter combination.
#' @param which_gen Which generations to look for. Defaults to 991 -- 998.
#' @param layers Which layers to read in. Defaults to all layers.
#' @param n_time The timesteps per generation.
#' @param capacity_matrix The capacity matrix.
#'
#' @return A data.table of the per-capacity, per-timestep, per-generation
#' values (mean, median, standard deviation) of each layer.
#' @export
#'
get_agent_distribution <- function(data_folder,
                                   which_gen = seq(991, 998, 1),
                                   n_time = 200,
                                   capacity_matrix,
                                   layers = c(
                                     "items", "foragers", "klepts",
                                     "klepts_intake", "foragers_intake"
                                   )) {
  # list files
  data_files <- data.table::CJ(
    which_gen,
    layers
  )
  data_files$filepath <- glue::glue_data(
    .x = data_files,
    "{data_folder}/{which_gen}{layers}.txt"
  )

  # split by layer
  data_files <- split(data_files, data_files$layers)

  # get only filepaths
  data_files <- lapply(data_files, `[[`, "filepath")

  # now read in all files as matrices
  data_in <- rapply(object = data_files, function(file_list) {
    matrices <- lapply(as.list(file_list), function(fl) {
      tseries::read.matrix(fl)
    })

    # sum the agents over the generations 991 -- 998
    # matrices <- Reduce(f = `+`, x = matrices)
  }, how = "list")

  # convert to dataframe for capacity wise mean
  data_proc <- rapply(data_in, function(matrix_) {
    vals <- as.vector(matrix_) # / length(which_gen) # for N gen mean
    vals <- vals / n_time # for timestep mean

    # convert the capacity matrix into a vector
    cap <- as.vector(capacity_matrix)

    # for the capacity, get the per-time per-gen mean layer value
    val_by_cap <- data.table::data.table(
      value = vals,
      cap = cap
    )

    return(val_by_cap)
  }, how = "list")

  # get per capita forager intake
  pc_intake_forager <- Map(
    function(df1, df2) {
      data.table::data.table(
        value = df1$value / df2$value,
        cap = df1$cap
      )
    },
    data_proc[["foragers_intake"]],
    data_proc[["foragers"]]
  )

  # get per capita klepto intake
  pc_intake_klepts <- Map(
    function(df1, df2) {
      data.table(
        value = df1$value / df2$value,
        cap = df1$cap
      )
    },
    data_proc[["klepts_intake"]],
    data_proc[["klepts"]]
  )

  # replace NANs with 0
  pc_intake_forager <- lapply(pc_intake_forager, function(x) {
    x[is.nan(x$value), ]$value <- 0
    return(x)
  })
  pc_intake_klepts <- lapply(pc_intake_klepts, function(x) {
    x[is.nan(x$value), ]$value <- 0
    return(x)
  })

  # add pc intake to list
  data_proc <- append(data_proc, list(
    pc_intake_forager = pc_intake_forager,
    pc_intake_klepts = pc_intake_klepts
  ))

  # add gens and get mean and sd per quality
  data_proc <- lapply(data_proc, function(le) {
    data.table::rbindlist(
      Map(function(le2, name) {
        # setDT(le2)
        le2[, gen := name]
        le2[, list(
          mean_val = mean(value, na.rm = TRUE),
          med_val = stats::median(value, na.rm = TRUE),
          sd_val = stats::sd(value, na.rm = TRUE)
        ),
        by = c("cap", "gen")
        ]
      }, le, which_gen)
    )
  })

  # assign layer name
  data_proc <- mapply(function(le, le_name) {
    le$layer <- le_name

    return(le)
  },
  data_proc, names(data_proc),
  SIMPLIFY = FALSE
  )

  # bind the list
  data_proc <- data.table::rbindlist(data_proc)

  return(data_proc)
}
