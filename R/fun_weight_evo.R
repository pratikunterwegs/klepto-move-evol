#### helper functions for kleptomove ####

get_generation_data <- function(filepath,
                                generation = 991) {

  # get filepath of the function sourceME.r
  toSource <- list.files(
    path = filepath,
    pattern = "sourceMe.R",
    full.names = TRUE
  )

  # source it
  source(toSource)
  # get summary data
  temp_gen_data <- generation(991) # who decided to name this after a common R func?

  return(temp_gen_data)
}

# a generalised function

#' Get weight data from \code{generation} output.
#'
#' A function to return scaled weight proportion data from weight values
#' returned by the \code{generation} function. Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#'
#' @param gen_data A list object produced by \code{generation()}.
#' @param weight_w The weight identity to be retrieved.
#' @param min_w_val The lower bound of the scaled weight values.
#' @param max_w_val The upper bound.
#' @param steps The number of steps between bounds.
#'
#' @return Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#' @export
#'
get_weights_prop <- function(gen_data, weight_w,
                             min_w_val = -1.01,
                             max_w_val = 1.01,
                             steps = 50) {

  # calculate stepsize
  step_size <- (max_w_val - min_w_val) / steps

  # get generation data times 20 (why 20?)
  weights <- gen_data[["agents"]][["ann"]][, weight_w] * 20
  weight_tanh <- tanh(weights)
  weight_class <- cut(weight_tanh,
    seq(min_w_val, max_w_val, step_size),
    right = TRUE
  )
  # get proportions
  weight_prop <- table(weight_class) / length(weights)

  # get the weight names
  weight_value_names <- names(weight_prop)

  # make a data.table
  weight_data <- data.table::data.table(
    weight_id = weight_w,
    weight_value = weight_value_names,
    weight_prop = as.vector(weight_prop)
  )
  return(weight_data)
}

#' Get weight proportions across generations.
#'
#' @param generations A sequence of generations.
#' @param config_list The config list object which must have at least
#' @param which_weight Which weights, a numeric vector or \code{NA} for all.
#' @param min_w_val The minimum scaled weight value.
#' @param max_w_val The maximum scaled weight value.
#' @param steps The bins between min and max scaled weight values.
#' the number of weights per agent.
#' @return A data.table of the weight proportions over the generations provided.
#' @export
#'
get_weights_timeline <- function(generations,
                                 config_list = config,
                                 which_weight = seq(2, 8),
                                 min_w_val = -1.01,
                                 max_w_val = 1.01,
                                 steps = 50) {
  
  if(all(is.na(which_weight))) {
    seq_weights <- seq(config_list[["agents.ann.weights"]])[-1]
  } else {
    seq_weights = which_weight
  }
  # get data per generation
  weight_data_gen <- lapply(generations, function(g) {

    # subtract 1 because C++ counting
    G <- generation(g - 1)

    # get weight data as a list
    weight_data <- lapply(seq_weights, function(w) {
      weight_dt <- get_weights_prop(
        gen_data = G, 
        weight_w = w,
        min_w_val = min_w_val,
        max_w_val = max_w_val,
        steps = steps
      )
    })

    # bind the list
    weight_data <- data.table::rbindlist(weight_data)
    
    # assertthat::assert_that(
    #   is.data.table(weight_data),
    #   message(sprintf("not a data table, has class %s", class(weight_data)[1]))
    # )
    
    # set datatable
    data.table::setDT(weight_data)
    # add generation
    weight_data$gen = g

    return(weight_data)
  })

  # bind all generations
  weight_data_gen <- data.table::rbindlist(weight_data_gen)

  return(weight_data_gen)
}

#' Get weight evolution data for a simulation replicate.
#'
#' @param generations Which generations, a numeric vector.
#' @param which_weight Which weights, a numeric vector or \code{NA} for all.
#' @param min_w_val The minimum scaled weight value.
#' @param max_w_val The maximum scaled weight value.
#' @param steps The bins between min and max scaled weight values.
#' @param data_folder Where the \code{sourceMe.R} file should be sourced from.
#'
#' @return A data.table of weight evolution
#' @export
#'
get_sim_weight_evol <- function(data_folder,
                                generations,
                                which_weight,
                                min_w_val = -1.01,
                                max_w_val = 1.01,
                                steps = 50) {

  # source 'source_me.R' in the data folder
  # this isn't how I'd do it but oh well
  source(sprintf("%s/sourceMe.R", data_folder))

  # get generation weights
  weight_data_gen <- get_weights_timeline(generations,
                                          config_list = config,
                                          which_weight = which_weight,
                                          min_w_val = -1.01,
                                          max_w_val = 1.01,
                                          steps = 50)

  # # get named vector corresponding to weights
  # # this is reall silly but oh well
  # if (is.null(weight_names)) {
  #   weight_names <- sprintf(
  #     "wt_%s",
  #     as.character(seq(
  #       max(weight_data_gen$weight_id) + 1
  #     )[-1])
  #   )
  # }
  # names(weight_names) <- seq(length(weight_names) + 1)[-1]

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
