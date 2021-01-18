
#### a helper function to floor values ####
#' Bin a vector.
#'
#' @param x The numeric vector.
#' @param binsize The binsize
#'
#' @return Vector binned.
#' @export
bin_vec <- function(x, binsize) {
  minx <- 0
  maxx <- max(x, na.rm = TRUE)

  # handle bad maxx
  if (maxx == 0 | is.infinite(maxx) | is.nan(maxx) | is.na(maxx)) {
    maxx <- 1
  }

  seqx <- seq(0, maxx, by = binsize)

  cut(x, breaks = seqx, include.lowest = T)
}


#' Read in simulation data.
#'
#' @param data_folder Folder to read from.
#' @param which_gen Generations to read. Numeric sequence.
#' @param n_time Number of timesteps. Single numeric.
#' @param layers Which layers to add.
#'
#' @return A list of data.tables each with the cell specific,
#' per-timestep value of a single variable in each of the generations
#' queried.
#' @export
do_read_data <- function(
                         data_folder,
                         which_gen = seq(991, 998, 1),
                         n_time = 400,
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
    vals <- as.vector(matrix_) # for N gen mean
    vals <- vals / n_time # for timestep mean

    return(vals)
  }, how = "list")

  # get per capita forager intake
  pc_intake_forager <- Map(
    "/",
    data_proc[["foragers_intake"]],
    data_proc[["foragers"]]
  )

  # get per capita klepto intake
  pc_intake_klepts <- Map(
    "/",
    data_proc[["klepts_intake"]],
    data_proc[["klepts"]]
  )

  # replace NANs with 0
  pc_intake_forager <- lapply(pc_intake_forager, function(x) {
    x[is.nan(x)] <- 0
    return(x)
  })
  pc_intake_klepts <- lapply(pc_intake_klepts, function(x) {
    x[is.nan(x)] <- 0
    return(x)
  })

  # total pc intake
  pc_intake_total <- Map(
    "+",
    pc_intake_forager,
    pc_intake_klepts
  )

  # total agents
  total_agents <- Map(
    "+",
    data_proc[["foragers"]],
    data_proc[["klepts"]]
  )

  # add pc intake to list
  data_proc <- append(data_proc, list(
    pc_intake_forager = pc_intake_forager,
    pc_intake_klepts = pc_intake_klepts,
    pc_intake_total = pc_intake_total,
    total_agents = total_agents
  ))

  #### get pc intake per items and per agents ####
  # first make data.table
  data_proc <- lapply(data_proc, function(le) {
    Map(function(x, this_gen) {
      data.table::data.table(
        value = x,
        gen = this_gen,
        cell = seq(length(x))
      )
    }, le, which_gen)
  })

  # bind within list elements
  data_proc <- lapply(data_proc, data.table::rbindlist)

  # assign name
  data_proc <- Map(function(df, name) {
    df[, variable := name]
  }, data_proc, names(data_proc))

  # change names
  data_proc <- lapply(data_proc, function(df) {
    data.table::setnames(df, old = "value", new = unique(df$variable))
    df[, c("variable") := NULL]
  })

  data_proc <- Reduce(function(dt1, dt2) {
    data.table::merge.data.table(dt1, dt2, by = c("cell", "gen"))
  }, data_proc)

  return(data_proc)
}

#### functions to get the functional response in kleptomove ####

#' Get generalised functional response data.
#'
#' @param data_folder Which data folder to summarise. Data folders should
#' contain the results of ONE replicate of ONE parameter combination.
#' @param which_gen Which generations to look for. Defaults to 991 -- 998.
#' @param layers Which layers to read in. Defaults to all layers.
#' @param n_time The timesteps per generation.
#' @param response Which variables as reponse.
#' @param drivers Which drivers to select.
#' @param round_value The increments to bin by.
#'
#' @return A data.table with the functional response over cells grouped
#' by items and total number of agents.
#' @export
#'
get_functional_response <-
  function(response = c(
             "pc_intake_forager",
             "pc_intake_klepts",
             "pc_intake_total"
           ),
           drivers = c(
             "klepts",
             "foragers",
             "total_agents",
             "items"
           ),
           round_value = 0.005,
           data_folder,
           which_gen = seq(991, 998, 1),
           n_time = 200,
           layers = c(
             "items", "foragers", "klepts",
             "klepts_intake", "foragers_intake"
           )) {

    # warn for more than two drivers
    if (length(drivers) > 2) {
      warning("more than 2 drivers")
    }

    data_proc <- do_read_data(
      data_folder,
      which_gen = seq(991, 998, 1),
      n_time = n_time,
      layers = layers
    )

    # floor drivers to the nearest floor value
    data_proc[, (drivers) := lapply(.SD, bin_vec, binsize = round_value),
      .SD = drivers
    ]

    # subset data for driver and response columnsd
    cols <- c(response, drivers, "gen")
    data_fun_response <- data_proc[, ..cols]

    # melt here before NANs appear
    data_fun_response <- data.table::melt(data_fun_response, id.vars = setdiff(
      colnames(data_fun_response),
      response
    ))

    # get mean intake rate per unique driver and gen value
    data_fun_response <-
      data_fun_response[, lapply(.SD, mean),
        .SD = c("value"),
        by = c(drivers, "variable", "gen")
      ]

    # check cols
    assertthat::assert_that(all(unique(data_fun_response$variable) == response))
    return(data_fun_response)
  }
