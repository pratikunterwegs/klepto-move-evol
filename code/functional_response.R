
#### a helper function to floor values ####
floor_any <- function(x, v) {
  floor(x / v) * v
}

#### functions to get the functional response in kleptomove ####

#' Get generalised functional response data.
#'
#' @param data_folder Which data folder to summarise. Data folders should
#' contain the results of ONE replicate of ONE parameter combination.
#' @param which_gen Which generations to look for. Defaults to 991 -- 998.
#' @param layers Which layers to read in. Defaults to all layers.
#' @param n_time The timesteps per generation.
#' 
#' @return A data.table with the functional response over cells grouped
#' by items and total number of agents.
#' @export
#' 
get_functional_response <-
  function(
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
      matrices <- Reduce(f = `+`, x = matrices)
    }, how = "list")

    # convert to dataframe for capacity wise mean
    data_proc <- rapply(data_in, function(matrix_) {
      vals <- as.vector(matrix_) / length(which_gen) # for N gen mean
      vals <- vals / n_time # for timestep mean

      return(vals)
    }, how = "list")

    # get per capita forager intake
    pc_intake_forager <- data_proc[["foragers_intake"]] /
      data_proc[["foragers"]]

    # get per capita klepto intake
    pc_intake_klepts <- data_proc[["klepts_intake"]] /
      data_proc[["klepts"]]
    
    # replace NANs with 0
    pc_intake_forager[is.nan(pc_intake_forager)] <- 0
    pc_intake_klepts[is.nan(pc_intake_klepts)] <- 0
    
    # total pc intake
    pc_intake_total <- pc_intake_forager + pc_intake_klepts

    # add pc intake to list
    data_proc <- append(data_proc, list(
      pc_intake_forager = pc_intake_forager,
      pc_intake_klepts = pc_intake_klepts,
      pc_intake_total = pc_intake_total
    ))

    #### get pc intake per items and per agents ####
    # first make data.table
    data_proc <- as.data.table(data_proc)

    # get total agents
    data_proc[, `:=`(total_agents = foragers + klepts)]

    # floor agents and items to the nearest 0.01
    data_proc[, `:=`(total_agents = floor_any(total_agents, 0.005),
                     items = floor_any(items, 0.005))]
    
    # melt here before NANs appear
    data_fun_response <- melt(data_proc, id.vars = setdiff(
      colnames(data_proc),
      c("pc_intake_forager", "pc_intake_klepts", "pc_intake_total")
    ))

    # get intake rate
    data_fun_response <-
      data_fun_response[, lapply(.SD, mean),
        .SD = c("value"),
        by = c("items", "total_agents", "variable")
      ]

    return(data_fun_response)
  }
