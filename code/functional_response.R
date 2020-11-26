
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
  function(layers = c(
             "items", "foragers", "klepts",
             "klepts_intake", "foragers_intake"
           ),
           response = c("pc_intake_forager",
                        "pc_intake_klepts",
                        "pc_intake_total"),
           drivers = c("klepts",
                       "foragers",
                       "total_agents",
                       "items"),
           data_folder,
           which_gen = seq(991, 998, 1),
           n_time = 400) {
  
    # assert that drivers are only two
    assertthat::assert_that(length(drivers) == 2,
                            msg = "only two drivers allowed")
    
    data_proc <- do_read_data(
      data_folder,
      which_gen = seq(991, 998, 1),
      n_time = 400
    )
    

    # floor agents and items to the nearest 0.01
    data_proc[, `:=`(total_agents = floor_any(total_agents, 0.005),
                     items = floor_any(items, 0.005))]
    
    # subset data for driver and response columnsd
    cols <- c(response, drivers)
    data_fun_response <- data_proc[, ..cols]
    
    # melt here before NANs appear
    data_fun_response <- melt(data_fun_response, id.vars = setdiff(
      colnames(data_fun_response),
      response
    ))

    # get intake rate
    data_fun_response <-
      data_fun_response[, lapply(.SD, mean),
        .SD = c("value"),
        by = c(drivers, "variable")
      ]
    
    # check cols
    assertthat::assert_that(all(unique(data_fun_response$variable) == response))
    return(data_fun_response)
  }
