#' Get handler preference by strategy.
#'
#' @param data_folder The data folder, should refer to an obligate simulation.
#' @param generations The generations to extact, recommended 1 - 100.
#' @param weight_klept_bias Which weight (mostly 5) refers to the kleptoparasite
#' strategy, this is the bias in obligate simulations.
#' @param weight_handler_pref Which weight (mostly 3) refers to the handler
#' preference.
#'
#' @return A data.table of counts of handler preference across strategy.
#' @export
#'
get_pref_handler_by_strat <- function(
  data_folder,
  generations,
  weight_klept_bias,
  weight_handler_pref
) {
  
  # source 'source_me.R' in the data folder
  source(sprintf("%s/sourceMe.R", data_folder))
  
  # get generation objects across generations
  weight_data_gen <- lapply(generations, function(g) {
    # get generation object
    G <- generation(g - 1)
    
    # get the two weights we want
    # why do we multiply by 20, who knows
    weights <- G[["agents"]][["ann"]][, c(weight_klept_bias, weight_handler_pref)] * 20
    
    # set names
    colnames(weights) <- c("klept_bias", "handler_pref")
    
    # this is a matrix, discretise the klept bias and handler pref 1,1 is forager, has handler preference
    weights <- weights > 0
    
    # make data tables
    weights = data.table::as.data.table(weights)
    
    # count by unique values
    weights = weights[, .N, by = c("klept_bias", "handler_pref")]
    
    # add generation
    weights$gen <- g
    
    return(weights)
  })
  
  # bind all the data together
  weight_data <- data.table::rbindlist(weight_data_gen)
  
  return(weight_data)
}
