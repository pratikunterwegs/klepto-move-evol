#' Get generation data.
#'
#' @param filepath Simulation output path.
#' @param gens Which generations to get.
#' @param n_agents How many agents to get. Max is 10,000.
#'
#' @return A \code{generation} object.
#' @export
#'
get_scaled_move_prefs <- function(filepath,
                        gens,
                        weights = c(2,3,4),
                        n_agents = 100) {

  # get filepath of the function sourceME.r
  toSource <- list.files(
    path = filepath,
    pattern = "sourceMe.R",
    full.names = TRUE
  )

  # source it
  source(toSource, local = T)
  
  # get data using the custom generation function
  tmp_data = lapply(gens, function(g) {
    
    # generation data
    g_data = generation(g)
    
    # data table of weights
    dt_ = data.table::as.data.table(
      g_data$agents$ann[seq_len(n_agents),]
    )
    
    # change names
    data.table::setnames(dt_, sprintf("wt_%i", seq(ncol(dt_))))
    
    # pick out the fixed strategy weight for scenario 2 cases
    comp_strat = dt_[["wt_5"]] # this is hardcoded
    
    # the competition strategy is forager if comp_strat value >= 0, else klept
    comp_strat = data.table::fifelse(
      comp_strat < 0, "klept", "forager"
    )
    
    # add fitness
    dt_[, intake := g_data$agents$fit[seq_len(n_agents)]]
    
    # add gen
    dt_[, gen := g]

    # add id
    dt_[, id := seq(nrow(dt_)) - 1]

    # select required weights
    weights_to_exclude = sprintf("wt_%i", as.integer(setdiff(seq(8), weights)))
    weights_to_scale = sprintf("wt_%i", as.integer(weights))

    dt_ = dt_[, !..weights_to_exclude]
    
    # get the sum of the absolute weights
    wt_abs_sum_ = apply(
      dt_[, ..weights_to_scale], 
      1, 
      FUN = function(x) {
        sum(abs(x))
      })
    
    # assign the sum of absolute values to the data frame
    dt_[, wt_abs_sum := wt_abs_sum_]

    # scale by sum of values
    dt_[, c(weights_to_scale) := lapply(
        .SD, `/`, wt_abs_sum
    ), .SDcols = c(weights_to_scale)]
    
    if (all(weights %in% c(2, 3, 4))) {
      # set names
      data.table::setnames(
          dt_,
          c("wt_2", "wt_3", "wt_4"),
          c("sN", "sH", "sP")
      )
    }
    
    # assign a strategy
    # this is only useful in scenario 2
    dt_[, comp_strat := comp_strat]
    
    dt_
    
  })
  
  # bind to list
  tmp_data = data.table::rbindlist(tmp_data)
  
  tmp_data
}
