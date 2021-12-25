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
    
    # add fitness
    dt_[, intake := g_data$agents$fit[seq_len(n_agents)]]
    
    # add gen
    dt_[, gen := g]

    # add id
    dt_[, id := seq(nrow(dt_)) - 1]

    # select required weights
    weights_to_exclude = sprintf("wt_%i", as.integer(setdiff(weights, seq(8))))
    weights_to_scale = sprintf("wt_%i", as.integer(weights))

    dt_ = dt_[, !(weights_to_exclude)]

    # get sum of absolute values
    dt_[, wt_abs_sum := apply(
        dt_[, c(weights_to_scale)], 
        1, 
        FUN = function(x) {
            sum(abs(x))
        })
    ]

    # scale by sum of values
    dt_[, c(weights_to_scale) := lapply(
        .SD, `/`, wt_abs_sum
    ), .SDcols = c(weights_to_scale)]
    
    if (weights == c(2, 3, 4)) {
      # set names
      data.table::setnames(
          dt_,
          c("wt_2", "wt_3", "wt_4"),
          c("sN", "sH", "sP")
      )
    }
    
    dt_
    
  })
  
  # bind to list
  tmp_data = data.table::rbindlist(tmp_data)
  
  tmp_data
}
