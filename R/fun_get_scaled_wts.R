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

    # select movement weights
    dt_ = dt_[, !(sprintf("wt_%i", c(1, seq(6, 8))))]

    # get sum of absolute values
    dt_[, wt_abs_sum := apply(
        dt_[, c("wt_2", "wt_3", "wt_4")], 
        1, 
        FUN = function(x) {
            sum(abs(x))
        })
    ]

    # scale by sum of values
    dt_[, c("wt_2", "wt_3", "wt_4") := lapply(
        .SD, `/`, wt_abs_sum
    ), .SDcols = c("wt_2", "wt_3", "wt_4")]
    
    # set names
    data.table::setnames(
        dt_,
        c("wt_2", "wt_3", "wt_4"),
        c("sN", "sH", "sP")
    )
    dt_
    
  })
  
  # bind to list
  tmp_data = data.table::rbindlist(tmp_data)
  
  tmp_data
}
