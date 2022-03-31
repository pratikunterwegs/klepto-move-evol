#' Get handler preference by strategy.
#'
#' @param data_folder The data folder, should refer to an obligate simulation.
#' @param generations The generations to extact, recommended 1 - 100.
#' @param weight_comp_strat Which weight (mostly wt_5) refers to the 
#' inherited preference that controls the competition strategy in scenario 2.
#' @param weight_of_interest Which weight are we interested in. A named vector
#' of weight numbers.
#'
#' @return A data.table of counts of handler preference across strategy.
#' @export
#'
get_pref_handler_by_strat <- function(
  data_folder,
  generations,
  weight_comp_strat,
  weight_of_interest,
  handler_pref_by_strategy = FALSE
  ) {

  # source 'source_me.R' in the data folder
  source(sprintf("%s/sourceMe.R", data_folder))

  # get generation objects across generations
  weight_data_gen <- lapply(generations, function(g) {
    # get generation object
    G <- generation(g - 1)

    # get the two weights we want
    # why do we multiply by 20, who knows
    weights <- G[["agents"]][["ann"]][,
      c(weight_comp_strat, weight_of_interest)
    ]

    # set names
    colnames(weights) <- c("comp_strat", names(weight_of_interest))

    # this is a matrix, discretise the klept bias and handler pref 1,1 is
    # forager, has handler preference
    if (!handler_pref_by_strategy) {
      weights <- weights > 0
      # make data tables
      weights <- data.table::as.data.table(weights)
      # count by unique values
      weights <- weights[, .N, by = c("comp_strat", names(weight_of_interest))]
      # add generation
      weights$gen <- g
    } else {
      assertthat::assert_that(length(weight_of_interest) == 1,
        msg = "get syndrome: can only take one weight, the handler preference"
      )
      # assign whether klept or forager (forager if TRUE, > 0)
      weights <- weights > 0

      weights <- data.table::as.data.table(weights)
      weights <- weights[, list(.N),
        by = c("comp_strat", names(weight_of_interest))
      ]
      weights[, prop_handler_pref := N / sum(N), by = "comp_strat"]
      weights$gen <- g
      
      weights[, comp_strat := fifelse(comp_strat, "forager", "klept")]
    }
    weights
  })

  # bind all the data together
  weight_data <- data.table::rbindlist(weight_data_gen)

  return(weight_data)
}
