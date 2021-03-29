#' Get handler preference by strategy.
#'
#' @param data_folder The data folder, should refer to an obligate simulation.
#' @param generations The generations to extact, recommended 1 - 100.
#' @param weight_klept_bias Which weight (mostly 5 or 7) refers to the kleptoparasite
#' strategy, this is the bias in obligate simulations.
#' @param weight_of_interest Which weight are we interested in. A named vector
#' of weight numbers.
#'
#' @return A data.table of counts of handler preference across strategy.
#' @export
#'
get_pref_handler_by_strat <- function(
                                      data_folder,
                                      generations,
                                      weight_klept_bias,
                                      weight_of_interest,
                                      handler_pref_by_strategy = FALSE) {

  # source 'source_me.R' in the data folder
  source(sprintf("%s/sourceMe.R", data_folder))

  # get generation objects across generations
  weight_data_gen <- lapply(generations, function(g) {
    # get generation object
    G <- generation(g - 1)

    # get the two weights we want
    # why do we multiply by 20, who knows
    weights <- G[["agents"]][["ann"]][
      ,
      c(weight_klept_bias, weight_of_interest)
    ] * 20

    # set names
    colnames(weights) <- c("klept_bias", names(weight_of_interest))

    # this is a matrix, discretise the klept bias and handler pref 1,1 is
    # forager, has handler preference
    if (!handler_pref_by_strategy) {
      weights <- weights > 0
      # make data tables
      weights <- data.table::as.data.table(weights)
      # count by unique values
      weights <- weights[, .N, by = c("klept_bias", names(weight_of_interest))]
      # add generation
      weights$gen <- g
    } else {
      assertthat::assert_that(length(weight_of_interest) == 1,
        msg = "get syndrome: can only take one weight,
                                the handler preference"
      )
      weights[, "klept_bias"] <- weights[, "klept_bias"] > 0
      weights[, names(weight_of_interest)] <-
        tanh(weights[, names(weight_of_interest)])
      # this next is hardcoded
      weights[, names(weight_of_interest)] <- as.numeric(
        stringi::stri_extract_first(
          str = cut(
            x = weights[, names(weight_of_interest)],
            breaks = seq(-1.01, 1.01, 0.05)
          ),
          regex = "[-0-9]+\\.\\d{2}"
        )
      )
      weights <- data.table::as.data.table(weights)
      weights <- weights[, list(.N),
        by = c("klept_bias", names(weight_of_interest))
      ]
      weights[, prop_handler_pref := N / sum(N), by = "klept_bias"]
      weights$gen <- g
    }
    weights
  })

  # bind all the data together
  weight_data <- data.table::rbindlist(weight_data_gen)

  return(weight_data)
}
