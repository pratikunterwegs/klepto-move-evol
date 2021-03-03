get_pref_handler_by_strat <- function(
                                      data_folder,
                                      generations,
                                      folder,
                                      gen_data,
                                      weight_klept_bias,
                                      weight_handler_pref) {

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

    # table the weights, there will be four combinations
    weight_table <- table(weights)

    # convert to data.table
    weight_table <- data.table::as.data.table(weight_table)

    # add generation
    weight_table$gen <- g

    return(weight_table)
  })

  # bind all the data together
  weight_data <- data.table::rbindlist(weight_data_gen)

  return(weight_data)
}
