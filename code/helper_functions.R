#### helper functions for kleptomove ####

# a generalised function

#' Get weight data from \code{generation} output.
#' 
#' A function to return scaled weight proportion data from weight values
#' returned by the \code{generation} function. Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#'
#' @param gen_data A list object produced by \code{generation}.
#' @param weight_w The weight identity to be retrieved.
#' @param min_w_val The lower bound of the scaled weight values.
#' @param max_w_val The upper bound.
#' @param steps The number of steps between bounds.
#'
#' @return Returns a \code{data.table}
#' with columns \code{weight_id}, \code{weight_value} (-1 -- +1), and
#' \code{weight_prop}.
#' @export
#'
get_weights_prop <- function(gen_data, weight_w,
                             min_w_val = -1.01,
                             max_w_val = 1.01,
                             steps = 50) {
  
  # calculate stepsize
  step_size <- (max_w_val - min_w_val) / steps
  
  # get generation data times 20 (why 20?)
  weights <- gen_data[["agents"]][["ann"]][, weight_w] * 20
  weight_tanh <- tanh(weights)
  weight_class <- cut(weight_tanh, 
                      seq(min_w_val, max_w_val, step_size),
                      right = TRUE)
  # get proportions
  weight_prop <- table(weight_class) / length(weights)
  
  # get the weight names
  weight_value_names <- names(weight_prop)
  
  # make a data.table
  weight_data <- data.table::data.table(
    weight_id = weight_w,
    weight_value = weight_value_names,
    weight_prop = as.vector(weight_prop)
  )
  return(weight_data)
}

#' Get weight proportions across generations.
#'
#' @param generations A sequence of generations.
#' @param config_list The config list object which must have at least
#' the number of weights per agent.
#' @param ... Arguments passed to \code{get_weights_prop}.
#'
#' @return A data.table of the weight proportions over the generations provided.
#' @export
#' 
get_weights_timeline <- function(generations,
                                 config_list = config,
                                 ...) {
  # get weights sequence but exclude 1st weight
  seq_weights <- seq(config_list[["agents.ann.weights"]])[-1]
  
  # get data per generation
  weight_data_gen <- lapply(generations, function(g) {
    
    # subtract 1 because C++ counting
    G <- generation(g - 1)
    
    # get weight data as a list
    weight_data <- lapply(seq_weights, function(w) {
      weight_dt <- get_weights_prop(gen_data = G,
                                    weight_w = w, ...)
    })
    
    # bind the list
    weight_data <- data.table::rbindlist(weight_data)
    # add generation
    weight_data[, gen := g]
    
    return(weight_data)
  })
  
  # bind all generations
  weight_data_gen <- data.table::rbindlist(weight_data_gen)
  
  return(weight_data_gen)
  
}

#' Get weight evolution plot for a simulation replicate.
#'
#' @param data_folder Where the \code{sourceMe.R} file should be sourced from.
#' @param weight_names The weight names.
#' @param ... Argumentns pass to \code{get_weights_timeline}.
#'
#' @return A \code{ggplot} of weight evolution
#' @export
#'
get_sim_weight_evol <- function(data_folder, 
                                weight_names = c(), 
                                ...) {
  
  # source 'source_me.R' in the data folder
  # this isn't how I'd do it but oh well
  source(sprintf('%s/sourceMe.R', data_folder))
  
  # get generation weights
  weight_data_gen <- get_weights_timeline(...)
  
  # get named vector corresponding to weights
  # this is reall silly but oh well
  if (is.null(weight_names)) {
    weight_names <- sprintf("wt_%s",
                            as.character(seq(
                              max(weight_data_gen$weight_id) + 1)[-1]))
  }
  names(weight_names) <- seq(length(weight_names) + 1)[-1]

  # plot weights data
  plot_weights <-
    ggplot2::ggplot()+
    ggplot2::geom_tile(data = weight_data_gen,
                       ggplot2::aes(gen, weight_value,
                                    fill = weight_prop),
                       show.legend = FALSE) +
    ggplot2::facet_grid(~ weight_id,
                        labeller = ggplot2::labeller(
                          weight_id = weight_names
                        )
    ) +
    ggplot2::scale_fill_distiller(palette = "YlOrRd") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())+
    ggplot2::labs(x = "generation",
                  y = "weight value")

  # return the ggplot
  return(plot_weights)
  
}
