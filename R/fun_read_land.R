
#' Read landscape data from PNG output.
#'
#' @param landscape_file Path to the landscape file.
#' @param layer Which layer to read.
#' @param crop_dim How many rows and columns to return.
#' @param type What to return, either "items" for the item counts, or "gradient"
#' for the gradient in item counts.
#' @param max_K Carrying capacity of a cell.
#'
#' @return A data.table of X, Y and value
#' @export
read_landscape <- function(landscape_file, layer, crop_dim,
                           type = c("items", "gradient"),
                           max_K = 5) {

  # read the layer-th layer
  land <- png::readPNG(landscape_file)[, , layer]

  # sum across layers
  if (length(dim(land)) == 3) {
    land <- rowSums(land, dims = 2)
  }

  # crop to square matrix of size dim
  land <- land[seq(crop_dim), seq(crop_dim)]

  # multiply by carrying capacity if items asked
  if (layer == 4) {
    land <- land * max_K
  }

  if (type == "items") {
    # make data.table
    land <- data.table::as.data.table(land)
    # set names
    setnames(land, as.character(seq(crop_dim)))
    # add x col
    land$x <- seq(crop_dim)
    # melt and make long
    land <- data.table::melt(land,
      id.vars = "x",
      variable.name = "y",
      value.name = "items"
    )

    # make x and y numeric
    land[, c("x", "y") := list(
      as.numeric(x),
      as.numeric(y)
    )]
    # return
    return(land)
  } else if (type == "gradient") {
    # get slope in X and Y
    grad_matrix <- pracma::gradient(land, h1 = 1, h2 = 1)
    grad_x <- grad_matrix$X
    grad_y <- grad_matrix$Y

    # get slope magnitude overall
    slope <- t(sqrt((grad_x^2) + (grad_y^2)))

    # convert to data.table
    slope <- data.table::as.data.table(slope)

    setnames(slope, as.character(seq(crop_dim)))
    # add x col
    slope$x <- seq(crop_dim)
    # melt and make long
    slope <- data.table::melt(slope,
      id.vars = "x",
      variable.name = "y",
      value.name = "gradient"
    )

    # make x and y numeric
    slope[, c("x", "y") := list(
      as.numeric(x),
      as.numeric(y)
    )]

    # return
    return(slope)
  }
}

#' Get layer variance.
#'
#' @param landscape_file Path to the landscape file.
#' @param layer Which layer to read.
#' @param crop_dim How many rows and columns to return.
#' @param type What to return, either "items" for the item counts, or "gradient"
#' for the gradient in item counts.
#' @param max_K Carrying capacity of a cell.
#'
#' @return A single layer variance.
#' @export
#'
get_layer_variance <- function(
                               landscape_file, layer, crop_dim,
                               type = c("items", "gradient"),
                               max_K = 5) {

  # read the layer-th layer
  land <- png::readPNG(landscape_file)[, , layer]

  # sum across layers
  if (length(dim(land)) == 3) {
    land <- rowSums(land, dims = 2)
  }

  # crop to square matrix of size dim
  land <- land[seq(crop_dim), seq(crop_dim)]

  # multiply by carrying capacity if items asked
  if (layer == 4) {
    land <- land * max_K
  }

  var(as.vector(land))
}
