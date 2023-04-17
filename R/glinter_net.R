#' Wrapper for glinteret
#'
#' @param x A matrix or data frame of predictors.
#' @param y A vector (numeric or factor)
#' @param penalty A sequence of penalty values.
#'
#' @examples
#' library(modeldata)
#' library(dplyr)
#'
#' ames <-
#'   ames %>%
#'   select(Sale_Price, Bldg_Type, Neighborhood, Year_Built, Gr_Liv_Area, Full_Bath,
#'          Half_Bath, Year_Sold, Lot_Area, Central_Air, Longitude, Latitude) %>%
#'   mutate(
#'     Sale_Price = log10(Sale_Price),
#'     Baths = Full_Bath  + Half_Bath / 2
#'   ) %>%
#'   select(-Half_Bath, -Full_Bath)
#'
#' glinter_net(ames[, -1], ames$Sale_Price)
#' @export
glinter_net <- function(x, y, penalty, ...) {
  # check y class, binomial, and family

  if (is.factor(y)) {
    y_ind <- as.numeric(y)
    y_ind <- y_ind - 1
  }

  # ----------------------------------------------------------------------------
  # Convert your data to the format expected by glinternet::glinternet
  x_mappings <- map_data(x)
  x_prepped <- prepare_data(x_mappings, x)

  res <- list(mapping = x_mappings)
  res
}
