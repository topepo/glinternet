
map_data <- function(x) {
  nms <- tibble::tibble(column = names(x), order = seq_along(names(x)))
  factors <- purrr::map_lgl(x, is.factor)
  numerics <- purrr::map_lgl(x, is.numeric)
  # check for neither

  nms$numeric <- unname(numerics)
  nms$factor <- unname(factors)

  nms$numeric_ind <- cumsum(nms$numeric)
  nms$numeric_ind[factors] <- NA_integer_

  nms$factor_ind <- cumsum(nms$factor)
  nms$factor_ind[numerics] <- NA_integer_
  nms$levels <- purrr::map(x, map_factors)
  nms$nun_lvl <- purrr::map_int(nms$levels, num_vals)
  nms
}

map_factors <- function(x) {
  if (!is.factor(x)) {
    return(NULL)
  }
  x <- tibble::tibble(level = levels(x))
  x$integer <- 0:(nrow(x) - 1)
  x
}

num_vals <- function(x) {
  if (is.null(x)) {
    res <- 1L
  } else {
    res <- nrow(x)
  }
  res
}

prepare_data <- function(info, dat) {
  dat$..row <- 1:nrow(dat)
  factor_cols <- info$column[info$factor]
  for (i in factor_cols) {
    new_vals <- info$levels[[which(info$column == i)]]
    names(new_vals)[1] <- i
    old_vals <- dplyr::select(dat, ..row, dplyr::all_of(i))
    old_vals <-
      dplyr::left_join(old_vals, new_vals, by = i) %>%
      dplyr::arrange(..row)
    dat[[i]] <- old_vals$integer
  }
  dat$..row <- NULL
  as.matrix(dat)
}
