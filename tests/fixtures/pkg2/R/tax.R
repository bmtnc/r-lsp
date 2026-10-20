#' Add tax
#' @param x amount
#' @param rate tax rate
#' @export
add_tax <- function(x, rate = 0.2) {
  x * (1 + rate)
}

net_price <- function(x) add_tax(x) - 1
