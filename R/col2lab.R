#' Convert color to lab
#' @description Converts an R color or a hexadecimal string to CIE Lab.
#' @param color An R color or a hexadecimal string of the form "#rrggbb"
#' @details See the help for \link[grDevices]{convertColor}.
#' @return A matrix of LAB values useful for colorgorical starting points.
#'
#' @seealso \link[visibly]{colorgorical} \link[grDevices]{col2rgb} \link[grDevices]{convertColor}
#'
#' @examples
#' library(visibly)
#' col2lab('red')
#' x = list('red', 'white', 'blue')
#' col2lab(x)
#'
#' @export
col2lab <- function(color) {
  if (!is.character(unlist(color)))
    stop('Elements must be character string as named R color or
         hex (e.g. "#ffffff")')

  col <- t(grDevices::col2rgb(color))
  col <- grDevices::convertColor(col, from = "sRGB", to = "Lab", scale.in = 255)
  col
}
