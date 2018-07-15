#' Convert color to hex
#'
#' @param color Character string. An R color or color as hex.
#' @param alpha Transparency. A value from 0 to 1 (default).
#' @details This function is primarily to be used on R colors, and will convert
#'   them to hexmode sufficient for typical use in visualization, web
#'   presentation, etc.  If you already have a hex value, it can be useful for
#'   tacking on the alpha level.
#'
#' @return A character string of a hexadecimal color
#' @seealso \link[base]{as.hexmode} \link[grDevices]{colors}
#' \link[grDevices]{col2rgb} \link[grDevices]{rgb}
#'
#' @examples
#' library(visibly)
#' col2hex('red')
#' col2hex('#ff5500', alpha = .1)
#' @importFrom grDevices colors col2rgb rgb
#' @export
col2hex <- function(color, alpha = 1) {

  if (alpha < 0 | alpha > 1)
    stop('alpha must be between 0 and 1.')

  if (missing(color))
    stop('Need a color')

  if (!is.character(color))
    stop(strwrap('Elements must be character string as a named R color or
         hex (e.g. "#ffffff")'))

  if (!color %in% colors()) {
    if (!nchar(color) == 7 | !grepl('^#', color))
      stop(strwrap('color must be an R color, e.g. see colors(),
                   or a hex of the form #ff5500'))
  }

  rgb(t(col2rgb(color)/255), alpha = alpha)
}
