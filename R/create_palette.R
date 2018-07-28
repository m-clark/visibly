#' Create a color palette
#'
#' @description Uses colortools package (and possibly scales) to easily create a
#'   color palette based on a initial input color.
#'
#' @param colorhex  Hexadecimal value of color or an R color name.
#' @param name optional name of color
#' @param toHCL Convert colors to hcl. Defaults to \code{FALSE}.
#' @param plot Plot the results. Defaults to \code{FALSE}.
#' @param alpha Transparency. Takes values from 0 to 1. Default is 1.
#'
#' @details Will return complementary, analogous/adjacent, split complementary,
#'   triadic, square and tetradic color values relative to the initial color.
#'   Note that if you want sequential, qualitative, diverging etc., other
#'   packages like
#'   \href{https://cran.rstudio.com/web/packages/colorspace/}{RColorBrewer},
#'   \href{https://cran.rstudio.com/web/packages/colorspace/}{colorspace}, and
#'   \href{https://cran.rstudio.com/web/packages/colortools/}{colortools} will
#'   do that for you.
#'
#' @return A list of colors
#' @importFrom graphics layout mtext par
#' @examples
#' library(visibly)
#' create_palette(colorhex = '#ff5500', name='orange')
#' create_palette(colorhex = '#ff5500', name='orange', alpha=.5)
#'
#' @export
create_palette <- function(colorhex,
                           name=NULL,
                           toHCL=FALSE,
                           plot=FALSE,
                           alpha=1) {

  if (!requireNamespace("colortools", quietly = TRUE)) {
    stop("colortools package is needed for this function to work.
         Please install it.",
         call. = FALSE)
  }

  if (is.null(name)) name <- colorhex

  if (!is.character(colorhex))
    stop('color hex must be a character string of the form #ffffff
    or an R color name.')

  if (alpha < 0 | alpha > 1) stop('alpha must be between 0 and 1.')

  l <- list()
  l[[name]] <- colorhex


  if (plot) par(mfrow=c(2,3))

  l$complementary <- colortools::complementary(colorhex, plot=plot)
  l$analogous <- colortools::adjacent(colorhex, plot=plot)
  l$split_complentary <- colortools::splitComp(colorhex, plot=plot)
  l$triadic <- colortools::triadic(colorhex, plot=plot)
  l$square <- colortools::square(colorhex, plot=plot)
  l$tetradic <- colortools::tetradic(colorhex, plot=plot)

  if (alpha != 1) {
    l <- lapply(l, function(x) unname(purrr::map_chr(x, scales::alpha, alpha)))
    if (plot) {
      lapply(l[-1], colortools::pizza)
      mtext('Alpha applied', side = 3, line = -3, outer = TRUE)
    }
  }

  # not sure if this is legit or not
  if (toHCL) {
    l <- lapply(l, function(x) unname(purrr::map_chr(x,
                                                     scales::col2hcl,
                                                     alpha=alpha)))
    if (plot) {
      lapply(l[-1], colortools::pizza)
      mtext('HCL version', side = 3, line = -3, outer = TRUE)
    }
  }

  # unlike most R packages that screw with the layout, return it to standard
  if (plot) graphics::layout(1)

  l
}
