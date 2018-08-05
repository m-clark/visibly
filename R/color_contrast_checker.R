#' Color contrast
#' @description Get the color contrast of two colors
#' @param foreground Character string. An R color or color as hex.
#' @param background Character string. An R color or color as hex. Default is
#'   white/'#FFFFFF'.
#'
#' @details This uses the api at
#'   \href{https://webaim.org/resources/contrastchecker/}{webaim.org} to
#'   determine the contrast ratio of the foreground color to the background
#'   color.  This is useful to ensure chosen colors meet web standards for
#'   accessibility.
#' @return If you have \link[jsonlite]{jsonlite}, it will return a data frame of
#'   the ratio and which standards are passed. Otherwise you get the same
#'   information but as a character vector.
#'
#'
#' @examples
#' \dontrun{
#' library(visibly)
#' color_contrast_checker(foreground = 'red', background = 'papayawhip')
#'
#' # standard blue is not good for contrasting links from other text
#' color_contrast_checker(foreground = '#000080', background = 'black')
#'}
#' @export
color_contrast_checker <- function(foreground, background='#FFFFFF') {

  # initial checks
  if ((is.null(foreground) | rlang::is_empty(foreground)) |
      (is.null(background) | rlang::is_empty(background)))
    stop('Need both foreground and background colors')

  if (!is.character(foreground) | !is.character(background))
    stop(strwrap('Elements must be character string as a named R color or
         hex (e.g. "#ffffff")'))

  # note: alpha returned by col2hex will be ignored
  if (foreground %in% colors()){
    foreground <- col2hex(foreground)
  } else {
    if (!nchar(foreground) == 7 | !grepl('^#', foreground))
      stop(strwrap('foreground must be an R color, e.g. see colors(),
                   or a hex of the form #ff5500'))
  }

  if (background %in% colors()) {
    background <- col2hex(background)
  } else {
    if (!nchar(background) == 7 | !grepl('^#', background))
      stop(strwrap('background must be an R color, e.g. see colors(),
                   or a hex of the form #ff5500'))
  }

  # remove pound sign
  foreground <- substr(foreground, start = 2, stop = nchar(foreground))
  background <- substr(background, start = 2, stop = nchar(background))

  url <- paste0('https://webaim.org/resources/contrastchecker/?fcolor=',
               foreground,
               '&bcolor=',
               background,
               '&api')

  result <- suppressWarnings({readLines(url)})

  if (!requireNamespace('jsonlite', quietly = TRUE)) {
    result <- strsplit(
      gsub(result, pattern = '\\{|\\}|\"', replacement = ''),
      ',')
    return(result[[1]])
  }

  data.frame(jsonlite::fromJSON(result))
}
