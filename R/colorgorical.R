#' @title  Create a colorgorical palette
#' @description An interface for creating palettes from
#'   \href{http://vrl.cs.brown.edu/color/}{colorgorical}.
#'
#' @param n Number of color values to return.
#' @param perceptualDifference Value from 0 to 1. See details.
#' @param nameDifference Value from 0 to 1. See details.
#' @param nameUniqueness Value from 0 to 1. See details.
#' @param pairPreference Value from 0 to 1. See details.
#' @param hueFilters  Must be given as an element of a list. See details.
#' @param lightnessRange See details.
#' @param startPalette A starting point for the color as a vector of 3 CIE Lab
#'   values. Must be given as an element of a list. See details.
#' @param output Character string. Output may be given as LAB, sRGB, or HEX
#'   values. If 'LAB' or 'sRGB', a matrix of those values where each row
#'   represents a color. If 'HEX', the default, a character vector of hex values
#'   is returned.
#'
#' @details This function accesses the colorgorical website to create a color
#'   palette. It requires two other packages to work: \code{httr} and
#'   \code{jsonlite}, and if you want a hex value, \code{colorspace}.
#'
#'   The following are relevant parts from the descriptions at the website.
#'
#'   \bold{Perceptual Distance}: Increasing Perceptual Distance favors palette
#'   colors that are more easily discriminable to the human eye. To accurately
#'   model human color acuity, this is performed using CIEDE2000 in CIE Lab
#'   color space.
#'
#'   \bold{Name Difference}: Increasing Name Difference favors palette colors
#'   that share few common names. This is similar to perceptual distance, but
#'   can lead to different results in certain areas of color space. This happens
#'   when there are many different names for perceptually close colors (e.g.,
#'   red and pink are perceptually close but named differently). Colorgorical
#'   calculates this using Heer and Stone's Name Difference function, which is
#'   built on top of the XKCD color-name survey.
#'
#'   \bold{Pair Preference}: Increasing Pair Preference favors palette colors
#'   that are, on average, predicted to be more aesthetically preferable
#'   together. Typically these colors are similar in hue, have different
#'   lightness, and are cooler colors (blues and greens). Pair Preference is
#'   based off of Schloss and Palmer's research on color preference.
#'
#'   \bold{Name Uniqueness}: Increasing Name Uniqueness favors palette colors
#'   that are uniquely named. Some colors like red are readily named and are
#'   favored, whereas other colors are less obviously named and are ignored.
#'   Like, Name Difference, Name Uniqueness is based on Heer and Stone's
#'   color-name research.
#'
#'   \bold{Select hue filter}: You can limit which colors are selected by either
#'   dragging over the wheel to select a hue range, or by entering the angles
#'   manually (e.g., select only reds). You can also make multiple selections
#'   after one another to select many different hue ranges (e.g., select both
#'   greens and purples).
#'
#'   \bold{Select lightness range}: You can change whether Colorgorical samples
#'   lighter or darker colors.
#'
#'   \bold{Add starting colors}: You can guarantee that certain colors are in
#'   your palette. Note, as near as I can tell, it is only guaranteed if you
#'   have already created a palette at the website and start with one of the
#'   generated colors. Otherwise, it appears to pick something close to the
#'   starting point(s) given the settings. See the example.
#'
#'   NOTE: Because JSON is being used behind the scenes, for \code{hueFilters}
#'   and \code{startPalette}, the values must be given as a list.
#'
#'   For example:
#'
#'   \code{hueFilters = list(c(90, 180))}
#'
#'   \code{startPalette = list(c(59, 62, 70))}
#'
#' The R code is based on the gist by Kamil Slowikowski found here:
#' \href{https://gist.github.com/slowkow/22daea426607416bfcd573ce9cbd89ab}{link}.
#'
#' @references
#'
#' \href{http://vrl.cs.brown.edu/color/}{The colorgorical website}.
#'
#' \href{https://github.com/connorgr/colorgorical}{Original Python source code
#' at GitHub}.
#'
#' @return A character vector of color values in hex form.
#'
#' @examples
#' \dontrun{
#' library(visibly)
#' colorgorical(n=12, pairPreference = 1, startPalette = list(c(10, -60, 45)))
#'
#' # go from hex to lab to use single starting point, but see note about
#' starting colors
#' col = t(col2rgb(palettes$Rblue$Rblue))
#' col = convertColor(col, from = "sRGB", to = "Lab", scale.in = 255)[1,]
#' testcol = colorgorical(n=12, pairPreference = 1, startPalette = list(col))
#' testcol = c(palettes$Rblue$Rblue, testcol)
#' ggplot2::qplot(x=factor(testcol, levels = testcol),
#'                y=1:12,
#'                color=I(testcol),
#'                size=I(10))
#' }
#'
#' @importFrom grDevices convertColor
#'
#' @export
colorgorical <- function(n = 10,
                         perceptualDifference = 0,
                         nameDifference = 0,
                         nameUniqueness = 0,
                         pairPreference = 0,
                         hueFilters = list(),
                         lightnessRange = c("25", "85"),
                         startPalette = list(),
                         output = 'HEX') {

  weights <- c(perceptualDifference,
              nameDifference,
              nameUniqueness,
              pairPreference)
  lr <- as.numeric(lightnessRange)
  output <- tolower(output)  # to accept lowercase

  # basic checks
  if (any(c(weights > 1, weights < 0)))
    stop('Invalid weight supplied. All must be between 0 and 1.')
  if (any(lr > 100 | lr < 0))
    stop('Invalid value for lightnessRange supplied.

         Must be between 0 and 100 (as character string).')
  if (! output %in% c('lab', 'srgb', 'hex'))
    stop("Invalid value for output supplied. Must be 'LAB', 'sRGB', or 'HEX'.")
  if (!is.list(hueFilters) | !is.list(startPalette))
    stop('hueFilters and startPalette must be supplied as a list.')

  post_body <- jsonlite::toJSON(auto_unbox = TRUE,
                                list(
                                  'paletteSize' = n,
                                  'weights' = list(
                                    'ciede2000' = perceptualDifference,
                                    'nameDifference' = nameDifference,
                                    'nameUniqueness' = nameUniqueness,
                                    'pairPreference' = pairPreference
                                  ),
                                  'hueFilters' = hueFilters,
                                  'lightnessRange' = lightnessRange,
                                  'startPalette' = startPalette
                                )
  )

  retval <- httr::POST(url = 'http://vrl.cs.brown.edu/color/makePalette',
                       body = post_body) %>%
    httr::content()

  labs <- lapply(retval$palette, unlist) %>% do.call(rbind,.)
  if (output == 'lab') return(labs)

  rgbs <- grDevices::convertColor(labs, from = 'Lab', to = 'sRGB')
  if (output == 'srgb') return(rgbs)

  hex <- apply(rgbs,
               1,
               function(x) colorspace::hex(colorspace::sRGB(x[1], x[2], x[3])))
  return(hex)
}
