#' Clean visualization themes
#'
#' @description Clean up plots from their defaults.
#'
#' @param vis A visualization created by \link[ggplot2]{ggplot2} or
#'   \link[plotly]{plotly}.
#' @param font_size Font size for axis labels in theme_trueMinimal
#' @param font_family Font family for axis labels in theme_trueMinimal
#' @param center_axis_labels Logical. Center axis labels in theme_trueMinimal.
#'   Default is FALSE.
#' @param MB For plotly, an option to display the mode bar. Defaults to FALSE.
#'
#' @details From a gray background, to unnecessary gridlines, to by-default
#'   reference lines, some of the more popular visualization packages come out
#'   75\% fantastic and 25\% questionable/arbitrary/problematic.  These functions
#'   remove unnecessary gridlines, 'de-bold' the blacks, etc.
#'
#'   - \bold{ggplot2}: \code{theme_trueMinimal} This function takes a ggplot object
#'   and removes the gray background, gridlines and adds opacity to the default
#'   black axes and labels, allowing the pattern of the visual to be expressed
#'   in unimpeded fashion.
#'
#'   - \bold{plotly}: \code{theme_plotly}, \code{theme_blank} removes reference
#'   lines at zero, and some of its 'modebar' is unnecessary.  Otherwise little
#'   is changed at this point, except for theme_blank, which is like theme_void
#'   for ggplot.
#' @note
#' You may continue to override any aspect of these themes. For example with
#' ggplot2, you would just add a theme afterward just like you would any other
#' plot.
#'
#' @importFrom magrittr '%>%'
#' @family themes
#'
#' @examples
#' library(visibly)
#' library(ggplot2)
#'
#' data(mtcars)
#'
#' ggplot(aes(wt, mpg), data=mtcars) +
#'   geom_point() +
#'   labs(title='Plot') +
#'   theme_trueMinimal()
#'
#' library(plotly)
#' mtcars %>%
#'   plot_ly(x=~wt, y=~mpg, mode='markers') %>%
#'   theme_plotly()
#'
#' mtcars %>%
#'   plot_ly(x=~wt, y=~mpg, mode='markers') %>%
#'   theme_blank()
#'
#' @export
theme_trueMinimal <- function(font_size = 12,
                              font_family = "",
                              center_axis_labels = FALSE){
  if (center_axis_labels) {
    axis_just <- 0.5
  }
  else {
    axis_just <- 1
  }
  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      face = "plain",
      colour = "gray46",
      size = font_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    # axis.text.x = ggplot2::element_text(),
    # axis.text.y = ggplot2::element_text(),
    axis.title.x = ggplot2::element_text(hjust = axis_just),
    axis.title.y = ggplot2::element_text(hjust = axis_just),
    title = ggplot2::element_text(colour='gray33', size = font_size*1.25),
    legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
    legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}



#' @rdname theme_trueMinimal
#' @export
theme_plotly <- function(vis, MB=FALSE) {
  if(! inherits(vis, 'plotly'))  stop('vis is not a plotly object.')
  vis <- vis %>%
    plotly::layout(xaxis = list(zeroline=FALSE,
                                showgrid=FALSE),
                   yaxis = list(zeroline=FALSE,
                                showgrid=FALSE),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent')
    if(!MB) {
      vis <- vis %>%
        plotly::config(displayModeBar=FALSE)
    }

  vis
}

#' @rdname theme_trueMinimal
#' @export
theme_blank <- function(vis, MB=FALSE) {
  if(! inherits(vis, 'plotly'))  stop('vis is not a plotly object.')

  a <- list(
    title = '',
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  vis <- vis %>%
    plotly::layout(xaxis = a,
                   yaxis = a,
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent')
  if(!MB) {
    vis <- vis %>%
      plotly::config(displayModeBar=FALSE)
  }

  vis
}
