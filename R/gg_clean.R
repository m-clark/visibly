#' Clean visualization themes
#'
#' @description Clean up plots from their defaults.
#'
#' @param vis A visualization created by \link[ggplot2]{ggplot2} or
#'   \link[plotly]{plotly}.
#' @param font_size Font size for axis labels in theme_clean
#' @param font_family Font family for axis labels in theme_clean
#' @param center_axis_labels Logical. Center axis labels in theme_clean.
#'   Default is FALSE.
#' @param MB For plotly, an option to display the mode bar. Defaults to FALSE.
#'
#' @details From a gray background, to unnecessary gridlines, to by-default
#'   reference lines, some of the more popular visualization packages come out
#'   75\% fantastic and 25\% questionable/arbitrary/problematic.  These
#'   functions remove unnecessary gridlines, 'de-bold' the blacks, etc.
#'
#'   - \bold{ggplot2}: \code{theme_clean} This function takes a ggplot object
#'   and removes the gray background, gridlines and adds opacity to the default
#'   black axes and labels, allowing the pattern of the visual to be expressed
#'   in unimpeded fashion.  \code{theme_trueMinimal} was the old name of this
#'   function and is still usable.
#'
#'   - \bold{plotly}: \code{theme_plotly}, \code{theme_blank} removes reference
#'   lines at zero, and some of its 'modebar' is unnecessary.  Otherwise little
#'   is changed at this point, except for theme_blank, which is like theme_void
#'   for ggplot.
#' @note
#' You may continue to override any aspect of these themes. For example with
#' ggplot2, you would just add a \code{theme} afterward just like you would any other
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
#'   theme_clean()
#'
#' ggplot(aes(wt, mpg), data=mtcars) +
#'   geom_point() +
#'   labs(title='Plot') +
#'   theme_clean(center_axis_labels = TRUE)
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
#' @aliases theme_trueMinimal
#' @export
theme_clean <- function(font_size = 12,
                        font_family = "",
                        center_axis_labels = FALSE){
  if (center_axis_labels) {
    haxis_just_x <- 0.5
    vaxis_just_y <- 0.5
    v_rotation_x <- 0
    v_rotation_y <- 0
  }
  else {
    haxis_just_x <- 1
    vaxis_just_y <- 1
    v_rotation_x <- 0
    v_rotation_y <- 0
  }

  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      face = "plain",
      colour = "gray30",
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
    axis.title.x = ggplot2::element_text(hjust = haxis_just_x, angle = v_rotation_x, size = .8*font_size),
    axis.title.y = ggplot2::element_text(vjust = vaxis_just_y, angle = v_rotation_y, size = .8*font_size),
    title = ggplot2::element_text(colour='gray30', size = font_size*1.25),
    legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
    legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}

#' @rdname theme_clean
#' @export
theme_trueMinimal <- theme_clean


#' @rdname theme_clean
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

#' @rdname theme_clean
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
