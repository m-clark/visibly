#' Clean visualizations
#'
#' @description Clean up plots from their defaults.
#'
#' @param vis A plotly, or ggvis
#' @param MB For plotly, an option to dispaly the mode bar. Defaults to FALSE.
#' @details From a gray background, to unnecessary gridlines, to by-default
#'   reference lines, some of the more popular visualization packages come out
#'   75% fantastic and 25% wtf?.  These functions remove unnecessary gridlines,
#'   'de-bold' the blacks, etc. **ggplot2**: This function takes a ggplot object
#'   and removes the gray background, gridlines and adds opacity to the default
#'   black axes and labels, allowing the pattern of the visual to be expressed
#'   in unimpeded fashion.
#'
#'   Plotly will put reference lines at zero, and some of its 'modebar' is
#'   unnecessary.  Otherwise little is changed at this point, except for
#'   theme_blank, which is like theme_void for ggplot.
#' @importFrom magrittr '%>%'
#'
#' @examples
#' library(visibly)
#' library(ggplot2)
#' data(mtcars)
#' # currently ggplot is having a problem with rlang so this is commented until they fix it
#' # ggplot(aes(wt, mpg), data=mtcars) + theme_trueMinimal()
#' library(plotly)
#' mtcars %>% plot_ly(x=~wt, y=~mpg, mode='markers') %>% theme_plotly()
#' mtcars %>% plot_ly(x=~wt, y=~mpg, mode='markers') %>% theme_blank()



#' @export
theme_trueMinimal <- function(){
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(colour='gray50'),
    axis.text.y = ggplot2::element_text(colour='gray50'),
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
