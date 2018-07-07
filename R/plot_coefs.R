#' Plot coefficients with uncertainty
#'
#' @description This isn't really meant to be directly called, but is instead
#'   internally used by the plot_coefficients function.
#'
#' @param model_input Processed model effects
#' @inheritParams plot_coefficients
#'
#' @return  A ggplot of the coefficients with their corresponding uncertainty
#'   bars.
#'
plot_coefs <- function(model_input,
                       palette,
                       ref_line,
                       trans) {

  if (!requireNamespace("scico", quietly = TRUE)) {
    stop("scico package is needed for this function to work.
         Please install it.",
         call. = FALSE)
  }

  model_input <- model_input %>%
    dplyr::mutate(bold = ifelse(sign(ui_l)*sign(ui_u) == 1, 1, .9),
                  Coefficient = ordered(Coefficient, levels=Coefficient)) # sigh
  if (!is.null(trans))
    model_input <- model_input %>%
      dplyr::mutate_at(vars(value, contains('ui')), trans)

  listcol <- model_input %>%
    split(.$Coefficient) %>%
    purrr::map(function(x) c(seq(x$ui_l, x$value, length.out = 2000),
                             seq(x$value, x$ui_u, length.out = 2000))) %>%
    dplyr::bind_rows() %>%
    tidyr::gather(key=Coefficient, value=ui_value) %>%
    dplyr::group_by(Coefficient) %>%
    dplyr::mutate(value_sc = abs(scale(ui_value))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Coefficient = ordered(Coefficient,
                                        levels=levels(model_input$Coefficient)))
  # sigh again

  pointcol <- scico::scico(n = 1, begin = 1, palette = palette)
  if (pointcol == "#FFFFFF")
    pointcol <- scico::scico(n = 1, begin = 0, end = 0, palette = palette)


  model_input %>%
    ggplot2::ggplot(aes(x = Coefficient, y=value)) +
    ggplot2::geom_hline(yintercept = ref_line, alpha = .1) +
    geom_line(aes(group=Coefficient, y=ui_value, color=value_sc),
              data=listcol,
              size=5, show.legend = FALSE) +
    ggplot2::geom_point(size=3 + .05,     # point border
                        color = pointcol,
                        alpha=1) +
    ggplot2::geom_point(size=3,
                        color = '#FFFFFF',
                        alpha=1) +
    ggplot2::geom_point(size=.5,
                        color = pointcol,
                        alpha=1) +
    scico::scale_color_scico(begin = 1, end = 0, palette = palette) +
    ggplot2::coord_flip() +
    ggplot2::labs(x='', y='Coefficient') +
    theme_trueMinimal()


}



plot_coefs_re <- function(model_input,
                          ref_line) {

  model_input <- model_input %>%
    dplyr::mutate(bold = ifelse(sign(ui_l)*sign(ui_u) == 1, 1, .9),
                  Coefficient = factor(Coefficient, levels=Coefficient))  # sigh

  model_input %>%
    ggplot2::ggplot(aes(x = Coefficient, y=value)) +
    ggplot2::geom_hline(yintercept = ref_line, alpha = .25, color='#ff5500') +
    geom_linerange(aes(group=Coefficient, ymin=ui_l, ymax=ui_u, alpha=bold),
              color = 'gray50',
              size=1,
              show.legend = FALSE) +
    ggplot2::geom_point(size=3 + .05,     # point border
                        color = 'lightblue',
                        alpha=1) +
    ggplot2::geom_point(size=3,
                        color = '#FFFFFF',
                        alpha=1) +
    ggplot2::geom_point(size=.5,
                        color = 'lightblue',
                        alpha=1) +
    ggplot2::labs(x='', y='Coefficient') +
    theme_trueMinimal()


}
