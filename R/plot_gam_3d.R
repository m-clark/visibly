#' @title Plot 2d smooths in 3d

#' @param model The mgcv gam model
#'
#' @param main_var The 'x' axis.
#' @param second_var The 'y' axis'
#' @param conditional_data Values for other covariates. Default is NULL see
#'   details.
#' @param n_plot Points to plot.  100 (the default) works well.  Embiggen at the
#'   cost of your own waiting time.
#' @param dmb Whether to use plotly's display mode bar. Default is FALSE.
#' @param ... Arguments for \link[scico]{scico}
#'
#' @details This works like \link[visibly]{plot_gam_2d}, the only difference
#'   being that a 3d plot is generated instead. It uses \link[scico]{scico} for
#'   the palette.  It is expected that the two input variables are continuous
#' @family model visualization
#' @return A plotly surface object
#' @examples
#' library(mgcv); library(visibly)
#' set.seed(0)
#'
#' d = gamSim(2, scale=.1)$data
#' mod <- gam(y ~ s(x, z), data = d)
#' plot_gam_3d(mod, main_var = x, second_var = z)
#' plot_gam_3d(mod, main_var = x, second_var = z, palette='tokyo')
#' @export
plot_gam_3d <- function(model,
                        main_var,
                        second_var,
                        conditional_data = NULL,
                        n_plot = 100,
                        dmb = FALSE,
                        ...) {

  if (!inherits(model, 'gam'))
    stop('This function is for gam objects from mgcv')

  if(missing(main_var))
    stop('main_var and second_var are required.')

  if(missing(second_var))
    stop('main_var and second_var are required.')

  model_data <- model$model

  # test_second_var <- model_data %>% pull(!!enquo(second_var))
  # do_by <- n_distinct(test_second_var)

  mv <- rlang::enquo(main_var)
  sv <- rlang::enquo(second_var)

  mv_range <- range(na.omit(pull(model_data, !!mv)))
  sv_range <- range(na.omit(pull(model_data, !!sv)))

  cd <- data_frame(!!quo_name(mv) := seq(mv_range[1],
                                         mv_range[2],
                                         length.out = n_plot),
                   !!quo_name(sv) := seq(sv_range[1],
                                         sv_range[2],
                                         length.out = n_plot)) %>%
    tidyr::expand(!!mv, !!sv)

  data_list <-
    create_prediction_data(model_data = model_data,
                           conditional_data = cd) %>%
    mutate(prediction = predict(model, ., type = 'response'))

  mv_name <- quo_name(mv)
  sv_name <- quo_name(sv)

  xlo <- list(
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = mv_name
  )

  ylo <- list(
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = sv_name
  )

  zlo <- list(
    ticktext = '',
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = 'Prediction'
  )

  colnames(data_list)[1:2] = c('x', 'y')

  # Sigh, but it works
  pred_mat <- matrix(data_list$prediction, n_plot, n_plot)

  # override plotly's default x y z labels/text
  custom_txt <- paste0("Prediction: ", round(data_list$prediction, 3),
                       "\n", mv_name, ": ", round(data_list$x, 3),
                       "\n", sv_name, ": ", round(data_list$y, 3)) %>%
    matrix(n_plot, n_plot)

  data_list %>%
    plotly::plot_ly(x = unique(.$x),
                    y = unique(.$y),
                    colors = colorRamp(scico::scico(nrow(.), ...))) %>%
    plotly::add_surface(z=~pred_mat, text = custom_txt, hoverinfo='text') %>%
    plotly::layout(scene = list(      # scene!
      xaxis = xlo,
      yaxis = ylo,
      zaxis = zlo
    )) %>%
    theme_plotly() %>%
    plotly::config(displayModeBar = dmb)
}

