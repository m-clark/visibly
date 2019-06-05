#' Plot 2-way GAM smooths
#' Plot 2-dimensional smooth terms
#' @param second_var Required for plot_gam_2d. The second continuous variable of
#'   interest.
#' @param by_var Required for plot_gam_by. The categorical variable of interest.
#' @param n_plot How many plotting points for the main_var/second_var? Default
#'   is 100, creating a 100 x 100 grid of points.
#' @param force_2d If the second_var has <= 5 values, the plot_gam_by is called.
#'   This will override that.
#' @param ... Options to scale_fill_scico for plot_gam_2d or
#'   scale_color_viridis_d for plot_gam_by (scale_color_scico_d if using scico
#'   development version).
#'
#' @details These functions plot the predictions for two covariates in a GAM
#'   model produced by the \link[mgcv]{mgcv} package. The \code{plot_gam_2d}
#'   function is used for plotting two continuous predictors, while
#'   \code{plot_gam_by} is used in the case where one of the variables is
#'   categorical. If \code{plot_gam_2d} is called with the second variable being
#'   categorical or of very few distinct values, a message will follow along
#'   with a switch to \code{plot_gam_by}. One can override this with the
#'   \code{force_2d} argument.
#'
#' @note Any attempt to use a non-numeric variable for the main_var will result in
#'   failure.
#'
#' @note If you are using gamm or gamm4 then you need to supply the mgcv model
#'   as the model object.
#'
#'
#' @seealso \link[scico]{scale_fill_scico} \link[ggplot2]{scale_colour_viridis_d}
#' @return A ggplot of the 2d effect.
#' @inheritParams plot_gam
#' @importFrom stats na.omit
#' @importFrom tibble tibble
#' @examples
#' library(mgcv); library(dplyr)
#' set.seed(0)
#'
#' d = gamSim(2, scale=.1)$data
#' mod <- gam(y ~ s(x, z), data = d)
#' plot_gam_2d(mod, main_var = x, second_var = z)
#' plot_gam_2d(mod, main_var = x, second_var = z, palette='oslo')
#'
#' d2 = gamSim(4)
#' mod_by <- gam(y ~ s(x2, by=fac), data = d2)
#' plot_gam_by(mod_by, main_var = x2, by_var = fac)
#'
#' @family model visualization
#'
#' @export
plot_gam_2d <- function(model,
                        main_var,
                        second_var,
                        conditional_data = NULL,
                        n_plot = 100,
                        force_2d = FALSE,
                        ...) {

  if (!inherits(model, 'gam'))
    stop('This function is for gam objects from mgcv')

  if(missing(main_var))
    stop('main_var and second_var are required.')

  if(missing(second_var))
    stop('main_var and second_var are required.')

  model_data <- model$model

  test_second_var <- model_data %>% pull(!!enquo(second_var))
  do_by <- n_distinct(test_second_var)

  if (!inherits(test_second_var, c('numeric', 'integer')) |
      do_by <= 5 &&
      force_2d == FALSE) {
    message(glue::glue('second_var is not numeric or has very few distinct
                       values, switching to plot_gam_by. If not desired,
                       put force_2d = TRUE and change class of second_var to
                       numeric/integer if necessary.'))
    return(
      plot_gam_by(model = model,
                  main_var = !!enquo(main_var),
                  by_var   = !!enquo(second_var),
                  conditional_data = conditional_data,
                  n_plot = n_plot,
                  ...)
    )
  }

  mv <- rlang::enquo(main_var)
  sv <- rlang::enquo(second_var)

  mv_range <- range(na.omit(pull(model_data, !!mv)))
  sv_range <- range(na.omit(pull(model_data, !!sv)))

  cd <- tibble::tibble(!!quo_name(mv) := seq(mv_range[1],
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

  data_list %>%
    ggplot(aes(x=!!mv, y=!!sv)) +
    geom_tile(aes(fill=prediction)) +
    scico::scale_fill_scico(...) +
    theme_trueMinimal()
}

#' @rdname plot_gam_2d
#' @export
plot_gam_by <- function(model,
                        main_var,
                        by_var,
                        conditional_data=NULL,
                        n_plot=500,
                        ...) {

  if (!inherits(model, 'gam'))
    stop('This function is for gam objects from mgcv')

  if(missing(main_var))
    stop('main_var and by_var are required.')

  if(missing(by_var))
    stop('main_var and by_var are required.')

  model_data <- model$model
  mv <- rlang::enquo(main_var)
  bv <- rlang::enquo(by_var)

  mv_range <- range(na.omit(model_data %>% pull(!!mv)))

  cd <- tidyr::crossing(!!quo_name(mv) := seq(mv_range[1],
                                             mv_range[2],
                                             length.out = n_plot),
                       !!quo_name(bv) := model_data %>% pull(!!bv) %>% unique())

  data_list <-
    create_prediction_data(model_data = model_data,
                           conditional_data = cd) %>%
    mutate(prediction = predict(model, ., type = 'response'))

  if (inherits(data_list %>% pull(!!bv), 'numeric'))
    data_list <- data_list %>%
    mutate(!!quo_name(bv) := as.factor(!!bv))

  # check for palette until scico updates on CRAN; use viridis if not
  if (!requireNamespace("scico", quietly = TRUE) ||
      !'batlow' %in% scico::scico_palette_names()) {
    col_scale = scale_color_viridis_d(...)
  } else {
    col_scale = scico::scale_color_scico_d(...)
  }

  data_list %>%
    ggplot(aes(x=!!mv, y=prediction, color=!!bv)) +
    geom_line() +
    col_scale +
    theme_trueMinimal()
}
