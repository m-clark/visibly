#' Plot Generalized Additive Model Results
#'
#' @description Plot 1d marginal effects from mgcv GAM model results.
#'
#' @param model The mgcv GAM.
#' @param main_var Which variable do you want to plot? Uses bare variable names
#'   and can take multiple variables via \code{vars()}.
#' @param conditional_data This is the same as the newdata argument for predict.
#'   Supply a data frame with desired values of the model covariates.
#' @param line_color The color of the fitted line.
#' @param ribbon_color The color of the uncertainty interval around the line.
#' @param nrow If plotting multiple smooths, these are passed to facet_wrap.
#' @param ncol If plotting multiple smooths, these are passed to facet_wrap.

#' @details This function is fairly 'no-frills' at the moment. Only 1d or
#'   multiple 1d smooths of numeric variables are able to be plotted. If
#'   conditional data is not supplied, it will be created by
#'   \link[visibly]{create_prediction_data}, which defaults to means for
#'   numeric, most common category for categorical variables, and 500
#'   observations.  It currently will fail if you have a mix of 2d and 1d and do
#'   not specify a smooth.
#'
#' @return a ggplot2 object of the effects of main_var.
#'
#' @importFrom stats predict
#'
#' @examples
#' library(mgcv) # you don't need this function if you don't use this package
#' library(dplyr)
#' # example taken from the mgcv plot.gam help file.
#' set.seed(0)
#' ## fake some data...
#' f1 <- function(x) {
#'   exp(2 * x)
#' }
#' f2 <- function(x) {
#'   0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
#' }
#' f3 <- function(x) {
#'   x * 0
#' }
#'
#' n <- 200
#' sig2 <- 4
#'
#' d = data_frame(
#'   x0 = rep(1:4, 50),
#'   x1 = runif(n, 0, 1),
#'   x2 = runif(n, 0, 1),
#'   x3 = runif(n, 0, 1),
#'   e  = rnorm(n, 0, sqrt(sig2)),
#'   y  = 2 * x0 + f1(x1) + f2(x2) + f3(x3) + e
#' ) %>%
#'   mutate(x0 = factor(x0))
#'
#' b <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = d)
#'
#' library(visibly)
#'
#' plot_gam(b,
#'          conditional_data = data_frame(x2 = runif(500)),
#'          main_var = x2)
#'
#' plot_gam(b, main_var = x2)
#'
#'
#' plot_gam(b, main_var = vars(x2, x1))
#'
#' plot_gam(b,
#'          conditional_data = data_frame(x1 = runif(500),
#'                                        x2 = runif(500)),
#'          main_var = vars(x2, x1))
#'
#' # compare with mgcv plot
#' plot(b, pages=1)
#'
#' @family model visualization
#' @importFrom tibble tibble as_tibble
#'
#' @export
plot_gam <- function(model,
                     main_var,
                     conditional_data = NULL,
                     line_color = '#7B321C',
                     ribbon_color = '#28688640',
                     ncol = NULL,
                     nrow = NULL) {

  if (!inherits(model, 'gam'))
    stop('This function is for gam objects from mgcv')

  model_data <- model$model

  mv <- rlang::enquo(main_var)

  if (rlang::quo_is_missing(mv)) {
    main_var <- map_chr(model$smooth, function(x) x$vn)
  }

  check_mv <- tryCatch(rlang::is_quosures(main_var), error = function(c) {
    msg <- conditionMessage(c)
    invisible(structure(msg, class = "try-error"))
  })


  if (class(check_mv) != 'try-error') {
    plot_gam_multi1d(model = model,
                     conditional_data = conditional_data,
                     main_var = main_var,
                     line_color = line_color,
                     ribbon_color = ribbon_color,
                     ncol = ncol,
                     nrow = nrow)
  } else {
    plot_gam_1d(model = model,
                conditional_data = conditional_data,
                main_var = mv,
                line_color = line_color,
                ribbon_color = ribbon_color)
  }
}




#' @rdname plot_gam
plot_gam_1d <- function(model,
                        main_var,
                        conditional_data = NULL,
                        line_color = '#ff5500',
                        ribbon_color = '#00aaff') {

  model_data <- model$model
  init <- pull(model_data, !!main_var)

  if (!is.numeric(init)) {
    vname <- rlang::quo_name(main_var)
    return(
      message(glue::glue('{vname} appears not to be numeric. Skipping.
                           Functionality may be added in the future.')))
  }

  if (is.null(conditional_data)) {
    init <- select(model_data, !!main_var)

    cd <- data_frame(!!quo_name(main_var) := seq(min(init, na.rm = TRUE),
                                                 max(init, na.rm = TRUE),
                                                 length.out = 500))

    data_list <-
      create_prediction_data(model_data = model_data,
                             conditional_data = cd) %>%
      bind_cols(
        tibble::as_tibble(
          predict(model, ., type = 'response', se=TRUE))) %>%
      mutate(ll = fit - 2*se.fit,
             ul = fit + 2*se.fit) %>%
      select(!!!main_var, fit, ll, ul) %>%
      rename(value = !!main_var) %>%
      mutate(term = quo_name(main_var))
  } else {
    data_list <-
      create_prediction_data(model_data = model_data,
                             conditional_data = conditional_data) %>%
      bind_cols(
        tibble::as_tibble(
          predict(model, ., type = 'response', se=TRUE))) %>%
      mutate(ll = fit - 2*se.fit,
             ul = fit + 2*se.fit) %>%
      select(!!!main_var, fit, ll, ul) %>%
      rename(value = !!main_var) %>%
      mutate(term = quo_name(main_var))
  }

  data_list %>%
    ggplot(aes(x=value, y=fit)) +
    geom_ribbon(aes(ymin=ll, ymax=ul), fill=ribbon_color) +
    geom_line(color=line_color) +
    theme_trueMinimal()
}



#' @rdname plot_gam
plot_gam_multi1d <- function(model,
                             main_var,
                             conditional_data = NULL,
                             line_color = '#ff5500',
                             ribbon_color = '#00aaff',
                             ncol = ncol,
                             nrow = nrow) {

  model_data <- model$model
  n_terms <- length(main_var)
  data_list <- vector('list', n_terms)

  # create conditional data
  for (i in 1:n_terms){
    if (is.null(conditional_data)) {
      init <- select(model_data, !!main_var[[i]])

      if (!is.numeric(unlist(init))) {
        # cd = data_frame(!!quo_name(main_var[[i]]) :=
        #                   unique(unlist(init)))
        vname <- names(init)
        message(glue::glue('{vname} appears not to be numeric. Skipping.
                           Functionality may be added in the future.'))
        data_list[[i]] <- NULL
      } else {
        cd <- data_frame(!!quo_name(main_var[[i]]) :=
                          seq(min(init, na.rm = TRUE),
                              max(init, na.rm = TRUE),
                              length.out = 500))

        data_list[[i]] <-
          create_prediction_data(model_data = model_data,
                                 conditional_data = cd) %>%
          bind_cols(tibble::as_tibble(
            predict(model, ., type = 'response', se=TRUE))
          ) %>%
          mutate(ll = fit - 2*se.fit,
                 ul = fit + 2*se.fit) %>%
          select(!!!main_var[[i]], fit, ll, ul) %>%
          rename(value = !!main_var[[i]]) %>%
          mutate(term = quo_name(main_var[[i]]))
      }
    } else {

      # check if variable to be plotted is provided in the conditional data; if
      # not simulate based on range
      check_cd <- tryCatch(select(conditional_data, !!main_var[[i]]),
                           error = function(c) {
                             msg <- conditionMessage(c)
                             invisible(structure(msg, class = "try-error"))
                           })

      if (inherits(check_cd, 'try-error')) {
        var_range <- model_data %>%
          pull(!!main_var[[i]]) %>%
          range()
        cd <- data_frame(
          !!quo_name(main_var[[i]]) := seq(var_range[1],
                                           var_range[2],
                                           length.out = nrow(conditional_data))
        )
      } else {
        cd <- select(conditional_data, !!main_var[[i]])
      }

      data_list[[i]] <-
        create_prediction_data(model_data = model_data,
                               conditional_data = cd) %>%
        bind_cols(
          tibble::as_tibble(
            predict(model, ., type = 'response', se=TRUE))) %>%
        mutate(ll = fit - 2*se.fit,
               ul = fit + 2*se.fit) %>%
        select(!!!main_var[[i]], fit, ll, ul) %>%
        rename(value = !!main_var[[i]]) %>%
        mutate(term = quo_name(main_var[[i]]))
    }
  }

  bind_rows(data_list) %>%
    ggplot(aes(x=value, y=fit)) +
    geom_ribbon(aes(ymin=ll, ymax=ul), fill=ribbon_color) +
    geom_line(color=line_color) +
    facet_wrap(~ term, ncol = ncol, nrow = nrow, scales = 'free') +
    theme_trueMinimal()
}
