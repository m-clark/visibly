#' Plot coefficients with uncertainty
#' @description This isn't really meant to be directly called, but is instead
#'   internally used by the plot_coefficients function.
#' @inheritParams plot_coefficients
#' @return a ggplot2 object or the effect estimates
#'
#' @seealso \link[visibly]{plot_coefficients}
#'
#' @examples
#' #placeholder
plot_fixefs <- function(model,
                        order = 'decreasing',
                        sd_multi = 2,
                        keep_intercept = FALSE,
                        palette = 'bilbao',
                        ref_line = 0,
                        trans = NULL,
                        plot = TRUE,
                        ...) {

  UseMethod(generic = 'plot_fixefs')

}

#' @export
#' @rdname plot_fixefs
plot_fixefs.brmsfit <- function(model,
                                order,
                                sd_multi,
                                keep_intercept,
                                palette,
                                ref_line,
                                trans,
                                plot,
                                ...) {

  init <- broom::tidy(model, par_type = 'non-varying')

  if (!isTRUE(keep_intercept)) {
    init <- init %>%
      dplyr::filter(!grepl(term, pattern = 'Intercept'))
  }

  if (is.character(order) && order == 'decreasing') {
    ord <- order(init[,'estimate'], decreasing = TRUE)
  } else if (is.character(order) && order == 'increasing') {
    ord <- order(init[,'estimate'])
  } else if (is.numeric(order)) {
    ord <- order
  }

  init <- init[ord,]

  # grab coefs and sd
  coefs <- init[,'estimate']
  sds   <- init[,'std.error']

  # create uis based on multiplier
  ui  <- coefs  + outer(sds, c(-sd_multi, sd_multi))

  out <-
    data.frame(value = coefs,
               ui) %>%
    dplyr::mutate(Coefficient = init$term) %>%
    dplyr::rename(ui_l = X1,
                  ui_u = X2)

  # call internal gg
  if (plot) {
    plot_coefs(out,
               palette = palette,
               ref_line = ref_line,
               trans =  trans)
  } else {
    out
  }
}

#' @rdname plot_fixefs
plot_fixefs.merMod <- function(model,
                               order,
                               sd_multi,
                               keep_intercept,
                               palette,
                               ref_line,
                               trans,
                               plot,
                               ...) {

  # suppress char/fac warnings
  init <- suppressWarnings({broom::tidy(model)}) %>%
    dplyr::filter(group == 'fixed')

  if (!isTRUE(keep_intercept)) {
    init <- init %>%
      filter(!grepl(term, pattern = 'Intercept'))
  }

  if (is.character(order) && order == 'decreasing') {
    ord <- order(dplyr::pull(init, estimate), decreasing = TRUE)
  } else if (is.character(order) && order == 'increasing') {
    ord <- order(dplyr::pull(init, estimate))
  } else if (is.numeric(order)) {
    ord <- order
  }

  init <- init[ord, , drop = FALSE]

  # grab coefs and sd
  coefs <- dplyr::pull(init, estimate)
  sds   <- dplyr::pull(init, std.error)

  # create uis based on multiplier
  ui  <- coefs  + outer(sds, c(-sd_multi, sd_multi))

  out <-
    data.frame(value = coefs,
               ui) %>%
    mutate(Coefficient = init$term) %>%
    dplyr::rename(ui_l = X1,
                  ui_u = X2)

  # call internal gg
  if (plot) {
    plot_coefs(out,
               palette = palette,
               ref_line = ref_line,
               trans =  trans)
  } else {
    out
  }
}

# plot_fixefs.glmerMod <- plot_fixefs.merMod
