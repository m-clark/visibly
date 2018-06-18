#' Plot coefficients with uncertainty
#' @description This isn't really meant to be directly called, but is instead
#'   internally used by the plot_coefficients function.
#' @inheritParams plot_coefficients
#' @return a ggplot2 object or the effect estimates
#'
#' @seealso plot_coefficients
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

  init <- brms::fixef(model)

  if (is.character(order) && order == 'decreasing') {
    ord <- order(init[,'Estimate'], decreasing = TRUE)
  } else if (is.character(order) && order == 'increasing') {
    ord <- order(init[,'Estimate'])
  } else if (is.numeric(order)) {
    ord <- order
  }

  init <- init[ord,]

  # grab coefs and sd
  coefs <- init[,'Estimate']
  sds   <- init[,'Est.Error']

  if (isFALSE(keep_intercept)) {
    sds   <- sds[!grepl(names(coefs), pattern = 'Intercept')]
    coefs <- coefs[!grepl(names(coefs), pattern = 'Intercept')]
  }

  # create uis based on multiplier
  ui  <- coefs  + outer(sds, c(-sd_multi, sd_multi))

  out <-
    data.frame(value = coefs,
               ui) %>%
    tibble::rownames_to_column(var='Coefficient') %>%
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

  init <- summary(model)$coefficients

  if (is.character(order) && order == 'decreasing') {
    ord <- order(init[,'Estimate'], decreasing = TRUE)
  } else if (is.character(order) && order == 'increasing') {
    ord <- order(init[,'Estimate'])
  } else if (is.numeric(order)) {
    ord <- order
  }

  init <- init[ord,]

  # grab coefs and sd
  coefs <- init[,'Estimate']
  sds   <- init[,'Std. Error']

  if (isFALSE(keep_intercept)) {
    sds   <- sds[!grepl(names(coefs), pattern = 'Intercept')]
    coefs <- coefs[!grepl(names(coefs), pattern = 'Intercept')]
  }

  # create uis based on multiplier
  ui  <- coefs  + outer(sds, c(-sd_multi, sd_multi))

  out <-
    data.frame(value = coefs,
               ui) %>%
    tibble::rownames_to_column(var='Coefficient') %>%
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
