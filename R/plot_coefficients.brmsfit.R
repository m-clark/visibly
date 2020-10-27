#' Plot fixed or random effects coefficients for brmsfit objects.
#'
#' @inheritParams plot_coefficients
#' @param order The order of the plots- "increasing", "decreasing", or a numeric
#'   vector giving the order. The default is NULL, i.e. the default ordering.
#'   Not applied to random effects.
#' @param prob For `brmsfit` objects, the interval for the uncertainty level.
#'   Default is .95.
#' @param palette A scico palette. Default is 'bilbao'.
#' @param keep_intercept Default is FALSE. Intercepts are typically on a very
#'   different scale than covariate effects.
#' @param ref_line A reference line. Default is zero.
#' @param trans A transformation function to be applied to the coefficients
#'   (e.g. exponentiation).
#' @param plot Default is TRUE, but sometimes you just want the data.
#' @param ranef If applicable, whether to plot random effects instead of fixed
#'   effects.
#' @param which_ranef If plotting random effects, which one to plot.
#'
#' @return A ggplot of the coefficients and their interval estimates. Or the
#'   data that would be used to create the plot.
#' @examples
#' # placeholder
#'
#' @family model visualization
#'
#' @export
plot_coefficients.brmsfit <- function(
  model,
  order = 'decreasing',
  prob = .95,
  keep_intercept = FALSE,
  palette = 'bilbao',
  ref_line = 0,
  trans = NULL,
  plot = TRUE,
  ranef = FALSE,
  which_ranef = NULL,
  ...
) {

  if (!isTRUE(ranef))
    plot_fixefs(model,
                order,
                prob,
                keep_intercept,
                palette,
                ref_line,
                trans,
                plot)
  else
    plot_ranefs(model,
                prob,
                ref_line,
                trans,
                plot,
                which_ranef)
}

