#' Plot fixed or random effects coefficients for brmsfit objects.
#' @inheritParams plot_coefficients
#'
#' @return A ggplot of the coefficients and their interval estimates. Or the
#'   data that would be used to create the plot.
#' @examples
#' # placeholder
#'
#' @family model visualization
#'
#' @export
plot_coefficients.brmsfit <- function(model,
                                      order = 'decreasing',
                                      sd_multi = 2,
                                      keep_intercept = FALSE,
                                      palette = 'bilbao',
                                      ref_line = 0,
                                      trans = NULL,
                                      plot = TRUE,
                                      ranef = FALSE,
                                      which_ranef = NULL,
                                      ...) {

  if (!isTRUE(ranef)) plot_fixefs(model,
                                  order,
                                  sd_multi,
                                  keep_intercept,
                                  palette,
                                  ref_line,
                                  trans,
                                  plot)
  else plot_ranefs(model,
                   sd_multi,
                   ref_line,
                   trans,
                   plot,
                   which_ranef)
}

