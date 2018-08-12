#' Plot fixed or random effects coefficients for merMod objects.
#' @inheritParams plot_coefficients

#' @details This plots the fixed or random effects of lme4 objects. For more
#'   information on the fixed effects, see \link[visibly]{plot_coefficients}.It
#'   requires the \link[lme4]{lme4} package.  The plot for random effects is
#'   basically the dotplot demonstrated at \code{?lme4::ranef}, but instead uses
#'   \link[ggplot2]{ggplot2} so you would have a little easier time working with
#'   it to do with as you wish (for multiple random effects, a list of ggplot
#'   objects can be returned). Many of the options for fixed effects are
#'   removed, as they either don't make much sense or for practical reasons.
#' @return A ggplot of the coefficients and their interval estimates. Or the
#'   data that would be used to create the plot.
#' @examples
#' library(lme4)
#' fit_mer = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' plot_coefficients(fit_mer, ranef = TRUE, which_ranef = 'Subject')
#'
#' @family model visualization
#' @export
plot_coefficients.merMod <- function(model,
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

