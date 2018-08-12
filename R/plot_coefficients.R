#' Plot coefficients
#'
#' @description A basic plot of coefficients with their uncertainty interval.
#'
#' @param model The model that is the point of this function. For example, lm, glm, gam, lme4, brms.
#' @param order The order of the plots- "increasing", "decreasing", or a numeric
#'   vector giving the order. The default is NULL, i.e. the default ordering. Not applied to random effects.
#' @param sd_multi The multiplier that determines the width of the interval. Default is 2.
#' @param palette A scico palette. Default is 'bilbao'.
#' @param keep_intercept Default is FALSE. Intercepts are typically on a very
#'   different scale than covariate effects.
#' @param ref_line A reference line. Default is zero.
#' @param trans A transformation function to be applied to the coefficients
#'   (e.g. exponentiation).
#' @param plot Default is TRUE, but sometimes you just want the data.
#' @param ranef If applicable, whether to plot random effects instead of fixed effects.
#' @param which_ranef If plotting random effects, which one to plot.
#' @param ... Other arguments applied for specific methods.
#'
#' @return A ggplot of the coefficients and their interval estimates. Or the
#'   data that would be used to create the plot.
#'
#' @export
#' @family model visualization
#' @examples
#' mod = lm(mpg ~ ., mtcars)
#' plot_coefficients(mod, order = 'increasing')
#'
plot_coefficients <- function(model,
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

  UseMethod(generic = 'plot_coefficients')
}



