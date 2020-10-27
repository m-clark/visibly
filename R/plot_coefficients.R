#' Plot coefficients
#'
#' @description A basic plot of coefficients with their uncertainty interval.
#'
#' @param model The model. For example, lm, glm, gam, lme4, brms.
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
plot_coefficients <- function(
  model,
  ...
  ) {

  UseMethod(generic = 'plot_coefficients')
}



