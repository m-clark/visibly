#' Plot coefficients for standard linear models.
#'
#' @description A basic plot of coefficients with their uncertainty interval for
#'   lm and glm objects.
#'
#' @param model The lm or glm model
#'
#' @details This is more or less a function that serves as the basis for other models I actually use.
#'
#' @inheritParams plot_coefficients
#' @return A ggplot of the coefficients and their interval estimates. Or the
#'   data that would be used to create the plot.
#' @examples
#' mod = lm(mpg ~ ., mtcars)
#' plot_coefficients(mod, order = 'increasing')
#'
#' @family model visualization
#'
#' @export
plot_coefficients.lm <- function(model,
                                 order = 'decreasing',
                                 sd_multi = 2,
                                 keep_intercept = FALSE,
                                 palette = 'bilbao',
                                 ref_line = 0,
                                 trans = NULL,
                                 plot = TRUE,
                                 ...) {

  init <- summary(model)[['coefficients']]

  if (!isTRUE(keep_intercept)) {
    init <- init[!grepl(rownames(init), pattern = 'Intercept'), ]
  }

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
  sds <- init[,'Std. Error']

  # create uis based on multiplier
  ui <- coefs  + outer(sds, c(-sd_multi, sd_multi))

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

#' @rdname plot_coefficients.lm
#'
#' @family model visualization
#'
#' @export
plot_coefficients.glm <- plot_coefficients.lm

