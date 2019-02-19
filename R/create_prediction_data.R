#' Generate prediction data
#'
#' @description A port of the tidyext function.
#'
#' @param model_data The original data. Ideally this would come from a model
#'   object.
#' @param conditional_data A data.frame constructed from something like
#'   \code{base::expand.grid}
#' @param num A function like mean or median to be applied to numeric data.
#'   Should return a single value. Default is mean.
#' @param cat Set categorical variables to the reference level ('ref') or the
#'   most frequently occurring category (most_common, the default).
#' @param ... Additional arguments to num, e.g. \code{na.rm=T}
#' @details This is a port of the tidyext function...
#'
#'   Given data that was used in a model, create data that can be used
#'   for predictions at key values, especially as a prelude to visualization.
#'   Some package functions can be found that do this, but are specific to
#'   certain models or don't quite provide the flexibility I want. Specifically,
#'   this allows for an arbitrary function to apply to numeric variables, and
#'   for categorical(ish) variables, one has the option for the most common
#'   category (ties go to the first category), or the reference level if a
#'   factor. For now class Date is treated as categorical.
#'
#'   In addition, one may supply their own data to set certain variables at any
#'   particular values via the \code{conditional_data} argument, for example,
#'   using \code{expand.grid} or \code{tidyr::crossing}. Variables not supplied
#'   as columns in the \code{conditional_data} are treated as above.
#'
#' @return A data frame suitable for the \code{newdata} argument for predict functions.
#' @seealso \link[tidyext]{create_prediction_data}
#' @importFrom utils type.convert
#'
#' @examples
#' library(visibly)
#' create_prediction_data(iris)
#' create_prediction_data(iris, num = median,
#'                        expand.grid(
#'                          Sepal.Width=c(0,3,5),
#'                          Species = c('setosa', 'virginica')
#'                          )
#'                        )
#' create_prediction_data(iris, num = function(x) quantile(x, p=.75))
#'
#' test_mod = lm(mpg  ~ wt + cyl, mtcars)
#' nd = create_prediction_data(test_mod$model)
#' predict(test_mod, newdata = nd)
#'
#' @export
create_prediction_data <- function(model_data,
                                   conditional_data = NULL,
                                   num = mean,
                                   cat = 'most_common',
                                   ...) {
  if (cat == 'most_common') {
    catfun <- function(x) {
      cx <- class(x)
      x <- suppressWarnings(names(sort(table(x), decreasing = TRUE))[1])
      if (cx == 'Date') {
        x <-  as.Date(x)
      } else {
        x <- type.convert(x, cx)
      }
    }
  } else {
    # use reference level
    catfun <-  function(x) {
      cx <- class(x)
      x <- levels(factor(x))[1]
      if (cx == 'Date') {
        x <-  as.Date(x)
      } else {
        x <- type.convert(x, cx)
      }
      x
    }
  }

  # quick fix of lack of select_not; return to later?
  if (!is.null(conditional_data)) {
    not_these <- names(conditional_data)
    pred_data <- model_data %>%
      select(-dplyr::one_of(not_these))
  } else {
    pred_data <- model_data
  }

  pred_data <- pred_data %>%
    mutate_if(function(x) is.numeric(x), num, ...) %>%
    mutate_if(function(x)
      rlang::inherits_any(x, c('factor', 'string', 'logical', 'Date')),
              catfun) %>%
    slice(1)

  if (!is.null(conditional_data)) {
    data.frame(conditional_data, pred_data)
  } else {
    data.frame(pred_data)  # tibbles don't always work with some predict methods
  }
}



