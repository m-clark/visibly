#' Plot coefficients with uncertainty
#' @description This isn't really meant to be directly called, but is instead
#'   internally used by the plot_coefficients function.
#'
#' @param model  The brmsfit or lme4 model
#' @param sd_multi The sd multiplier
#' @param ref_line The reference line
#' @param trans A transformation to be applied to the coefficient.  Currently unused.
#' @param plot Whether to plot or just provide the data.
#' @param which_ranef Which random effect to plot
#' @param ...  Other options passed to specific methods.  Currently unused.
#'
#' @return a ggplot2 object or the effect estimates
#'
#' @seealso \link[visibly]{plot_coefficients}
#'
#' @examples
#' #placeholder
plot_ranefs <- function(model,
                        sd_multi = 2,
                        ref_line = 0,
                        trans = NULL,
                        plot = TRUE,
                        which_ranef = NULL,
                        ...) {

  UseMethod(generic = 'plot_ranefs')

}


#' @describeIn  plot_ranefs
plot_ranefs.brmsfit <- function(model,
                                sd_multi,
                                ref_line,
                                trans,
                                plot,
                                which_ranef,
                                ...) {
  init <- brms::ranef(model)

  if(is.null(which_ranef))
    stop('Need the name of the random effect to be plotted.')

  if(!isTRUE(which_ranef %in% names(init)))
    stop(
      paste('which_ranef not found among names of random effects. Names are:',
                 c(paste(names(init), collapse = ', ')))
      )

  init <- init[[which_ranef]]
  n_ranef <- dim(init)[3]

  re_plot_list <- vector('list', n_ranef)

  for (re in 1:n_ranef) {
    res <- init[,,re]
    ord <- order(res[,'Estimate'])

    res <- res[ord,]

    # grab coefs and sd
    coefs <- res[,'Estimate']
    sds   <- res[,'Est.Error']

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
      re_plot_list[[re]] <- plot_coefs_re(out,
                                         ref_line = ref_line)

    } else {
      re_plot_list[[re]] <- out
    }
  }
  if (length(re_plot_list) == 1)
    re_plot_list[[1]]
  else re_plot_list
}



#' @describeIn  plot_ranefs
plot_ranefs.merMod <- function(model,
                               sd_multi,
                               ref_line,
                               trans,
                               plot,
                               which_ranef,
                               ...) {

  init <- lme4::ranef(model, condVar=TRUE)

  if(is.null(which_ranef))
    stop('Need the name of the random effect to be plotted.')

  if(!isTRUE(which_ranef %in% names(init)))
    stop(
      paste('which_ranef not found among names of random effects. Names are:',
               c(paste(names(init), collapse = ', ')))
      )

  init <- init[[which_ranef]]
  group_names  <- rownames(init)
  effect_names <- names(init)
  init_sd <- attributes(init)$postVar %>% apply(3, diag)

  # check for different non-matrix return if single effect
  if (is.null(dim(init_sd))) {
    init_sd <- matrix(sqrt(init_sd), ncol = 1)
  } else {
    init_sd <- t(sqrt(init_sd))
  }

  n_ranef <- dim(init)[2]

  re_plot_list <- vector('list', n_ranef)

  for (re in 1:n_ranef) {
    coefs <- init[,re]
    ord   <- order(coefs)

    coefs <- coefs[ord]
    sds   <- init_sd[ord, re]

    # create uis based on multiplier
    ui  <- coefs  + outer(sds, c(-sd_multi, sd_multi))

    out <-
      data.frame(value = coefs,
                 ui) %>%
      mutate(Coefficient = group_names[ord]) %>%
      dplyr::rename(ui_l = X1,
                    ui_u = X2)

    # call internal gg
    if (plot) {
      re_plot_list[[re]] <- plot_coefs_re(out,
                                         ref_line = ref_line)

    } else {
      re_plot_list[[re]] <- out
    }
  }
  names(re_plot_list) <- effect_names
  if (length(re_plot_list) == 1)
    re_plot_list[[1]]
  else re_plot_list
}
