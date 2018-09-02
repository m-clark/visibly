#' The visualization part of gam.check
#'
#' Residual plots etc.
#'
#' @param model The mgcv gam model
#' @param single_page Plot all on a single page. Requires
#' \link[gridExtra]{grid.arrange}
#' @param scatter Whether to plot fitted vs. observed target variable as scatter
#'   plot or density. Default is FALSE (density).
#' @param type The type of residuals wanted. Usually one of "deviance",
#'   "pearson","scaled.pearson", "working", or "response"., See
#'   \link[mgcv]{residuals.gam}.
#' @param kcheck If you want the slightly less verbose basis dimension (k)
#'   checking results. Default is FALSE. When true, only uses the defaults of
#'   gam.check.  If you need to do more, use mgcv.
#'
#' @details Just a single page version of gam.check.
#'
#' @return A ggplot that provides visual inspection of residuals and more.
#' @seealso \link[mgcv]{gam.check}
#' @examples
#' library(mgcv); library(visibly)
#'
#' d <- gamSim(1, n = 400)
#'
#' g_fit <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = d)
#'
#' plot_gam_check(g_fit)
#' plot_gam_check(g_fit, scatter=TRUE)
#'
#' @importFrom stats fitted napredict printCoefmat residuals sd
#' @importFrom utils getFromNamespace
#'
#' @family model visualization
#'
#' @export
plot_gam_check <- function(model,
                           single_page = TRUE,
                           type = "deviance",
                           scatter = FALSE,
                           kcheck = FALSE) {

  if (!inherits(model, 'gam'))
    stop('This function is for gam objects from mgcv')

  resid <- residuals(model, type=type)

  # taken from original gam.check (currently not sure how to check this)
  if (is.matrix(model$linear.predictors) && !is.matrix(resid)) {
    linpred <- napredict(model$na.action, model$linear.predictors[, 1])
  } else {
    linpred <- napredict(model$na.action, model$linear.predictors)
  }

  y_name <- colnames(model$model)[1]
  fits =  predict(model, type = 'response')

  # for ordinal/multinom, get most likely category
  catcheck = grepl(model$family$family, pattern = 'Ord|multi')
  if (catcheck) {
    probs = fits # for scatter
    fits = apply(fits, 1, which.max)
    fits = sort(unique(model$y))[fits]
  }


  fit_dat <- data_frame(
    `fitted values` = fits,
    residuals = resid,
    `linear predictor` = linpred,
  ) %>%
    bind_cols(model$model[, 1, drop=FALSE])

  res_fit_plot <-
    ggplot(aes(x = `linear predictor`, y=residuals), data=fit_dat) +
    geom_hline(yintercept = 0, alpha=.25, color='#ff5500') +
    geom_point(aes(size=abs(residuals)), alpha=.25, show.legend = FALSE) +
    scale_size_continuous(range = c(1, 6), trans = 'exp') +
    ylim(values = c(min(fit_dat$residuals)-sd(fit_dat$residuals),
                    max(fit_dat$residuals)+sd(fit_dat$residuals))) +
    theme_trueMinimal()

  if (scatter) {
    if (!catcheck) {
      fit_plot <-
        ggplot(aes(x = `fitted values`, y=model$y), data=fit_dat) +
        geom_point(aes(), alpha=.25) +
        labs(y = y_name) +
        theme_trueMinimal()
    } else {
      cat_fit_dat = data.frame(y = model$y,
                           probs) %>%
        tidyr::gather(key = y, value = `fitted values`) #%>%
        # mutate(y = as.numeric(factor(y)))
      fit_plot <-
        ggplot(aes(x = `fitted values`, y=y), data=cat_fit_dat) +
        geom_point(aes(), alpha=.25) +
        labs(y = y_name) +
        theme_trueMinimal()
    }
  } else {
    fit_plot <-
    fit_dat %>%
      select(-residuals, -`linear predictor`) %>%
      tidyr::gather(key=var) %>%
      ggplot(aes(x = value, fill=var, color=var)) +
      geom_density(alpha=.25) +
      scale_color_viridis_d(end=.5) +
      scale_fill_viridis_d(end=.5) +
      theme_trueMinimal() +
      theme(
        legend.title = element_blank(),
        legend.key.size = unit(.005, 'npc'),
        legend.text = element_text(margin = margin(l=3))
      )
  }

  res_dens_plot <-
    ggplot(aes(x = residuals), data=fit_dat) +
    geom_density(color='#440154FF',
                 fill='#440154FF',
                 alpha=.25,
                 show.legend = FALSE) +
    theme_trueMinimal()

  qq_plot <-
    ggplot(aes(sample = residuals), data=fit_dat) +
    geom_qq_line(alpha=.25, color='#ff5500') +
    geom_qq(alpha=.1) +
    labs(y='sample', x='theoretical') +
    theme_trueMinimal()

  ps  <- list(qq_plot,
              res_fit_plot,
              res_dens_plot,
              fit_plot)

  # from mgcv
  if (kcheck) {
    k.check <- getFromNamespace("k.check", "mgcv")
    kchck <- k.check(model, subsample = 5000, n.rep = 200)

    if (!is.null(kchck)) {
      message(
        paste0(
          strwrap(
             "Basis dimension (k) checking results. Low p-value
             (k-index < 1) \nmay indicate that k is too low,
             especially if edf is close to k'.\n",
             width=70), collapse='\n'
          )
        )
      printCoefmat(kchck, digits = 3)
    }
  }

   if (single_page && !requireNamespace('gridExtra', quietly=TRUE)) {
     message('Sorry, gridExtra required for single page plot')
     ps
   } else if (!single_page) {
     ps
   } else {
    gridExtra::grid.arrange(grobs=ps, ncol = 2)
   }
}
