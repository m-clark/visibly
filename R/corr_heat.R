#' Visualize a correlation matrix
#'
#' @param cormat A correlation matrix
#' @param n_factors The number of factors for the factor analysis. Default is
#'   NULL.
#' @param psych_opts a list of arguments to pass to \code{psych::fa}.
#' @param ordering Order of the result. See details
#' @param three_d Return a surface plot.
#' @param diagonal Value for the diagonal. Must be 1 (default), 0, or NA.
#' @param dir Passed to \link[scico]{scico}.
#' @param pal Passed to \link[scico]{scico}.
#' @param plot_only Don't run the factor analysis and just return the heatmap.
#'   Default is FALSE.
#'
#' @details
#'   Correlation matrices are typically better visualized rather than parsed
#'   numerically, and while one can do so with various packages at this point,
#'   they either don't reorder the data, don't show the actual values, or only
#'   order based on cluster analysis, and one often may not want the cluster
#'   based approach to ordering if dealing with a correlation matrix, which may
#'   be too small column-wise to be useful for a cluster analysis, or may be a
#'   specific type of data more amenable to a measurement error approach (e.g.
#'   items from a particular scale).
#'
#'   The default \code{corrheat} visualization produces a color-coded heatmap in
#'   which Blue represents positive, and Red, negative correlations (the 'vik'
#'   palette), and fades to white the smaller the values are. It is interactive,
#'   such that one can hover over the image to obtain the correlation value.
#'
#'   The ordering is based on the results of a factor analysis
#'   from the \link[psych]{psych} package (which is required). It can be based
#'   on \code{\link[psych]{fa.sort}} (default), max raw ("raw") loading,
#'   absolute ("absolute") values of the loadings, loadings of the first factor
#'   only ('first'), or polar coordinates (see \link[psych]{polar} ) across all
#'   factors.
#'
#'   By default, the number of factors is chosen to more likely 'just work' for
#'   visualization purposes.  If the number of columns is three or fewer, it
#'   will force only one factor, otherwise it will be
#'   \code{floor(sqrt(ncol(x)))}).  While you may supply the number of factors,
#'   as well as other options to the \link[psych]{fa} function via
#'   \code{psych_opts}, if you want to explore a factor analysis you should you
#'   probably should do that separately. In addition, factor analysis can't fix
#'   a poorly conditioned correlation matrix, and errors in the analysis may
#'   result in the plot failing.
#'
#'   There are two options regarding the palette, the name of the
#'   \link[scico]{scico} palette and the direction, in case you want to flip the
#'   color scheme.  Typically, you'll want to use a diverging palette such as vik
#'   or cork.  If you have all positive (or rarely, negative), you might
#'   consider the sequential ones like bilbao or oslo. The lower or upper limit
#'   of the color bar will change accordingly.
#'
#'   I'm normally against 3-d images as they really don't apply in many cases
#'   where they are used, but I think it can actually assist in seeing factor
#'   structure here.
#'
#' @note This function essentially assumes a \emph{complete} correlation matrix like that
#'   returned from \link[stats]{cor}, and one which should have row/column
#'   names. With \code{plot_only = TRUE}, you may potentially supply any matrix,
#'   as the factor analysis will not be done.
#'
#' @return A plotly object
#'
#' @seealso \link[scico]{scico}, \link[plotly]{plotly}, \link[psych]{fa}
#'
#' @examples
#' \dontrun{
#' # not run due to package dependencies noted above. make sure to install them.
#' library(visibly)
#' data('bfi')
#' corr_heat(cor(bfi, use='pair'), diagonal = NA, pal = 'broc')
#' corr_heat(cor(bfi, use='pair'), diagonal = NA, pal = 'broc', dir=1)
#' corr_heat(psych::Harman23.cor$cov, n_factors = 2, pal = 'oslo')
#' corr_heat(cormat = cor(mtcars))
#' corr_heat(cormat = cor(mtcars), ordering = 'polar', three_d=TRUE)
#' }
#'
#' @export
corr_heat <- function(cormat,
                      n_factors = NULL,
                      psych_opts = NULL,
                      ordering = c('fa', 'raw', 'absolute', 'first', 'polar'),
                      three_d = FALSE,
                      diagonal = 1,
                      dir = -1,
                      pal = 'vik',
                      plot_only = FALSE) {

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(strwrap("psych package and its dependency, the GPArotation package,
                  are needed for the factor analysis.
                 Please install."),
         call. = FALSE)
  }

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required to make the visualization.
         Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("scico", quietly = TRUE)) {
    stop("scico package is needed for the color.
         Please install it.",
         call. = FALSE)
  }


  # Check that x is a square, symmetric matrix ------------------------------

  if (!is.matrix(cormat))
    stop("cormat does not appear to be a matrix.")

  nr <- dim(cormat)[1]
  nc <- dim(cormat)[2]

  if (!plot_only && nr != nc) stop("cormat must be a square matrix")

  if (!plot_only && !isSymmetric(cormat))
    stop('cormat must be a symmetric matrix')

  # Other checks ------------------------------------------------------------

  if (!rlang::is_null(n_factors) &&
      (as.integer(n_factors) != n_factors | !is.numeric(n_factors)))
    stop('n_factors must be integer/numeric.')

  if (!dir %in% c(-1, 1)) stop('dir argument must be -1, 0, or 1.')

  if (!diagonal %in% c(1, 0, NA)) stop('diagonal argument must be 1 or NA.')

  if (!pal %in% scico::scico_palette_names())
    stop(strwrap('pal must be one of the scico palettes.
                 See scico::scico_palette_names().'))



  # Factor analysis ---------------------------------------------------------

  if (!plot_only) {
    if (rlang::is_null(dimnames(cormat)))
      stop('Need a correlation matrix with row and column names')

    # number of factors to use
    if (!rlang::is_null(n_factors) && nc < 4) {  # necessary?
      n_factors <- 1
    } else {
      n_factors <- floor(sqrt(nc))
    }

    if (rlang::is_null(psych_opts)) {
      message('No FA options specified, using psych package defaults.')
      args <- list(r = cormat, nfactors = n_factors)
    } else {
      args <- append(list(r = cormat, nfactors = n_factors), psych_opts)
    }

    # suppress to get rid of GPArotation message

    fa_result <- suppressMessages({do.call(psych::fa, args)})

    # Order results -----------------------------------------------------------

    load <- fa_result$loadings
    class(load) <- 'matrix'
    rn = rownames(load)

    mat_order <- ordering[1]

    if (mat_order == 'absolute') {
      cluster <- apply(abs(load), 1, which.max)
      ord <- sort(cluster, index.return = TRUE)
      index <- ord$ix
    } else if (mat_order == 'raw') {
      cluster <- apply(load, 1, which.max)
      ord <- sort(cluster, index.return = TRUE)
      index <- ord$ix
    } else if (mat_order == 'first') {
      index <- rownames(load)[order(load[, 1], decreasing = TRUE)]
    } else {
      sorted_load <- psych::fa.sort(load, polar = (mat_order == 'polar'))
      index <- rownames(sorted_load)
    }

    # reorder cormat
    cormat <- cormat[index, index]

  }

  # Plot Setup --------------------------------------------------------------

  diag(cormat) <- diagonal
  cors <- cormat[lower.tri(cormat)]
  pal <- scico::scico(500, direction = dir, palette = pal)

  # limits for colorbar
  ll <- ifelse(all(cors > 0), 0, -1)
  ul <- ifelse(all(cors < 0), 0,  1)

  # plotly will drop names because, who needs those anyway? it will also reverse
  # the axes just for fun!
  xlo <- list(
    tickmode = "array",
    tickvals = 1:nrow(cormat) - 1,
    ticktext = rownames(cormat),
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = ''
  )

  ylo <- list(
    tickmode = "array",
    tickvals = 1:nrow(cormat) - 1,
    ticktext = rownames(cormat),
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = '',
    autorange = 'reversed'
  )

  zlo <- list(
    ticktext = '',
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = ''
  )


  # Plot --------------------------------------------------------------------

  if (three_d) {
    p <-
      plotly::plot_ly(z = ~cormat, name = "") %>%
      plotly::add_surface(colors = pal) %>%
      plotly::colorbar(limits = c(ll, ul)) %>%
      plotly::layout(scene = list(      # scene!
        xaxis = xlo,
        yaxis = ylo,
        zaxis = zlo
      )) %>%
      theme_plotly()

    # plotly gives a warning it can't do limits, then does them; this appears
    # the only way to suppress, but then won't work for Rmd html output; see
    # issue #1202
    suppressWarnings({ print(p) })

  } else {
    plotly::plot_ly(z = ~cormat) %>%
      plotly::add_heatmap(colors = pal) %>%
      plotly::colorbar(limits = c(ll, ul)) %>%
      plotly::layout(
        xaxis = xlo,
        yaxis = ylo
      ) %>%
      theme_plotly()
  }

}

