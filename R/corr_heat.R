#' Visualize a correlation matrix
#'
#' @param cormat A correlation matrix
#' @param n_factors The number of factors for the factor analysis. Default is NULL.
#' @param psychOptions a list of arguments to pass to \code{psych::fa}
#' @param ordering Order of the result. See details
#' @param three_d Return a surface plot.
#' @param diagonal Value for the diagonal. Must be 1 (default) or NA.
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
#'   \code{corrheat} produces a color coded matrix in which Blue represents
#'   positive, and Red, negative correlations, and fades to white the smaller
#'   the values are. It is interactive, such that one can hover over the image
#'   to obtain the correlation value.
#'
#'   The ordering is based on the results of a factor analysis
#'   from the \link[psych]{psych} package (which is required). It can be based
#'   on \code{\link[psych]{fa.sort}} (default), max raw ("raw") loading,
#'   absolute ("absolute") values of the loadings, or polar coordinates (see
#'   \link[psych]{polar} ) across all factors.
#'
#'   By default the number of factors is chosen to more likely 'just work' for
#'   visualization purposes.  If the number of columns is three or fewer, it
#'   will force only one factor, otherwise it will be
#'   \code{floor(sqrt(ncol(x)))}).  While you may supply the number of factors,
#'   as well as other options to the \link[psych]{fa} function via
#'   \code{psychOptions}, if you want explore a factor analysis you should you
#'   probably should do that separately.
#'
#' @return A plotly object
#'
#' @examples
#' library(visibly)
#'
#' corr_heat(cormat = cor(mtcars), ordering = 'polar')
#'
#' @export
corr_heat <- function(cormat,
                     n_factors = NULL,
                     psychOptions = NULL,
                     ordering = c('fa', 'polar', 'raw', 'first', 'absolute'),
                     three_d = FALSE,
                     diagonal = 1,
                     dir = -1,
                     pal = 'vik') {

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(strwrap("psych package and its dependency, the GPArotation package,
                  are needed for the factor analysis.
                 Please install."),
         call. = FALSE)
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required to make the visualization.
         Please install.",
         call. = FALSE)
  }
  if (!requireNamespace("scico", quietly = TRUE)) {
    stop("scico package is needed for the color.
         Please install it.",
         call. = FALSE)
  }


  # Check that x is a square, symmetric matrix ------------------------------

  if (!is.matrix(cormat))
    stop("x does not appear to be something like a matrix.")

  nr <- dim(cormat)[1]
  nc <- dim(cormat)[2]

  if(nr != nc) stop("x must be a square matrix")

  if(!isSymmetric(cormat)) stop('x must be a symmetric matrix')

  # Other checks ------------------------------------------------------------

  if (!rlang::is_null(n_factors) &&
      (as.integer(n_factors) != n_factors | !is.numeric(n_factors)))
    stop('n_factors must be integer/numeric.')


  # Factor analysis ---------------------------------------------------------

  # number of factors to use
  if (!rlang::is_null(n_factors) && n_factors >= 4) {
    nf = n_factors
  } else if (nc < 4) {  # necessary?
    nf = 1
  } else {
    nf = floor(sqrt(nc))
  }

  if (is.null(psychOptions)) {
    message('No FA options specified, using psych package defaults')
    args = list(r=cormat, nfactors=nf)
  } else {
    args = append(list(r=cormat, nfactors=nf), psychOptions)
  }

  # to get rid of GPArotation warning
  faResult = do.call(suppressWarnings({psych::fa}), args)

  # extract loadings, order by if desired
  load = faResult$loadings
  class(load) = 'matrix'

  mat_order = ordering[1]
  if (mat_order == 'absolute') {
    cluster <- apply(abs(load), 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (mat_order == 'raw') {
    cluster <- apply(load, 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (mat_order == 'first'){
    load = data.frame(var=rownames(load), load)
    index = order(load[,1], decreasing=T)
  } else {
    sortload = psych::fa.sort(load, polar = (mat_order=='polar'))
    index = rownames(sortload)
  }

  ## reorder x
  ##=======================
  cormat <- cormat[index, index]


  # Plot --------------------------------------------------------------------

  diag(cormat) = diagonal

  # plotly will drop names because, who needs those anyway? it will also reverse
  # the axes just for fun!
  xlo = list(
    tickmode = "array",
    tickvals = 1:nrow(cormat)-1,
    ticktext = rownames(cormat),
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = ''
  )
  ylo = list(
    tickmode = "array",
    tickvals = 1:nrow(cormat)-1,
    ticktext = rownames(cormat),
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = '',
    autorange = 'reversed'
  )

  zlo = list(
    # tickmode = "array",
    # tickvals = 1:nrow(cormat)-1,
    ticktext = '',
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = ''
  )

  cors = cormat[lower.tri(cormat)]

  pal = scico::scico(500, direction=dir, palette = pal)

  if (three_d) {

    p = plotly::plot_ly(z=~cormat, name='') %>%
      plotly::add_surface(colors = pal) %>%
      plotly::colorbar(limits = c(-1, 1)) %>%
      plotly::layout(scene = list(xaxis=xlo,  # scene!
                                  yaxis=ylo,
                                  zaxis=zlo)
      ) %>%
      theme_plotly()

    # plotly gives a warning it can't do limits, then does them; this appears
    # the only way to supprsess
    suppressWarnings({print(p)})

  } else {
    plotly::plot_ly(z=~cormat) %>%
      plotly::add_heatmap(colors = pal) %>%
      plotly::colorbar(limits = c(-1, 1)) %>%
      plotly::layout(xaxis=xlo,
                     yaxis=ylo) %>%
      theme_plotly()
  }



}
