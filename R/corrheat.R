#' Title
#'
#' @param cormat
#' @param n_factors
#' @param psychOptions
#' @param ordering
#' @param three_d
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
corrheat <- function(cormat,
                     n_factors = NULL,
                     psychOptions = NULL,
                     ordering = c('fa','raw','first', 'absolute'),
                     three_d = FALSE,
                     dir = -1) {

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("psych package and its dependency, the GPArotation package, are needed for the factor analysis.
         Please install.",
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
      as.integer(n_factors) != n_factors | !is.numeric(n_factors))
    stop('n_factors must be integer/numeric.')


  # Factor analysis ---------------------------------------------------------

  # number of factors to use
  if (!rlang::is_null(n_factors)) {
    nf = n_factors
  } else if (nc <= 4) {  # necessary?
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

  suppressWarnings({
    faResult = do.call(psych::fa, args)
  })

  # extract loadings, order by if desired
  load = faResult$loadings
  class(load) = 'matrix'

  if (ordering[1] == 'absolute') {
    cluster <- apply(abs(load), 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (ordering[1] == 'raw') {
    cluster <- apply(load, 1, which.max)
    ord <- sort(cluster, index.return = TRUE)
    index = ord$ix
  } else if (ordering[1] == 'first'){
    load = data.frame(var=rownames(load), load)
    index = order(load[,1], decreasing=T)
  } else {
    sortload = psych::fa.sort(load)
    index = rownames(sortload)
  }

  ## reorder x
  ##=======================
  cormat <- cormat[index, index]


  # Plot --------------------------------------------------------------------

  diag(cormat) = NA

  # plotly will drop names because, who needs those anyway?
  lo = list(
    tickmode = "array",
    tickvals = 1:nrow(cormat)-1,
    ticktext = rownames(cormat),
    gridcolor = 'transparent',
    zerolinecolor = 'transparent',
    title = ''
  )

  z = lo
  z$ticktext = ''    # sigh

  cors = cormat[lower.tri(cormat)]
  cors = dir*cors/2 + .5

  pal = scico::scico(500,
                     begin = min(cors),
                     end = max(cors),
                     direction=dir,
                     palette = 'vik')

  if (three_d) {
    plotly::plot_ly(z=~cormat, name='') %>%
      plotly::add_surface(colors = pal) %>%
      plotly::layout(scene = list(xaxis=lo,  # scene!
                                  yaxis=lo,
                                  zaxis=z)
      ) %>%
      theme_plotly()
  } else {
    plotly::plot_ly(z=~cormat) %>%
      plotly::add_heatmap(colors = pal) %>%
      plotly::layout(xaxis=lo,
                     yaxis=lo) %>%
      theme_plotly()
  }



}
