context('test plot_gam_check')

# example taken from the mgcv plot.gam help file.
library(mgcv); library(dplyr)
set.seed(0)
## fake some data...
f1 <- function(x) {
  exp(2 * x)
}
f2 <- function(x) {
  0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
}
f3 <- function(x) {
  x * 0
}

n <- 200
sig2 <- 4

d <- data.frame(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = runif(n, 0, 1),
  x3 = runif(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2))
) %>%
  mutate(
    y  = 2 * x0 + f1(x1) + f2(x2) + f3(x3) + e,
    x0 = factor(x0)
    )

d2 <- data.frame(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = rnorm(n, 0, 5),
  x3 = rnorm(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2))
) %>%
  mutate(
    y  = 2*x0 + f1(x1) + sin(x2) + cos(x3) + e,
    x0 = factor(x0)
  )

b <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = d)




test_that('plot_gam_check returns a ggplot',{
  expect_s3_class(plot_gam_check(b), 'grob')
})

test_that('plot_gam_check fails if not gam object',{
  expect_error(plot_gam_check(lm(y ~ x1, d)))
})

# note that char will use multi1d
test_that('plot_gam_check can do multi page',{
  expect_is(plot_gam_check(b, single_page = FALSE), 'list')
})

test_that('plot_gam_check can do different residuals',{
  expect_s3_class(plot_gam_check(b, type = 'working'), 'grob')
})

test_that('plot_gam_check can do scatterfit',{
  expect_s3_class(plot_gam_check(b, scatter = TRUE), 'grob')
})

test_that('plot_gam_check can do kcheck',{
  expect_message(plot_gam_check(b, kcheck = TRUE))
})

test_that('plot_gam_check can do other families',{
  # Negbin

  library(mgcv)
  set.seed(3)
  n <- 400
  dat <- gamSim(1, n = n)
  g <- exp(dat$f / 5)

  ## negative binomial data...
  dat$y <- rnbinom(g, size = 3, mu = g)

  ## known theta fit ...
  negbin_model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                      family = negbin(3),
                      data = dat)

  expect_s3_class(plot_gam_check(negbin_model), 'grob')


  # Ordinal
  set.seed(3); n<-400
  dat <- gamSim(1, n = n)
  dat$f <- dat$f - mean(dat$f)

  alpha <- c(-Inf, -1, 0, 5, Inf)
  R <- length(alpha) - 1
  y <- dat$f
  u <- runif(n)
  u <- dat$f + log(u / (1 - u))
  for (i in 1:R) {
    y[u > alpha[i] & u <= alpha[i + 1]] <- i
  }
  dat$y <- y

  ord_model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                   family = ocat(R = R),
                   data = dat)

  expect_s3_class(plot_gam_check(ord_model), 'grob')
  expect_s3_class(plot_gam_check(ord_model, scatter = TRUE), 'grob')


  # Multinomial
  n <- 1000
  f1 <- function(x)
    sin(3 * pi * x) * exp(-x)
  f2 <- function(x)
    x ^ 3
  f3 <- function(x)
    .5 * exp(-x ^ 2) - .2
  f4 <- function(x)
    1
  x1 <- runif(n)
  x2 <- runif(n)
  eta1 <- 2 * (f1(x1) + f2(x2)) - .5
  eta2 <- 2 * (f3(x1) + f4(x2)) - 1
  p <- exp(cbind(0, eta1, eta2))
  p <- p / rowSums(p) ## prob. of each category
  cp <- t(apply(p, 1, cumsum)) ## cumulative prob.

  ## simulate multinomial response with these probabilities
  y <- apply(cp, 1, function(x) min(which(x > runif(1)))) - 1
  # gam won't take factors

  multinom_model <- gam(list(y ~ s(x1) + s(x2),
                               ~ s(x1) + s(x2)),
                        data = data.frame(y = y,
                                          x1 = x1,
                                          x2 = x2),
                        family = multinom(K = 2))
  expect_s3_class(plot_gam_check(multinom_model), 'grob')
  expect_s3_class(plot_gam_check(multinom_model, scatter = TRUE), 'grob')
})
