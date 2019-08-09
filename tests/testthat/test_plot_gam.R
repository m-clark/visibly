context('test plot_gam')

library(mgcv) # you don't need this function if you don't have this package


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

d <- tibble(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = runif(n, 0, 1),
  x3 = runif(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2)),
  y  = 2 * x0 + f1(x1) + f2(x2) + f3(x3) + e
) %>%
  mutate(x0 = factor(x0))

d2 <- tibble(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = rnorm(n, 0, 5),
  x3 = rnorm(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2)),
  y  = 2*x0 + f1(x1) + sin(x2) + cos(x3) + e
) %>%
  mutate(x0 = factor(x0))

b <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = d)






test_that('plot_gam returns a ggplot',{
  expect_s3_class(plot_gam(b, main_var = x2), 'ggplot')
})


test_that('plot_gam fails if not gam object',{
  expect_error(plot_gam(lm(y ~ x1, d), main_var = x1))
})


# note that char will use multi1d
test_that('plot_gam can handle character',{
  expect_s3_class(plot_gam(b, main_var = 'x2'), 'ggplot')
})

test_that('plot_gam can handle multi character',{
  expect_s3_class(plot_gam(b, main_var = c('x1', 'x2')), 'ggplot')
})


test_that('plot_gam can take conditional data',{
  expect_s3_class(
    plot_gam(b,
             conditional_data = tibble(x2 = runif(500)),
             main_var = x2),
    'ggplot')
})

test_that('plot_gam can do multiple 1d smooths',{
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1)),
    'ggplot')
})

test_that('plot_gam can handle incomplete conditional data',{
  expect_s3_class(
    plot_gam(b,
             conditional_data = tibble(#x1 = runif(500),
                                           x2 = runif(500)),
             main_var = vars(x2, x1)),
    'ggplot')
})




test_that('plot_gam can do plot options',{
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1, x3),
             line_color = palettes$stan_red$stan_red,
             ribbon_color = '#00B295',
             ncol = 1),
    'ggplot')
})

test_that('plot_gam will do no smooth',{
  b <- gam(y ~ x0 + x1 + s(x2, bs = 'gp') + s(x3, bs = 'ps'), data = d)
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1, x3),
             ncol = 1),
    'ggplot')
})

test_that('plot_gam will handle different scales',{
  b <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = d2)
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1, x3),
             ncol = 1),
    'ggplot')
})

test_that('plot_gam can take no main_var',{
  b <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = d2)
  expect_s3_class(plot_gam(b, ncol = 1),'ggplot')
})



# for later
test_that('plot_gam can do different smooths',{
  b <- gam(y ~ x0 + s(x1, bs = 'cr') + s(x2, bs = 'gp') +
             s(x3, bs = 'ps'), data = d)
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1, x3),
             ncol = 1),
    'ggplot')

  b <- gam(y ~ s(x0, bs='re') + s(x1, bs = 'ds') + s(x2, bs = 'cc') +
             s(x3, bs = 'ps'), data = d)
  expect_s3_class(
    plot_gam(b,
             main_var = vars(x2, x1, x3, x0),
             ncol = 1),
    'ggplot')
})

test_that('plot_gam will message with categorical',{
  b <- gam(y ~ s(x0, bs='re') + s(x1, bs = 'cr') + s(x2, bs = 'gp'), data = d)
  expect_message(
    plot_gam(b,
             main_var = x0)
    )

  expect_message(
    plot_gam(b,
             main_var = vars(x2, x1, x0))
    )
})


test_that('plot_gam can do different dist',{
  d <- gamSim(1, n = 400, dist = "binary", scale = .33)

  binom_fit <- gam(
    y ~ s(x0) + s(x1) + s(x2) + s(x3),
    family = binomial,
    data = d,
    method = "REML"
  )
  expect_s3_class(
    plot_gam(binom_fit,
             main_var = vars(x2, x1, x3, x0),
             ncol = 1),
    'ggplot')


  # test that it will return correctly scaled results
  p = plot_gam(binom_fit,
               main_var = vars(x2, x1, x3, x0),
               ncol = 1)
  expect_gt(min(dplyr::select(p$data, fit, ul, ll)), 0)
  expect_lt(max(dplyr::select(p$data, fit, ul, ll)), 1)

  # test poisson
  d <- gamSim(1, n = 400, dist = "poisson", scale = .2)

  pois_fit <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
                  family = poisson,
                  data = d)
  expect_s3_class(
    plot_gam(pois_fit,
             main_var = vars(x2, x1, x3, x0),
             ncol = 1),
    'ggplot')

})




# covr::file_coverage(source_files = 'R/plot_gam.R', test_files = 'tests/testthat/test_plot_gam.R')
# covr::zero_coverage(covr::file_coverage(source_files = 'R/plot_gam.R', test_files = 'tests/testthat/test_plot_gam.R'))
