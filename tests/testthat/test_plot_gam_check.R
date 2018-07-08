context('test plot_gam_check')

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

d <- data_frame(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = runif(n, 0, 1),
  x3 = runif(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2)),
  y  = 2 * x0 + f1(x1) + f2(x2) + f3(x3) + e
) %>%
  mutate(x0 = factor(x0))

d2 <- data_frame(
  x0 = rep(1:4, 50),
  x1 = runif(n, 0, 1),
  x2 = rnorm(n, 0, 5),
  x3 = rnorm(n, 0, 1),
  e  = rnorm(n, 0, sqrt(sig2)),
  y  = 2*x0 + f1(x1) + sin(x2) + cos(x3) + e
) %>%
  mutate(x0 = factor(x0))

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
