context('test plot_gam_2d')


# initial prep ------------------------------------------------------------

# example taken from the mgcv plot.gam help file.
library(mgcv); library(dplyr)
set.seed(0)
d <- gamSim(2, scale=.1)$data
d$misc <- rnorm(nrow(d), mean = 50, sd = 10)
b <- gam(y ~ s(x, z), data = d)


d2 <- gamSim(4)
d2$fac_num <-  as.numeric(d2$fac)
by_mod1 <- gam(y ~ s(x2, by=fac), data = d2)
by_mod2 <- gam(y ~ s(x2, by=fac_num), data = d2)
by_mod3 <- gam(y ~ s(x2, fac, bs='fs'), data = d2)
by_mod4 <- gam(y ~ s(x2, fac, bs='re'), data = d2, method = 'REML')
# by_mod5 <- gam(y ~ s(x2, fac, bs=c('tp','re')), data = d2)



# plot_gam_2d -------------------------------------------------------------


test_that('plot_gam_2d returns a ggplot',{
  expect_s3_class(plot_gam_2d(b, main_var = x, second_var = z), 'ggplot')
})

test_that('plot_gam_2d fails if not gam object',{
  expect_error(plot_gam_2d(lm(y ~ x*z, d), main_var = x, second_var=z))
})

test_that('plot_gam_2d fails if no main_var',{
  expect_error(plot_gam_2d(b))
})

test_that('plot_gam_2d fails if no second/by_var',{
  expect_error(plot_gam_2d(b, main_var = x))
  expect_error(plot_gam_by(by_mod1, main_var = x2))
})

test_that('plot_gam_2d will switch to by',{
  expect_message(plot_gam_2d(by_mod2, main_var = x2, second_var = fac_num))
})

test_that('plot_gam_2d takes viridis args',{
  expect_s3_class(plot_gam_2d(b, main_var = x, second_var = z, option='C'),
                  'ggplot')
})



# plot_gam_by -------------------------------------------------------------



test_that('plot_gam_by returns a ggplot',{
  expect_s3_class(plot_gam_by(by_mod1, main_var = x2, by_var = fac), 'ggplot')
})

test_that('plot_gam_by fails if not gam object',{
  expect_error(plot_gam_by(lm(y ~ x*z, d), main_var = x, by_var = z))
})

test_that('plot_gam_by fails if no main_var',{
  expect_error(plot_gam_by(b))
})

test_that('plot_gam_by will use numeric',{
  expect_s3_class(plot_gam_by(by_mod2, main_var = x2, by_var = fac_num),
                  'ggplot')
})

test_that('plot_gam_by works with factor smooth',{
  expect_s3_class(plot_gam_by(by_mod3, main_var = x2, by_var = fac), 'ggplot')
})

test_that('plot_gam_by works with random effect',{
  expect_s3_class(plot_gam_by(by_mod4, main_var = x2, by_var = fac), 'ggplot')
})



test_that('plot_gam will handle different scales',{
  b2 <- gam(y ~ s(x, misc), data = d)
  expect_s3_class(
    plot_gam_2d(b2,
                main_var = x,
                second_var = misc),
    'ggplot')
})
