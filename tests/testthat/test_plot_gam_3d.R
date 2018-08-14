context('Test plot_gam_3d')

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



# Tests -------------------------------------------------------------------

test_that('plot_gam_3d returns a plotly object',{
  expect_s3_class(plot_gam_3d(b, main_var = x, second_var = z), 'plotly')
})

test_that('plot_gam_3d fails if not gam object',{
  expect_error(plot_gam_3d(lm(y ~ x*z, d), main_var = x, second_var=z))
})

test_that('plot_gam_3d fails if no main_var',{
  expect_error(plot_gam_3d(b))
})
