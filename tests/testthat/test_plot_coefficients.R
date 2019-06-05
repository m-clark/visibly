context('test coefficient plots')


test_lm <- lm(mpg ~ ., mtcars)

x <- rnorm(100)
q <- rnorm(100)
z <- rnorm(100)
y <- rpois(100, exp(.25*x + .1*q - .5*z))

test_glm <- glm(y ~ x + q + z, family = poisson)
fit_mer  <- lme4::lmer(Reaction ~ Days + (Days|Subject), lme4::sleepstudy)
fit_mer2 <- lme4::lmer(count ~ zAge + zBase * Trt + (1 | patient),
                      brms::epilepsy)
mer_re <- plot_coefficients(fit_mer, ranef = T, which_ranef = 'Subject')

test_that('It works', {
  expect_s3_class(mer_re[[1]], 'ggplot')
})

test_that('It works', {
  expect_s3_class(plot_coefficients(test_lm, plot = FALSE), 'data.frame')
})



# test lm glm coefficients ------------------------------------------------

test_that('test plot_coefficients.lm', {
  expect_s3_class(plot_coefficients(test_lm), 'ggplot')
})

test_that('test palette', {
  expect_s3_class(plot_coefficients(test_lm, palette = 'oslo'), 'ggplot')
})

test_that('test order numeric', {
  expect_s3_class(plot_coefficients(test_lm, order = sample(1:10)), 'ggplot')
})

test_that('test order increasing', {
  expect_s3_class(plot_coefficients(test_lm, order = 'increasing'), 'ggplot')
})


test_that('test plot_coefficients.glm and trans', {
  expect_s3_class(plot_coefficients(test_glm, trans = exp, ref_line = 1),
                  'ggplot')
})





# test lme4 ---------------------------------------------------------------

# test fixed effects plots ------------------------------------------------
test_that('test fixef options', {
  test_fe <- plot_coefficients(fit_mer, ranef = F)
  expect_s3_class(test_fe, 'ggplot')
})

test_that('test fixef options order increasing', {
  test_fe <- plot_coefficients(fit_mer2, order = 'increasing', plot=F)
  expect_equal(test_fe$Coefficient[1], 'Trt1')
})

test_that('test fixef options order numeric', {
  test_fe <- plot_coefficients(fit_mer2, order = 4:1, plot=F)
  expect_equal(test_fe$Coefficient[2], 'Trt1')
})

test_that('no plot works', {
  expect_s3_class(plot_coefficients(fit_mer, plot = FALSE), 'data.frame')
})


# test random effect plots ------------------------------------------------

test_that('test ranef options', {
  expect_s3_class(mer_re[[1]], 'ggplot')
})

test_that('test merMod ranef errors with wrong ranef', {
  expect_error(plot_coefficients(fit_mer, ranef = T, which_ranef = 'blah'))
})

test_that('test merMod ranef errors with null ranef', {
  expect_error(plot_coefficients(fit_mer, ranef = T, which_ranef = NULL))
})

test_that('test merMod ranef can return data', {
  expect_s3_class(plot_coefficients(fit_mer, ranef = T, which_ranef = 'Subject',
                                    plot=F)[[1]], 'data.frame')
})


test_that('test merMod ranef can take single ranef and
          returns ggplot with 1 ranef', {
            expect_s3_class(plot_coefficients(fit_mer2,
                                              ranef = T,
                                              which_ranef = 'patient'),
                            'ggplot')
          })



# test brms ---------------------------------------------------------------


# test fixed effects plots ------------------------------------------------

test_that('test fixef options', {
  test_fe <- plot_coefficients(fit1, ranef = F)
  expect_s3_class(test_fe, 'ggplot')
})

test_that('test fixef options order increasing', {
  test_fe <- plot_coefficients(fit1, order = 'increasing', plot=F)
  expect_equal(test_fe$Coefficient[1], 'Trt1')
})

test_that('test fixef options order numeric', {
  test_fe <- plot_coefficients(fit1, order = 4:1, plot=F)
  expect_equal(test_fe$Coefficient[2], 'Trt1')
})

test_that('no plot works', {
  expect_s3_class(plot_coefficients(fit1, plot = FALSE), 'data.frame')
})


# test random effect plots ------------------------------------------------


test_that('test ranef options', {
  expect_s3_class(
    plot_coefficients(fit1, ranef = T, which_ranef='patient'), 'ggplot')
})

test_that('test error with wrong ranef', {
  expect_error(plot_coefficients(fit1, ranef = T, which_ranef = 'blah'))
})

test_that('test error with null ranef', {
  expect_error(plot_coefficients(fit1, ranef = T, which_ranef = NULL))
})

test_that('test can return data', {
  expect_s3_class(
    plot_coefficients(fit1, ranef = T, which_ranef = 'patient', plot=F),
    'data.frame')
})

test_that('test merMod ranef returns ggplot with 1 ranef', {

            expect_s3_class(plot_coefficients(fit1,
                                              ranef = T,
                                              which_ranef = 'patient'),
                            'ggplot')
          })

test_that('test merMod ranef returns list with multi ranef', {
            test <- plot_coefficients(fit_brms_slope,
                                     ranef = T,
                                     which_ranef = 'Subject')
            expect_is(test, 'list')
          })
