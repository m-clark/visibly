context('test create_prediction_data')


test_that('create_prediction_data returns a data frame', {
  expect_is(create_prediction_data(iris), 'data.frame')
})

test_that('create_prediction_data takes an alternate num function and args', {
  expect_is(create_prediction_data(iris, num=quantile, prob=.25), 'data.frame')
})


test_that('create_prediction_data takes an alternate cat option', {
  iris2 <- iris %>% mutate(Species = relevel(Species, 'virginica'))
  expect_equal(create_prediction_data(iris2, cat = 'ref')[['Species']],
               factor('virginica'))
})

test_that('create_prediction_data takes additional data', {
  pd <- create_prediction_data(iris, num_cen = median,
                              expand.grid(
                                Sepal.Width=c(0,3,5),
                                Species = c('setosa', 'virginica'))
                              )
  expect_equal(nrow(pd), 6)
})

test_that('create_prediction_data handles logical', {
  iris2 <- iris %>% mutate(Setosa = Species=='setosa')
  expect_is(create_prediction_data(iris2), 'data.frame')
})

test_that('create_prediction_data takes an alternate cat option for non-factor',
          {
            iris2 <- iris %>% mutate(Setosa = Species=='setosa')
            expect_is(create_prediction_data(iris2, cat = 'ref'), 'data.frame')
          })

test_that('create_prediction_data handles date', {
  mtcars2 <- mtcars %>%
    mutate(some_date = rep(as.Date(c('1977-07-11', '1962-11-29')),
                           nrow(mtcars)/2))
  expect_is(create_prediction_data(mtcars2)$some_date, 'Date')
})

test_that('create_prediction_data handles date and ref', {
  mtcars2 <- mtcars %>%
    mutate(some_date = rep(as.Date(c('1977-07-11', '1962-11-29')),
                           nrow(mtcars)/2))
  expect_is(create_prediction_data(mtcars2, cat = 'ref')$some_date, 'Date')
})

test_that('create_prediction_data handles NA', {
  iris[3,2] <- NA
  expect_true(is.na(create_prediction_data(iris)[,2]))
})

test_that('create_prediction_data handles NA', {
  iris[3,2] <- NA
  expect_false(is.na(create_prediction_data(iris, na.rm=TRUE)[,2]))
})
