context('test visualization themes')

require(ggplot2)  # because travis

test_that('Error returned for non-plotly class for theme_plotly', {
  expect_error(data.frame(x=1:3, y=4:6) %>% theme_plotly())
})
test_that('Error returned for non-plotly class for theme_blank', {
  expect_error(data.frame(x=1:3, y=4:6) %>% theme_blank())
})

test_that('Check if plotly object returned', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_plotly(), 'plotly')
})

test_that('Check if plotly object returned', {
  expect_s3_class(plotly::plot_ly(x = ~rnorm(100)) %>% theme_blank(),
                  'plotly')
})


test_that('Check if ggplot object returned', {
  expect_s3_class(ggplot2::qplot(x = rnorm(100)) + theme_trueMinimal(),
                  'ggplot')
})

test_that('Center axis', {
  g <- ggplot2::qplot(x = rnorm(100)) + theme_trueMinimal(center_axis_labels = T)
  expect_equal(g$theme$axis.title.x$hjust, .5)
})

