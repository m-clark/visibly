context('test color_contrast_checker')

t1 <- color_contrast_checker(foreground = 'red', background = 'papayawhip')
t2 <- color_contrast_checker('#ff5500', '#ffffff')

# note data.frame only with jsonlite
test_that('color_contrast_checker works',{
  expect_is(t1, 'data.frame')
  expect_is(t2, 'data.frame')
})

test_that('color_contrast_checker fails with empty',{
  expect_error(color_contrast_checker())
})

test_that('color_contrast_checker fails with NULL',{
  expect_error(color_contrast_checker(foreground = NULL))
})

test_that('color_contrast_checker fails if not color',{
  expect_error(color_contrast_checker('reddy'))
})

test_that('color_contrast_checker fails if not color',{
  expect_error(color_contrast_checker(foreground = '#fff'))
})

test_that('color_contrast_checker fails if not color',{
  expect_error(color_contrast_checker(foreground = 'fffffff'))
})

test_that('color_contrast_checker fails if not color',{
  expect_error(color_contrast_checker(foreground = 'red', background = '#fff'))
})


test_that('color_contrast_checker fails if not character',{
  expect_error(color_contrast_checker(foreground = 123))
})

test_that('color_contrast_checker fails if not character',{
  expect_error(color_contrast_checker(foreground = 'red', background = 123))
})

test_that('color_contrast_checker fails if not character',{
  expect_error(color_contrast_checker(foreground = '#fff'))
})

test_that('color_contrast_checker fails if not appropriate hex',{
  expect_error(color_contrast_checker('#fff'))
})
