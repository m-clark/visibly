context('test col2hex')


test_that('col2hex works',{
  expect_is(col2hex('blue'), 'character')
  expect_is(col2hex('#ff5500', alpha = .1), 'character')
})

test_that('col2lab returns same result for name vs. hex',{
  c1 <- col2hex('red')
  c2 <- col2hex('#ff0000')
  expect_identical(c1, c2)
})

test_that('col2hex fails if not color',{
  expect_error(col2hex('reddy'))
})

test_that('col2hex fails if no color',{
  expect_error(col2hex())
})

test_that('col2hex fails if no color',{
  expect_error(col2hex(color = NULL))
})

test_that('col2hex fails if not character',{
  expect_error(col2hex(123))
})

test_that('col2hex fails if not appropriate hex',{
  expect_error(col2hex('#fff'))
})

test_that('col2hex fails if not appropriate alpha',{
  expect_error(col2hex('black', alpha=2))
})
