context('test col2lab')

test_that('col2lab works',{
  c1 <- round(col2lab('red'), 5)
  c2 <- round(matrix(c(53.48418, 80.01027, 67.38407), nrow=1), 5)
  colnames(c2) <- colnames(c1)
  expect_identical(c1, c2)
})

test_that('col2lab returns same result for name vs. hex',{
  c1 <- col2lab('red')
  c2 <- col2lab('#ff0000')
  expect_identical(c1, c2)
})

test_that('col2lab fails if not color',{
  expect_error(col2lab('reddy'))
})

test_that('col2lab fails if not character',{
  expect_error(col2lab(123))
})

test_that('col2lab returns matrix',{
  x <- list('red', 'white', 'blue')
  expect_is(col2lab(x), 'matrix')
})


test_that('col2lab wont error on alpha',{
  expect_is(col2lab('#ff000080'), 'matrix')
})


