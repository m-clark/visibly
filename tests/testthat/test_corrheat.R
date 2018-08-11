context('test corrheat')

main = cor(mtcars)
m_asym = matrix(runif(25), ncol = 5)
m_nonsq = matrix(runif(30), ncol = 6)


z = corrheat(main, n_factors = 2)


# input chex --------------------------------------------------------------

test_that('not a matrix', {
  expect_error(corrheat(mtcars))
})

test_that('square check', {
  expect_error(corrheat(m_nonsq))
})

test_that('asymmetrical check', {
  expect_error(corrheat(m_asym))
})


# n_factors ---------------------------------------------------------------

test_that('errs if n_factors not int', {
  expect_error(corrheat(main, n_factors = 1.5))
})

test_that('can take n_factors', {
  expect_s3_class(corrheat(main, n_factors = 2), 'plotly')
})

test_that('will automate less than 4 cols', {
  expect_s3_class(corrheat(cor(mtcars[,1:4])), 'plotly')
})

test_that('will automate more than 4 cols', {
  expect_s3_class(corrheat(main), 'plotly')
})



# psych options -----------------------------------------------------------

test_that('will take psych options', {
  expect_s3_class(corrheat(main, psychOptions = list(rot='promax')), 'plotly')
})



# order checks ------------------------------------------------------------

test_that('will take psych options', {
  expect_s3_class(corrheat(main, ordering = 'raw'), 'plotly')
})

test_that('will take psych options', {
  expect_s3_class(corrheat(main, ordering = 'absolute'), 'plotly')
})

test_that('will take psych options', {
  expect_s3_class(corrheat(main, ordering = 'first'), 'plotly')
})


# misc --------------------------------------------------------------------

test_that('can do 3d', {
  expect_s3_class(corrheat(main, n_factors = 2, three_d = TRUE), 'plotly')
})
