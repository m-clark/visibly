context('test corrheat')

main = cor(mtcars)
m_asym = matrix(runif(25), ncol = 5)
m_nonsq = matrix(runif(30), ncol = 6)


z = corrheat(main, n_factors = )

test_that('square check', {
  expect_error(corrheat(m_nonsq))
})

test_that('asymmetrical check', {
  expect_error(corrheat(m_asym))
})

test_that('n_factors', {
  expect_error(corrheat(main, n_factors = 1.5))
})
