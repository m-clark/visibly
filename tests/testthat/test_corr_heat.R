context('test corr_heat')

main <- cor(mtcars)
m_small <- matrix(runif(25), ncol = 3)
m_asym <- matrix(runif(25), ncol = 5)
m_nonsq <- matrix(runif(30), ncol = 6)

# z <- corr_heat(main, n_factors = 2)


# input chex --------------------------------------------------------------

test_that('not a matrix', {
  expect_error(corr_heat(mtcars))
})

test_that('square check', {
  expect_error(corr_heat(m_nonsq))
})

test_that('asymmetrical check', {
  expect_error(corr_heat(m_asym))
})

test_that('palette direction', {
  expect_error(corr_heat(main, dir=4))
})

test_that('diagonal check', {
  expect_error(corr_heat(main, diagonal = 3))
})

test_that('palette check', {
  expect_error(corr_heat(main, pal = 'annarbor'))
})


# n_factors ---------------------------------------------------------------

test_that('errs if n_factors not int', {
  expect_error(corr_heat(main, n_factors = 1.5))
})

test_that('can take n_factors', {
  expect_s3_class(corr_heat(main, n_factors = 2), 'plotly')
})

test_that('can take n_factors', {
  expect_s3_class(corr_heat(main, n_factors = 5), 'plotly')
})


test_that('will automate less than 4 cols', {
  expect_s3_class(corr_heat(cor(mtcars[,1:3])), 'plotly')
})

test_that('will automate less than 4 cols', {
  expect_s3_class(corr_heat(cor(mtcars[,1:3]), n_factors = 2), 'plotly')
})

test_that('will automate more than 4 cols', {
  expect_s3_class(corr_heat(main), 'plotly')
})



# psych options -----------------------------------------------------------

test_that('will take psych options', {
  expect_s3_class(corr_heat(main, psych_opts = list(rot='simplimax')), 'plotly')
})



# order checks ------------------------------------------------------------

test_that('will take psych options', {
  expect_s3_class(corr_heat(main, ordering = 'raw'), 'plotly')
})

test_that('will take psych options', {
  expect_s3_class(corr_heat(main, ordering = 'absolute'), 'plotly')
})

test_that('will take psych options', {
  expect_s3_class(corr_heat(main, ordering = 'first'), 'plotly')
})


# misc --------------------------------------------------------------------

test_that('Will ignore FA and just return the plot', {
  expect_s3_class(corr_heat(main, plot_only = T), 'plotly')
})

test_that('can do 3d', {
  expect_s3_class(corr_heat(main, n_factors = 2, three_d = TRUE), 'plotly')
})


test_that('handle all pos', {
  expect_s3_class(corr_heat(Harman23.cor$cov, n_factors = 2,
                            pal = 'oslo'), 'plotly')
})

test_that('handle all neg', {
  nh <- -Harman23.cor$cov
  diag(nh) <- 1
  suppressWarnings({
  expect_s3_class(corr_heat(nh, n_factors = 1, pal='bilbao'), 'plotly')
  })
})


test_that('errors bare matrix', {
  h23 <- Harman23.cor$cov
  dimnames(h23) <- NULL
  expect_error(corr_heat(h23))
})

test_that('errors with NA and fa', {
  h23 <- Harman23.cor$cov
  h23[, 2] <- NA
  h23[2, ] <- NA
  # h23[2, 2] <- 1
  expect_error(suppressWarnings(corr_heat(h23)))
})
