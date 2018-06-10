context('test create_palette')

test_that('create_palette returns a list',{
  expect_type(create_palette('#ff5500', name='orange', toHCL=F), 'list')
})

test_that('create_palette takes HCL arg and plot',{
  expect_type(create_palette('#ff5500', toHCL=T, plot=T), 'list')
})

test_that('create_palette takes alpha arg',{
  expect_type(create_palette('#ff5500', name='orange', alpha=.5, plot=T),
              'list')
})

test_that('create_palette errs on bad alpha',{
  expect_error(create_palette('#ff5500', name='orange', alpha=2))
})

test_that('create_palette errs on bad form',{
  expect_error(create_palette(225500, name='orange', alpha=2))
})

test_that('create_palette errs on bad form',{
  expect_error(create_palette('ff5500', name='orange', alpha=2))
})

test_that('create_palette errs on bad color',{
  expect_error(create_palette('barf'))
})
