context('test colorgorical')


# From the source at https://github.com/connorgr/colorgorical/blob/master/src/model/model.py
# palSize (int): the number of colors to sample for the palette.
# numPalettes (int): the number of palettes to sample preference from.
# hueFilters (list): a two-dimensional list, such that each element of
# hue filters is a two-element list that contains the lower and
# upper hue angle boundary for each hue angle region to include
# when sampling colors.
# lightnessRange (list): a two-element list that sets the lightness
# range for filtering for color space before sampling.
# onlyUseRGB (bool): whether color space should be restricted to RGB.
# noticeableDifferenceAngle (float): the visual angle that should be
# used when calculating CIE Lab noticeable difference intervals
# using Stone, Szafir, and Setlur's engineering color difference
# model http://www.danielleszafir.com/2014CIC_48_Stone_v3.pdf.
# startPalette (list): a two-dimensional list, such that each element
# of startPalette is a 3-element list that specifies a valid CIE
# Lab D65 color. Any dimension that are not a multiple of 5 will
# be rounded accordingly.
# weights (dict): user-defined weights ([0,1]) for the four palette
# scores such that the total weight always sums to 1. The weight
# names are `ciede2000`, `nameDifference`, `nameUniqueness`, and
# `pairPreference`.


# No testing is actually conducted due to call to web.

library(colorspace)
x <- RGB(runif(1000), runif(1000), runif(1000))
x = hex2RGB('#ff5500')
y <- as(x, "LAB")

# test_that('colorgorical works', {
#   expect_s3_class(colorgorical(n=4), 'character')
# })
#
# test_that('colorgorical can take startPalette', {
#   expect_s3_class(colorgorical(n=4, startPalette = y@coords[1,]), 'character')
# })
#
# test_that('colorgorical can take hueFilter', {
#   expect_s3_class(colorgorical(n=4, pairPreference = 1, hueFilters = c(90, 180)), 'character')
# })
#
# test_that('colorgorical can return RGB', {
#   expect_is(colorgorical(n=4, output = 'sRGB'), 'matrix')
# })
#
# test_that('colorgorical can return LAB', {
#   expect_is(colorgorical(n=4, output = 'LAB'), 'matrix')
# })

test_that('colorgorical will fail with invalid weights', {
  expect_error(colorgorical(n=4, pairPreference = 10))
})

test_that('colorgorical will fail with invalid lightnessRange', {
  expect_error(colorgorical(n=4, lightnessRange = c('25', '101')))
})

test_that('colorgorical will fail with invalid output', {
  expect_error(colorgorical(n=4, output = 'blah'))
})

test_that('colorgorical will fail if hueFilters not a list', {
  expect_error(colorgorical(n=4, hueFilters = c(25,80)))
})

test_that('colorgorical will fail if startPalette not a list', {
  expect_error(colorgorical(n=4, startPalette =  '#ff5500'))
})



# test = colorgorical(n=12)
#
# library(ggplot2)
# qplot(x=1:length(test), y=1, color=I(test), geom='point', size=I(10))
#
# library(colorspace)
# x = hex2RGB('#ff5500')
# y <- as(x, "LAB")
# head(x)
# head(y)
# plot(y)
#
# test = colorgorical(n=12, pairPreference = 1, startPalette = y@coords[1,])
# test
# plot(qplot(x=1:length(test), y=1, color=I(test), geom='point', size=I(10)))
#
#
# debugonce(colorgorical)
# test = colorgorical(n=12, pairPreference = 1, startPalette = y@coords[1,])
# test = colorgorical(n=12, pairPreference = 1, hueFilters = c(90, 180), startPalette = y@coords[1,])
#
#
# test = colorgorical(n=12, pairPreference = 1)
#
# test = colorgorical(n=12, pairPreference = 1, perceptualDifference = 1, startPalette = list(y@coords[1,]))
# qplot(x=1:length(test), y=1, color=I(test), geom='point', size=I(10))
#
# test = colorgorical(n=4,
#                     pairPreference = 1,
#                     perceptualDifference = 1,
#                     startPalette = list(y@coords[1,]),
#                     output = 'HEX')
# test
# qplot(x=1:length(test), y=1, color=I(test), geom='point', size=I(10))
