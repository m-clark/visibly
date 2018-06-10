[![Build
Status](https://travis-ci.org/m-clark/visibly.svg?branch=master)](https://travis-ci.org/m-clark/visibly)
[![Coverage
Status](https://img.shields.io/codecov/c/github/m-clark/visibly/master.svg)](https://codecov.io/github/m-clark/visibly?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# visibly

<img src="man/figures/visibly_hex.png" align="right" width = 200/>

Visibly is a handful of functions I use for color palettes and themes in
R.

## Example

Create a palette.

``` r
library(visibly)
create_palette('papayawhip')
$papayawhip
[1] "papayawhip"

$complementary
[1] "#FFEFD5" "#D5E5FF"

$analogous
[1] "#FFEFD5" "#FAFFD5" "#FFDAD5"

$split_complentary
[1] "#FFEFD5" "#D5FAFF" "#DAD5FF"

$triadic
[1] "#FFEFD5" "#D5FFEF" "#EFD5FF"

$square
[1] "#FFEFD5" "#D5FFDA" "#D5E5FF" "#FFD5FA"

$tetradic
[1] "#FFEFD5" "#E5FFD5" "#D5E5FF" "#EFD5FF"
```

A paletted based on the blue in the R logo.

``` r
palettes$Rblue
$Rblue
[1] "#1f65b7"

$complementary
[1] "#1f65b7" "#b7701f"

$monochromatic
[1] "#1f65b7" "#366caa" "#4a719e" "#5a7491"

$analagous
[1] "#1f65b7" "#241fb7" "#1fb2b7"

$split_complementary
[1] "#1f65b7" "#b2b71f" "#b7241f"

$triadic
[1] "#1f65b7" "#66b71f" "#b71f66"

$tetradic
[1] "#1f65b7" "#b7701f" "#66b71f" "#701fb7"
```
