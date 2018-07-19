[![Build
Status](https://travis-ci.org/m-clark/visibly.svg?branch=master)](https://travis-ci.org/m-clark/visibly)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/m-clark/visibly?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/visibly)
[![Coverage
Status](https://img.shields.io/codecov/c/github/m-clark/visibly/master.svg)](https://codecov.io/github/m-clark/visibly?branch=master)

# visibly

<img src="man/figures/visibly_hex.png" align="right" width = 200/>

Visibly is a handful of functions I use for color palettes, themes, etc.
in R. Inside you will find:

  - some ready-made palettes, e.g. based on R blue and Stan red
  - a function to quickly and easily create palettes with using
    `colortools::complementary` `colortools::adjacent` etc.
  - clean, web-friendly themes for ggplot2 and plotly
  - a function to interact with
    [colorgorical](http://vrl.cs.brown.edu/color/)
  - coefficient plots for fixed and random effects, plotting of GAM
    results.

## Installation

Install the development version directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("m-clark/visibly")
```

Visibly is currently in its very early stages, so more may be added
soon. For some additional palettes for those fond of another time, you
might be interested in
[NineteenEightyR](https://github.com/m-clark/NineteenEightyR).

## Examples

Create a palette from a single starting point. This requires the
<span class="pack">colortools</span> package to create equally spaced
colors.

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

Plot it to get a feel for
things.

``` r
create_palette('#ff5500', plot = T)
```

<img src="man/figures/README-plot-1.png" width="75%" style="display: block; margin: auto;" />

    $`#ff5500`
    [1] "#ff5500"
    
    $complementary
    [1] "#FF5500" "#00AAFF"
    
    $analogous
    [1] "#FF5500" "#FFD500" "#FF002B"
    
    $split_complentary
    [1] "#FF5500" "#00FFD4" "#002BFF"
    
    $triadic
    [1] "#FF5500" "#00FF55" "#5500FF"
    
    $square
    [1] "#FF5500" "#2AFF00" "#00AAFF" "#D500FF"
    
    $tetradic
    [1] "#FF5500" "#AAFF00" "#00AAFF" "#5500FF"

There are some other options to express.

``` r
create_palette('#ff5500', 
               alpha = .5, 
               plot = T, 
               name = 'orange_you_glad_you_have_this_color')
```

<img src="man/figures/README-plot2-1.png" width="75%" style="display: block; margin: auto;" /><img src="man/figures/README-plot2-2.png" width="75%" style="display: block; margin: auto;" />

    $orange_you_glad_you_have_this_color
    [1] "#FF550080"
    
    $complementary
    [1] "#FF550080" "#00AAFF80"
    
    $analogous
    [1] "#FF550080" "#FFD50080" "#FF002B80"
    
    $split_complentary
    [1] "#FF550080" "#00FFD480" "#002BFF80"
    
    $triadic
    [1] "#FF550080" "#00FF5580" "#5500FF80"
    
    $square
    [1] "#FF550080" "#2AFF0080" "#00AAFF80" "#D500FF80"
    
    $tetradic
    [1] "#FF550080" "#AAFF0080" "#00AAFF80" "#5500FF80"

One of the built-in palettes is based on R’s blue. Others are based on
[Stan’s](https://github.com/stan-dev/stan) red,
[plotly’s](https://github.com/ropensci/plotly) base colors, and the
red-blue palette from
[RColorBrewer](https://github.com/cran/RColorBrewer/blob/master/R/ColorBrewer.R).

``` r
palettes$Rblue
$Rblue
[1] "#1f65b7"

$complementary
[1] "#1f65b7" "#b7701f"

$monochromatic
[1] "#1f65b7" "#366caa" "#4a719e" "#5a7491"

$analogous
[1] "#1f65b7" "#241fb7" "#1fb2b7"

$split_complementary
[1] "#1f65b7" "#b2b71f" "#b7241f"

$triadic
[1] "#1f65b7" "#66b71f" "#b71f66"

$tetradic
[1] "#1f65b7" "#b7701f" "#66b71f" "#701fb7"
```

A clean theme for <span class="pack">plotly</span>.

``` r
library(plotly)
mtcars %>% 
  plot_ly(x=~wt, y=~mpg, color=~cyl) %>% 
  add_markers(size =~ I(20)) %>% 
  theme_plotly()
```

<img src="man/figures/README-example4-1.png" width="75%" style="display: block; margin: auto;" />

Plot some model coefficients. Requires the
<span class="pack">scico</span> package.

``` r
fit_lm = lm(mpg ~ ., mtcars)
plot_coefficients(fit_lm)
```

<img src="man/figures/README-lm0-1.png" width="75%" style="display: block; margin: auto;" />

See the [intro](https://m-clark.github.io/visibly/articles/intro.html)
for more.
