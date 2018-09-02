[![Build
Status](https://travis-ci.org/m-clark/visibly.svg?branch=master)](https://travis-ci.org/m-clark/visibly)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/m-clark/visibly?branch=master&svg=true)](https://ci.appveyor.com/project/m-clark/visibly)
[![Coverage
Status](https://img.shields.io/codecov/c/github/m-clark/visibly/master.svg)](https://codecov.io/github/m-clark/visibly?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)

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

Visibly is currently in its early stages, so more may be added soon. For
some additional palettes for those fond of another time, you might be
interested in
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

One of the built-in palettes is based on R’s blue. Others are based on
[Stan’s](https://github.com/stan-dev/stan) red,
[plotly’s](https://github.com/ropensci/plotly) base colors, and the
red-blue palette from
[RColorBrewer](https://github.com/cran/RColorBrewer/blob/master/R/ColorBrewer.R).

A clean theme for
<span class="pack">plotly</span>.

<!-- Plotly does all wonder of screwup here because size is so hard, so just export the png -->

``` r
library(plotly)
mtcars %>% 
  plot_ly(x=~wt, y=~mpg, color=~cyl) %>% 
  add_markers(marker=list(size=15)) %>% 
  theme_plotly()
```

<img src="man/figures/plotly_wtf.png" style="display:block; margin: 0 auto;" width='50%'>

Visualize a correlation matrix via factor analysis.

``` r
data('bfi', package = 'visibly')
cor_matrix = cor(bfi, use='pair')
corr_heat(cor_matrix)
```

<img src='man/figures/corr_heat.png' style="display:block; margin: 0 auto;" width=50%>
<br>
<img src='man/figures/corr_heat_3d.png' style="display:block; margin: 0 auto;" width=50%>

Plot some model coefficients. Requires the
<span class="pack">scico</span> package.

``` r
fit_lm = lm(mpg ~ ., mtcars)
plot_coefficients(fit_lm)
```

<img src="man/figures/README-lm0-1.png" width="75%" style="display: block; margin: auto;" />

Plot GAM results

``` r
library(mgcv)
d = gamSim()
Gu & Wahba 4 term additive model

gam_model = gam(y ~ x0 + s(x1) + s(x2, bs='gp') + s(x3, bs='ps'), data=d)

plot_gam(gam_model, main_var = x2)
```

<img src="man/figures/README-gam-1.png" width="75%" style="display: block; margin: auto;" />

``` r
plot_gam_check(gam_model)
```

<img src="man/figures/README-gam-2.png" width="75%" style="display: block; margin: auto;" />

See the [intro](https://m-clark.github.io/visibly/articles/intro.html)
for more.
