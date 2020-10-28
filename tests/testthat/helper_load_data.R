# # for quicker loading vignettes/tests
#
# library(brms)
#
# fit_brms_single <-
#   brm(count ~ zAge + zBase + Trt + zBase:Trt + (1 | patient),
#       data = epilepsy,
#       cores = 4,
#       refresh = -1,
#       verbose = FALSE,
#       thin = 40
#   )
#
# fit_brms_two <-
#   brm(count ~ zAge + zBase + Trt + zBase:Trt + (1 | patient)  + (1 | obs),
#       data = epilepsy,
#       cores = 4,
#       refresh = -1,
#       verbose = FALSE,
#       thin = 40
#   )
#
# fit_brms_slope <-
#   brm(Reaction ~ Days + (Days | Subject),
#       data = lme4::sleepstudy,
#       cores = 4,
#       refresh = -1,
#       verbose = FALSE,
#       thin = 40
#   )


load('brms_res.RData')
library(dplyr)
