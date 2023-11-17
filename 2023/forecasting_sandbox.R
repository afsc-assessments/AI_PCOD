## Sanity checking and updating AI PCOD forecasts

require(here)
require(dplyr)
require(r4ss)


## Model 23.0a
## Copied stuff from https://github.com/afsc-assessments/AI_PCOD/tree/main/M23.0/run
## renamed to ".ss" and updated starter file accordingly
## downloaded v3.30.21 from vLab
## I'm not sure why devPH is turned on for K but not going to adjust it just now

## running with hessian takes <1 min
# setwd(here('2023/m23.0a'))
# shell('ss')

mod230a <- SS_output(here('2023','m23.0a'))
SS_plots(mod230a)
