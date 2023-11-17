## Sanity checking and updating AI PCOD forecasts

require(here)
require(dplyr)
require(r4ss)


## Model 23.0a
## Copied stuff from Ingrid's google drieve under November Models/Sensitivity_Anal/M23.0a/run
## renamed to ".ss" and updated starter file accordingly
## only swapped in control files after tjat
## downloaded v3.30.21 from vLab
## I'm not sure why devPH is turned on for K but not going to adjust it just now
## also unclear where there are  > 1 time block for 23.0a

## running with hessian takes <1 min
mod230a <- SS_output(here('2023','m23.0a'))
# SS_plots(mod230a) 

## 
mod230b <- SS_output(here('2023','m23.0b'))
# SS_plots(mod230b) 

mod230c <- SS_output(here('2023','m23.0c'))
# SS_plots(mod230c)

## neither model a nor c realy exhibits a strong different in the resultant growth curve.
# M does vary considerably for model 230c.
# selex is not that different across models.

## let's make some basic comparison plots
basemods <- SSsummarize(list(mod230a,mod230b, mod230c))
llabs <- c('23.0a tv growth', '23.0b tv growth & selex','23.0c tv k & m' )
SSplotComparisons(basemods, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15)


## thinking thru the inputs to the forecast file;

