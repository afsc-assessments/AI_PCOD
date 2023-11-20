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
mod230a <- SS_output(here('2023','original_nov_models','m23.0a'))
forecast_dirs <- list.dirs(here('2023','original_nov_models'), recursive = FALSE)[grepl(list.dirs(here('2023','original_nov_models'), recursive = FALSE), pattern = 'M23.0a')] %>%
lapply(., SS_output) %>% SSsummarize(.)
llabs <- c('23.0a (original forecast file)',
           'Lower B limit to 0.02 (from 0.175)',
           'Modify Btarget (should not have impact)',
           'Change end years for inputs')
SSplotComparisons(forecast_dirs, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15)
## this illustrated that the lwr b limit is indeed impactful, but not as impactful as the end years
## going to leave it at 0.175 but adjust the end years.

# SS_plots(mod230a) 

## 
mod230b <- SS_output(here('2023','original_nov_models','m23.0b'))
# SS_plots(mod230b) 

mod230c <- SS_output(here('2023','original_nov_models','m23.0c'))
# SS_plots(mod230c)

## neither model a nor c realy exhibits a strong different in the resultant growth curve.
# M does vary considerably for model 230c.
# selex is not that different across models.

## let's make some basic comparison plots

basemods <- SSsummarize(list(mod230a,mod230b, mod230c))
llabs <- c('23.0a tv growth', '23.0b tv growth & selex','23.0c tv k & m' )
SSplotComparisons(basemods, print = TRUE, png = T,plotdir = here('2023','figs','base_models'),
                  legendlabels = llabs,
                  endyrvec = 2022+15)

## now I am going to copy the three november models into new folders and run all with the same
# terminal forecast folder established previously.
## KIM the SAFE uses model 23.0b for the SAFE.
list.dirs(here('2023','model_runs'), recursive = F)[grepl(list.dirs(here('2023','model_runs'), 
                                                                                         recursive = F), 
                                                                               pattern = '*-newforecast*')] %>%
  
  lapply(., FUN = function(x) SS_plots(SS_output(x)))


new_forecast_mods <- list.dirs(here('2023','model_runs'), recursive = F)[grepl(list.dirs(here('2023','model_runs'), 
                                                                                         recursive = F), 
                                                                               pattern = '*M23*')] %>%
  
  lapply(., SS_output) %>% SSsummarize(.) 
llabs <- c('23.0a tv growth', 
           '23.0a tv growth - new forecast', 
           '23.0b tv growth & selex',
           '23.0b tv growth & selex - new forecast',
           '23.0c tv k & m',
           '23.0c tv k & m - new forecast')
SSplotComparisons(new_forecast_mods, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15)

## looks like 23.0c was affected the most. this is sensible because originally it was averaging the biology
## from teh whole time series, whereas this model has a higher estimate of M in the terminal year
## when we use the terminal year m is higher therefore abc is higher therefore sb is lower.
## the model used in th

## thinking thru the inputs to the forecast file; the do_ak_scenarios script
## indicates that the setup in the original forecast file is used as a baseline.
## The first vector "bmark" indicates what years you want to use to establish your reference points.
## generally these should all be 1) the same as one another and 2) probably correspond to the terminal year
## (this gets into ugly questions about violation of equilibrium given temporal variablity, but whatever)

## for the second vecot, "fcast years", you're telling it what years to base the projections on.
## these don't necessarily have to all be the same as one another, but we would probably
## want to use the most recent notion of things like selex to move fwd,
# and be extra cautious of averaging across years (because we then are using a value that is out of sync with the model)
## finally, we wouldn't want to use novel time blocks for certain values that might be in conflict with the biology above.
## in our case the relF doesnt matter because we only have one fishery fleet
## and the recruits don't cross the regime shift 1977
# https://www.npfmc.org/wp-content/uploads/BSAIfmp.pdf

## steve always used 0.4 for SPR and B target, following the ABC setup


