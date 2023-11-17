## Sanity checking and updating AI PCOD forecasts

require(here)
require(dplyr)
require(r4ss)


## Model 23.0a
## Copied stuff from Ingrid's github
## renamed to ".ss" and updated starter file accordingly
## downloaded v3.30.21 from vLab
## I'm not sure why devPH is turned on for K but not going to adjust it just now

## running with hessian takes <1 min
mod230a <- SS_output(here('2023','m23.0a'))
# SS_plots(mod230a) 

## looking in the control file, it doesn't appear that tv selex was implemented here
mod230b <- SS_output(here('2023','m23.0b'))
# SS_plots(mod230b) ## same story for the selex; this is incorrectly represented in the document, too

mod230c <- SS_output(here('2023','m23.0c'))
# SS_plots(mod230c)

## neither model a nor c realy exhibits a strong different in the resultant growth curve.
# M does vary considerably for model 230c.
# selex is not that different across models.

## let's make some basic comparison plots
basemods <- SSsummarize(list(mod230a, mod230c))
llabs <- c('3 timeblocks on VB K', 'Timeblocks on K and M' )
SSplotComparisons(basemods, print = T, plotdir = here('2023','figs'),
                  legendlabels = llabs)

