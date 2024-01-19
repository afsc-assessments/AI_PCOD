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
mod230a <- SS_output(here('2023','model_runs','m23.0a')) ## aka 230
M23_2 <- mod230c <- SS_output(here('2023','model_runs','m23.0c')) ## aka 232

llabs = c('Model 23.0: 3 Growth Blocks','Model 23.2: 2 Growth Blocks, 2 M Blocks')
SSplotComparisons(SSsummarize(list(mod230a,mod230c)), print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2023,
                  col = c('black','dodgerblue2','navy','red'))



SSB0_M23_2<-data.table(M23_2$derived_quants)[Label=='SSB_unfished']$Value

SSB0_M23_2<-data.table(M23_2$derived_quants)[Label=='SSB_SPR']$Value
SSB_M23_2<-data.table(M23_2$derived_quants)[Label%like%'SSB']
SSB_M23_2$YEAR<-as.numeric(do.call(rbind,strsplit(SSB_M23_2$Label,"_"))[,2])
SSB_M23_2<-SSB_M23_2[YEAR%in%1991:2025]
SSB35_M23_2<-SSB0_M23_2*0.35

Bmsy=data.table(M23_2$derived_quants)[Label=="SSB_MSY"]$Value
BoverBmsy_M23_2=(M23_2$timeseries$SpawnBio[3:37]/Bmsy)
Bpercent_M23_2=(M23_2$timeseries$SpawnBio[3:37]/SSB0_M23_2)



mod230cF <- SS_output(here('2023','model_runs','m23.0c-newforecast'))
exp(mod230cF$parameters[grep('R0', mod230cF$parameters$Label),'Value'])
exp(mod230cF$parameters[grep('SSB_Virgin', mod230cF$parameters$Label),'Value'])

mod230cF$timeseries %>% filter(Era == 'VIRG')
mod230c$timeseries %>% filter(Era == 'VIRG')


head(mod230c$recruit)
exp(mod230c$parameters[grep('R0', mod230c$parameters$Label),'Value'])
mean(mod230c$recruit$exp_recr[mod230c$recruit$Yr %in% 1991:2023])
mean(mod230c$recruit$pred_recr[mod230c$recruit$Yr %in% 1991:2023])
mean(mod230c$recruit$bias_adjusted )

forecast_dirs <-    c(  
    here('2023','model_runs','m23.0c'),
    here('2023','model_runs','m23.0c-newforecast'),
     list.dirs(here('2023','model_runs','test'), 
              recursive = FALSE)[grepl(list.dirs(here('2023','model_runs','test'), 
                                                  recursive = FALSE), pattern = 'M23.0c')]
    )[c(1,4,3,2)] %>%
  lapply(., SS_output) %>%  
  SSsummarize(.)

llabs <- c('23.0c - original forecast file',
           '23.0c - Lower B limit to 0.02 (from 0.175)',
      
           '23.0c - Change end years for inputs',
           '23.0c - Change B limit and End Years (new forecast file)'
     
           )
SSplotComparisons(forecast_dirs, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15,
                  col = c('black','dodgerblue2','navy','red'))


forecast_dirs <-    c(  
  here('2023','model_runs','m23.0a'),
  here('2023','model_runs','m23.0a-newforecast'),
  list.dirs(here('2023','model_runs','test'), 
            recursive = FALSE)[grepl(list.dirs(here('2023','model_runs','test'), 
                                               recursive = FALSE), pattern = 'M23.0a')]
)[c(1,3,4,2)] %>%
  lapply(., SS_output) %>%  
  SSsummarize(.)

llabs <- c('23.0a - original forecast file',
           '23.0a - Lower B limit to 0.02 (from 0.175)',
           
           '23.0a - Change end years for inputs',
           '23.0a - Change B limit and End Years (new forecast file)'
           
)
SSplotComparisons(forecast_dirs, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15,
                  col = c('black','dodgerblue2','navy','red'))


forecast_dirs <-    c(  
  here('2023','model_runs','m23.0a'),
  here('2023','model_runs','m23.0a-newforecast'),
  here('2023','model_runs','m23.0c'),
  here('2023','model_runs','m23.0c-newforecast')) %>%
  lapply(., SS_output) %>%  
  SSsummarize(.)

llabs <- c('23.0a - original forecast file', 
           '23.0a - Change B limit and End Years (new forecast file)',
           '23.0c - original forecast file', 
           '23.0c - Change B limit and End Years (new forecast file)')
           

SSplotComparisons(forecast_dirs, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15,
                  col = c('grey50','black','dodgerblue2','blue'))
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



new_forecast_mods <- list.dirs(here('2023','model_runs'), recursive = F)[grepl(list.dirs(here('2023',
                                                                                              'model_runs'), 
                                                                                         recursive = F), 
                                                                               pattern = 'M23.0c')] %>%
  
  lapply(., SS_output) %>% SSsummarize(.) 
llabs <- c('23.0c - old forecast file (scenario 1)',
           '23.0c - new forecast file (scenario 1)')
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





new_forecast_mods <- list.dirs(here('2023','model_runs'), recursive = F)[grepl(list.dirs(here('2023',
                                                                                              'model_runs'), 
                                                                                         recursive = F), 
                                                                               pattern = 'm23_c')] %>%
  
  lapply(., SS_output) %>% SSsummarize(.) 
llabs <- c('23.0c - 1991-2023 [from google drive]',
           '23.0c - 2004-2023 [from google drive]')
SSplotComparisons(new_forecast_mods, print = TRUE, png = T,plotdir = here('2023','figs'),
                  legendlabels = llabs,
                  endyrvec = 2022+15)


