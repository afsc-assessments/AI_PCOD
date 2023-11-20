# ============================================================ M23_0a
# Script used to develop the model(s) considered in the
# sensitivity analysis: Item M23.0 
# ============================================================ #
# 
# Sensitivity analysis summary
# 
# *** 
# Topic of the sensitivity analysis:  biological_Info 
# Specific item in that topic: 
# 	- Selectivity 
# Author:  Ingrid Spies 
# Date: 2023-09-10 10:39:19.271133 
# Name of the model created:
# 	- M23_0a K timeblock
# *** 
# 
# This analysis has been developed based on the following model: 
# M23_0_data 
# 
# Results are stored in the following folder: 
#	 /Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/models/Sensitivity_Anal/4.6_Model 
# 
# Features: 
#  implementing time varying growth
# ============================================================ #

# ------------------------------------------------------------ #
# ------------------------------------------------------------ #

# This script holds the code used to develop the models considered in this sensitivity analysis.
# For each model, the base model is modified using the r4ss package.
# The new input files are then written in the root directory of each model and the
# model outputs are stored in the '/run/' folder housed in that directory.
# The results of this sensitivity analysis are analyzed using the following script:
# /Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/R/Stock_Synthesis/Sensitivity_Anal/4.6_Model_Outputs.R 


# ------------------------------------------------------------ #
# ------------------------------------------------------------ #


rm(list = ls(all.names = TRUE)) 

# 1. Update r4ss ----

update <- FALSE 

if (update) { 
  # Indicate the library directory to remove the r4ss package from
  mylib <- '~/R/win-library/4.1' 
  remove.packages('r4ss', lib = mylib) 
  # Install r4ss from GitHub 
  pak::pkg_install('r4ss/r4ss') 
} 
# ----------------------------------------------------------- 

# 2. Set up ---- 

rm(list = ls(all.names = TRUE)) 
# Local declaration 
fsep <- .Platform$file.sep #easy for file.path() function 

# packages 
library(r4ss) 
library(dplyr) 
library(reshape2) 
library(stringr) 

# Directories 
# Path to the model folder 
dir_model <- file.path(here::here(), 'November Models', fsep = fsep)

# Path to the Executable folder 
Exe_path <- file.path(dir_model, 'ss_executables') 

# Path to the R folder 
dir_script <- file.path(here::here(), 'R', fsep = fsep)

# Path to the Sensitivity analysis folder 
dir_SensAnal <- file.path(dir_model, 'Sensitivity_Anal', fsep = fsep)

# Path to data folder 
dir_data <- file.path(here::here(), 'data', 'for_ss', fsep = fsep)

# Useful function
source(file=file.path(dir_script,'utils','ss_functions.R', fsep = fsep))
source(file=file.path(dir_script,'utils','sensitivity_analysis_utils.R', fsep=fsep))

# Load the Rdata base with parameters and data for 2023 assessment
#load(file=file.path(dir_script,'Stock_Synthesis','SS_2023_Data_Parameters.Rdata', fsep = fsep))

# Save directories and function
# var.to.save <- c('dir_model',
# 'Exe_path',
# 'dir_script',
# 'dir_SensAnal') 


# Compute the hessian matrix 
# For each model, indicate if you want to compute the Hessian matrix.
# If noHess = TRUE for a given model, then the Hessian matrix
# won't be estimated.
# Reminder - The following models are considered:
# 	-  M23_1 
noHess <- c(FALSE)

var.to.save <- ls()


# ----------------------------------------------------------- 

#  3. Developing model M23_1  ----
# ----------------------------------------------------------- #

# Path to the M23_0 repertory
Dir_M23_0_data <- file.path(dir_model,'M23_0_data', fsep = fsep)
Dir_M23_0a <- file.path(dir_SensAnal, 'M23.0a','M23_0a' ,fsep = fsep)

# Add the model directory to the saved variables
var.to.save <- c(var.to.save, 'Dir_M23_0a') 


# For each SS input file, the following variable names will be used:
# Starter file :			 StartM23_0 
# Data file :			 DatM23_0 
# Control file :			 CtlM23_0 
# Forecast file :			 ForeM23_0 


# Do you want to copy the SS input files from the base model?
# This is useful if you are developing a model based on a base model that
# that did not exist when you set up the sensitivity analysis or if you already
# wrote a new SS input file for your new model and need to modify it (It ensure
# to start again from scratch and get the same
# basis of comparison.
#Restart_SA_modeldvpt(
#      base.model = 'M23_0',
##      curr.model = 'M23_1',
#      files = 'all')

#copy starter
file.copy(from=file.path(Dir_M23_0_data,'run','starter.ss'),to=file.path(Dir_M23_0a,'starter.ss'),overwrite=TRUE)
file.copy(from=file.path(Dir_M23_0_data,'run','control.ss_new'),to=file.path(Dir_M23_0a,'control.ss_new'),overwrite=TRUE)
file.copy(from=file.path(Dir_M23_0_data,'run','forecast.ss'),to=file.path(Dir_M23_0a,'forecast.ss'),overwrite=TRUE)
file.copy(from=file.path(Dir_M23_0_data,'run','data_echo.ss_new'),to=file.path(Dir_M23_0a,'data_echo.ss_new'),overwrite=TRUE)


# 3.1  Work on the Starter file ----
# ======================= #
# Read in the file
StarterFile <- file.path(Dir_M23_0a, 'run','starter.ss', fsep = fsep)
StartM23_0a <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

# Make your modification if applicable
# Code modifying the starter file
# ..... 
# ..... 


# Save the starter file for the model
SS_writestarter(
  mylist =  StartM23_0a,
  dir =  Dir_M23_0a, 
  overwrite = TRUE,
  verbose = TRUE
)

# Check file structure
# StarterFile <- file.path(Dir_M23_1, 'starter.ss')
#  StartM23_1 <- SS_readstarter(
# file = StarterFile,
# verbose = TRUE
# )

# clean environment
var.to.save <- c(var.to.save, 'StartM23_0a')
#rm(list = setdiff(ls(), var.to.save))
var.to.save <- ls()
# =======================

# 3.2  Work on the data file ----
# ======================= #


# Read in the file
DatFile <-file.path(Dir_M23_0a,'data_echo.ss_new', fsep = fsep)
DatM23_0a <- SS_readdat_3.30(
  file = DatFile,
  verbose = TRUE,
  section = TRUE
)

# Save the data file for the model
SS_writedat(
  datlist =  DatM23_0a,
  outfile = file.path(Dir_M23_0a, 'data_echo.ss_new', fsep = fsep),
  version = '3.30',
  overwrite = TRUE
)

# Check file structure
DatFile <- file.path(Dir_M23_0a, 'data_echo.ss_new', fsep = fsep)
DatM23_0a <-
  SS_readdat_3.30(
    file = DatFile,
    verbose = TRUE,
    section = TRUE
  )

# clean environment
var.to.save <- c(var.to.save, 'DatM23_0a')
#rm(list = setdiff(ls(), var.to.save))
var.to.save <- ls()
# =======================

# 3.3  Work on the control file ----
# ======================= #
# The SS_readctl_3.30() function needs the 'data_echo.ss_new' file to read the control file
# This file is created while running SS. You may have had a warning when building
# this script. Please check that the existence of the 'data_echo.ss_new' file
# in the 'run' folder of your new model.
# If the file does not exist, please use the RunSS_CtlFile() function that run SS
# in a designated file.


# Read in the file

Ctlfile <-file.path(Dir_M23_0a,'control.ss_new', fsep = fsep)#This one was from M23.3 but then I added in the short matrix for TV growth
CtlM23_0a <- SS_readctl_3.30(
  file = Ctlfile,
  use_datlist = TRUE,
  datlist = file.path(Dir_M23_0a,'data_echo.ss_new', fsep = fsep),
  verbose = TRUE
)

CtlM23_0a$N_Block_Designs=2
CtlM23_0a$time_vary_auto_generation=c(0,1,1,1,1)
CtlM23_0a$Block_Design[[1]]=c(1991, 2003, 2004, 2017, 2018, 2023)
CtlM23_0a$Block_Design[[2]]=c(1991,2002,2003,2012,2013,2016,2017,2019,2020,2022)#selectivity
CtlM23_0a$blocks_per_pattern=c(3,5)

CtlM23_0a$MG_parms[1,3]=0.36
CtlM23_0a$MG_parms[1,4]=0.36
CtlM23_0a$MG_parms[1,5]=0.4
CtlM23_0a$MG_parms[1,7]=2
CtlM23_0a$MG_parms[4,9:14]=c(0,1991,2022,5,1,1)#just K. #is first one time varying?
#CtlM23_0a$MG_parms_tv[1:2,7]=-4


CtlM23_0a$size_selex_parms[1:6,7]=c(3,3,3,-3,-3,-3)
CtlM23_0a$size_selex_parms[7:12,7]=c(3,-3,3,-3,-3,-3)
CtlM23_0a$size_selex_parms[,1]=c(40,-20,-10,-10,-1000,-10,10,-20,-10,0,-12,-10)
CtlM23_0a$size_selex_parms[,2]=c(103,10,10,10,2.71828,10,90,10,10,10,2.71828,10)
CtlM23_0a$size_selex_parms[,3]=c(90,25,6.7303,-0.838869,-999,4,58.4071,-3.05,5.71361,3.98,-2.35,10)
CtlM23_0a$size_selex_parms[,4]=c(95,25,3,10,-999,4,58.1,-3.05,5.76,3.98,-2.35,10)
CtlM23_0a$size_selex_parms[,5]=0.001

#based on the likelihood profile
CtlM23_0a$SR_parms[1,3:4]=10.3

CtlM23_0a$Variance_adjustment_list$Value=c(0.043206,	0.106765,	1.04947)

#add in recdev tuning parameters
CtlM23_0a$recdev_early_phase=2 #regular phase is 1 so this comes after
CtlM23_0a$recdev_early_start=1980
CtlM23_0a$MainRdevYrLast=2020 #age 2 first observed and last year of model is 2022


CtlM23_0a$last_early_yr_nobias_adj=1975.5   #_last_early_yr_nobias_adj_in_MPD
CtlM23_0a$first_yr_fullbias_adj=1989.0   #_first_yr_fullbias_adj_in_MPD
CtlM23_0a$last_yr_fullbias_adj=2019.6   #_last_yr_fullbias_adj_in_MPD
CtlM23_0a$first_recent_yr_nobias_adj=2022.2   #_first_recent_yr_nobias_adj_in_MPD
CtlM23_0a$max_bias_adj=0.8468  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models) 

SS_writectl(
  ctllist =  CtlM23_0a,
  outfile = file.path(Dir_M23_0a, 'control.ss_new', fsep = fsep),
  version = '3.30',
  overwrite = TRUE
)
# Check file structure
# We actually need to run the model to check the file structure

# clean environment
var.to.save <- c(var.to.save, 'CtlM23_0a')
#rm(list = setdiff(ls(), var.to.save))
var.to.save <- ls()
# =======================

# 3.4  Work on the forecast file ----
# ======================= #

# Read in the file
ForeFile <- file.path(Dir_M23_0a, 'forecast.ss', fsep = fsep)
ForeM23_0a <-SS_readforecast(
  file = ForeFile,
  version = '3.30',
  verbose = T,
  readAll = T
)

# Make your modification if applicable
# Code modifying the forecast file 
# ..... 
# ..... 


# Save the forecast file for the model
SS_writeforecast(
  mylist =  ForeM23_0a ,
  dir = Dir_M23_0a, 
  file = 'forecast.ss',
  writeAll = TRUE,
  verbose = TRUE,
  overwrite = TRUE
)

# Check file structure
# ForeFile <- file.path(Dir_M23_1, 'forecast.ss', fsep = fsep)
#  ForeM23_1 <-SS_readforecast(
# file = ForeFile,
# version = '3.30',
# verbose = T,
# readAll = T
# )

# clean environment
var.to.save <- c(var.to.save, 'ForeM23_0a')
#rm(list = setdiff(ls(), var.to.save))
var.to.save <- ls()
# =======================

# If you are done with your implementations, you can now run this new model

# *********************************************************** #
# 3.5  Run the new model using the new input files ----
# ======================= #
run_SS(SS_version = '3.30.21',
       # version of SS
       base_path =Dir_M23_0a,
       # root directory where the input file are housed
       pathRun = NULL,
       # A 'run' folder is created if needed (where output files will be stored)
       copy_files = TRUE,
       # copy the input files from theM23_1folder
       cleanRun = FALSE,
       # clean the folder after the run
       extra = ifelse(noHess[1], yes = '-nohess', no = ''),
       # this is if we want to use '-nohess'
       # show_console = FALSE
)

# 3.6  Let's plot the outputs from this model ----
# ======================= #
# read the model output and print diagnostic messages
Dirplot <- file.path(Dir_M23_0a, 'run', fsep = fsep)

replist <- SS_output(
  dir = Dirplot,
  verbose = TRUE,
  printstats = TRUE
)

# plots the results (store in the 'plots' sub-directory)
SS_plots(replist,
         dir = Dirplot,
         printfolder = 'plots'
)

# =======================

# 3.4  Make comparison plots between models ----
# ======================= #

# Use the SSgetoutput() function that apply the SS_output()
# to get the outputs from different models


# Path to the base model (M23_0a) repertory
Dir_M23_0r <- file.path(Dir_M23_0_data,  'run', fsep = fsep)
Dir_M23_0ar <- file.path(Dir_M23_0a,  'run', fsep = fsep)

# Extract the outputs for all models
SensiMod <- SSgetoutput(dirvec = c(
  Dir_M23_0r,
  Dir_M23_0ar))

# Rename the list holding the report files from each model
names(SensiMod)
names(SensiMod) <- c(
  'M23_0',
  'M23_0a')

# summarize the results
Version_Summary <- SSsummarize(SensiMod)

SSplotComparisons(
  Version_Summary,
  # print = TRUE,
  pdf = TRUE,
  plotdir = file.path(Dir_M23_0a, fsep = fsep),
  legendlabels = c(
    'M23_0',
    'M23_0a')
)


# Create comparison table for this analisys
# ####################################### #

SStableComparisons(Version_Summary)

tmp <- purrr::transpose(SensiMod)$parameters %>%
  purrr::map_df(~dplyr::as_tibble(.x), .id = 'Model') %>%
  dplyr::select(Model, Label, Value, Phase, Min, Max, Init,
                Gradient, Pr_type, Prior, Pr_SD, Pr_Like,
                LCI95 = `Value-1.96*SD`, UCI95 = `Value+1.96*SD`)

tmp %>%
  readr::write_csv(paste(dir_SensAnal,'M23.0a', 'Update_Data_comparison_table_all_params.csv', sep = fsep))

tmp %>%
  dplyr::filter(grepl('LnQ|R0', Label)) %>%
  tidyr::pivot_wider(id_cols = c(Label, Phase), names_from = Model, values_from = Value) %>%
  readr::write_csv(paste(dir_SensAnal,'M23.0a', 'Update_Data_comparison_table_lnQ_SRlnR0.csv', sep = fsep))

out <- SStableComparisons(Version_Summary)
names(out) <- c('Label', unique(tmp$Model))

out %>%
  readr::write_csv(paste(dir_SensAnal,'M23.0a', 'Update_Data_comparison_table_likelihoods_and_brps.csv', sep = fsep))


# -----------------------------------------------------------
# -----------------------------------------------------------

## End section ##

# You are ready to analyze the differences between the models
# considered in this sensitivity analysis.
# This can be done using the 4.6_Model_Outputs.R script.

#Do Francis weighting

# Create a temporary directory, feel free to change this location
mod_path <- file.path(Dir_M23_0a, "Francis")
# Path to simple model in r4ss and copy files to mod_path
# copy model input files
copy_SS_inputs(dir.old = file.path(Dir_M23_0a,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)
# copy over the Report file
file.copy(
  from = file.path(Dir_M23_0a, 'run',"Report.sso"),
  to = file.path(mod_path, "Report.sso")
)
# copy comp report file
file.copy(
  from = file.path(Dir_M23_0a,'run', "CompReport.sso"),
  to = file.path(mod_path, "CompReport.sso")
)

weight_table <- tune_comps(
  dir = mod_path,
  option = "none",
  verbose = FALSE
)

#run retro
Exe_path <- "/Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/Executables/SS_V3_30_21/ss_osx"

retro(dir=Dir_M23_0a,years=0:-10,newsubdir="retrospectives1",exe=Exe_path)

retroModelsM1 <- SSgetoutput(
  dirvec = file.path(Dir_M23_0a, "retrospectives1", paste("retro", 0:-10, sep = ""))
)

retroSummary1 <- SSsummarize(retroModelsM1)
endyrvec <- retroSummary1[["endyrs"]] + c(0:-10)
SSmohnsrho(retroSummary1, endyrvec, 2023, verbose = TRUE)#This is the r4ss that Steve thinks is wrong

SSplotComparisons(subplot=1,retroSummary1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.175)
SSplotComparisons(subplot=4,retroSummary1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.175)

library(ss3diags)
Mohn_SS3diag=SShcbias(
  retroSummary1,
  quants = c("SSB", "F"),
  models = "all",
  endyrvec = "default",
  verbose = TRUE
)
write.csv(Mohn_SS3diag,file.path(Dir_M23_0a,'retrospectives1/Mohn.csv'))


SSplotComparisons(subplot=1,retroSummaryM1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.175)
SSplotComparisons(subplot=4,retroSummaryM1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.175)

library(ss3diags)
Mohn_SS3diag=SShcbias(
  retroSummary1,
  quants = c("SSB", "F"),
  models = "all",
  endyrvec = "default",
  verbose = TRUE
)
write.csv(Mohn_SS3diag,paste(Dir_M23_0a,"/retrospectives5/Mohn_SS3diag_M1.csv",sep=""))



#Likelihood profile on R0
#To run this: in starter.ss change the AIPCOD21.ctl to control_modified.ss.
#In line 12 of starter.ss, do 0 # 0=use init values in control file; 1=use ss.par
#Also, line 19 of starter.ss: 1 # Include prior_like for non-estimated parameters (0,1) 
#in the starter.ss change the control model to control_modified.ss.
#Model1
R0.vec <- c(seq(9.0,11.0,.1))
Nprofile <- length(R0.vec)

mod_path <- file.path(Dir_M23_0a, "LikelihoodProfiles2")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M23_0a,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <- "/Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/Executables/SS_V3_30_21/ss_osx"

StarterFile <- file.path(mod_path, 'starter.ss', fsep = fsep)
StartM23_0_LP <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

StartM23_0_LP$ctlfile="control_modified.ss"

# Save the starter file for the model
SS_writestarter(
  mylist =  StartM23_0_LP,
  dir =  mod_path, 
  overwrite = TRUE,
  verbose = TRUE
)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "SR_LN(R0)",
  profilevec=R0.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(9,11),
              ymax=20,
              # string = "SR_LN(R0)", # substring of profile parameter
              profile.label = 'ln(R0)',
              profile.string ="SR_LN"
)

SSplotProfile(profilesummary,           # summary object
              xlim=c(10,10.5),
              ymax=0.75,
              # string = "SR_LN(R0)", # substring of profile parameter
              profile.label = 'ln(R0)',
              profile.string ="SR_LN"
)
#SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "R0")

PinerPlot(profilesummary,plot=TRUE,ymax=5,xlim=c(9,11))


#now try LnQ_base_Srv

mod_path <- file.path(Dir_M23_0a, "LikelihoodProfiles2")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M23_0a,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <- "/Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/Executables/SS_V3_30_21/ss_osx"

StarterFile <- file.path(mod_path, 'starter.ss', fsep = fsep)
StartM23_0_LP <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

StartM23_0_LP$ctlfile="control_modified.ss"

# Save the starter file for the model
SS_writestarter(
  mylist =  StartM23_0_LP,
  dir =  mod_path, 
  overwrite = TRUE,
  verbose = TRUE
)

Q.vec <- seq(-1,1,.1)
Nprofile <- length(Q.vec)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "LnQ_base_Srv",
  profilevec=Q.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "LnQ_base_Srv",xlim=c(-1,1),ymax=.1,profile.label = 'LnQ_base_Srv')

PinerPlot(profilesummary,plot=TRUE,ymax=5)

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(-1,1),
              ymax=20, profile.label = 'LnQ_base_Srv')#
              # string = "SR_LN(R0)", # substring of profile parameter
              profile.label = 'LnQ_base_Srv',
              profile.string =Q.vec
)


#now try natural mortality

mod_path <- file.path(Dir_M23_0a, "LikelihoodProfiles3")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M23_0a,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <- "/Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/Executables/SS_V3_30_21/ss_osx"

StarterFile <- file.path(mod_path, 'starter.ss', fsep = fsep)
StartM23_0_LP <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

StartM23_0_LP$ctlfile="control_modified.ss"

# Save the starter file for the model
SS_writestarter(
  mylist =  StartM23_0_LP,
  dir =  mod_path, 
  overwrite = TRUE,
  verbose = TRUE
)

M.vec <- c(seq(0.2,0.45,.02))
Nprofile <- length(M.vec)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "NatM",
  profilevec=M.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "NatM")
SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "NatM",xlim=c(.3,.42),ymax=1,profile.label = 'NatM')

PinerPlot(profilesummary,,profile.string = "NatM",plot=TRUE,ymax=5,profile.label = 'NatM')

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(0.2,0.45),
              ymax=20, profile.label = 'NatM')#
# string = "SR_LN(R0)", # substring of profile parameter
profile.label = 'LnQ_base_Srv',
profile.string =Q.vec
)

#now try fish peak parameter1

mod_path <- file.path(Dir_M23_0a, "LikelihoodProfiles4")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M23_0a,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <- "/Users/ingrid.spies/Documents/AI_cod-assessment/AI_Pcod_2023/Executables/SS_V3_30_21/ss_osx"

StarterFile <- file.path(mod_path, 'starter.ss', fsep = fsep)
StartM23_0_LP <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

StartM23_0_LP$ctlfile="control_modified.ss"

# Save the starter file for the model
SS_writestarter(
  mylist =  StartM23_0_LP,
  dir =  mod_path, 
  overwrite = TRUE,
  verbose = TRUE
)

Peak.vec <- c(seq(80,110,3))
Nprofile <- length(Peak.vec)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "Size_DblN_peak_FshComb(1)",
  profilevec=Peak.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "Size_DblN_peak")
SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "Size_DblN_peak_FshComb",xlim=c(80,110),ymax=1,profile.label = 'Size_DblN_peak_FshComb')

PinerPlot(profilesummary,,profile.string = "NatM",plot=TRUE,ymax=5,profile.label = 'NatM')

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(0.2,0.45),
              ymax=20, profile.label = 'NatM')#
# string = "SR_LN(R0)", # substring of profile parameter
profile.label = 'LnQ_base_Srv',
profile.string =Q.vec
)



# !!!!! WARNING !!!!!
# ------------------- #
# Please do not develop any script that you want to keep after this 
# warning section - It might be overwritten in the case you add a new 
# model to this SA.
# ------------------- #

## End script to develop SA models ##

# -----------------------------------------------------------
# -----------------------------------------------------------


