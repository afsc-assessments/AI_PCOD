# ============================================================ 
# Script used to develop models
# ============================================================ #
# 
# Sensitivity analysis summary
# 
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
# Local declaration 
fsep <- .Platform$file.sep #easy for file.path() function 

# packages 
library(r4ss) 
library(dplyr) 
library(reshape2) 
library(stringr) 

# Directories 
# Path to the model folder 
setwd("/Users/ingrid.spies/Documents/AI_PCOD/2025/")
dir_model <- file.path(getwd(), 'Models', fsep = fsep)


# Path to the R folder 
dir_script <- file.path(getwd(), 'R', fsep = fsep)

# Path to data folder 
dir_data <- file.path(getwd(), 'data', 'SS3_2023', fsep = fsep)

# Useful function
source(file=file.path(dir_script,'utils','ss_functions.R', fsep = fsep))
source(file=file.path(dir_script,'utils','sensitivity_analysis_utils.R', fsep=fsep))

# Compute the hessian matrix 
# For each model, indicate if you want to compute the Hessian matrix.
# If noHess = TRUE for a given model, then the Hessian matrix will not be estimated.

noHess <- c(FALSE)

# ----------------------------------------------------------- 

#  3. Developing model M23.2_base1  ----
# ----------------------------------------------------------- #

#This directory has the final files from 2024.
Dir_2024="/Users/ingrid.spies/Documents/AI_PCOD/2024/Models/Sensitivity_Anal/November2024/M24_1_data/run"
Dir_M24_1_data="/Users/ingrid.spies/Documents/AI_PCOD/2025/Models/M24_1_data"

# Add the model directory to the saved variables


# For each SS input file, the following variable names will be used:
# Starter file :			 StartM23_0 
# Data file :			     DatM23_0 
# Control file :			 CtlM23_0 
# Forecast file :			 ForeM23_0 


# Do you want to copy the SS input files from the base model?
# This is useful if you are developing a model based on a base model that
# that did not exist when you set up the sensitivity analysis or if you already
# wrote a new SS input file for your new model and need to modify it (It ensures
# to start again from scratch and get the same
# basis of comparison.

#copy starter

#Do not need to run each time - just first time
#file.copy(from=file.path(Dir_2024,'starter.ss'),to=file.path(Dir_M24_1_data,'starter.ss'),overwrite=TRUE)
#file.copy(from=file.path(Dir_2024,'control.ss_new'),to=file.path(Dir_M24_1_data,'control.ss'),overwrite=TRUE)
#file.copy(from=file.path(Dir_2024,'forecast.ss'),to=file.path(Dir_M24_1_data,'forecast.ss'),overwrite=TRUE)
#file.copy(from=file.path(Dir_2024,'data_echo.ss_new'),to=file.path(Dir_M24_1_data,'data_echo.ss_new'),overwrite=TRUE)
#file.copy(from=file.path(Dir_2024,'wtatage.ss_new'),to=file.path(Dir_M24_1_data,'wtatage.ss'),overwrite=TRUE)


# 3.1  Work on the Starter file ----
# ======================= #
# Read in the file
StarterFile <- file.path(Dir_M24_1_data,'starter.ss', fsep = fsep)

Start_base <- SS_readstarter(
  file = StarterFile,
  verbose = TRUE
)

Start_base$init_values_src=1

# Save the starter file for the model
SS_writestarter(
  mylist =  Start_base,
  dir =  file.path(Dir_M24_1_data,"run"), 
  overwrite = TRUE,
  verbose = TRUE
)

#Reopen the starter file and make last line compatibility =1.
#Using prelease 3.30.24
#Start_base$#_Compatability=1


# Check file structure
 StarterFile <- file.path(Dir_M24_1_data, 'starter.ss')
  StartM23_1 <- SS_readstarter(
 file = StarterFile,
 verbose = TRUE
 )

# =======================
# 3.2  Work on the data file ----
# ======================= #


# Read in the file
DatFile <-file.path(Dir_M24_1_data,'data_echo.ss_new', fsep = fsep)
Dat_base <- SS_readdat_3.30(
  file = DatFile,
  verbose = TRUE,
  section = TRUE
)

Dat_base$endyr=2025
Dat_base$catch[35,4]=4676 #Final for 2024
Dat_base$catch[36,]=c(2025, 1, 1, 4934.67, 0.05) #Just a guess for 2025, also need to extrapolate to full year
Dat_base$CPUE[35,]=c(2025, 7, -2, 0, 0)
SS_writedat(
  datlist =  Dat_base,
  outfile = file.path(Dir_M24_1_data,'run/data_echo.ss_new',fsep=fsep),
  version = '3.30',
  overwrite = TRUE
)


# Check file structure
DatFile <- file.path(Dir_M24_1_data, 'run/data_echo.ss_new', fsep = fsep)
Dat_base <-
  SS_readdat_3.30(
    file = DatFile,
    verbose = TRUE,
    section = TRUE
  )

# =======================
# 3.3  Work on the control file ----
# ======================= #
# The SS_readctl_3.30() function needs the 'data_echo.ss_new' file to read the control file

Ctlfile <-file.path(Dir_M24_1_data,'control.ss', fsep = fsep)#This one was from M23.3 but then I added in the short matrix for TV growth

Ctl_base <- SS_readctl(
  file = Ctlfile,
  use_datlist = TRUE,
  datlist = file.path(Dir_M24_1_data,"data_echo.ss_new", fsep = fsep),
  verbose = TRUE
)

#redownload the code
#Ctl_base$Use_steep_init_equi
#Ctl_base$SR_parms
#SR_BH_steep

Ctl_base$Block_Design[[2]][2]=2025

Ctl_base$last_early_yr_nobias_adj=1974.3
Ctl_base$first_yr_fullbias_adj=1987.9
Ctl_base$last_yr_fullbias_adj=2017.7
Ctl_base$first_recent_yr_nobias_adj=2038.9
Ctl_base$max_bias_adj=0.9057

SS_writectl(
  ctllist =  Ctl_base,
  outfile = file.path(Dir_M24_1_data, 'run/control.ss_new', fsep = fsep),
  version = '3.30',
  overwrite = TRUE
)
#Needs file called control.ss_new rather than control.ss. 

# =======================
# 3.4  Work on the forecast file ----
# ======================= #

# Read in the file
ForeFile <- file.path(Dir_M24_1_data, 'forecast.ss', fsep = fsep)
Fore_base <-SS_readforecast(
  file = ForeFile,
  version = '3.30',
  verbose = T,
  readAll = T
)

#Way to specify how to do projections:
#For annually varying you can specify differently.
#New way to set it up to get more control f you need it.

# Save the forecast file for the model
SS_writeforecast(
  mylist =  Fore_base ,
  dir = file.path(Dir_M24_1_data,'run',fsep=fsep), 
  file = 'forecast.ss',
  writeAll = TRUE,
  verbose = TRUE,
  overwrite = TRUE
)

# Check file structure
 ForeFile <- file.path(Dir_M24_1_data, 'forecast.ss', fsep = fsep)
 ForeM24_1 <-SS_readforecast(
 file = ForeFile,
 version = '3.30',
 verbose = T,
 readAll = T
 )

# =======================


# *********************************************************** #
# 3.5  Run the new model using the new input files ----
# ======================= #
 # Path to the Executable folder 

Exe_path="/Users/ingrid.spies/Documents/AI_PCOD/2025/Executables/ss3_osx"
Exe_path="/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

Dir_M24_1_datar <- file.path(Dir_M24_1_data,  'run', fsep = fsep)

run(dir=Dir_M24_1_datar,exe=Exe_path,skipfinished=FALSE)  
#extras = ifelse(noHess[1], yes = '-nohess', no = '')

#compare the two
replist <- SS_output(
  dir = Dir_M24_1_datar,
  verbose = TRUE,
  printstats = TRUE
)

SS_plots(replist,
         dir = Dir_M24_1_datar,
         printfolder = 'plots'
)


replist$derived_quants[which(replist$derived_quants=="OFLCatch_2025"),]

replist$derived_quants[which(replist$derived_quants=="ForeCatch_2025"),]

replist$derived_quants[which(replist$derived_quants=="SSB_unfished"),]

replist$derived_quants[which(replist$derived_quants=="SSB_Virgin"),]

replist$derived_quants[which(replist$derived_quants=="SSB_Initial"),]

replist$derived_quants[which(replist$derived_quants=="SSB_2024"),]

replist$derived_quants[which(replist$derived_quants=="SSB_2025"),]$Value/replist$derived_quants[which(replist$derived_quants=="SSB_Virgin"),]$Value
replist$derived_quants[which(replist$derived_quants=="SSB_2025"),]$Value/replist$derived_quants[which(replist$derived_quants=="SSB_unfished"),]$Value


# You are ready to analyze the differences between the models
# considered in this sensitivity analysis.
# This can be done using the 4.6_Model_Outputs.R script.

#Do Francis weighting

# Create a temporary directory, feel free to change this location
mod_path <- file.path(Dir_M24_1_data, "Francis")
# Path to simple model in r4ss and copy files to mod_path
# copy model input files
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)
# copy over the Report file
file.copy(
  from = file.path(Dir_M24_1_data, 'run',"Report.sso"),
  to = file.path(mod_path, "Report.sso"), overwrite=TRUE
)
# copy comp report file
file.copy(
  from = file.path(Dir_M24_1_data,'run', "CompReport.sso"),
  to = file.path(mod_path, "CompReport.sso"), overwrite=TRUE
)

weight_table <- tune_comps(
  dir = mod_path,
  option = "none",
  verbose = FALSE
)

#run retro


Dir_M24_1_datar=file.path(Dir_M24_1_data, 'run',fsep = fsep)
retro(dir=Dir_M24_1_datar,years=0:-10,newsubdir="retrospectives",exe=Exe_path)

retroModelsM1 <- SSgetoutput(
  dirvec = file.path(Dir_M24_1_datar, "retrospectives", paste("retro", 0:-10, sep = ""))
)

retroSummary1 <- SSsummarize(retroModelsM1)
endyrvec <- retroSummary1[["endyrs"]] + c(0:-10)
SSmohnsrho(retroSummary1, endyrvec, 2024, verbose = TRUE)#This is the r4ss that Steve thinks is wrong

library(ss3diags)
SSplotRetro(
  retroSummary1,
  subplots = c("SSB", "F"),
  plot = TRUE,
  legendcex = 1.8,
  legendloc = "bottomleft",
  ptsize = 15,ylim=c(0,160000))

SSplotComparisons(subplot=1,retroSummary1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.2)
SSplotComparisons(subplot=4,retroSummary1,endyrvec = endyrvec,legendlabels = paste("Data", 0:-10, "years"),btarg=0.4, minbthresh=0.2)


Mohn_SS3diag=SShcbias(
  retroSummary1,
  quants = c("SSB", "F"),
  models = "all",
  endyrvec = "default",
  verbose = TRUE
)
write.csv(Mohn_SS3diag,file.path(Dir_M24_1_data,'run/retrospectives/Mohn.csv'))


SSrunstest()

library(ss3diags)
r4ss::SSgetoutput()$replist1

Dir_M24_1_data=file.path(dir_SensAnal, 'M24_1/run',fsep = fsep)
model1=r4ss::SSgetoutput(dirvec = file.path(Dir_M24_1_data))
SSrunstest(model1$replist1)

model1a=r4ss::SSsummarize(model1)
SSplotModelcomp(model1a)

model1b=r4ss::SS_output(file.path(Dir_M24_1_data))
SSdelta_M24_1=SSdeltaMVLN(model1b,plot=TRUE)



output=SS_output(dirvec = file.path(Dir_M24_1_data))

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
write.csv(Mohn_SS3diag,paste(Dir_M24_1_data,"/retrospectives5/Mohn_SS3diag_M1.csv",sep=""))


M24_1=r4ss::SS_output(file.path(Dir_M24_1_datar))
SSdelta_M24_1=SSdeltaMVLN(M24_1,plot=TRUE,summary=TRUE,verbose=TRUE)  #This is Kobe.

SSdeltaMVLN()$kb


##SS3diags better phase plot
SSplotKobe(
  retroSummary1)

,
  joint = TRUE,
  posterior = c("points", "kernel"),
#  ci.levels = c(0.5, 0.8, 0.95),
  path = TRUE,
  xlab = expression(SSB/SSB[MSY]),
  ylab = expression(F/F[MSY]),
  pt.cex = 0.8,
  ylim = NULL,
  xlim = NULL,
  fill = TRUE,
  legend = TRUE,
  legendpos = "right",
  legendcex = 0.7,
  legendruns = TRUE,
  yr.label = TRUE,
  yr.int = 5,
  verbose = TRUE
)

SSplotKobe(
  kb,
  joint = TRUE,
  year = NULL,
  posterior = c("points", "kernel"),
  ci.levels = c(0.5, 0.8, 0.95),
  path = TRUE,
  xlab = expression(SSB/SSB[MSY]),
  ylab = expression(F/F[MSY]),
  pt.cex = 0.8,
  ylim = NULL,
  xlim = NULL,
  fill = TRUE,
  legend = TRUE,
  legendpos = "right",
  legendcex = 0.7,
  legendruns = TRUE,
  yr.label = TRUE,
  yr.int = 5,
  verbose = TRUE
)


library(ss3diags)
library(r4ss)

mod1=SS_output(Dir_M24_1_data)
SSrunstest(mod1)
SSplotRunstest(mod1)
SSplotRunstable(mod1)



#Likelihood profile on R0
#To run this: in starter.ss change the AIPCOD21.ctl to control_modified.ss.
#In line 12 of starter.ss, do 0 # 0=use init values in control file; 1=use ss.par
#Also, line 19 of starter.ss: 1 # Include prior_like for non-estimated parameters (0,1) 
#in the starter.ss change the control model to control_modified.ss.
#Model1
R0.vec <- c(seq(10.4,12,.2))
Nprofile <- length(R0.vec)

mod_path <- file.path(Dir_M24_1_data,"run", "LikelihoodProfile_R0")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data,"run"), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <-"/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

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
  newctlfile = "control_modified.ss",
  profilevec=R0.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(10.4,12),
              ymax=20,
              # string = "SR_LN(R0)", # substring of profile parameter
              profile.label = 'ln(R0)',
              profile.string ="SR_LN", main="Model 24.1 likelihood profile over R0"
)


PinerPlot(profilesummary,plot=TRUE,ymax=5,xlim=c(10.4,12))

# read the output files (with names like Report1.sso, Report2.sso, etc.)
library(data.table)
params=vector(mode='list')
derived=vector(mode='list')

for(j in 1:Nprofile){
  
  params[[j]]<-data.table(profilemodels[[j]]$parameters)[Active_Cnt>0][,c(1:3,11)]
  params[[j]]$mortality <- R0.vec[j]
  derived[[j]]<-data.table(profilemodels[[j]]$derived)[Label%in%c('annF_Btgt','SSB_unfished','ForeCatch_2024','ForeCatch_2025','SSB_2024')][,c(1:3)]
  derived[[j]]$mortality <- R0.vec[j]
  derived[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  params[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  
  
}

all_pm23       <-do.call(rbind,params)
all_pm23$model <-"Model base 1"
all_dm23       <-do.call(rbind,derived)
all_dm23$model <-"Model base 1"


## you will need to select which parameters you want to present and change the numbers below.
ggplot(all_pm23[Label%in%unique(all_pm23$Label)[c(1:3,62:66)]],aes(x=Value,y=LL,color=mortality))+geom_point(size=2)+facet_wrap(~Label,scales='free_x',ncol=3)+theme_bw()+labs(y='Likelihood',x='Parameter values',color="R0 initial recruitment",title="Model base 1")+geom_path()



#now try LnQ_base_Srv
mod_path <- file.path(Dir_M24_1_data,"run", "LikelihoodProfile_LnQ")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data,"run"), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)
#add ss.par too
Exe_path <-"/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

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

SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "LnQ_base_Srv",xlim=c(-0.8,0.1),ymax=1,profile.label = 'LnQ_base_Srv')

PinerPlot(profilesummary,plot=TRUE,ymax=5)

# plot profile using summary created above
SSplotProfile(profilesummary,           # summary object
              xlim=c(0.9999,1),
              ymax=200, profile.label = 'LnQ_base_Srv')#
              # string = "SR_LN(R0)", # substring of profile parameter
              profile.label = 'LnQ_base_Srv',
              profile.string =Q.vec
)

#What else is in profile summary?
#Survey likelihood
plot(exp(seq(-1,1,.1)),profilesummary$likelihoods_by_fleet$Srv[which(profilesummary$likelihoods_by_fleet$Label=="Surv_like")],xlab="catchability Q",ylab="Survey data, survey likelihood",cex.lab=1.4,cex.axis=1.4,cex.main=1.4,main="Model Base 1 likelihood over Q, Survey likelihood")
#Fishery likelihood on catch 
plot(exp(seq(-1,1,.1)),profilesummary$likelihoods_by_fleet$FshComb[which(profilesummary$likelihoods_by_fleet$Label=="Catch_like")],xlab="catchability Q",ylab="Fishery data, catch likelihood",cex.lab=1.4,cex.axis=1.4,cex.main=1.4,main="Model Base 1 likelihood over Q, Fishery Catch likelihood")

# read the output files (with names like Report1.sso, Report2.sso, etc.)
library(data.table)
params=vector(mode='list')
derived=vector(mode='list')

for(j in 1:Nprofile){
  
  params[[j]]<-data.table(profilemodels[[j]]$parameters)[Active_Cnt>0][,c(1:3,11)]
  params[[j]]$mortality <- Q.vec[j]
  derived[[j]]<-data.table(profilemodels[[j]]$derived)[Label%in%c('annF_Btgt','SSB_unfished','ForeCatch_2024','ForeCatch_2025','SSB_2024')][,c(1:3)]
  derived[[j]]$mortality <- Q.vec[j]
  derived[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  params[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  
  
}

all_pm23       <-do.call(rbind,params)
all_pm23$model <-"Model base 1"
all_dm23       <-do.call(rbind,derived)
all_dm23$model <-"Model base 1"


## you will need to select which parameters you want to present and change the numbers below.
ggplot(all_pm23[Label%in%unique(all_pm23$Label)[c(1:4,63:67)]],aes(x=Value,y=LL,color=mortality))+geom_point(size=2)+facet_wrap(~Label,scales='free_x',ncol=3)+theme_bw()+labs(y='Likelihood',x='Parameter values',color="Ln(Q)",title="Model base 1")+geom_path()

#now try natural mortality
mod_path <- file.path(Dir_M24_1_data, "run/LikelihoodProfile_M")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <-"/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

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

M.vec <- c(seq(0.2,0.9,.05))
Nprofile <- length(M.vec)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "NatM_uniform_Fem_GP_1_BLK2repl_2016",
  profilevec=M.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

n <- length(profilemodels)

SSplotProfile(profilesummary,           # summary object
              xlim=c(0.2,0.9),
              ymax=20,
              # string = "NatM", # substring of profile parameter
              profile.label = 'Natural mortality 2016-2024 timeblock',
              profile.string ="NatM_uniform_Fem_GP_1_BLK2repl_2016", main="Model 24.1 likelihood profile over natural mortality 2016-2024 timeblock"
)
PinerPlot(profilesummary,profile.string = "NatM",plot=TRUE,ymax=50,profile.label = 'NatM')

# read the output files (with names like Report1.sso, Report2.sso, etc.)
library(data.table)
params=vector(mode='list')
derived=vector(mode='list')

for(j in 1:Nprofile){
  
  params[[j]]<-data.table(profilemodels[[j]]$parameters)[Active_Cnt>0][,c(1:3,11)]
  params[[j]]$mortality <- M.vec[j]
  derived[[j]]<-data.table(profilemodels[[j]]$derived)[Label%in%c('annF_Btgt','SSB_unfished','ForeCatch_2024','ForeCatch_2025','SSB_2024')][,c(1:3)]
  derived[[j]]$mortality <- M.vec[j]
  derived[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  params[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  
  
}

all_pm23       <-do.call(rbind,params)
all_pm23$model <-"Model base 1"
all_dm23       <-do.call(rbind,derived)
all_dm23$model <-"Model base 1"




## you will need to select which parameters you want to present and change the numbers below.
ggplot(all_pm23[Label%in%unique(all_pm23$Label)[c(1:4,63:67)]],aes(x=Value,y=LL,color=mortality))+geom_point(size=2)+facet_wrap(~Label,scales='free_x',ncol=3)+theme_bw()+labs(y='Likelihood',x='Parameter values',color="Natural Mortality",title="Model base 1")+geom_path()
#ggplot(all_dm23,aes(x=Value,y=LL,color=exp(mortality)))+geom_point()+facet_wrap(~Label,scales='free_x')+theme_bw()+labs(y='Likelihood',x='Value',color="Natural Mortality",title="Model base 1")+geom_path()
#dev.off()

#now try CV of young fish
mod_path <- file.path(Dir_M24_1_data, "run/LikelihoodProfile_CVyoung")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <-"/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

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

CV.vec <- c(seq(0.1,0.3,.02))
Nprofile <- length(CV.vec)

ModProfile <- r4ss::profile(
  exe = Exe_path,
  dir = mod_path, # directory
  string = "CV_young_Fem_GP_1",
  profilevec=CV.vec
)

profilemodels <- SSgetoutput(dirvec=mod_path, keyvec=1:Nprofile)
profilesummary <- SSsummarize(profilemodels)

SSplotProfile(profilesummary,           # summary object
              xlim=c(0.1,0.3),
              ymax=20,
              # string = "NatM", # substring of profile parameter
              profile.label = 'CV_young_Fem_GP_1',
              profile.string ="CV_young_Fem_GP_1", main="Model 24.1 likelihood profile over CV_young_Fem_GP_1",cex.axis=2,cex.lab=1.5,cex.main=3
)

#now try fish peak parameter1

mod_path <- file.path(Dir_M24_1_data, "run/LikelihoodProfile_Fishsel1")
#copy files to a Likelihood Profile folder
copy_SS_inputs(dir.old = file.path(Dir_M24_1_data,'run'), dir.new = mod_path, verbose = FALSE,overwrite=TRUE)

Exe_path <-"/Users/ingrid.spies/Documents/AI_PCOD/2024/Executables/SS_V3_30_21/ss_osx"

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

Peak.vec <- c(seq(55,110,5))
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


SSplotProfile(profilesummary,plot=TRUE,models="all",profile.string = "Size_DblN_peak_FshComb",xlim=c(55,85),profile.label = 'Size_DblN_peak_FshComb',ymax=5000)

PinerPlot(profilesummary,profile.string = "Size_DblN_peak_FshComb",plot=TRUE,ymax=5000,profile.label = 'Size_DblN_peak_FshComb(1)',xlim=c(50,80))

# read the output files (with names like Report1.sso, Report2.sso, etc.)
library(data.table)
params=vector(mode='list')
derived=vector(mode='list')

for(j in 1:Nprofile){
  
  params[[j]]<-data.table(profilemodels[[j]]$parameters)[Active_Cnt>0][,c(1:3,11)]
  params[[j]]$mortality <- Peak.vec[j]
  derived[[j]]<-data.table(profilemodels[[j]]$derived)[Label%in%c('annF_Btgt','SSB_unfished','ForeCatch_2024','ForeCatch_2025','SSB_2024')][,c(1:3)]
  derived[[j]]$mortality <- Peak.vec[j]
  derived[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  params[[j]]$LL<-profilemodels[[j]]$likelihoods_used$values[1]
  
  
}

all_pm23       <-do.call(rbind,params)
all_pm23$model <-"Model base 1"
all_dm23       <-do.call(rbind,derived)
all_dm23$model <-"Model base 1"


## you will need to select which parameters you want to present and change the numbers below.
ggplot(all_pm23[Label%in%unique(all_pm23$Label)[c(1:4,63:67)]],aes(x=Value,y=LL,color=mortality))+geom_point(size=2)+facet_wrap(~Label,scales='free_x',ncol=3)+theme_bw()+labs(y='Likelihood',x='Parameter values',color="Size_DblN_peak_FshComb(1)",title="Model base 1")+geom_path()
#ggplot(all_dm23,aes(x=Value,y=LL,color=exp(mortality)))+geom_point()+facet_wrap(~Label,scales='free_x')+theme_bw()+labs(y='Likelihood',x='Value',color="Natural Mortality",title="Model base 1")+geom_path()
#dev.off()


#Do 

# Path to the base model (M23_0a) repertory
Dir_M24_0_datar=file.path(dir_SensAnal, 'November2024/M24_0_data/run',fsep = fsep)
Dir_M24_1_datar=file.path(dir_SensAnal, 'November2024/M24_1_data/run',fsep = fsep)
Dir_M24_1a_datar=file.path(dir_SensAnal, 'November2024/M24_1a_data/run',fsep = fsep)

SensiMod <- SSgetoutput(dirvec = c(Dir_M24_1_datar,Dir_M24_1a_datar,Dir_M24_0_datar))

# Rename the list holding the report files from each model
names(SensiMod)
names(SensiMod) <- c( 'Model24.1',
  'Model24.1a','Model24.0')

# summarize the results
Version_Summary <- SSsummarize(SensiMod)

SSplotComparisons(
  Version_Summary,
  # print = TRUE,
  pdf = TRUE,
  plotdir = file.path(Dir_M24_1_data, fsep = "/"),
  legendlabels = c('M24.1',
    'M24.1a','M24.0'),btarg=0.40, minbthresh=0.2
)


SSplotComparisons(Version_Summary,pdf = TRUE,plotdir = file.path(Dir_M24_1_data, fsep = "/"),legendlabels = c('M24_1','M24_1a','M24.0'),btarg=0.40, minbthresh=0.2)

SStableComparisons(Version_Summary)

thingnames <- c(
  "Recr_Virgin", "steep", "NatM", "Linf",
  "SmryBio_unfished",
  "SSB_Virg", "SSB_2024",
  "Bratio_2024", "SPRratio_2024",
  "Bratio_2025", "SPRratio_2025", "Ret_Catch_MSY", "Dead_Catch_MSY",
  "OFLCatch_2024","SR_LN(R0)",
  "LnQ","Size_DblN_peak_FshComb","Size_DblN_top_logit_FshComb", 
  "Size_DblN_ascend_se_FshComb(1)","Size_DblN_peak_Srv(2)", "Size_DblN_ascend_se_Srv(2)" )

likenames <- c(
  "TOTAL", "Survey", "Length_comp", "Age_comp",
  "Discard", "Mean_body_wt", "Recruitment", "priors"
)

tmp <- purrr::transpose(SensiMod)$parameters %>%
  purrr::map_df(~dplyr::as_tibble(.x), .id = 'Model') %>%
  dplyr::select(Model, Label, Value, Phase, Min, Max, Init,
                Gradient, Pr_type, Prior, Pr_SD, Pr_Like,
                LCI95 = `Value-1.96*SD`, UCI95 = `Value+1.96*SD`)

tmp %>%
  readr::write_csv(paste(Dir_M24_1_data, 'Update_Data_comparison_table_all_params.csv', sep = fsep))

tmp %>%
  dplyr::filter(grepl('LnQ|R0', Label)) %>%
  tidyr::pivot_wider(id_cols = c(Label, Phase), names_from = Model, values_from = Value) %>%
  readr::write_csv(paste(Dir_M24_1_data, 'Update_Data_comparison_table_lnQ_SRlnR0.csv', sep = fsep))

out <- SStableComparisons(Version_Summary,
                          names = thingnames,
                          likenames = likenames)
names(out) <- c('Label','M24_1','M24_1a','M24.0')

out %>%readr::write_csv(paste(Dir_M24_1_data,'Update_Data_comparison_table_likelihoods_and_brps.csv', sep = fsep))




#What if I take the survey length frequencies and multiply by maturity at age?

plot(c(1:117),Dat_base$lencomp[46,7:123])
plot(c(1:117),Dat_base$lencomp[43,7:123])

ggplot(seq(19,116,1),Dat_base$agecomp[,10])

yrs=as.numeric(names(table(Dat_base$agecomp$Yr)))


#What is mean length by yr and age directly from age data?

age1=matrix(0,13,13)#down is years, across is ages
for(i in 1:length(yrs)){
data=Dat_base$agecomp[which(Dat_base$agecomp$Yr==yrs[i]&Dat_base$agecomp$Lbin_lo>0),]
for(j in 1:13){
a1=(data$Lbin_lo*data[,9+j])
age1[j,i]=sum(a1)/length(a1[which(a1>0)])
}
}



plot(data$Lbin_lo,data$a1)
ggplot(data=Dat_base$agecomp[which(Dat_base$agecomp$Yr==1991),])+geom_line(aes(x=as.numericLbin_lo,y=a2,col=as.factor(Yr)))

#combine and weight by the samgeom_histogram()#combine and weight by the sample size.
Dat_base$lencomp[34:46,7:123]



Dat_base$lencomp[34:46,6]
yrs=Dat_base$lencomp[34:46,1]

surveylengths=matrix(0,13,117)
for(i in 34:46){
  surveylengths[(i-33),]=as.numeric((Dat_base$lencomp[i,6]*Dat_base$lencomp[i,7:123]))
}

#get fishery age comps
fisherylengths=matrix(0,33,117)
for(i in 1:33){
  fisherylengths[i,]=as.numeric((Dat_base$lencomp[i,6]*Dat_base$lencomp[i,7:123]))
}

plot(c(1:117),colSums(surveylengths)/sum(Dat_base$lencomp[34:46,6]),ylab="Relative Frequency",xlab="Size (cm)",cex.lab=1.4,cex.axis=1.4,lty=1)
#lines(matlen*colSums(surveylengths)/sum(Dat_base$lencomp[34:46,6]),col="green",lwd=4)
lines(c(1:117),colSums(fisherylengths)/sum(fisherylengths),col="blue",lwd=3)

legend("topleft",c("Survey","Fishery"),pch=c(1,-1),lwd=c(-3,3),col=c("black","blue"),bty="n",cex=1.6)

A=54.9
B=-0.1472
len=seq(1,117,1)

plot(1/(1+(exp(B*(len-A)))))

matlen=(1/(1+(exp(B*(len-A)))))


SSplotKobe(
  kb,
  joint = TRUE,
  year = NULL,
  posterior = c("points", "kernel"),
  ci.levels = c(0.5, 0.8, 0.95),
  path = TRUE,
  xlab = expression(SSB/SSB[MSY]),
  ylab = expression(F/F[MSY]),
  pt.cex = 0.8,
  ylim = NULL,
  xlim = NULL,
  fill = TRUE,
  legend = TRUE,
  legendpos = "right",
  legendcex = 0.7,
  legendruns = TRUE,
  yr.label = TRUE,
  yr.int = 5,
  verbose = TRUE
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


