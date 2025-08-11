
## ALASKA PROJECTION SCENARIOS FOR STOCK SYNTHESIS 3 
## Version October 7, 2021
## Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 
## 
##
## In the starter.ss file you should change it to read from the converged .par file 
##   1 # 0=use init values in control file; 1=use ss.par
## 
## Assumes you already have the forecast parameters already specified appropriately in the forecast.ss for scenario 1, 
## Make sure there is no catch or F already specified in the forecast file.
##
## DIR is the model directory
## CYR is the model current year, SYR is the start year for the model, SEXES is the number of sexes in model, fleets= the fleet number in SS for your fisheries,
## Scenario2 indicates whether you wish to have a different catch for scenario 2 (1= FmaxABC,2= F as S2_F, 3 = specified catch from a 
## formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
## that differ from Fmaxabc
## with columns "Year,Seas,Fleet,Catch_or_F"
## do_fig whether to plot figures
##
##

#SS3 models
Dir_M23_0a=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0a/M23_0a")
Dir_M23_0ar=file.path(Dir_M23_0a,'run')
Dir_M23_0b=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0b/M23_0b")
Dir_M23_0br=file.path(Dir_M23_0b,'run')
Dir_M23_0c=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c")
Dir_M23_0cr=file.path(Dir_M23_0c,'run')
Dir_M23_0cM=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c_Mthru2036/M23_0cM")
Dir_M23_0cr=file.path(Dir_M23_0cM,'run')

M23_0 <- SS_output(Dir_M23_0ar)

M23_1 <- SS_output(Dir_M23_0br)

M23_2 <- SS_output(Dir_M23_0cr)
M23_2M <- SS_output(Dir_M23_0cMr)

Do_AK_Scenarios<-function(DIR,CYR=2023,SYR=1991,FCASTY=13,SEXES=1,FLEETS=1,Scenario2=1,S2_F=0.4,do_fig=TRUE){
  
  require(r4ss)
  require(data.table)
  require(ggplot2)
  require(R.utils)
  
  setwd(DIR) ## folder with converged model setwd(Dir_M23_0ar)
  
  scenario_1 <- SS_readforecast(file = "forecast.ss")
  
  copyDirectory(getwd(),paste0(getwd(),"/scenario_1"),recursive=FALSE)
  scenario_1$Btarget   <- 0.4
  scenario_1$SPRtarget <- 0.4
  scenario_1$Flimitfraction <- 1.0
  scenario_1$Bmark_years[1:2]=c(2017,2023)#uses base M 0.404237
  #scenario_1$Bmark_years[1:2]=c(2004,2023)#
  #scenario_1$Fcast_years=rep(c(2021,2023),3)
  SS_writeforecast(scenario_1, dir = paste0(getwd(),"/scenario_1"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  
  scenario_2 <- scenario_1
  copyDirectory(getwd(),paste0(getwd(),"/scenario_2"),recursive=FALSE)
  
  if(Scenario2==2){
    scenario_2$SPRtarget <- S2_F
  }
  
  if(Scenario2==3){
    scenario_2$ForeCatch <- read.csv("Scenario2_catch.csv",header=T)
  }
  SS_writeforecast(scenario_2, dir = paste0(getwd(),"/scenario_2"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  ## Average f for previous 5 years  cyear=year(Sys.Date()) 
  scenario_3<-scenario_1
  copyDirectory(getwd(),paste0(getwd(),"/scenario_3"),recursive=FALSE)
  scenario_3$Forecast<-4
  scenario_3$Fcast_years [c(3,4)]<-c(CYR-5, CYR-1)
  
  SS_writeforecast(scenario_3, dir = paste0(getwd(),"/scenario_3"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  #F75%
  scenario_4<-scenario_1
  copyDirectory(getwd(),paste0(getwd(),"/scenario_4"),recursive=FALSE)
  scenario_4$Btarget <- 0.75
  scenario_4$SPRtarget <- 0.75
  
  #F1=mods1$derived_quants$Value[which(M23_2$derived_quants$Label=="F_2018")]
  #F2=mods1$derived_quants$Value[which(M23_2$derived_quants$Label=="F_2019")]
  #F3=mods1$derived_quants$Value[which(M23_2$derived_quants$Label=="F_2020")]
  #F4=mods1$derived_quants$Value[which(M23_2$derived_quants$Label=="F_2021")]
  #F5=mods1$derived_quants$Value[which(M23_2$derived_quants$Label=="F_2022")]
  
  #Favg=mean(F1,F2,F3,F4,F5)
  ## must enter a 0 in for all fisheries
  #catch <- expand.grid(Year=c((CYR+1):(CYR+FCASTY)),Seas=1,Fleet=FLEETS,Catch_or_F=Favg)
  #names(catch)<-names(scenario_4$ForeCatch)
  #scenario_4$ForeCatch <- rbind(scenario_4$ForeCatch,catch)
  SS_writeforecast(scenario_4, dir = paste0(getwd(),"/scenario_4"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  #F=0
  scenario_5<-scenario_1
  copyDirectory(getwd(),paste0(getwd(),"/scenario_5"),recursive=FALSE)
  ## must enter a 0 in for all fisheries
  catch <- expand.grid(Year=c((CYR+1):(CYR+FCASTY)),Seas=1,Fleet=FLEETS,Catch_or_F=0)
  names(catch)<-names(scenario_5$ForeCatch)
  scenario_5$ForeCatch <- rbind(scenario_5$ForeCatch,catch)
  SS_writeforecast(scenario_5, dir = paste0(getwd(),"/scenario_5"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  
  ## Fofl = F35% for all years
  scenario_6<-scenario_1
  copyDirectory(getwd(),paste0(getwd(),"/scenario_6"),recursive=FALSE)
  scenario_6$Btarget   <- 0.35
  scenario_6$SPRtarget <- 0.35
  #scenario_4$BforconstantF <- 0.4
  scenario_6$Flimitfraction <- 1.0
  SS_writeforecast(scenario_6, dir = paste0(getwd(),"/scenario_6"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  ## F40%=Fabc for 20&21 and Fofl for all further years
  
  scenario_7<-scenario_6
  copyDirectory(getwd(),paste0(getwd(),"/scenario_7"),recursive=FALSE)
  x<-SS_output(dir=paste0(getwd(),"/scenario_1"))
  scenario_7$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+2))
  SS_writeforecast(scenario_7, dir = paste0(getwd(),"/scenario_7"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  ## for calculating OFL values for Yr+2 F40% for 20 & Fofl for all further years
  scenario_8<-scenario_6
  copyDirectory(getwd(),paste0(getwd(),"/scenario_8"),recursive=FALSE)
  x<-SS_output(dir=paste0(getwd(),"/scenario_1"))
  scenario_8$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+1))
  SS_writeforecast(scenario_8, dir = paste0(getwd(),"/scenario_8"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  ## run all the scenarios
  scen<-c("scenario_1","scenario_2","scenario_3","scenario_4","scenario_5","scenario_6","scenario_7","scenario_8")
  
  Exe_path <- "/Users/ingrid.spies/Documents/AI_PCOD/Executables/SS_V3_30_21/ss_osx"

  for(i in 1:8){
    file.copy(Exe_path,file.path(DIR,paste0(scen[i])))
    setwd(paste0(DIR,"/",scen[i]))
    system("./ss_osx")
  }
  sex=2
#  if(SEXES>1) sex=1
  setwd(DIR)
  mods1<-SSgetoutput(dirvec=scen[1:8])
  
  
  summ<-vector("list",length=8)
  Pcatch<-vector("list",length=8)
  EYR<- CYR+FCASTY
  yr1<- EYR-SYR+3
  
  for(i in 1:8){
    summ[[i]]<-data.table(Yr=SYR:EYR,TOT=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_all,SUMM=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_smry,SSB=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$SpawnBio/sex,std=data.table(mods1[[i]]$stdtable)[name%like%"SSB"][3:yr1,]$std/sex,F=data.table(mods1[[i]]$sprseries)[Yr%in%c(SYR:EYR)]$F_report,Catch=data.table(mods1[[i]]$sprseries)[Yr%in%c(SYR:EYR)]$Enc_Catch,SSB_unfished=data.table(mods1[[i]]$derived_quants)[Label=="SSB_unfished"]$Value/sex,model=scen[i])
    Pcatch[[i]]<-data.table(Yr=(CYR+1):EYR,Catch=data.table(mods1[[i]]$sprseries)[Yr%in%c((CYR+1):EYR)]$Enc_Catch,Catch_std=data.table(mods1[[i]]$stdtable)[name%like%"ForeCatch_"]$std[2:FCASTY+1], model=scen[i])
    
  }
  
  summ8<-data.table(Yr=SYR:EYR,TOT=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_all,SUMM=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_smry,SSB=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$SpawnBio/sex,std=data.table(mods1[[8]]$stdtable)[name%like%"SSB"][3:yr1,]$std/sex,F=data.table(mods1[[8]]$sprseries)[Yr%in%c(SYR:EYR)]$F_report,Catch=data.table(mods1[[8]]$sprseries)[Yr%in%c(SYR:EYR)]$Enc_Catch,SSB_unfished=data.table(mods1[[8]]$derived_quants)[Label=="SSB_unfished"]$Value/sex,model=scen[8])
  Pcatch8<-data.table(Yr=(CYR+1):EYR,Catch=data.table(mods1[[8]]$sprseries)[Yr%in%c((CYR+1):EYR)]$Enc_Catch,Catch_std=data.table(mods1[[8]]$stdtable)[name%like%"ForeCatch_"]$std[1:FCASTY], model=scen[8])
  ## Calculate 2 year projections for catch and F
  SB100=summ[[1]][Yr==CYR+1]$SSB_unfished
  F40_1=summ[[1]][Yr==CYR+1]$F
  F35_1=summ[[6]][Yr==CYR+1]$F
  catchABC_1=Pcatch[[1]][Yr==CYR+1]$Catch
  catchOFL_1=Pcatch[[6]][Yr==CYR+1]$Catch
  
  F40_2=summ[[1]][Yr==CYR+2]$F
  F35_2=summ8[Yr==CYR+2]$F
  catchABC_2=Pcatch[[1]][Yr==CYR+2]$Catch
  catchOFL_2=Pcatch8[Yr==CYR+2]$Catch
  SSB_1<-summ[[1]][Yr==CYR+1]$SSB
  SSB_2<-summ[[1]][Yr==CYR+2]$SSB
  
  Two_Year=data.table(Yr=c((CYR+1):(CYR+2)),SSB=c(SSB_1,SSB_2),SSB_PER=c(SSB_1/SB100,SSB_2/SB100),SB100=c(SB100,SB100),SB40=c(SB100*0.4,SB100*0.4),SB35=c(SB100*0.35,SB100*0.35),F40=c(F40_1,F40_2),F35=c(F35_1,F35_2),C_ABC=c(catchABC_1,catchABC_2),C_OFL=c(catchOFL_1,catchOFL_2))
  
  ## rbind vectors into tables
  summ=do.call(rbind,summ)
  Pcatch=do.call(rbind,Pcatch)
  
  output=list(SSB=summ,CATCH=Pcatch,Two_year=Two_Year)
  
  ## create scenario tables for document
  BC=vector("list")
  BC$Catch<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="Catch") 
  BC$F<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="F")
  BC$SSB<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="SSB")
  
  output$Tables<-BC
  
  if(do_fig){
    
    x<-SS_output(file.path(DIR,'scenario_1'))
    SSB_unfished<-data.table(x$derived_quants)[Label=="SSB_unfished"]$Value/sex
    
    y<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SSB_unfished*0.4,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB40%")
    y1<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SSB_unfished*0.35,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB35%")
    y2<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SSB_unfished*0.2,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB20%")
    summ2<-rbind(y,y1,y2,summ)
    
    summ2$model<-factor(summ2$model,levels=unique(summ2$model))
    summ2$UCI<-summ2$SSB+1.96*summ2$std
    summ2$LCI<-summ2$SSB-1.96*summ2$std
    summ2[LCI<0]$LCI=0
    
    
    y<-data.table(Yr=c(CYR+1:EYR),Catch=Pcatch[model=="scenario_1" & Yr==EYR]$Catch,Catch_std=0,model="Catch Fmaxabc")
    y1<-data.table(Yr=c(CYR+1:EYR),Catch=Pcatch[model=="scenario_6" & Yr==EYR]$Catch,Catch_std=0,model="Catch Fofl")
    Pcatch2<-rbind(y,y1,Pcatch)
    
    Pcatch2$model<-factor(Pcatch2$model,levels=unique(Pcatch2$model))
    Pcatch2$UCI<-Pcatch2$Catch+1.96*Pcatch2$Catch_std
    Pcatch2$LCI<-Pcatch2$Catch-1.96*Pcatch2$Catch_std
    Pcatch2[LCI<0]$LCI=0
    
    ##SSB_Figures max(summ2$UCI)
    SS_ALL<-ggplot(summ2[model%in%unique(summ2$model)[1:10]],aes(x=Yr,y=SSB,color=model,linetype=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,0.6e5))+
      scale_linetype_manual(values=c(rep(1,3),2:8),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",2:6,8,9),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections")
    #max(summ2$UCI)
    SS_1<-ggplot(summ2[model%in%unique(summ2$model)[1:4]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,1e5),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),2),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",2),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",2),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections MaxFABC")
    #max(summ2$UCI)
    SS_2<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,5)]],aes(x=Yr,y=SSB,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,1e5),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),3),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",3),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",3),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario_2")
    #max(summ2$UCI)
    SS_3<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,6)]],aes(x=Yr,y=SSB,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,1e5),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),4),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",4),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",4),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 3 - Average F")
    #max(summ2$UCI)
    SS_4<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,7)]],aes(x=Yr,y=SSB,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,2e5),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),5),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",5),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",5),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 4 - F75%")
    
    SS_5<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,8)]],aes(x=Yr,y=SSB,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),6),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",6),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",6),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 5 - No catch")   
    
    SS_6<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,9,10)]],aes(x=Yr,y=SSB,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,76000),x=c(CYR-1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,3),2:8),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange","red",8,9),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange","red",8,9),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenarios 6 and 7")
    
    Figs_SSB<-list(SS_ALL,SS_1,SS_2,SS_3,SS_4,SS_5,SS_6)
    ## Catch Figures
    C_ALL<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[1:9]],aes(x=Yr,y=Catch,color=model,linetype=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,40000),x=c(CYR+1,EYR))+
      scale_linetype_manual(values=c(rep(1,2),2:8),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",2:6,8,9),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections")
    
    C_1<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[1:3]],aes(x=Yr,y=Catch,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,2),2),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange",2),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",2),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections MaxFABC")
    
    C_2<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,4)]],aes(x=Yr,y=Catch,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,2),3),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange",3),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",3),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 2")
    
    C_3<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,5)]],aes(x=Yr,y=Catch,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)*1.25),x=c(CYR+1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,2),5),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange",5),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",5),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 3 - Average F")
    
    C_4<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,6)]],aes(x=Yr,y=Catch,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,100000),x=c(CYR+1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,2),4),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange",4),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",4),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 4 - F75%")
    
    C_6<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,8,9)]],aes(x=Yr,y=Catch,color=model,linetype=model,fill=model))+
      geom_line()+theme_bw(base_size=16)+lims(y=c(0,150000),x=c(CYR+1,EYR))+
      geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
      scale_linetype_manual(values=c(rep(1,2),8,9),name="Scenarios")+
      scale_fill_manual(values=c("dark green","orange",8,9),name="Scenarios")+
      scale_color_manual(values=c("dark green","orange",8,9),name="Scenarios")+
      scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenarios 6 and 7")
    
    Figs_Catch<-list(C_ALL,C_1,C_2,C_3,C_4,C_6)
    output$FIGS=list(Figs_SSB,Figs_Catch)
  }
  
  return(output)
}


#profiles_19.12<-Do_AK_Scenarios(DIR="C:/WORKING_FOLDER/EBS_PCOD/2022_ASSESSMENT/NOVEMBER_MODELS/GRANT_MODELS/Model19_12/PROJ",CYR=2022,SYR=1977,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)
Dir_M23_0a=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0a/M23_0a")
Dir_M23_0ar=file.path(Dir_M23_0a,'run')
Dir_M23_0b=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0b/M23_0b")
Dir_M23_0br=file.path(Dir_M23_0b,'run')
Dir_M23_0c=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c")
Dir_M23_0cr=file.path(Dir_M23_0c,'run')

profilesM23_0a<-Do_AK_Scenarios(DIR=Dir_M23_0ar,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)
AK=file.path(Dir_M23_0ar,'AK_Scenarios')
setwd(AK)
write.csv(profilesM23_0a$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_0a$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_0a$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_0a$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_0a$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_0a$Two_year,"Two_Year_refpts_Model2.csv")

profilesM23_0b<-Do_AK_Scenarios(DIR=Dir_M23_0br,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)
AK=file.path(Dir_M23_0br,'AK_Scenarios')
setwd(AK)
write.csv(profilesM23_0b$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_0b$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_0b$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_0b$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_0b$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_0b$Two_year,"Two_Year_refpts_Model2.csv")
# profiles<-Do_AK_Scenarios(DIR="Z:/Steve WorkingFolders/BS_COD",CYR=2018,SYR=1977,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)
# profiles<-Do_AK_Scenarios(DIR="C:/WORKING_FOLDER/Turbot",CYR=2020,SYR=1977,SEXES=2,FLEETS=c(1,2),Scenario2=1,S2_F=0.4,do_fig=TRUE)

profilesM23_0c<-Do_AK_Scenarios(DIR=Dir_M23_0cr,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

AK=file.path(Dir_M23_0cr,'AK_Scenarios')
AK2=file.path(Dir_M23_0cr,'AK_Scenarios_bmark_1991_2023')
AK2=file.path(Dir_M23_0cr,'AK_Scenarios_bmark_1991_2023_act2019')
setwd(AK2)
write.csv(profilesM23_0c$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_0c$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_0c$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_0c$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_0c$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_0c$Two_year,"Two_Year_refpts_Model2.csv")
# profiles<-Do_AK_Scenarios(DIR="Z:/Steve WorkingFolders/BS_COD",CYR=2018,SYR=1977,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)
# profiles<-Do_AK_Scenarios(DIR="C:/WORKING_FOLDER/Turbot",CYR=2020,SYR=1977,SEXES=2,FLEETS=c(1,2),Scenario2=1,S2_F=0.4,do_fig=TRUE)

#You can double check in the report files just to make sure they match what is coming out. the OFL for 2025 should be the futureCatch_2025 in scenario 8 report file

profilesM23_0Mc<-Do_AK_Scenarios(DIR=Dir_M23_0cMr,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

AK2=file.path(Dir_M23_0cMr,'AK_Scenarios_bmark_1991_2023')
setwd(AK2)
write.csv(profilesM23_0Mc$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_0Mc$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_0Mc$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_0Mc$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_0Mc$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_0Mc$Two_year,"Two_Year_refpts_Model2.csv")
# profiles<-Do_AK_Scenarios(DIR="Z:/Steve WorkingFolders/BS_COD",CYR=2018,SYR=1977,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

#MOdel 23.2
Dir_M23_0c=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c")
Dir_M23_0cr=file.path(Dir_M23_0c,'run')
M23_2 <- SS_output(Dir_M23_0cr)
M23_2$derived_quants[1,]
#SSB_Virgin SSB_Virgin 194596 23824.9               NA      NA
M23_2$derived_quants[which(M23_2$derived_quants$Label=="SSB_unfished"),]
#SSB_unfished SSB_unfished 158869 17670.9               NA      NA
cbind(M23_2$Natural_Mortality$Yr,M23_2$Natural_Mortality[,23])
#Uses based M 0.4035100 through 2038

#Model 23.2M
Dir_M23_0cM=("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c_Mthru2036/M23_0cM")
Dir_M23_0cr=file.path(Dir_M23_0cM,'run')
M23_2M <- SS_output(Dir_M23_0cMr)
M23_2M$derived_quants[1,]
#SSB_Virgin SSB_Virgin 194596 23824.9               NA      NA
M23_2M$derived_quants[which(M23_2M$derived_quants$Label=="SSB_unfished"),]#=                    Label   Value  StdDev (Val-1.0)/Stddev CumNorm
#SSB_unfished SSB_unfished 58221.2 13716.7               NA      NA
cbind(M23_2M$Natural_Mortality$Yr,M23_2M$Natural_Mortality[,23])
#uses 0.487377 through 2038

Dir_M23_2r <- "/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.2r"
Exe_path <- "/Users/ingrid.spies/Documents/AI_PCOD/Executables/SS_V3_30_21/ss_osx"

Dir_M23_2rr <- file.path(Dir_M23_2r,  'run', fsep = fsep)

copy_SS_inputs(
  dir.old = Dir_M23_2r,
  dir.new = Dir_M23_2rr,overwrite=TRUE
)

run(dir=Dir_M23_2rr,exe=Exe_path,
    skipfinished=FALSE,
    extras = ifelse(noHess[1], yes = '-nohess', no = ''))

#This is mean M mean(c(rep(0.319643,26),rep(0.487376,7))) #1991-2023
0.3552227

#TRy to get figures
DIR=Dir_M23_0cr;CYR=2023;SYR=1991;SEXES=1;FLEETS=c(1);Scenario2=1;S2_F=0.4;do_fig=TRUE;FCASTY=13


profilesM23_0Mc<-Do_AK_Scenarios(DIR=Dir_M23_0cMr,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

#test
profilesM23_2r<-Do_AK_Scenarios(DIR=Dir_M23_2rr,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

AK2=file.path(Dir_M23_2rr,'AK_Scenarios')
setwd(AK2)
write.csv(profilesM23_2r$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_2r$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_2r$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_2r$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_2r$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_2r$Two_year,"Two_Year_refpts_Model2.csv")

#M23.2
Dir_M23_2="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.2"
Dir_M23_2R="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.2/run"

#***************#
Dir_M23_2R <- "/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.2_19912023"

Exe_path <- "/Users/ingrid.spies/Documents/AI_PCOD/Executables/SS_V3_30_21/ss_osx"

Dir_M23_2RR <- file.path(Dir_M23_2R,  'run', fsep = fsep)

copy_SS_inputs(
  dir.old = Dir_M23_2R,
  dir.new = Dir_M23_2RR,overwrite=TRUE
)

run(dir=Dir_M23_2RR,exe=Exe_path,
    skipfinished=FALSE,
    extras = ifelse(noHess[1], yes = '-nohess', no = ''))

profilesM23_2R<-Do_AK_Scenarios(DIR=Dir_M23_2RR,CYR=2023,SYR=1991,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

AK2=file.path(Dir_M23_2RR,'AK_Scenarios')
setwd(AK2)
write.csv(profilesM23_2R$SSB,"SSB_scenarios.csv")
write.csv(profilesM23_2R$CATCH,"CATCH_scenarios.csv")
write.csv(profilesM23_2R$Tables$Catch,"TABLES_scenarios_Catch.csv")
write.csv(profilesM23_2R$Tables$F,"TABLES_scenarios_F.csv")
write.csv(profilesM23_2R$Tables$SSB,"TABLES_scenarios_SSB.csv")
write.csv(profilesM23_2R$Two_year,"Two_Year_refpts_Model2.csv")



#Profiles.RData

#med
Med="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_MedM"
setwd(Med)

write.csv(profiles_MedM$Two_year,"Two_Year_refpts_Model2.csv")
write.csv(profiles_MedM$CATCH,"CATCH_scenarios.csv")
write.csv(profiles_MedM$SSB,"SSB_scenarios.csv")
write.csv(profiles_MedM$Tables,"TABLES_scenarios_Catch.csv")
write.csv(profiles_MedM$FIGS,"Figs.pdf")


#high
setwd("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_highM")

high="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_highM"
setwd(high)

write.csv(profiles_highM$Two_year,"Two_Year_refpts_Model2.csv")
write.csv(profiles_highM$CATCH,"CATCH_scenarios.csv")
write.csv(profiles_highM$SSB,"SSB_scenarios.csv")
write.csv(profiles_highM$Tables,"TABLES_scenarios_Catch.csv")


#low
setwd("/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_lowM")

low="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_lowM"
setwd(low)

write.csv(profiles_lowM$Two_year,"Two_Year_refpts_Model2.csv")
write.csv(profiles_lowM$CATCH,"CATCH_scenarios.csv")
write.csv(profiles_lowM$SSB,"SSB_scenarios.csv")
write.csv(profiles_lowM$Tables,"TABLES_scenarios_Catch.csv")

#OLDREF
OLDREF="/Users/ingrid.spies/Documents/AI_PCOD/November Models/Sensitivity_Anal/M23.0c/M23_0c/run/profiles_OLDREF"
setwd(OLDREF)

write.csv(profiles_OLDREF$Two_year,"Two_Year_refpts_Model2.csv")
write.csv(profiles_OLDREF$CATCH,"CATCH_scenarios.csv")
write.csv(profiles_OLDREF$SSB,"SSB_scenarios.csv")
write.csv(profiles_OLDREF$Tables,"TABLES_scenarios_Catch.csv")

