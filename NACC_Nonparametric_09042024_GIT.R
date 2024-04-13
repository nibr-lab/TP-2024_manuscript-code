################### Nonparametric multivariate analysis for first visit ##(NY)#################
#'''  Date of saving the final script 08-04-2024 
#     data used: NACC Longitudinal data
#     File name: 09042024_first_visit_NACC_new_normalization_subjectID_anaimous.xlsx
#     created by: NIBR 
#     created For: TP revision Manuscript tracking # 2023TP001189R
###################################################################################


rm( list=ls() )
library( ggplot2 )
library( readxl )
library( ggpubr )
library(naniar)

library(emmeans)
library(tidyverse)
library(mediation)
library(car)
library(npmv)

setwd("D:/Translation psychiatry/Revision_10-2-2024/Manuscript_revision_resubmission_09042024/Codes_and_files/")

sink("Nonparametric_test_BNRV_WMH_24-2-24.txt")
df <- read_excel("09042024_first_visit_NACC_new_normalization_subjectID_anaimous.xlsx")
names(df)<-str_replace_all(names(df), c(" " = "_"  ) )

df_0<- subset(df, NACCALZD==0)
df_8<-subset(df,NACCALZD==8)
df_1<-subset(df, NACCALZD==1)

df_50<- subset(df, AgeGroup=="50-64")
df_65<-subset(df,AgeGroup=="65-79")
df_80<-subset(df, AgeGroup==">=80")

{
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  print("nonpartest Results_ age group 50-65",quote=FALSE)

  NP_50 <-nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV| Normalized_WMHVOL ~ Cognition , data = df_50, permreps = 5000)
  cat("\n")
  print(NP_50$results)
  cat("\n")
  print("relative effect")
  print(NP_50$releffect)
  cat("\n")
  
  print("ssnonpartest Results_ age group 50-65", quote=FALSE)
  SS_50<- ssnonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL  ~ Cognition, data = df_50, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
}

{
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  print("nonpartest Results_ age group 65-79")
  cat("\n")
  NP_50 <-nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ Cognition , data = df_65, permreps = 5000)
  cat("\n")
  print(NP_50$results)
  
  print("relative effect")
  print(NP_50$releffect)
  cat("\n")
  
  print("ssnonpartest Results_ age group 65-79")
  SS_50<- ssnonpartest(Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ Cognition, data = df_65, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
}

{
  print("*************************************************************************") 
  cat("\n")
  print("nonpartest Results_ age group 80 above")
  cat("\n")
  NP_50 <-nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ Cognition , data = df_80, permreps = 5000)
  
  print(NP_50$results)
  cat("\n")
  
  print("relative effect")
  print(NP_50$releffect)
  cat("\n")
  
  print("ssnonpartest Results_ age group 80 above")
  SS_50<- ssnonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ Cognition, data = df_80, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
}


{
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  print("nonpartest Results_ cognition CN")
  cat("\n")
  NP_CN <- nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup , data = df_8, permreps = 5000)
  
  print( NP_CN$results)
  cat("\n")
  print("relative effect")
  print( NP_CN$releffect)
  
  cat("\n")
  print("ssnonpartest Results_ cognition CN")
  SS_CN<- ssnonpartest(Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM |Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup, data = df_8, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
}

{
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  print("nonpartest Results_ cognition CI")
  cat("\n")
  NP_CI <-nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup , data = df_0, permreps = 5000)
  
  print(NP_CI$results)
  
  cat("\n")
  print("relative effect")
  print(NP_CI$releffect)
  
  cat("\n")
  print("ssnonpartest Results_ cognition CI")
  SS_CI<- ssnonpartest(Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM |Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup, data = df_0, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
  
}

{
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  print("nonpartest Results_ cognition AD")
  cat("\n")
  NP_AD <-nonpartest( Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup , data = df_1, permreps = 5000)
  
  print(NP_AD$results)
  cat("\n")
  
  print("relative effect")
  print(NP_AD$releffect)
  
  
  cat("\n")
  print("ssnonpartest Results_ cognition AD")
  SS_AD<- ssnonpartest(Normalized_GRAYVOL | Normalized_WHITEVOL | Normalized_CSFVOL | Normalized_HIPPOVOL | Normalized_LATVENT | PARHIPM |  ENTM | Normalized_NACCBRNV | Normalized_WMHVOL ~ AgeGroup, data = df_1, test = c(1, 0, 0, 0), alpha = 0.05, factors.and.variables = TRUE)
  cat("\n")
  print("*************************************************************************",quote=FALSE)
  cat("\n")
  cat("\n")
  cat("\n")
  
}


sink()

