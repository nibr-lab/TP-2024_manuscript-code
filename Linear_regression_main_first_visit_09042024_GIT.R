################### Linear Regression model for first visit ##(NY)#################
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
library(lme4)
library(tidyverse)

library(car)
library(lmerTest)
library(multilevelTools)
library(haven)

setwd("D:/Translation psychiatry/Revision_10-2-2024/Manuscript_revision_resubmission_09042024/Codes_and_files/")

sink("NACC_Linear_regressin_FirstVisit_analysis.txt")

df <- read_excel("09042024_first_visit_NACC_new_normalization_subjectID_anaimous.xlsx")


df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)

# to convert the ALZD  type as factors ( categorical variables )

df_other <- df[ df$Cn0Ci1Ad2 == 1 | df$Cn0Ci1Ad2 == 2, ]

df_8 <- df[  df$NACCMRIA <= 95 & df$Cn0Ci1Ad2 == 0, ]

df <- rbind(df_other, df_8)

names(df)<-str_replace_all(names(df), c(" " = "_"  ) )


# To fix a data frame (or any other named structure) â????xâ?? that already has names containing spaces and/or commas:
# NACCMRIA is Age
min_age = min(df$NACCMRIA)

df$NACCMRIA  = df$NACCMRIA - min_age  #recentring the MRI AGe around the minimum age present in dfset#

df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)


# to make sub set of data set
df_0<- subset(df, NACCALZD==0)
df_8<-subset(df, NACCALZD==8)
df_1<-subset(df,  NACCALZD==1)


{ 
  print("***********************************************************************************")
  print("For Normalised Gray Matter Volume")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm(  Normalized_GRAYVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
 
  print("***********************************************************************************" )
}


{ 
  print("***********************************************************************************")
  print("For Normalised Gray Matter Volume")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")

  LMM_m2 <- lm( Normalized_WHITEVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalised Hippocampus Volume")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm(  Normalized_HIPPOVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalized Lateral Ventricle Volume")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm( Normalized_LATVENT ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))

  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalized CSF Volume")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
 
  LMM_m2 <- lm(  Normalized_CSFVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Entrhinal Cortex Thickness")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm(  ENTM ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Parahippocampal Gyrus Thickness")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm(   PARHIPM ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print('For Intra Cranial Volume')
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
 
  LMM_m2 <- lm(  NACCICV ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  
  print("***********************************************************************************") 
}

{ 
  print("***********************************************************************************")
  print('For normalised Total Brain Volume')
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lm(  Normalized_NACCBRNV ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
   print("***********************************************************************************")
}


{ 
  print("***********************************************************************************")
  print("Normalized_WMHVOL")
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2 <- lmer(  Normalized_WMHVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))

  
  print("***********************************************************************************")
}
print(" ")
print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print("               Additional Parameters                 ")
print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print(" ")
{
  print("***********************************************************************************")
  print("log_NORM_WMH")
  jitter=0.02
  df$Norm_WMH_jitter=df$Normalized_WMHVOL+jitter
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2=lmer(log(Norm_WMH_jitter)~  NACCMRIA + Cn0Ci1Ad2 + NACCMRIA*Cn0Ci1Ad2, data=df)
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{
  print("***********************************************************************************")
  print("log_WMH")
  jitter=0.02
  df$WMH_jitter=df$WMHVOL+jitter
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")
  
  LMM_m2=lmer(log(WMH_jitter)~  NACCMRIA + Cn0Ci1Ad2 + NACCMRIA*Cn0Ci1Ad2 , data=df)
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print('For toltal Brain Volume')
  print("-------------------------------------------------------------")
  print("Model: Linear Regression")
  print("-------------------------------------------------------------")

  LMM_m2 <- lmer( NACCBRNV ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  
  x<- coefficients(LMM_m2)
  
  m1.lst <- emtrends(LMM_m2 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m2))
  
  print("***********************************************************************************")
}



# Close the sink function
# commands entered after this will have its outputs displayed in the console
sink()