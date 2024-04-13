######## Linear Mixed Effect model Analysis & LME and LR model comparison ##(NY)######
#'''  Date of saving the final script 08-04-2024 
#     data used: NACC Longitudinal data
#     File name: 09042024_all_NACC_new_normalization_Without_NACCID_but_SubjectID.Xlsx
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

sink("NACC_Linear_Mixed_Effect_analysis_Logitudinal_Data.txt")

df <- read_excel("09042024_all_NACC_new_normalization_Without_NACCID_but_SubjectID.Xlsx")


df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)

# to convert the ALZD  type as factors ( categorical variables )

df_other <- df[ df$Cn0Ci1Ad2 == 1 | df$Cn0Ci1Ad2 == 2, ]

df_8 <- df[  df$NACCMRIA <= 95 & df$Cn0Ci1Ad2 == 0, ]

df <- rbind(df_other, df_8)

# To fix a data frame (or any other named structure) â????xâ?? that already has names containing spaces and/or commas:
names(df)<-str_replace_all(names(df), c(" " = "_"  ) )


min_age = min(df$NACCMRIA)

df$NACCMRIA  = df$NACCMRIA - min_age  #recentring the MRI AGe around the minimum age present in dfset#

df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)


# to make sub set of data set
df_0<- subset(df, Cn0Ci1Ad2==0)
df_8<-subset(df,Cn0Ci1Ad2==8)
df_1<-subset(df, Cn0Ci1Ad2==1)


{ 
  print("***********************************************************************************")
  print("For Normalised Gray Matter Volume")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  LMM_m1 <- lmer(  Normalized_GRAYVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  # x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends( LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary( LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  Normalized_GRAYVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  
  print(anova(LMM_m1,LMM_m2))
  #print(Anova(lm.inter, type="III"))
  print("***********************************************************************************" )
}


{ 
  print("***********************************************************************************")
  print("For Normalised White Matter Volume")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer(  Normalized_WHITEVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df)
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm( Normalized_WHITEVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalised Hippocampus Volume")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer(  Normalized_HIPPOVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  Normalized_HIPPOVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalized Lateral Ventricle Volume")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer( Normalized_LATVENT ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df)
  
  x<- coefficients( LMM_m1)
  
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  print("-------------------------------------------------------------")
  print("Model2")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm( Normalized_LATVENT ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Normalized CSF Volume")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  
  LMM_m1 <- lmer(  Normalized_CSFVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2; Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  Normalized_CSFVOL ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Entrhinal Cortex Thickness")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer(  ENTM ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  ENTM ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}

{ 
  print("***********************************************************************************")
  print("For Parahippocampal Gyrus Thickness")
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer(  PARHIPM ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends(LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary(LMM_m1 ))
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(   PARHIPM ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************")
}



{ 
  print("***********************************************************************************")
  print('For normalised Total Brain Volume')
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  
  LMM_m1 <- lmer(  Normalized_NACCBRNV ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends( LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary( LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  Normalized_NACCBRNV ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  
  print("***********************************************************************************")
}


{ 
  print("***********************************************************************************")
  print('For Intra Cranial Volume')
  print("-------------------------------------------------------------")
  print("Model1: Linear Mixed Effect Model")
  print("-------------------------------------------------------------")
  LMM_m1 <- lmer(  NACCICV ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
  
  x<- coefficients(LMM_m1)
  
  m1.lst <- emtrends( LMM_m1 , "Cn0Ci1Ad2", var="NACCMRIA", data = df)
  
  
  print(pairs(m1.lst))
  
  print(summary( LMM_m1 ))
  
  print("-------------------------------------------------------------")
  print("Model2: Linear regression model")
  print("-------------------------------------------------------------")
  LMM_m2 <- lm(  NACCICV ~ NACCMRIA *Cn0Ci1Ad2 , data =df )
  print(summary(LMM_m2))
  
  print("Comparing Model1 and Model2")
  print(anova(LMM_m1,LMM_m2))
  
  print("***********************************************************************************") 
}



# Close the sink function
# commands entered after this will have its outputs displayed in the console
sink()