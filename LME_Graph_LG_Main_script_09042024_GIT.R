################# Linear Mixed Effect model Analysis script for graph ##(NY)#######
#'''  Date of saving the final script 08-04-2024 
#     data used: NACC Longitudinal data
#     File name: 09042024_all_NACC_new_normalization_Without_SubjectID_A_but_SubjectID.Xlsx
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
library(visreg)
library(plotrix)
library(extrafont)
font_import()

loadfonts(device = "win")

# set working directory
setwd("D:/Translation psychiatry/Revision_10-2-2024/Manuscript_revision_resubmission_09042024/Codes_and_files/")


df <- read_excel("09042024_all_NACC_new_normalization_Without_NACCID_but_SubjectID.Xlsx")

df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)

# to convert the ALZD  type as factors ( categorical variables )

df_other <- df[ df$Cn0Ci1Ad2 == 1 | df$Cn0Ci1Ad2 == 2, ]

df_8 <- df[  df$NACCMRIA <= 95 & df$Cn0Ci1Ad2 == 0, ]

df <- rbind(df_other, df_8)

# to replace any space in column name by underscore
names(df)<-str_replace_all(names(df), c(" " = "_"  ) )


df$Cn0Ci1Ad2 <- factor(df$Cn0Ci1Ad2)
levels(df$Cn0Ci1Ad2)


print("************************************************************************************")

# For Gray Matter LME Graph

LMM_m1 <- lmer(  Normalized_GRAYVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_GM_4-3-24_points.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)), 
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(450, 700),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE, tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 30, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For White Matter LME graph 

LMM_m1 <- lmer(  Normalized_WHITEVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_WM_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)), 
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(300, 550),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 30, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For Hippocampus  LME graph 

LMM_m1 <- lmer(  Normalized_HIPPOVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_HIP_17-2-24.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)), 
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(4.5, 7.5),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3, tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 0.35, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For Lateral Ventricles LME graph 

LMM_m1 <- lmer(  Normalized_LATVENT ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_LV_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)),
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(0, 70),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 8.5, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For CSF  LME graph 

LMM_m1 <- lmer(  Normalized_CSFVOL ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_CSF_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)),
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(250, 450),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 24, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex =3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For Entorhinal cortex thickness  LME graph 

LMM_m1 <- lmer(  ENTM ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_ENTM_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(5, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)),
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(1, 5),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3]- 0.5, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For Parahippocampus Thickness  LME graph 

LMM_m1 <- lmer(   PARHIPM ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_PHIP_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 5,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)), 
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(1, 2.5),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 0.18, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")


print("************************************************************************************")

# For ICV  LME graph 

LMM_m1 <- lmer(  NACCICV ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_ICV_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 6,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            points=list( col=c("seagreen", "blue", "red2")),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)), 
            xlim = c(40, 100), ylim = c(600, 1600),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 120, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex = 3)

dev.off()

print("************************************************************************************")



print("************************************************************************************")

# For Total Brain Volume  LME graph 

LMM_m1 <- lmer(  Normalized_NACCBRNV ~ NACCMRIA *Cn0Ci1Ad2 + (1|SubjectID_A), data =df )
summary(LMM_m1)
confint(LMM_m1, level=0.95, method="Wald")


tiff("LMM_BNRV_4-3-24_pt.tiff",units="in", width=7.5, height=7, res=600)

par(bty = "n", family = "ArialMT", cex.axis=3)

par(mar = c(3, 6,4, 0.5) + 1)
vis<-visreg(LMM_m1,"NACCMRIA", by="Cn0Ci1Ad2",
            line=list( col=c("seagreen", "blue", "red2"), lwd=4),
            fill=list(col=grey(c(0.7,0.7,0.7), alpha=0.4)),
            points=list( col=c("seagreen", "blue", "red2")),
            xlim = c(40, 100), ylim = c(800, 1200),
            overlay=TRUE,xaxt="n", yaxt="n",xlab="",ylab="",partial=TRUE,rug=FALSE,nn=2000)

axis(side = 1, lwd = 6,las=0,at = axTicks(1), labels = FALSE,tck=-0.03) # Hide the default tick labels
axis(side = 2, lwd = 6,las=1,cex.axis=3,tck=-0.03)

# Specify the new position for the tick labels
text(axTicks(1), par("usr")[3] - 50, labels = axTicks(1), xpd = TRUE, srt = 0, adj = c(0.5, 0),cex =3)

dev.off()

print("************************************************************************************")



