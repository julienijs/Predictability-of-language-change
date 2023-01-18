# This script is part of the Predictability of language change project and analyses the residuals of the models

# load libraries:
library(xlsx)
library(readxl)
library(effects)
library(dplyr)
library(stats)
library(ggplot2)
library(tools)
library(graphics)

# read residuals_analysis_annotated.xlsx (= annotated version of residuals_analysis.xlsx):
ResAnalysis <- read_xlsx("residuals_analysis_annotated.xlsx", col_names = TRUE)

#throw out datasets that were not significant
ResAnalysis <- droplevels(ResAnalysis[!ResAnalysis$Significance=="no",]) 

# relevel and refactor:
ResAnalysis$Change_Type <- as.factor(ResAnalysis$Change_Type)
ResAnalysis$Curve_Type <- relevel(as.factor(ResAnalysis$Curve_Type), ref="full")
ResAnalysis$Window <- relevel(as.factor(ResAnalysis$Windowposition), ref="middle omitted")

# boxplots of the standardized residuals:
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Windowposition) # vs windowposition
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Change_Type) # vs the type of change
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Curve_Type) # vs the type of curve

# boxplots of the raw residuals:
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Windowposition) # vs windowposition
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Change_Type) # vs the type of change
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Curve_Type) # vs the type of curve

# linear models:
summary(w <- lm(Standardized_residual ~ Window, 
                data=ResAnalysis))
summary(w <- lm(Standardized_residual ~ Curve_Type, 
                data=ResAnalysis))
summary(w <- lm(Standardized_residual ~ Change_Type, 
                data=ResAnalysis))
summary(w <- lm(Standardized_residual ~ Window*Curve_Type + Change_Type, 
                data=ResAnalysis))
summary(w <- lm(Standardized_residual ~ Window*Curve_Type, 
                data=ResAnalysis)) # --> most significant effects
plot(allEffects(w), 
     ylab="Standardized residuals", 
     xlab="Part of the curve",
     main="Interaction effect for window position and part of the curve")

# histogram:

hist(ResAnalysis$Standardized_residual, 
     main="Histogram of the standardized residuals (aggregated)",
     xlab="Standardized residuals",
     ylim=c(0,200),
     labels=TRUE)

