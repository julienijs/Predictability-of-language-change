# load libraries:
library(xlsx)
library(readxl)
library(effects)
library(dplyr)
library(stats)
library(ggplot2)
library(tools)
library(graphics)


# read residuals_analysis.xlsx:
ResAnalysis <- read_xlsx("residuals_analysis.xlsx", col_names = TRUE)

# add a column "significance", where "no" is added for those datasets were not significant in the previous analysis:
ResAnalysis <- cbind(ResAnalysis, Significance=NA)
ResAnalysis <- droplevels(ResAnalysis[!ResAnalysis$Significance=="no",]) #throw out datasets that were not significant

# relevel and refactor:
ResAnalysis$Change_Type <- as.factor(ResAnalysis$Change_Type)
ResAnalysis$Curve_Type <- relevel(as.factor(ResAnalysis$Curve_Type), ref="full")
ResAnalysis$Window <- relevel(as.factor(ResAnalysis$Windowposition), ref="middle omitted")

# boxplots:
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Windowposition)
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Change_Type)
boxplot(ResAnalysis$Standardized_residual ~ ResAnalysis$Curve_Type)

boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Windowposition)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Change_Type)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Curve_Type)

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

