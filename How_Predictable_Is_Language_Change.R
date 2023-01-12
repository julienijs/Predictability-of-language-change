#This is the script for the project How Predictable Is Language Change?

setwd("C:/Users/Datasets") # insert directory with the raw data

# load libraries:
library(xlsx)
library(readxl)
library(effects)
library(dplyr)
library(stats)
library(ggplot2)
library(tools)
library(graphics)

######PART 1: Processing the datasets######

# compile a list of .xlsx files (in working directory) that need to be processed:
filenames <- list.files(getwd(), pattern="xlsx*", full.names=TRUE)

# make a new data set for predictions:
temp.data <- data.frame(Decade = seq(1850, 1990, 10))

# f1 is a function that makes predictions based on an input file:
prediction <- function(file) {
  print(file)
  # read the file and put it into a data frame:
  Df <- read_xlsx(file, col_names = TRUE)
  
  # clean the data:
  Df$Decade <- as.numeric(Df$Decade) # convert to numeric
  Df$Change <- as.factor(Df$Change) # convert to factor
  Df <- droplevels(Df[!Df$Change=="NA",]) # throw away NA rows
  
  # observed values per decade:
  observed <- prop.table(table(Df$Change, Df$Decade), 2)[2,]
  print(observed)
  
  # make subsets: pick which levels of Decade to drop:
  subset1 <- Df[ which(Df$Decade >= 1900), ] # throw away first 5 decades
  subset2 <- Df[Df$Decade >1940 | Df$Decade < 1900,] # throw away 5 decades from the middle
  subset3 <- Df[ which(Df$Decade <= 1940), ] # throw away last 5 decades
  
  # logistic regression based on full dataset + prediction for temp.data:
  alldata.fit <- glm(Change ~ Decade, data = Df, family = binomial(link="logit"))
  print(summary(alldata.fit))
  alldata.pred <- as.data.frame(predict.glm(alldata.fit, temp.data, type="response", se.fit = T))
  
  # logistic regression with only subset1 + prediction for temp.data:
  subset1.fit <- glm(Change ~ Decade, data = subset1, family = binomial(link="logit"))
  print(summary(subset1.fit))
  subset1.pred <- predict.glm(subset1.fit, temp.data, type = "response")

  # logistic regression with only subset2 + prediction for temp.data:
  subset2.fit <- glm(Change ~ Decade, data = subset2, family = binomial(link="logit"))
  print(summary(subset2.fit))
  subset2.pred <- predict.glm(subset2.fit, temp.data, type="response")
  
  # logistic regression with only subset3 + prediction for temp.data:
  subset3.fit <- glm(Change ~ Decade, data = subset3, family = binomial(link="logit"))
  print(summary(subset3.fit))
  subset3.pred <- predict.glm(subset3.fit, temp.data, type="response")
  
  # put new generated data in a dataframe:
  combineddata <- data.frame(dec = temp.data, 
                             obs = observed,
                             full = alldata.pred$fit,
                             full.se = alldata.pred$se.fit,
                             ss1 = subset1.pred,
                             ss2 = subset2.pred,
                             ss3 = subset3.pred)
  
  # add raw residuals:
  combineddata <- transform(combineddata, full_raw_res = obs - full)
  combineddata <- transform(combineddata, ss1_raw_res = obs - ss1)
  combineddata <- transform(combineddata, ss2_raw_res = obs - ss2)
  combineddata <- transform(combineddata, ss3_raw_res = obs - ss3)
  
  # add standardized residuals:
  standdevfull <- sd(combineddata$full_raw_res)
  standdev1 <- sd(combineddata$ss1_raw_res)
  standdev2 <- sd(combineddata$ss2_raw_res)
  standdev3 <- sd(combineddata$ss3_raw_res)
  
  combineddata <- transform(combineddata, full_stand_res = full_raw_res/standdevfull)
  combineddata <- transform(combineddata, ss1_stand_res = ss1_raw_res/standdev1)
  combineddata <- transform(combineddata, ss2_stand_res = ss2_raw_res/standdev2)
  combineddata <- transform(combineddata, ss3_stand_res = ss3_raw_res/standdev3)
  
  return(combineddata)
}

# function that makes plots based on output of the function prediction:
plotting <- function(d, file) {
  
  std <- qnorm(0.95 / 2 + 0.5)
  
  p <- ggplot() + geom_line(data=d, aes(x=Decade, y = full, color = "No omissions")) + 
    geom_point(data=d, aes(x=Decade, y = obs, fill= "Observed values")) +
    geom_line(data=d, aes(x=Decade, y = ss1, color = "Begin omitted")) +
    geom_line(data=d, aes(x=Decade, y = ss2, color = "Middle omitted")) +
    geom_line(data=d, aes(x=Decade, y = ss3, color  = "End omitted")) +
    geom_ribbon(data=d, aes(x=Decade, y=full, ymin=full-std*full.se, ymax = full+std*full.se), 
                alpha = 0.25) +
    scale_x_continuous(limits = c(1850, 1990), breaks = seq(1850, 1990, by = 10)) +
    guides(x=guide_axis(angle = 45)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggtitle(file_path_sans_ext(basename(file))) +
    theme_bw() +
    scale_color_manual(name = "Models", 
                       values = c("No omissions" = "red", 
                                  "Begin omitted" = "green", 
                                  "Middle omitted" = "yellow", 
                                  "End omitted" = "blue")) +
    scale_fill_manual(name = "", 
                       values = c("Observed values" = "black")) +
    labs(x="Decade", y="")
    ggsave(paste(file_path_sans_ext(basename(file)),"_final.png", sep = ""), 
         plot = p, width = 18, height = 12, units = "cm")
  p
}

# function to reformat the data from the function prediction:
reformatting <- function(combineddata) {
  # prepare a new data frame for the output:
  output <- data.frame(File = character(), 
                       Decade = numeric(),
                       Windowsize = numeric(), 
                       Windowposition = character(),
                       Prediction = numeric(),
                       Glmfulldataset = numeric(),
                       Realvalue = numeric(),
                       Raw_Residual = numeric(),
                       Standardized_residual = numeric())
  
  # add data to output:
  for (val in 1:nrow(combineddata)) {
    newrow <-data.frame(file_path_sans_ext(basename(file)), 
                         combineddata$Decade[val],
                         5,
                         "begin",
                         combineddata$ss1[val], 
                         combineddata$full[val], 
                         combineddata$obs[val],
                         combineddata$ss1_raw_res[val],
                         combineddata$ss1_stand_res[val])
    names(newrow) <- names(output)
    output <- rbind(output, newrow)
  }
  
  for (val in 1:nrow(combineddata)) {
    newrow <- data.frame(file_path_sans_ext(basename(file)), 
                         combineddata$Decade[val],
                         5,
                         "middle",
                         combineddata$ss2[val], 
                         combineddata$full[val], 
                         combineddata$obs[val],
                         combineddata$ss2_raw_res[val],
                         combineddata$ss2_stand_res[val])
    names(newrow) <- names(output)
    output <- rbind(output, newrow)
  }
  
  for (val in 1:nrow(combineddata)) {
    newrow <- data.frame(file_path_sans_ext(basename(file)), 
                         combineddata$Decade[val],
                         5,
                         "end",
                         combineddata$ss3[val], 
                         combineddata$full[val], 
                         combineddata$obs[val],
                         combineddata$ss3_raw_res[val],
                         combineddata$ss3_stand_res[val])
    names(newrow) <- names(output)
    output <- rbind(output, newrow)
  }
  
  for (val in 1:nrow(combineddata)) {
    newrow <- data.frame(file_path_sans_ext(basename(file)), 
                         combineddata$Decade[val],
                         0,
                         "full",
                         combineddata$full[val], 
                         combineddata$full[val], 
                         combineddata$obs[val],
                         combineddata$full_raw_res[val],
                         combineddata$full_stand_res[val])
    names(newrow) <- names(output)
    output <- rbind(output, newrow)
  }
  
  # return the output
  return(output)
}

# apply the functions "prediction" and "plotting" to all files in filenames:
for (file in filenames) {
  out <- prediction(file)
  print(out)
  print(plotting(out, file))
}

# prepare a new data frame for the final data frame:
finaldf <- data.frame(File = character(), 
                     Decade = numeric(),
                     Windowsize = numeric(), 
                     Windowposition = character(),
                     Prediction = numeric(),
                     Glmfulldataset = numeric(),
                     Realvalue = numeric(),
                     Raw_Residual = numeric(),
                     Standardized_residual = numeric())

# apply the functions "prediction" and "formatting" to all files in filenames:
for (file in filenames) {
  out <- prediction(file)
  print(out)
  formatted <-reformatting(out)
  print(formatted)
  finaldf <- rbind(finaldf, formatted)
}

# save finaldf to .xlsx file:
write.xlsx(finaldf, "residuals_analysis.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######PART 2: Analysis of the residuals######

# read residuals_analysis.xlsx:
ResAnalysis <- read_xlsx("C:/Users/u0149275/Documents/HowPredictableIsLanguageChange/residuals_analysis.xlsx", col_names = TRUE)


ResAnalysis <- droplevels(ResAnalysis[!ResAnalysis$Significance=="no",]) #throw out datasets that were not significant

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

######PART 3: Analysis of the coefficients######

# read + clean dataset:
CoeffAnalysis <- read_xlsx("C:/Users/u0149275/Documents/HowPredictableIsLanguageChange/coefficients_2.xlsx", col_names = TRUE)
CoeffAnalysis <- droplevels(CoeffAnalysis[!CoeffAnalysis$Significant=="no",]) #throw out datasets that were not significant

# boxplot:
boxplot(CoeffAnalysis$Rico ~ CoeffAnalysis$Windowposition)


CoeffAnalysis$Curve_Type <- relevel(as.factor(CoeffAnalysis$Curve_Type), ref="full")

summary(v <- lm(Rico ~ Windowposition*Curve_Type, 
                  data=CoeffAnalysis))
plot(allEffects(v))



