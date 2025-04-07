#This is the script for the project How Predictable Is Language Change?

setwd("C:/Users/u0149275/OneDrive - KU Leuven/HowPredictableIsLanguageChange/Datasets")

# load libraries:
library(openxlsx)
library(readxl)
library(effects)
library(dplyr)
library(stats)
library(ggplot2)
library(tools)
library(graphics)
library(pROC)
library(lmtest)

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
  #print(summary(alldata.fit))
  alldata.pred <- as.data.frame(predict.glm(alldata.fit, temp.data, type="response", se.fit = T))
  
  # logistic regression with only subset1 + prediction for temp.data:
  subset1.fit <- glm(Change ~ Decade, data = subset1, family = binomial(link="logit"))
  #print(summary(subset1.fit))
  subset1.pred <- predict.glm(subset1.fit, temp.data, type = "response")

  # logistic regression with only subset2 + prediction for temp.data:
  subset2.fit <- glm(Change ~ Decade, data = subset2, family = binomial(link="logit"))
  #print(summary(subset2.fit))
  subset2.pred <- predict.glm(subset2.fit, temp.data, type="response")
  
  # logistic regression with only subset3 + prediction for temp.data:
  subset3.fit <- glm(Change ~ Decade, data = subset3, family = binomial(link="logit"))
  #print(summary(subset3.fit))
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
                       Raw_Residual = numeric())
  
  # add data to output:
  for (val in 1:nrow(combineddata)) {
    newrow <-data.frame(file_path_sans_ext(basename(file)), 
                         combineddata$Decade[val],
                         5,
                         "begin",
                         combineddata$ss1[val], 
                         combineddata$full[val], 
                         combineddata$obs[val],
                         combineddata$ss1_raw_res[val])
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
                         combineddata$ss2_raw_res[val])
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
                         combineddata$ss3_raw_res[val])
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
                         combineddata$full_raw_res[val])
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
                     Raw_Residual = numeric())

# apply the functions "prediction" and "formatting" to all files in filenames:
for (file in filenames) {
  out <- prediction(file)
  formatted <-reformatting(out)
  finaldf <- rbind(finaldf, formatted)
}

# Determine the part of the curve based on inflection point
library(purrr)
library(broom)

# Function to extract Beta0, BetaTime and compute inflection point
get_model_params <- function(file) {
  print(paste("Processing:", file))
  
  # Read file
  Df <- read_xlsx(file, col_names = TRUE)
  
  # Data cleaning
  Df$Decade <- as.numeric(Df$Decade)
  Df$Change <- as.factor(Df$Change)
  Df <- droplevels(Df[!Df$Change == "NA",])
  
  # Fit logistic regression model
  model <- glm(Change ~ Decade, data = Df, family = binomial(link="logit"))
  
  # Extract coefficients
  coefs <- tidy(model)
  Beta0 <- coefs$estimate[coefs$term == "(Intercept)"]
  BetaTime <- coefs$estimate[coefs$term == "Decade"]
  
  # Compute inflection point
  Inflection_Point <- ifelse(!is.na(Beta0) & !is.na(BetaTime) & BetaTime != 0, -Beta0 / BetaTime, NA)
  
  # Generate predictions
  temp.data <- data.frame(Decade = seq(1850, 1990, 10))
  predictions <- predict(model, temp.data, type="response")
  
  # Compute probability range
  Prob_Range <- max(predictions, na.rm = TRUE) - min(predictions, na.rm = TRUE)
  
  # Determine curve type
  Curve_Type <- case_when(
    Inflection_Point < 1850 ~ "end",
    Inflection_Point > 1999 ~ "beginning",
    Inflection_Point >= 1850 & Inflection_Point <= 1999 & Prob_Range >= 0.5 ~ "full",
    Inflection_Point >= 1850 & Inflection_Point <= 1999 & Prob_Range < 0.5 ~ "middle",
    TRUE ~ NA_character_
  )
  
  return(data.frame(File = file_path_sans_ext(basename(file)),
                    Beta0 = Beta0,
                    BetaTime = BetaTime,
                    Inflection_Point = Inflection_Point,
                    Prob_Range = Prob_Range,
                    Curve_Type = Curve_Type))
}

# Apply function to all files and combine results
model_results <- map_dfr(filenames, get_model_params)

# Merge with finaldf
finaldf <- left_join(finaldf, model_results, by="File")

# save finaldf to .xlsx file:
write.xlsx(finaldf, "residuals_analysis.xlsx", sheetName = "Sheet1", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)

# Function to fit the model and calculate AUC
get_auc <- function(data, formula) {
  # Fit logistic regression model
  model <- glm(formula, data = data, family = binomial)
  
  # Predict probabilities
  predicted_probabilities <- predict(model, type = "response")
  
  # Extract the response variable
  response_variable <- model$y
  
  # Calculate ROC and AUC
  roc_obj <- roc(response_variable, predicted_probabilities)
  auc(roc_obj)
}

c_value_results <- data.frame(filename = character(), c_value = numeric(), stringsAsFactors = FALSE)

for (file in filenames) {
  # read the file and put it into a data frame:
  Df <- read_xlsx(file, col_names = TRUE)
  # clean the data:
  Df$Decade <- as.numeric(Df$Decade) # convert to numeric
  Df$Change <- as.factor(Df$Change) # convert to factor
  Df <- droplevels(Df[!Df$Change=="NA",]) # throw away NA rows
  c_value = get_auc(Df, Change ~ Decade)
  c_value_results <- rbind(c_value_results, data.frame(File = file_path_sans_ext(basename(file)), c_value = c_value, stringsAsFactors = FALSE))
}

# save c_value_results to .xlsx file:
write.xlsx(c_value_results, "c_values.xlsx", sheetName = "Sheet1", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)

######PART 2: Analysis of the residuals######

# read residuals_analysis.xlsx:
ResAnalysis <- read_xlsx("residuals_analysis.xlsx", col_names = TRUE)

# drop non-significant changes?
ResAnalysis <- ResAnalysis %>% mutate(Significant = ifelse(File %in% c("The dat wat shift in the superlative", "The rise of experiencer-subject construal with psych verbs"), "No", "Yes"))

# relevel windowposition:
ResAnalysis$Windowposition <- relevel(as.factor(ResAnalysis$Windowposition), ref="full")

# relevel curve type:
ResAnalysis$Curve_Type <- relevel(as.factor(ResAnalysis$Curve_Type), ref="full")

# add change type:
ResAnalysis <- ResAnalysis %>% mutate(Change_Type = ifelse(File %in% c("The deflection of veel", "Plural ending in -s vs. plural ending in -en", "The demise of predeterminer al", "The rise of the periphrastic superlative", "Attributive first person singular possessive"), "Inflectional morphology", "Other"))
ResAnalysis$Change_Type <- relevel(as.factor(ResAnalysis$Change_Type), ref="Other")

# add deflection
ResAnalysis <- ResAnalysis %>% mutate(Deflection = ifelse(File %in% c("The deflection of veel", "The rise of the periphrastic superlative", "Attributive first person singular possessive"), "Deflection", "Other"))
ResAnalysis$Deflection <- relevel(as.factor(ResAnalysis$Deflection), ref="Other")

# add crystallization
ResAnalysis <- ResAnalysis %>% mutate(Crystallization = ifelse(File %in% c("Mass noun van Noun vs. Mass noun Noun", "The hortative alternation", "The loss of the article in complex prepositions", "The loss of the article in quantifier tal van", "The rise of the close apposition with soort"), "Crystallization", "Other"))
ResAnalysis$Crystallization <- relevel(as.factor(ResAnalysis$Crystallization), ref="Other")

# add larger trend:
ResAnalysis <- ResAnalysis %>% mutate(
  Larger_Trend = case_when(
    Crystallization == "Crystallization" ~ "Crystallization",
    Deflection == "Deflection" ~ "Deflection",
    TRUE ~ "Other"
  )
)
ResAnalysis$Larger_Trend <- relevel(as.factor(ResAnalysis$Larger_Trend), ref="Other")

# boxplots:
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Windowposition)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Change_Type)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Curve_Type)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Crystallization)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Deflection)
boxplot(ResAnalysis$Raw_Residual ~ ResAnalysis$Larger_Trend)

# Test for normal distribution of the residuals
# Perform Shapiro-Wilk test for each File and WindowPosition combination
shapiro_results <- ResAnalysis %>%
  group_by(File, Windowposition) %>% 
  summarise(W = shapiro.test(Raw_Residual)$statistic,
            p_value = shapiro.test(Raw_Residual)$p.value,
            .groups = 'drop')

# Print results
print(shapiro_results, n = Inf)

######PART 3: Analysis of the c-values######

# read c_values.xlsx:
C_values <- read_xlsx("c_values.xlsx", col_names = TRUE)

ResAnalysis <- merge(ResAnalysis, C_values, by = "File", all.x = TRUE)

# Convert c_value to numeric type
ResAnalysis$c_value <- as.numeric(ResAnalysis$c_value)

######PART 4: Look for the best model given all predictors######

# Load necessary libraries
library(stats)
library(utils) # For combn

#  List of predictor variables
predictors <- c("Windowposition", "Change_Type", "Curve_Type", "Larger_Trend", "c_value", "Significant")

# Generate all possible combinations of predictor variables
all_combinations <- unlist(
  lapply(1:length(predictors), function(i) {
    combn(predictors, i, simplify = FALSE)
  }), recursive = FALSE
)

# Initialize an empty list to store models and AIC values
aic_values <- numeric(0)
model_formulas <- character(0)

# Function to generate all possible two-way interaction terms for a given combination of predictors
generate_pairwise_interactions <- function(vars) {
  if (length(vars) < 2) return(character(0))
  combn(vars, 2, FUN = function(x) paste(x, collapse = "*"), simplify = TRUE)
}

# Fit each model and calculate AIC
for (i in seq_along(all_combinations)) {
  # Main effects only
  main_effects_formula <- paste("Raw_Residual ~", paste(all_combinations[[i]], collapse = " + "))
  main_effects_model <- lm(as.formula(main_effects_formula), data = ResAnalysis)
  
  # Store the formula and calculate AIC
  model_formulas <- c(model_formulas, main_effects_formula)
  aic_values <- c(aic_values, AIC(main_effects_model))
}

# Assign names to the AIC values for easier identification
names(aic_values) <- model_formulas

# Print AIC values
print(aic_values)

# Identify the best model based on the lowest AIC
best_model_index <- which.min(aic_values)
best_model_formula <- names(aic_values)[best_model_index]
best_model_aic <- aic_values[best_model_index]

cat("The best model is:", best_model_formula, "with an AIC of", best_model_aic, "\n")

summary(final_model <- lm(best_model_formula, data = ResAnalysis))

plot(allEffects(final_model))

######PART 5: Plot the variables######

# Histogram of raw residuals:
hist(ResAnalysis$Raw_Residual, 
     main="Histogram of the raw residuals (aggregated)",
     xlab="Raw residuals",
     ylim=c(0,400),
     labels=TRUE)

# Change_Type plot:
print(ggplot(data = ResAnalysis, aes(x = Change_Type, y = Raw_Residual)) +
        geom_boxplot() +         # Boxplot to show the distribution of Raw_Residual for each Change_Type
        geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
        labs(title = "",
             x = "Change type",
             y = "Residuals") +
        theme_minimal())

# Larger trend plot: 
print(ggplot(data = ResAnalysis, aes(x = Larger_Trend, y = Raw_Residual)) +
        geom_boxplot() +         # Boxplot to show the distribution of Raw_Residual for each Change_Type
        geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
        labs(title = "",
             x = "Larger trend",
             y = "Residuals") +
        theme_minimal())

# C Statistic:
print(ggplot(data = ResAnalysis, aes(x = c_value, y = Raw_Residual)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "black")  +
        labs(title = "Residuals vs. C Statistic",
             x = "C Statistic",
             y = "Residuals") +
        theme_minimal())

# Windowposition:
print(ggplot(data = ResAnalysis, aes(x = Windowposition, y = Raw_Residual)) +
        geom_boxplot() +         # Boxplot to show the distribution of Raw_Residual for each Change_Type
        geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
        labs(title = "",
             x = "Window position",
             y = "Residuals") +
        theme_minimal())

# Significant:
print(ggplot(data = ResAnalysis, aes(x = Significant, y = Raw_Residual)) +
        geom_boxplot() +         # Boxplot to show the distribution of Raw_Residual for each Change_Type
        geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
        labs(title = "",
             x = "Significance",
             y = "Residuals") +
        theme_minimal())

# Curve_Type plot:
print(ggplot(data = ResAnalysis, aes(x = Curve_Type, y = Raw_Residual)) +
        geom_boxplot() +         # Boxplot to show the distribution of Raw_Residual for each Change_Type
        geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
        labs(title = "",
             x = "Part of the curve",
             y = "Residuals") +
        theme_minimal())

######PART 6: Examining the standard deviation of the residuals######

# Calculate standard deviations per level for each variable
Change_Type_sd <-ResAnalysis %>%
  group_by(Change_Type) %>%
  summarise(sd_value = sd(Raw_Residual))

Curve_Type_sd <-ResAnalysis %>%
  group_by(Curve_Type) %>%
  summarise(sd_value = sd(Raw_Residual))

Larger_Trend_sd <-ResAnalysis %>%
  group_by(Larger_Trend) %>%
  summarise(sd_value = sd(Raw_Residual))

c_value_sd <-ResAnalysis %>%
  group_by(c_value) %>%
  summarise(sd_value = sd(Raw_Residual))

Windowposition_sd <-ResAnalysis %>%
  group_by(Windowposition) %>%
  summarise(sd_value = sd(Raw_Residual))

Significant_sd <-ResAnalysis %>%
  group_by(Significant) %>%
  summarise(sd_value = sd(Raw_Residual))


bptest(lm(Raw_Residual ~ c_value, data = ResAnalysis)) 

