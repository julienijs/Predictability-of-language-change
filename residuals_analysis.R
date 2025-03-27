# This script is part of the Predictability of language change project and analyses the residuals of the models

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

######PART 2: Analysis of the residuals######

# read residuals_analysis.xlsx:
ResAnalysis <- read_xlsx("residuals_analysis.xlsx", col_names = TRUE)

# drop non-significant changes?
ResAnalysis <- ResAnalysis %>% mutate(Significant = ifelse(File %in% c("The dat wat shift in the superlative", "The rise of experiencer-subject construal with psych verbs"), "No", "Yes"))


# relevel windowposition:
ResAnalysis$Windowposition <- relevel(as.factor(ResAnalysis$Windowposition), ref="full")

# add curve type:
ResAnalysis <- ResAnalysis %>% mutate(
  Curve_Type = case_when(
    File %in% c("The dat wat shift in indefinite numerals", "The deflection of veel", "The rise of the auxiliary zijn for the perfect tense", "Plural ending in -s vs. plural ending in -en", "The rise of the periphrastic superlative", "The rise of experiencer-subject construal with psych verbs") ~ "begin",
    File %in% c("Mass noun van Noun vs. Mass noun Noun", "niet meer ADJ vs. niet ADJ meer") ~ "middle",
    File %in% c("The dat wat shift in the superlative", "The demise of predeterminer al", "The loss of the article in quantifier tal van", "The loss of the article in complex prepositions") ~ "end",
    File %in% c("The hortative alternation", "Attributive first person singular possessive", "The rise of the close apposition with soort") ~ "full",
    TRUE ~ NA_character_
  )
)

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
        geom_smooth(method = "lm", se = FALSE)  +
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



