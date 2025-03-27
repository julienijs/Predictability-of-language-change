library(readxl)
library(ggplot2)
library(dplyr)
library(tools)

# Define the directory containing your Excel files
directory <- "C:/Users/u0149275/OneDrive - KU Leuven/HowPredictableIsLanguageChange/Datasets"

# List all Excel files in the directory
files <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop over each file and read the data, adding a column to identify the file
for (file in files) {
  # Read the entire sheet
  data <- read_excel(file)
  
  # Select only the 'Decade' and 'Change' columns
  if("Decade" %in% names(data) & "Change" %in% names(data)) {
    data <- data %>% select(Decade, Change)
    
    # Convert 'Change' column to factor
    data$Change <- as.factor(data$Change)
    
    data <- droplevels(data[!data$Change=="NA",]) # throw away NA rows
    data <- na.omit(data)
    
    # Convert 'Decade' column to numeric
    data$Decade <- as.numeric(data$Decade)
    
    # Add a column to identify the file
    data$Changes <- file_path_sans_ext(basename(file))
    
    # Combine the data
    combined_data <- bind_rows(combined_data, data)
  } else {
    warning(paste("File", Changes, "does not contain 'Decade' and 'Change' columns"))
  }
}

# Create a plot with all datasets
p <- ggplot(combined_data, aes(x = as.numeric(Decade), y = as.numeric(as.character(Change)), color = Changes)) +
  stat_smooth(method="glm", se=TRUE, 
              method.args = list(family=binomial(link = "logit"))) +
  geom_vline(xintercept=1900) +
  annotate("text", x=1897, y=0.95, label="Begin", size=3, angle=90) +
  geom_vline(xintercept=1950) +
  annotate("text", x=1953, y=0.95, label="End", size=3, angle=90) +
  labs(x = "Decade", y = "Change") +
  theme_minimal()

# Print the plot
print(p)
