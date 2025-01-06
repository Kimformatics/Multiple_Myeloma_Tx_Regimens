" Descriptive Statistics: These are used to summarize and describe the central tendency, variability, 
and distribution of a dataset. Measures like mean, median, variance, and standard deviation provide 
insights into the data's basic characteristics, critical for initial exploration in biostatistics."

# install.packages("psych") repeat for other packages that need to be installed

# Load necessary libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(psych)      # For descriptive statistics

# Load the data from a CSV file
data <- read_csv("multipleMyelomaGlucose.csv")
head(data)

#Option 1: Perform descriptive stats (individually) on a myeloma dataset

# Calculate the mean, median, mode, and SD of the dbp col
# Access the dbp col
dbp <- dbp$data

# Calculate Mean
mean_dbp <- mean(dbp, na.rm = TRUE) # `na.rm = TRUE` removes missing values
mean_dbp

# Median
median_dbp <- median(dbp, na.rm = TRUE)
median_dbp

# Mode function (custom implementation since R doesn't have a built-in mode function)
get_mode <- function(v) {
  uniq_values <- unique(v)
  freq <- tabulate(match(v, uniq_values))
  uniq_values[which.max(freq)]
}
mode_dbp <- get_mode(dbp)
mode_dbp

# Standard Deviation
sd_dbp <- sd(dbp, na.rm = TRUE)
sd_dbp

########################################################

# Option 2

# View the first few rows of the data
head(data)

# Summary of the entire dataset
summary(data)

# Calculate descriptive statistics using the psych package
descriptive_stats <- psych::describe(data)

# Print descriptive statistics
print(descriptive_stats)

# Save descriptive statistics file
write.csv(descriptive_stats, "descriptive_statisticsMelanoma.csv", row.names = TRUE)

#####################################################################################################

# Option 3 - Using dplyr

# Calculate statistics using dplyr
results <- data %>%
  summarize(
    mean_dbp = mean(dbp, na.rm = TRUE),
    median_dbp = median(dbp, na.rm = TRUE),
    mode_dbp = get_mode(dbp),
    sd_dbp = sd(dbp, na.rm = TRUE)
  )

# View the results
print(results)


