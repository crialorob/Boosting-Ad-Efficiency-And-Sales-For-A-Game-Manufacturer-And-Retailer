# 0. Downloading and importing the necessary packages.

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Install and load the 'conflicted' package.
install.packages("conflicted")
library(conflicted)

# Using the conflict_prefer() function to specify which 
##    version of the conflicting functions to use.
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Check the current CRAN mirror URL.
getOption("repos")

install.packages(c(
  "repr",
  "IRdisplay",
  "evaluate",
  "crayon",
  "pbdZMQ",
  "devtools",
  "uuid",
  "digest"
), 
repos = "https://cran.rstudio.com/",
dependencies = TRUE)

# Import the data set.
sales <- read.csv("turtle_sales.csv", header = TRUE)

# Print the data frame.
head(sales)
as_tibble(sales)
summary(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove redundant columns (Ranking, Year, Genre, Publisher).
new_sales <- subset(sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(new_sales)

# View the descriptive statistics.
summary(new_sales)

# Convert the 'product' column to a character type.
new_sales$Product <- as.character(new_sales$Product)

# Check the data types after conversion.
str(new_sales)

# Export and save the new data frame as new_sales as a CSV file.
write.csv(new_sales, file = "/Users/alonsoroblescristina/Desktop/COURSE 3/LSE_DA301_assignment_files/new_sales.csv", 
          row.names = FALSE)

# Import the new sales data set.
new_sales <- read.csv("new_sales.csv", header = TRUE)

# Print the data frame.
head(new_sales)

# Convert the 'product' column to a character type.
new_sales$Product <- as.character(new_sales$Product)

# Check the data types after conversion.
str(new_sales)

# Review plots to determine insights into the data set.
install.packages("ggplot2")
library(ggplot2)

# 2a) Create scatterplots.

# Does the Product typen and Platform influence Sales?

# View the plot for Global Sales.
qplot(x = Product, y = Global_Sales, data = new_sales,
      xlab = "Product", ylab = "Global Sales (Millions), (£)",
      main = "Global Sales by Product",
      geom = 'point')

# View the plot for NA Sales.
qplot(x = Product, y = NA_Sales, data = new_sales,
      xlab = "Product", ylab = "NA Sales (Millions), (£)",
      main = "NA Sales by Product",
      geom = 'point')

# View the plot for EU Sales.
qplot(x = Product, y = EU_Sales, data = new_sales, 
      xlab = "Product", ylab = "EU Sales (Millions), (£)",
      main = "EU Sales by Product",
      geom = 'point')

# Group data based on Product and determine the sum of Global Sales per Product.
global_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(Global_Sales = sum(Global_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(global_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of Global_Sales
global_sales_summary_ordered <- global_sales_summary %>%
  arrange(desc(Global_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(global_sales_summary_ordered, 10)
print(first_10_rows)

# Group data based on Product and determine the sum of NA Sales per Product.
NA_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(NA_Sales = sum(NA_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(NA_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of NA_Sales.
NA_sales_summary_ordered <- NA_sales_summary %>%
  arrange(desc(NA_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(NA_sales_summary_ordered, 10)
print(first_10_rows)

# Group data based on Product and determine the sum of EU Sales per Product.
EU_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(EU_Sales = sum(EU_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(EU_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of NA_Sales.
EU_sales_summary_ordered <- EU_sales_summary %>%
  arrange(desc(EU_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(EU_sales_summary_ordered, 10)
print(first_10_rows)

# Group data based on the Platform and determine the sum of Global Sales per Platform.
global_sales_summary <- new_sales %>%
  group_by(Platform) %>%
  summarise(Global_Sales = sum(Global_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(global_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of Global_Sales
global_sales_summary_ordered <- global_sales_summary %>%
  arrange(desc(Global_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(global_sales_summary_ordered, 10)
print(first_10_rows)

# Does the Platform type influence Sales?

# Reorder the Platform variable based on Global Sales.
global_sales_summary_ordered$Platform <- reorder(global_sales_summary_ordered$Platform, 
                                                 -global_sales_summary_ordered$Global_Sales)

# View the plot "Global Sales by Platform".
ggplot(global_sales_summary_ordered, aes(x = Global_Sales, y = Platform, fill = Platform)) +
  geom_col() +
  labs(x = "Global Sales (Millions, £)",
       y = "Platform",
       title = "Global Sales by Platform (Ordered by Sales)")

# Group data based on the Platform and determine the sum of NA Sales per Platform.
NA_sales_summary <- new_sales %>%
  group_by(Platform) %>%
  summarise(NA_Sales = sum(NA_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(NA_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of NA_Sales.
NA_sales_summary_ordered <- NA_sales_summary %>%
  arrange(desc(NA_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(NA_sales_summary_ordered, 10)
print(first_10_rows)

# Reorder the Platform variable based on NA Sales.
NA_sales_summary_ordered$Platform <- reorder(NA_sales_summary_ordered$Platform, 
                                                 -NA_sales_summary_ordered$NA_Sales)

# View the plot "NA Sales by Platform".
ggplot(NA_sales_summary_ordered, aes(x = NA_Sales, y = Platform, fill = Platform)) +
  geom_col() +
  labs(x = "NA Sales (Millions, £)",
       y = "Platform",
       title = "NA Sales by Platform (Ordered by Sales)")

# Group data based on the Platform and determine the sum of EU Sales per Platform.
EU_sales_summary <- new_sales %>%
  group_by(Platform) %>%
  summarise(EU_Sales = sum(EU_Sales))

# View the first 10 rows of the data frame.
first_10_rows <- head(EU_sales_summary, 10)
print(first_10_rows)

# Order the data frame in descending order of EU Sales.
EU_sales_summary_ordered <- EU_sales_summary %>%
  arrange(desc(EU_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(EU_sales_summary_ordered, 10)
print(first_10_rows)

# Reorder the Platform variable based on EU Sales.
EU_sales_summary_ordered$Platform <- reorder(EU_sales_summary_ordered$Platform, 
                                                 -EU_sales_summary_ordered$EU_Sales)

# View the plot "EU Sales by Platform".
ggplot(EU_sales_summary_ordered, aes(x = EU_Sales, y = Platform, fill = Platform)) +
  geom_col() +
  labs(x = "EU Sales (Millions, £)",
       y = "Platform",
       title = "EU Sales by Platform (Ordered by Sales)")

# 2c) Create boxplots.

# View the boxplot "Global Sales by Platform".
qplot(x = factor(Platform), y = Global_Sales, data = new_sales,
      xlab = "Platform", ylab = "Global Sales (Millions), (£)",
      main = "Global Sales by Platform",
      geom = "boxplot", fill = Platform)

# View the boxplot "NA Sales by Platform".
qplot(x = factor(Platform), y = NA_Sales, data = new_sales,
      xlab = "Platform", ylab = "NA Sales (Millions), (£)",
      main = "NA Sales by Platform",
      geom = "boxplot", fill = Platform)

# View the boxplot "EU Sales by Platform".
qplot(x = factor(Platform), y = EU_Sales, data = new_sales,
      xlab = "Platform", ylab = "EU Sales (Millions), (£)",
      main = "EU Sales by Platform",
      geom = "boxplot", fill = Platform)

# 0.Prepare your workstation

# Import the necessary libraries.
# Create insightful summaries of data set.
install.packages("skimr")
library(skimr)

# Create insightful reports of data set.
install.packages("DataExplorer")
library(DataExplorer)
library(tibble)

# Import the new_sales data set created in Week 4.
new_sales <- read.csv("new_sales.csv", header = TRUE)

# Print the data frame.
head(new_sales)

# Convert the 'product' column to a character type.
new_sales$Product <- as.character(new_sales$Product)

# Check the data types after conversion.
str(new_sales)

# Determine if there are missing values. 
new_sales[is.na(new_sales)] 
sum(is.na(new_sales))

# Check output: Determine the min, max, and mean values of the all the Sales columns.
# Using summary function.
summary(new_sales$Global_Sales)
summary(new_sales$NA_Sales)
summary(new_sales$EU_Sales)

# View the descriptive statistics.
summary(new_sales)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum of Global Sales per Product.
global_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(Total_Global_Sales = sum(Global_Sales))

# View the data frame for Total Global Sales.
head(global_sales_summary)

# Order the data frame in descending order of Total_Global_Sales.
total_global_sales_summary_ordered <- global_sales_summary %>%
  arrange(desc(Total_Global_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(total_global_sales_summary_ordered, 10)
print(first_10_rows)

# Group data based on Product and determine the sum of NA Sales per Product.
NA_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(Total_NA_Sales = sum(NA_Sales))

# View the data frame for Total NA Sales.
head(NA_sales_summary)

# Order the data frame in descending order of Total_NA_Sales
total_NA_sales_summary_ordered <- NA_sales_summary %>%
  arrange(desc(Total_NA_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(total_NA_sales_summary_ordered, 10)
print(first_10_rows)

# Group data based on Product and determine the sum of EU Sales per Product.
EU_sales_summary <- new_sales %>%
  group_by(Product) %>%
  summarise(Total_EU_Sales = sum(EU_Sales))

# View the data frame for Total Global Sales.
head(EU_sales_summary)

# Order the data frame in descending order of Total_EU_Sales
total_EU_sales_summary_ordered <- EU_sales_summary %>%
  arrange(desc(Total_EU_Sales))

# View the first 10 rows of the ordered data frame.
first_10_rows <- head(total_EU_sales_summary_ordered, 10)
print(first_10_rows)

# Create a new data frame 'total_sales' with the new aggregated sales columns.
total_sales <- data.frame(Product = global_sales_summary$Product,
                          Total_NA_Sales = NA_sales_summary$Total_NA_Sales,
                          Total_EU_Sales = EU_sales_summary$Total_EU_Sales,
                          Total_Global_Sales = global_sales_summary$Total_Global_Sales)

# View the first 10 rows of the new data frame 'total_sales'.
first_10_rows <- head(total_sales, 10)
print(first_10_rows)

# Explore the total_sales data frame.
head(total_sales)
tail(total_sales)
dim(total_sales)
str(total_sales)
summary(total_sales)

## 2b) Determine which plot is the best to compare game sales.

# Create scatterplots.

# Scatter Plot: Total Global Sales by Product
ggplot(data = total_sales, aes(x = Product, y = Total_Global_Sales)) +
  geom_point() +
  labs(title = "Scatter Plot: Total Global Sales by Product",
       x = "Product",
       y = "Total Global Sales")

# Create histograms.

# Histogram: Distribution of Total Global Sales
ggplot(data = total_sales, aes(x = Total_Global_Sales)) +
  geom_histogram(binwidth = 1) +  
  labs(title = "Histogram: Distribution of Global Sales",
       x = "Global Sales",
       y = "Frequency")

# 3a) Create Q-Q Plots.
# Create Q-Q Plot for Global Sales.
ggplot(data = new_sales, aes(sample = Global_Sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Global Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Create Q-Q Plot for NA Sales.
ggplot(data = new_sales, aes(sample = NA_Sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: NA Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Create Q-Q Plot for EU Sales.
ggplot(data = new_sales, aes(sample = EU_Sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: EU Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# 3b) Perform Shapiro-Wilk test.
# Install and import Moments.
install.packages("moments")

# Load the moments package
library(moments)

# Perform Shapiro-Wilk test.

# Perform Shapiro-Wilk test on all sales columns
shapiro_results <- lapply(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], shapiro.test)

# Create a data frame to store the results
shapiro_results_new_sales <- data.frame(
  Column = names(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")]),
  p_value = sapply(shapiro_results, function(x) x$p.value),
  is_normal = sapply(shapiro_results, function(x) x$p.value > 0.05)
)

# View the output.
print(shapiro_results_new_sales)

# 3c) Determine Skewness and Kurtosis.
# Skewness and Kurtosis.

# Calculate skewness and kurtosis of sales columns.
skewness_values <- sapply(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], skewness)
kurtosis_values <- sapply(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")], kurtosis)

# Create a data frame to store the results.
skewness_kurtosis_new_sales <- data.frame(
  Column = names(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")]),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

# View the output.
print(skewness_kurtosis_new_sales)

# 3d) Determine correlation between the three sales columns.

# Calculate the correlation matrix.
sales_correlation <- cor(new_sales[, c("NA_Sales", "EU_Sales", "Global_Sales")])

# View the output.
print(sales_correlation)

# The sales_correlation matrix will display the correlation coefficients between the specified sales columns. 
# The correlation coefficient ranges from -1 to 1, where -1 indicates a perfect negative correlation, 
# 1 indicates a perfect positive correlation, and 0 indicates no correlation.

# Install corrplot package.
install.packages("corrplot")

# Load the corrplot package.
library(corrplot)

# Visualize the correlation matrix as a heatmap.
corrplot(sales_correlation, method = "color")

# Create plots to gain insights into the data.
# Scatter plot for NA_Sales vs. EU_Sales.
ggplot(new_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "NA Sales vs. EU Sales",
       x = "NA_Sales",
       y = "EU_Sales")

# Scatter plot for NA_Sales vs. Global_Sales
ggplot(new_sales, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "NA Sales vs. Global Sales",
       x = "NA Sales",
       y = "Global Sales")

# Scatter plot for EU_Sales vs. Global_Sales
ggplot(new_sales, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  labs(title = "EU sales vs. Global Sales",
       x = "EU_Sales",
       y = "Global_Sales")

# Histogram of Sales in North America.
ggplot(new_sales, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Sales in North America",
       x = "Sales in North America",
       y = "Frequency")

# Histogram of Sales in European Union.
ggplot(new_sales, aes(x = EU_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Sales in EU",
       x = "Sales in the EU",
       y = "Frequency")

# Histogram of Global Sales.
ggplot(new_sales, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Global Sales",
       x = "Global Sales",
       y = "Frequency")

# Barplot of Global Sales.
ggplot(new_sales, aes(x = Product)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of Products",
       x = "Product",
       y = "Count")

# 0.Prepare your workstation

# Import the necessary libraries.
# Create insightful summaries of data set.
install.packages("skimr")
library(skimr)

# Create insightful reports of data set.
install.packages("DataExplorer")
library(DataExplorer)
library(tibble)

 # Import the new_sales data set created in Week 4. 
new_sales <- read.csv("new_sales.csv", header = TRUE) 

# Print the data frame. 
head(new_sales) 

# Convert the 'product' column to a character type.
new_sales$Product <- as.character(new_sales$Product)

# Check the data types after conversion.
str(new_sales)

# View the descriptive statistics. 
summary(new_sales) 

# A. Create a linear regression model to compare Global Sales vs NA Sales.

# Step 1: Create a simple linear regression model
model_A <- lm(Global_Sales ~ NA_Sales, data = new_sales)

# Step 2: Get the correlation between columns
correlation_A <- cor(new_sales$Global_Sales, new_sales$NA_Sales)

# Step 3: Print the model summary and correlation.
summary(model_A)
print(correlation_A)

# B. Create a linear regression model to compare Global Sales vs Total EU Sales.

# Step 1: Create a simple linear regression model
model_B <- lm(Global_Sales ~ EU_Sales, data = new_sales)

# Step 2: Get the correlation between columns
correlation_B <- cor(new_sales$Global_Sales, new_sales$EU_Sales)

# Step 3: Print the model summary and correlation
summary(model_B)
print(correlation_B)

# C. Create a linear regression model to compare Total NA Sales vs Total EU Sales.

# Step 1: Create a simple linear regression model
model_C <- lm(NA_Sales ~ EU_Sales, data = new_sales)

# Step 2: Get the correlation between columns
correlation_C <- cor(new_sales$NA_Sales, new_sales$EU_Sales)

# Step 3: Print the model summary and correlation
summary(model_C)
print(correlation_C)

# Create the plot for the lineal regression model A.
ggplot(new_sales, aes(x = Global_Sales, y = NA_Sales)) +
  geom_point() +                 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Global Sales", y = "NA Sales") +   
  ggtitle("Simple Linear Regression Model A - 
          Relationship between Global Sales and NA Sales") 

# Create the plot for the lineal regression model B.
ggplot(new_sales, aes(x = Global_Sales, y = EU_Sales)) +
  geom_point() +                 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Global Sales", y = "EU Sales") +   
  ggtitle("Simple Linear Regression Model B - 
          Relationship between Global Sales and EU Sales")  

# Create the plot for the lineal regression model C.
ggplot(new_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +                 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "NA Sales", y = "EU Sales") +   
  ggtitle("Simple Linear Regression Model C - 
          Relationship between NA Sales and EU Sales")  

# Import the original data frame.
sales <- read.csv("turtle_sales.csv", header = TRUE)

# Print the data frame.
head(sales)

# Convert the 'product' column to a character type.
sales$Product <- as.character(sales$Product)

# Check the data types after conversion.
str(sales)

# Multiple linear regression model.

# Select numeric columns for the model
numeric_cols <- sales[, c("NA_Sales", "EU_Sales", "Global_Sales")]

# Create the multiple linear regression model
model_multiple <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = numeric_cols)

# Print the summary of the model
summary(model_multiple)

# Provided NA_Sales_sum and EU_Sales_sum values.
# NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
# NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
# NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
# NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
# NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

# Create a data frame with the provided NA_Sales_sum and EU_Sales_sum values
predicted_global_sales <- data.frame(Provided_NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                       Provided_EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict global sales using the multiple linear regression model.
predicted_global_sales <- predict(model_multiple, predictedglobalsales = predicted_global_sales)

# Print the predicted global sales.
predicted_global_sales

# Create a data frame with the predicted sales.
predicted_scenarios <- data.frame(
  Predicted_NA_Sales_Sum = c(34.02, 3.93, 2.73, 2.26, 22.08),
  Predicted_EU_Sales_Sum = c(23.80, 1.56, 0.65, 0.97, 0.52),
  Predicted_Other_Sales_Sum = c(0, 0, 0, 0, 0)  # We could add the sum of other sales if available.
)

# Calculate the predicted Global_Sales
predicted_scenarios$Predicted_Global_Sales <- predicted_scenarios$Predicted_NA_Sales_Sum 
+ predicted_scenarios$Predicted_EU_Sales_Sum + predicted_scenarios$Predicted_Other_Sales_Sum

# Print the data frame with predicted values
predicted_scenarios

# View the descriptive statistics of the predicted_scenarios data frame.
summary(predicted_scenarios)
str(predicted_scenarios)

# Create a scatter plot to compare predicted global sales with actual global sales.
ggplot(new_sales, aes(x = Global_Sales, y = predicted_global_sales)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Global Sales", y = "Predicted Global Sales") +
  ggtitle("Predicted vs. Actual Global Sales") +
  theme_minimal()
