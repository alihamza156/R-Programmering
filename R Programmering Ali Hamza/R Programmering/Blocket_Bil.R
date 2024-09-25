install.packages("dplyr")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("moments")
install.packages("readxl")
library(dplyr)
library(ggplot2)
library("readxl")
library(gridExtra)
library(olsrr)
library(moments)
#-----------------------------------------------------------------------------------------------------
# IMPORT DATA
#-----------------------------------------------------------------------------------------------------

file_path <- "C:/Users/ali_h/Desktop/R Programmering Ali Hamza/R Programmering/Blocket_cars.xlsx"
car_data <- read_excel(file_path)

View(car_data)
#-----------------------------------------------------------------------------------------------------
# DATA CLEANING
#-----------------------------------------------------------------------------------------------------
# This line tells how many missing values NA is present in car_data.
sum(is.na(car_data))

# This will print out the number of missing values within the columns.
missing_value <- function(x)
  sum(is.na(x))
car_empty <- sapply(car_data, missing_value)
print(car_empty)
View(car_empty)

# This line will remove the unwanted columns and in our case between column 11 to 13.
car_data_fixed <- subset(car_data, select = -c(11:13))

# Alright so what this code basically says is that it converts specific columns into small letters and leaves the other ones untouched inside car_data_fixed data frame and to visually inspect I use view().
small_letter <- function(x)
  if (is.character(x))
    tolower(x) else x
car_data_fixed <- data.frame(lapply(car_data_fixed, small_letter))
View(car_data_fixed)

# To ensure everything within the R file works out smoothly there are some small touches. As for the price it is important to tell the program that "Hey, the prices are numeric and no letters, also to avoid errors and to make it solid.
car_data_fixed$Pris <- as.numeric(gsub("[^0-9.]", "", as.character(car_data_fixed$Pris)))

# This line removes brackets, I use this to kind of clean for more consistency.
car_data_fixed$Färg <- gsub("\\[|\\]", "", car_data_fixed$Färg)

#-----------------------------------------------------------------------------------------------------
# DATA EXPLORATION
#-----------------------------------------------------------------------------------------------------
# A quick overview of the structure. Str = structure as for python programming it means string.
str(car_data_fixed)

# Here I use summary to sum up the data frame.
summary(car_data_fixed)


# Instead of having all the columns I use specifically (Län) to be visual.
region <- data.frame(car_data_fixed$Län)
# To visually inspect the code.
View(region)

# Counts how many times regions appears in the column, including duplicates.
region_counts <- region %>% count(car_data_fixed.Län) 
region_counts <- region_counts[order(region_counts$n),]
print(region_counts)
sum(region_counts$n)

# Creates new data frame and clears duplicated rows. Example: If I have picked same car twice with mistake, this line of code, removes that mistake by removing the row.
unique_region <- distinct(region)
#If needed, a visual of the dataframe is available in the code below (73).
#View(unique_region)

# Sums up the total car brands.
company_counts <- car_data_fixed %>% count(Märke)
company_counts <- company_counts[order(company_counts$n),]
sum(company_counts$n)

#-----------------------------------------------------------------------------------------------------
# CORRELATION WITH PEARSON
#-----------------------------------------------------------------------------------------------------
# Check, is there any correlation between the variables?
# using Pearson r method, 0 = no correlation.
# Correlation helps us with statistic measurements for example if the Horsepowers are high then the price should also be high because it means the higher horsepower the more the price increases.
Mil_Pris <- cor(car_data_fixed$Miltal, car_data_fixed$Pris, method = "pearson")
print(Mil_Pris)

Häst_Pris <- cor(car_data_fixed$Hästkrafter, car_data_fixed$Pris, method = "pearson")
print(Häst_Pris)

År_Pris <- cor(car_data_fixed$Modellår, car_data_fixed$Pris, method = "pearson")
print(År_Pris)

Drivning_Pris <- cor(car_data_fixed$Pris, car_data_fixed$Drivning)
print(Drivning_Pris)
# The result shows that there are correlation but gives a

# Create Correlation dataframe
correlation <- as.data.frame(c(Mil_Pris,Häst_Pris,År_Pris,Drivning_Pris))
factor_names <- as.data.frame(c("Miltal-Pris","Hästkrafter-Pris","Modellår-Pris","Drivning-Pris"))
correlation$names <- as.data.frame(factor_names)
View(correlation)
# As for the Miltal-Pris the correlation shows a negative number, so as the miltal increases the pris decreases.
#-----------------------------------------------------------------------------------------------------
# CREATE PLOTS
#-----------------------------------------------------------------------------------------------------
# Creating new data frame using following below.
pris_observation <- car_data_fixed[c("Miltal", "Pris", "Märke", "Modell", "Modellår")]
View(pris_observation)

# Create a clear visual analysis of car price distribution. Visualizing changes in the prices based on model year, brand etc.

plot2 <- ggplot(pris_observation, aes(x = Pris)) +
  geom_histogram(binwidth = 10000, fill = "pink", color = "violet") +
  labs(title="Price Frequency",x = "Pris", y = "Frequency") +
  theme_minimal()

plot3 <- ggplot(pris_observation, aes(x = Modellår, y = Pris, color = factor(Märke))) +
  geom_point() +
  labs(title="Vehicle Price | per year | split by Company",x = "År", y = "Pris", color = "Märke") +
  theme(panel.background = element_rect(fill = 'violet'))

grid.arrange(plot2, plot3, ncol = 2)

# Creating a so called pair plot or so called scatterplot to visualise the relationships between all continous variables in the dataset.
pairs(car_data_fixed[c("Miltal", "Modellår","Hästkrafter","Pris")],
      col="red",
      pch=16,
      # Creating a title for the plot.
      main = "Pairplot of all continuous variable"
      )
#-----------------------------------------------------------------------------------------------------
# EXPLORING THE SELECTED VARIABLE, IN THIS CASE HÄSTKRAFTER
#-----------------------------------------------------------------------------------------------------
# Checking if the variable is normally distributed.
hist(car_data_fixed$Hästkrafter)
ggplot(car_data_fixed, aes(x= Hästkrafter)) +
  geom_histogram(
    color="red",
    fill="red"
  )
boxplot(car_data_fixed$Hästkrafter)
ggplot(car_data_fixed, aes(x= Hästkrafter)) +
  geom_boxplot(fill="red")

#-----------------------------------------------------------------------------------------------------
# FITTING A LINEAR MODEL WITH NON NORMAL DISTRIBUTION
#-----------------------------------------------------------------------------------------------------
attach(car_data_fixed)
lm_model <- lm(Pris ~ Hästkrafter)

summary(lm_model)
plot(lm_model)
abline(lm_model, col="yellow")
#-----------------------------------------------------------------------------------------------------
# RESIDUALS ANALYSIS
#-----------------------------------------------------------------------------------------------------
# Checking for normal distribution in the model.
ols_plot_resid_hist(lm_model)
ols_plot_resid_qq(lm_model)
skewness(car_data_fixed$Hästkrafter)
# p-value > 0.05 indicates we cannot reject h0 hypotheses, which means there is normality
shapiro.test(residuals(lm_model))

hist(residuals(lm_model))

qqnorm(residuals(lm_model), main = "Non Normal Q-Q Plot")
qqline(residuals(lm_model),col="yellow")
#-----------------------------------------------------------------------------------------------------
# TRANSFORM NON NORMAL DISTRIBUTION TO NORMAL
#-----------------------------------------------------------------------------------------------------
# A larger skewness value indicates a greater deviation from a normal distribution.
skewness(car_data_fixed$Hästkrafter)
skewness(car_data_fixed$Pris)

# To address moderate skewness, use the square root transformation. 
# First, identify the numerical columns, leaving out any categorical columns such as 'Category'.
numerical_columns <- sapply(car_data_fixed, is.numeric)
car_sqrt <- data.frame()

# Apply square root transformation to numerical columns.
car_sqrt <- sqrt(car_data_fixed[numerical_columns])

# Fit the normal distribution to the linear model.
lm_sqrt <- lm(Pris ~ Hästkrafter, data = car_sqrt)
summary(lm_sqrt)
#-----------------------------------------------------------------------------------------------------
# REDO RESIDUAL ANALYSIS
# Currently, the p-value is 0.0587, indicating that we have achieved approximate normality.
#-----------------------------------------------------------------------------------------------------
# is the model normal distributed
ols_plot_resid_hist(lm_sqrt)
ols_plot_resid_qq(lm_sqrt)

# p-value > 0.05 indicates we cannot reject h0 hypotheses, which means there is normality
shapiro.test(residuals(lm_sqrt))

hist(residuals(lm_sqrt))

# Manual qqplot
qqnorm(residuals(lm_sqrt))
qqline(residuals(lm_sqrt),col="yellow")
#-----------------------------------------------------------------------------------------------------
# EVALTUATION OF THE SIMPLE LINEAR MODEL
#-----------------------------------------------------------------------------------------------------
# Random sample
set.seed(1)
row.number <- sample(1:nrow(car_sqrt), 0.8*nrow(car_sqrt))

#subset our data
train <- car_sqrt[row.number,] # 80%
test <- car_sqrt[-row.number,] # 20%

# Estimate the linear fit with the training set
lm_fit0.8 <- lm(Pris~Hästkrafter, data = train)
summary(lm_fit0.8)

# Predict in the test dataset
prediction0.8 <- predict(lm_fit0.8, newdata = test)
summary(prediction0.8)
err0.8 <- prediction0.8 - test$Pris

# Root mean square error
rmse <- sqrt(mean(err0.8^2))
# Mean absolute percentage error
mape <- mean(abs(err0.8/test$Pris))
# Results
c(RMSE= rmse, mape=mape, R2=summary(lm_fit0.8)$r.squared)

#-----------------------------------------------------------------------------------------------------
# MULTIPLE LINEAR REGRESSION
#-----------------------------------------------------------------------------------------------------

attach(car_sqrt)
lm_multi <- lm(Pris~Hästkrafter+Modellår)

summary(lm_multi)

#-----------------------------------------------------------------------------------------------------
# EVALUATIN OF THE MULTIPLE LINEAR REGRESSION
#-----------------------------------------------------------------------------------------------------
# Random sample
set.seed(1)
row.number <- sample(1:nrow(car_sqrt), 0.8*nrow(car_sqrt))
# Subset our data
train <- car_sqrt[row.number,] # 80%
test <- car_sqrt[-row.number,] # 20%

# Estimate the linear fit with the training set
multi_lm_fit0.8 <- lm(Pris~Hästkrafter+Miltal+Modellår+Drivning, data = train)
summary(multi_lm_fit0.8)

# Predict in the test dataset
prediction0.8 <- predict(multi_lm_fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Pris

# Root mean square error
rmse <- sqrt(mean(err0.8^2))
# Mean absolute percentage error
mape <- mean(abs(err0.8/test$Pris))
# Result
c(RMSE= rmse, mape=mape, R2=summary(multi_lm_fit0.8)$r.squared)