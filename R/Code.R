#installing packages
install.packages("pROC")
install.packages("stats")
install.packages("rpart")
install.packages("MASS")
install.packages("e1071")
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("broom")
install.packages("randomForest")
install.packages("outliers")
#Important libraries that we will be using
library("randomForest")
library(broom)
library(tidyverse)
library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library('MASS')
library(rpart)
library("stats")
library(pROC)
library("outliers")
library(dplyr)


# importing data sets
test_data <- read.csv("Validation.csv")
test_data

train_data <- read.csv("Train_data.csv")
train_data

str(test_data)
summary(test_data)
table(test_data$galaxy)
#cleaning data

#1. Checking for outliers
remove_anomalies <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]]) && !is.factor(data[[col]])) {
      # Check if column is continuous
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      upper_fence <- q3 + 1.5*iqr
      lower_fence <- q1 - 1.5*iqr
      
      # Remove any rows containing anomalies
      data <- data[!is.na(data[[col]]) & data[[col]] >= lower_fence & data[[col]] <= upper_fence,]
    }
  }
  return(data)
}
train_data1 <- remove_anomalies(train_data)
train_data1

test_data1 <- remove_anomalies(test_data)
test_data1


#1. checking for missing values
sum(is.na(test_data1))
sum(is.na(train_data1))

fill_na_with_mean <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    }
  }
  return(df)
}


clean_train_df <- fill_na_with_mean(train_data1)
clean_train_df
str(clean_train_df)

clean_test_df <- fill_na_with_mean(test_data1)
clean_test_df
str(clean_test_df)

sum(is.na(clean_test_df))
sum(is.na(clean_train_df))

#2. Checking for duplicate records
sum(duplicated(clean_train_df))
sum(duplicated(clean_test_df))


#EDA analysis

#use clean_train_df








#Random Forest model

model <- randomForest(clean_train_df_1$Well.Being.Index ~ clean_train_df_1$ID + clean_train_df_1$galactic.year
                      + clean_train_df_1$galaxy + clean_train_df_1$existence.expectancy.index +clean_train_df_1$existence.expectancy.at.birth
                      + clean_train_df_1$Gross.income.per.capita + clean_train_df_1$Income.Index + clean_train_df_1$Expected.years.of.education..galactic.years.
                      + clean_train_df_1$Mean.years.of.education..galactic.years. + clean_train_df_1$Intergalactic.Development.Index..IDI...Rank
                      + clean_train_df_1$Education.Index + clean_train_df_1$Intergalactic.Development.Index..IDI...Rank, data =clean_train_df_1 )

model
print(importance(model))

sub_pred <- predict(model, clean_test_df)
sub_pred
clean_test_df$Predicted.Well.Being.Index <- sub_pred
str(clean_test_df)

output_data_set  <- clean_test_df[, c("ID","Predicted.Well.Being.Index")]
output_data_set

write.csv(output_data_set, file = "vincent_Maina_DSA.csv", row.names = FALSE)
