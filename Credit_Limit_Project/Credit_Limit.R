library(readr)

# Import dataset
credit <- read.csv("/Users/annabellepham/Documents/Credit_Limit_Project/credit.txt")
View(credit)
str(credit)

# Check correlation
cor(credit[, c('limit', 'balance', 'income', 'rating', 'cards', 'age', 'education')])

# Remove outliers
boxplot(credit)$out

# Remove outliers for variable income
Q <- quantile(credit$income, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(credit$income)
Cleandata1<- subset(credit, credit$income > (Q[1] - 1.5*iqr) & credit$income < (Q[2]+1.5*iqr))

# Remove outliers for variable limit
Q2 <- quantile(Cleandata1$limit, probs=c(.25, .75), na.rm = FALSE)
iqr2 <- IQR(Cleandata1$limit)
Cleandata2<- subset(Cleandata1, Cleandata1$limit > (Q2[1] - 1.5*iqr2) & Cleandata1$limit < (Q2[2]+1.5*iqr2))

# Remove outliers for variable rating
Q3 <- quantile(Cleandata2$rating, probs=c(.25, .75), na.rm = FALSE)
iqr3 <- IQR(Cleandata2$rating)
Cleandata3<- subset(Cleandata2, Cleandata2$rating > (Q3[1] - 1.5*iqr3) & Cleandata2$rating < (Q3[2]+1.5*iqr3))

# Remove outliers for variable cards
Q4 <- quantile(Cleandata3$rating, probs=c(.25, .75), na.rm = FALSE)
iqr4 <- IQR(Cleandata3$rating)
Cleandata4<- subset(Cleandata3, Cleandata3$cards > (Q4[1] - 1.5*iqr4) & Cleandata3$card < (Q4[2]+1.5*iqr4))

# Validate the dataset
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(e1071)
library(rattle)
library(tidyverse)

n = nrow(Cleandata4)
trainIndex <- sample(1:n,
                     size = round(0.7*n),
                     replace = FALSE)

train_data <- Cleandata4[trainIndex, ]  # we use the index to create training data
test_data <- Cleandata4[-trainIndex, ]  # we use the remaining 30% as the test data

# Regression
regressiontrain1 <- lm(limit ~., data = train_data)

summary(regressiontrain1)

# Remove statistically insignificant variables
# Drop one variable one at a time
# regression2 without age variable
regressiontrain2 <- lm(limit ~ balance + income + rating + cards + education, data = Cleandata4)
summary(regressiontrain2)

# Drop age variable
# regression3 without age and education variables
regressiontrain3 <- lm(limit ~ balance + income + rating + cards, data = Cleandata4)
summary(regressiontrain3)

predicted_values <- predict(regressiontrain3, newdata = test_data)
View(predicted_values)

# Calculate RMSE 
RMSE(predicted_values, test_data$limit)

#Calculate R2
R2(predicted_values, test_data$limit)
