# This script was used to test out different analyses before making a submission. The graph shown in k_values.pdf is the result of this script. 
# Script for KNN analysis of Titanic: Machine Learning from Disaster Kaggle competition
# This script will split the training set into training and testing

data_set = read.csv("train.csv")

# Remove Name, Ticket and Cabin columns
data_set <- data_set[,-c(4,9,11)]

# Change Sex to 0 = male, 1 = female
data_set$Sex <- sapply(as.character(data_set$Sex), switch, 'male' = 0, 'female' = 1)

# Change Embarked column to 0 = 'C', 1 = 'Q', 2 = 'S' and remove NAs
data_set$Embarked[data_set$Embarked == ''] <- 'S'
data_set$Embarked <- sapply(as.character(data_set$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)

# Remove NAs from Age and Fare columns
data_age <- na.omit(data_set$Age)
data_age_avg <- mean(data_age)
data_set$Age[is.na(data_set$Age)] <- data_age_avg

data_fare <- na.omit(data_set$Fare)
data_fare_avg <- mean(data_fare)
data_set$Fare[is.na(data_set$Fare)] <- data_fare_avg

# Change Age to 0 = adult, 1 = child
data_set$Age <- ifelse(data_set$Age<18, 1, 0)

# Normalize Fare and Class columns
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

data_set$Pclass = normalize(data_set$Pclass)
data_set$Fare = normalize(data_set$Fare)

# Split data into train and test sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  testindex <- sample(index, trunc(length(index)/3))
  trainset <- dataframe[-testindex, ]
  testset <- dataframe[testindex, ]
  list(trainset=trainset,testset=testset)
}

splits <- splitdf(data_set)
train <- splits$trainset
test <- splits$testset

# Remove and rename the Survived column from train
# Remove additional irrelevant columns
survived <- train$Survived
test_survived <- factor(test$Survived)
train <- train[,-c(1,2,6,7,9)]
test <- test[,-c(1,2,6,7,9)]

# Run the KNN function
for (i in 1:100) {
  knn_titanic <- knn(train, test, survived, k = i,  l = 0, prob = FALSE, use.all = TRUE)
  knn_result <- data.frame(test_survived, knn_titanic, isSame = (test_survived==knn_titanic))

  # Compare predictions to actual
  knn_accuracy[i] <- (sum(knn_result$isSame == TRUE))/length(knn_result)
}

k = c(1:100)
plot(k,knn_accuracy,xlab = "k Value", ylab = 'Prediction Accuracy (%)', main = 'Titanic Survivors: Prediction Accuracy Using KNN',pch = 20)
