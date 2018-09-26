#setwd("C:/Users/Install/Desktop/CASE_R_september")

##################################################
# Open Data Set
##################################################
library(mlbench)
data(Sonar)
##################################################
#Important Factor output
##################################################
#Class Factor with 2 levels

##################################################
#Try a 60/40 split
##################################################
# Shuffle row indices: rows
rows <- sample(nrow(Sonar))
# Randomly order data: Sonar
Sonar <- Sonar[rows,]
# Identify row to split on: split
split <- round(nrow(Sonar) * .60)
# Create train
train <- Sonar[1:split, ]
# Create test
test <- Sonar[(split + 1):nrow(Sonar), ]

##################################################
#GLM Logistic Matrix
##################################################
# Fit glm model: model
model <- glm(Class ~., family = "binomial", train)
# Predict on test: p
p <- predict(model, test, type ="response")
##################################################
#Calculate Confusion matrix
##################################################
library(caret)
# Calculate class probabilities: p_class
p_class_50 <- ifelse(p > .50, "M", "R")
##################################################
#Male sure that p_class has also 2 levels
##################################################
p_class_50 <- factor(p_class_50, levels = c("M", "R"))
levels(p_class_50)
nlevels(p_class_50)
# Create confusion matrix
confusionMatrix(p_class_50, test[["Class"]])
#Accuracy = 0.2892

##################################################
#Try another threshold 1 
##################################################
# Apply threshold of 1: p_class
p_class_90 <- ifelse(p > 1, "M", "R")
p_class_90 <- factor(p_class_90, levels = c("M", "R"))
levels(p_class_90)
nlevels(p_class_90)
# Create confusion matrix
confusionMatrix(p_class_90, test[["Class"]])
#Accuracy = 0.4217

##################################################
#Try another threshold 0.1 
##################################################
# Apply threshold of 1: p_class
p_class_10 <- ifelse(p > 0.1, "M", "R")
p_class_10 <- factor(p_class_10, levels = c("M", "R"))
levels(p_class_10)
nlevels(p_class_10)
# Create confusion matrix
confusionMatrix(p_class_10, test[["Class"]])
#Accuracy = 0.3012

##################################################
#Plot an ROC curve
##################################################
library(caTools)
# Predict on test: p
p <- predict(model, test, type = "response")
# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)

##################################################
#Customizing trainControl
##################################################
# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model_traincontrol <- train(Class~. , data = Sonar, method = "glm", trControl = myControl)
# Print model to console
model_traincontrol
#ROC 0.75