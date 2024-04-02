#Final Project 

library(glmnet)
library(tidyr)
library(caret)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


#import the data set
shaq <- read.csv("E:\\GMU\\STAT 515\\Final Project\\shaq-nba-career-regular-season-stats-by-game.csv")

#Checking the dataset
head(shaq, 10)

tail(shaq, 10)

str(shaq)



#Personal Fouls in his all time Game Score
Fouls <- ggplot(aes(x=PF,y=GmSc),data=shaq)+
  geom_point()+
  xlab("Personal Fouls (PF)")+
  ylab("Game Score (GmSc)")+ 
  ggtitle("Personal Fouls in his all time carrier game")+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_minimal()
Fouls





#Total Rebounds in his all time carrier game
Rebounds <- ggplot(aes(x=TRB,y=CarrGm),data=shaq)+
  geom_point()+
  xlab("Total Rebounds")+
  ylab("Carrier Games")+ 
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  ggtitle("Total Rebounds in his all time carrier game")
Rebounds


#Relation of Total rebounds and Personal fouls
Relation <- ggplot(aes(x=TRB,y=PF),data=shaq)+
  geom_point()+
  xlab("Total Rebounds")+
  ylab("Personal Fouls")+ 
  ggtitle("Relationship between Total Rebounds and Personal Fouls")+
  geom_smooth(method = "lm", se = FALSE, color = "blue")
Relation



# Split the data into training and testing sets
set.seed(8495)  # for reproducibility
sample_indices <- sample(nrow(shaq), 0.8 * nrow(shaq))
train_data <- shaq[sample_indices, ]
test_data <- shaq[-sample_indices, ]


#creating a linear regression model
model <- lm(PTS~PF + TRB, data=shaq)
summary(model)
par(mfrow = c(2, 2))
plot(model)

predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to class labels (Win or Loss)
predicted_classes <- ifelse(predictions > 0.5, "Win", "Loss")

# Create a confusion matrix to evaluate the model
confusion_matrix <- table(test_data$Win, predicted_classes)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))




# Build a logistic regression model
logistic_model <- glm(Win ~ TRB + PF + PTS + AST + BLK + STL + FT + GS ,
                      data = train_data, 
                      family = "binomial")

# Summary of the model
summary(logistic_model)

#Plotting the logistic model
par(mfrow = c(2, 2))
plot(logistic_model)

# Make predictions on the test set
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to class labels (Win or Loss)
predicted_classes <- ifelse(predictions > 0.5, "Win", "Loss")

# Create a confusion matrix to evaluate the model
confusion_matrix <- table(test_data$Win, predicted_classes)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))


# Regression Tree model
trb_tree_model <- rpart(PTS ~ PF + TRB, data = shaq,cp=0.001)
rpart.plot(trb_tree_model, type = 1, extra = 1, main = "Regression Tree")
var(shaq$Win)
