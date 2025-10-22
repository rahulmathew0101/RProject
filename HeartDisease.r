df <- read.csv('~/Documents/Udemy/Data Science with R/Projects/heart.csv')

head(df)
str(df)
summary(df)

df$Sex <- factor(df$Sex)
df$ChestPainType <- factor(df$ChestPainType)
df$ExerciseAngina <- factor(df$ExerciseAngina)
df$ST_Slope <- factor(df$ST_Slope)
df$RestingECG <- factor(df$RestingECG)
df$FastingBS <- factor(df$FastingBS)
df$HeartDisease <- factor(df$HeartDisease)

############ EDA ###################

library(ggplot2)
library(corrplot)
library(gridExtra)

num.cols <- sapply(df, is.numeric)
corr.data <- cor(df[,num.cols])
corrplot(corr.data, method = 'color')

## 1-D plots ##

den.list <- list()

num.col.names <- names(num.cols[num.cols == T])

for (n in num.col.names){
  den <- ggplot(df, aes_string(n)) + geom_density(fill = 'lightblue', color = 'black') 
  den.list[[n]] <- den
}

grid.arrange(grobs = den.list, ncol = 3)

cat.col.names <- names(num.cols[num.cols == F])

bar.list <- list()

for (b in cat.col.names){
  bar <- ggplot(df, aes_string(b)) + geom_bar(color = 'black', fill = 'lightblue')
  bar.list[[b]] <- bar
}

grid.arrange(grobs = bar.list, ncol = 3)

box.list <- list()

for (x in num.col.names){
  box <- ggplot(df, aes_string(x)) + geom_boxplot(fill = 'lightblue')
  box.list[[x]] <- box
}

grid.arrange(grobs = box.list, ncol = 3)

## 2-D Plots ##

den.1 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ Sex, ncol = 2) + labs(title = 'Resting BP v/s Sex')
den.1

den.2 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ ChestPainType, ncol = 2) + labs(title = 'Resting BP v/s Chest Pain Type')
den.2

den.3 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ FastingBS, ncol = 2) + labs(title = 'Resting BP v/s Fasting BS')
den.3

den.4 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ RestingECG, ncol = 2) + labs(title = 'Resting BP v/s Resting ECG')
den.4

den.5 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ ExerciseAngina, ncol = 2) + labs(title = 'Resting Bp v/s Exercise Angina')
den.5

den.6 <- ggplot(df, aes(RestingBP)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ ST_Slope, ncol = 2) + labs(title = 'Resting BP v/s ST Slope')
den.6

den.7 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ Sex, ncol = 2) + labs(title = 'Cholesterol v/s Sex')
den.7

den.8 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ ChestPainType, ncol = 2) + labs(title = 'Cholesterol v/s Chest Pain Type')
den.8

den.9 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
         facet_wrap(~ FastingBS, ncol = 2) + labs(title = 'Cholesterol v/s Fasting BS')
den.9

den.10 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
          facet_wrap(~ RestingECG, ncol = 2) + labs(title = 'Cholesterol v/s Resting ECG')
den.10

den.11 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
          facet_wrap(~ ExerciseAngina, ncol = 2) + labs(title = 'Cholesterol v/s Exercise Angina')
den.11

den.12 <- ggplot(df, aes(Cholesterol)) + geom_density(fill = 'lightblue', color = 'black') + 
          facet_wrap(~ ST_Slope, ncol = 2) + labs(title = 'Cholesterol v/s ST Slope')
den.12

den.13 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
          facet_wrap(~ Sex, ncol = 2) + labs(title = 'MaxHR v/s Sex')
den.13

den.14 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ChestPainType, ncol = 2) + labs(title = 'Max HR v/s Chest Pain Type')
den.14

den.15 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ FastingBS, ncol = 2) + labs(title = 'Max HR v/s Fasting BS')
den.15

den.16 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ RestingECG, ncol = 2) + labs(title = 'Max HR v/s Resting ECG')
den.16

den.17 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ExerciseAngina, ncol = 2) + labs(title = 'Max HR v/s Exercise Angina')
den.17

den.18 <- ggplot(df, aes(MaxHR)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ST_Slope, ncol = 2) + labs(title = 'Max HR v/s ST Slope')
den.18

den.19 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ Sex, ncol = 2) + labs(title = 'Old Peak v/s Sex')
den.19

den.20 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ChestPainType, ncol = 2) + labs(title = 'Old Peak v/s Chest Pain Type')
den.20

den.21 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ FastingBS, ncol = 2) + labs(title = 'Old Peak v/s Fasting BS')
den.21

den.22 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ RestingECG, ncol = 2) + labs(title = 'Old Peak v/s Resting ECG')
den.22

den.23 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ExerciseAngina, ncol = 2) + labs(title = 'Old Peak v/s Exercise Angina')
den.23

den.24 <- ggplot(df, aes(Oldpeak)) + geom_density(fill = 'lightblue', color = 'black') + 
  facet_wrap(~ ST_Slope, ncol = 2) + labs(title = 'Old Peak v/s ST Slope')
den.24

bar.1 <- ggplot(df, aes(ChestPainType)) + geom_bar(aes(fill = Sex), position = 'dodge', color = 'black')
bar.1

bar.2 <- ggplot(df, aes(ChestPainType)) + geom_bar(aes(fill = FastingBS), position = 'dodge', color = 'black')
bar.2

bar.3 <- ggplot(df, aes(ChestPainType)) + geom_bar(aes(fill = RestingECG), position = 'dodge', color = 'black')
bar.3

bar.4 <- ggplot(df, aes(ChestPainType)) + geom_bar(aes(fill = ExerciseAngina), position = 'dodge', color = 'black')
bar.4

bar.5 <- ggplot(df, aes(ChestPainType)) + geom_bar(aes(fill = ST_Slope), position = 'dodge', color = 'black')
bar.5

bar.6 <- ggplot(df, aes(RestingECG)) + geom_bar(aes(fill = Sex), position = 'dodge', color = 'black')
bar.6

bar.7 <- ggplot(df, aes(RestingECG)) + geom_bar(aes(fill = FastingBS), position = 'dodge', color = 'black')
bar.7

bar.8 <- ggplot(df, aes(RestingECG)) + geom_bar(aes(fill = ChestPainType), position = 'dodge', color = 'black')
bar.8

bar.9 <- ggplot(df, aes(RestingECG)) + geom_bar(aes(fill = ExerciseAngina), position = 'dodge', color = 'black')
bar.9

bar.10 <- ggplot(df, aes(RestingECG)) + geom_bar(aes(fill = ST_Slope), position = 'dodge', color = 'black')
bar.10

scat.1 <- ggplot(df, aes(Age, RestingBP)) + geom_point()
scat.1

scat.2 <- ggplot(df, aes(Age, Cholesterol)) + geom_point()
scat.2

scat.3 <- ggplot(df, aes(Age, MaxHR)) + geom_point()
scat.3

scat.4 <- ggplot(df, aes(Age, Oldpeak)) + geom_point()
scat.4


############## Data Pre-processing ##################

#### Handling Null Value #######

any(is.na(df))

# So we have no Null values.

#### Scaling ######

df.num <- df[, num.cols]


df.num$Age <- scale(df.num$Age, center = T, scale = T)
df.num$RestingBP <- scale(df.num$RestingBP, center = T, scale = T)
df.num$Cholesterol <- scale(df.num$Cholesterol, center = T, scale = T)
df.num$MaxHR <- scale(df.num$MaxHR, center = T, scale = T)

#### Encoding ####

df.cat <- df[, cat.col.names]

df.cat$Sex <- ifelse(df.cat$Sex == 'M', 0, 1)
df.cat$ExerciseAngina <- ifelse(df.cat$ExerciseAngina == 'N', 0, 1)

df.cat <- subset(df.cat, select = -HeartDisease)

df.cat <- fastDummies::dummy_cols(df.cat, select_columns = c('ChestPainType', 'RestingECG', 'ST_Slope'))

df.cat <- subset(df.cat, select = -c(ChestPainType, RestingECG, ST_Slope))


library(dplyr)

df.new <- bind_cols(df.num, df.cat)
df.new$HeartDisease <- df$HeartDisease


################# Machine Learning ##################

#### Logistic Regression #####

library(caTools)

set.seed(101)

sample <- sample.split(df.new$HeartDisease, SplitRatio = 0.7)
train <- subset(df.new, sample == T)
test <- subset(df.new, sample == F)

log.model <- glm(HeartDisease ~ ., family = binomial('logit'), data = train)
summary(log.model)


log.pred <- predict(log.model, test[1:18])
log.pred.values <- 1*(log.pred > 0.5)
table.log <- table(test$HeartDisease, log.pred.values)

library(caret)
library(Metrics)

log.conf.mat <- caret::confusionMatrix(table.log)
log.conf.mat

accuracy.log <- (110 + 130)/(110 + 130 + 22 + 13)
accuracy.log

recall.log <- caret::recall(table.log)
recall.log

precision.log <- caret::precision(table.log)
precision.log

f1_score.log <- 2*(recall.log * precision.log)/(recall.log + precision.log)
f1_score.log

##### Decision Tree ####

library(rpart)

tree.model <- rpart(HeartDisease ~ ., method = 'class', data = train)
summary(tree.model)

tree.pred <- predict(tree.model, test[1:18])
tree.pred <- as.data.frame(tree.pred)

joiner <- function(x){
  if(x >= 0.5){
    return(1)
  }else{
    return(0)
  }
}

tree.pred$Yes <- sapply(tree.pred$`1`, joiner)

#head(tree.pred)

tree.table <- table(test$HeartDisease, tree.pred$Yes)

conf.mat.tree <- caret::confusionMatrix(tree.table)
conf.mat.tree

accuracy.tree <- (99+134)/(99 + 134 + 24 + 18)
accuracy.tree

recall.tree <- caret::recall(tree.table)
recall.tree

precision.tree <- caret::precision(tree.table)
precision.tree

f1_score.tree <- 2*(recall.tree * precision.tree)/(precision.tree + recall.tree)
f1_score.tree

#library(rpart.plot)

#prp(tree.model)

###### Random Forest ######

library(randomForest)

rf.model <- randomForest(HeartDisease ~ ., data = train, importance = TRUE)

rf.model$confusion
rf.model$importance

rf.pred <- predict(rf.model, test[1:18])

rf.table <- table(rf.pred, test$HeartDisease)

conf.mat.rf <- caret::confusionMatrix(rf.table)
conf.mat.rf

accuracy.rf <- (103 + 139)/(103 + 139 + 13 + 20)
accuracy.rf

recall.rf <- caret::recall(rf.table)
recall.rf

precision.rf <- caret::precision(rf.table)
precision.rf

f1_score.rf <- 2*(precision.rf * recall.rf)/(precision.rf + recall.rf)
f1_score.rf

##### KNN Algorithm ####

library(class)

predict.knn <- NULL
error.rate <- NULL

for (i in 1:10){
  predict.knn <- knn(train, test, train$HeartDisease, k = i)
  error.rate[i] <- mean(test$HeartDisease != predict.knn)
}

k.values <- 1:10

error.df <- data.frame(error.rate, k.values)

elbow <- ggplot(error.df, aes(k.values, error.rate)) + geom_point() + 
         geom_line(lty = 'dotted', color = 'red')
elbow

# So we select k = 4

knn.model <- knn(train, test, train$HeartDisease, k = 4)

knn.table <- table(test$HeartDisease, knn.model)

conf.mat.knn <- caret::confusionMatrix(knn.table)
conf.mat.knn

accuracy.knn <- (109 + 147)/(109 + 147 + 5 + 14)
accuracy.knn

recall.knn <- caret::recall(knn.table)
recall.knn

precision.knn <- caret::precision(knn.table)
precision.knn

f1_score.knn <- 2*(precision.knn * recall.knn)/(precision.knn + recall.knn)
f1_score.knn

####### SVM ########

library(e1071)

svm.model <- svm(HeartDisease ~ ., data = train)
summary(svm.model)

pred.svm <- predict(svm.model, test[1:18])
table.svm <- table(test$HeartDisease, pred.svm)

conf.mat.svm <- caret::confusionMatrix(table.svm)
conf.mat.svm

accuracy.svm <- (103+140)/(103 + 140 + 20 + 12)
accuracy.svm

recall.svm <- caret::recall(table.svm)
recall.svm

precision.svm <- caret::precision(table.svm)
precision.svm

f1_score.svm <- 2*(precision.svm * recall.svm)/(precision.svm + recall.svm)
f1_score.svm

## Tuning the Hyperparameters ##

train.cp <- data.frame(train)
test.cp <- data.frame(test)

train.cp[] <- lapply(train.cp, function(x){
  if (is.factor(x)){
    as.numeric(as.character(x))
  }else{
    x
  }
})

test.cp[] <- lapply(test.cp, function(x){
  if (is.factor(x)){
    as.numeric(as.character(x))
  }else{
    x
  }
})

tune.results <- tune(svm, train.x = train.cp[1:18], train.y = train.cp[,19], kernel = 'radial', ranges = list(cost = c(0.1,1,10), gamma = c(0.5,1,2)))
summary(tune.results)

tuned.svm <- svm(HeartDisease ~ ., data = train, cost = 1, gamma = 0.5)
summary(tuned.svm)

tuned.svm.predict <- predict(tuned.svm, test[,1:18])
tuned.svm.table <- table(test$HeartDisease, tuned.svm.predict)

conf.mat.tuned_svm <- caret::confusionMatrix(tuned.svm.table)
conf.mat.tuned_svm

accuracy.tuned_svm <- (78 + 144)/(78 + 144 + 45 + 8)
accuracy.tuned_svm

recall.tuned_svm <- caret::recall(tuned.svm.table)
recall.tuned_svm

precision.tuned_svm <- caret::precision(tuned.svm.table)
precision.tuned_svm

f1_score.tuned_svm <- 2*(precision.tuned_svm * recall.tuned_svm)/(precision.tuned_svm + recall.tuned_svm)
f1_score.tuned_svm

####### Neural Nets ########

library(neuralnet)

n <- names(train)
f <- as.formula(paste('HeartDisease ~ ', paste(n[!n %in% 'HeartDisease'], collapse = '+')))

nn.model <- neuralnet(f, data = train.cp, hidden = c(5,3), linear.output = F)

#plot(nn.model)

predicted.nn.values <- compute(nn.model, test.cp[1:18])

true.predictions.nn <- predicted.nn.values$net.result * (max(train.cp$HeartDisease) - min(train.cp$HeartDisease)) + min(train.cp$HeartDisease)

test.r <- (test.cp$HeartDisease) * (max(train.cp$HeartDisease) - min(train.cp$HeartDisease)) + min(train.cp$HeartDisease)

error.df.nn <- data.frame(test.r, true.predictions.nn)

head(error.df.nn)

error.df.nn$Y <- sapply(error.df.nn$true.predictions.nn, joiner)

table.nn <- table(error.df.nn$Y, test$HeartDisease)
table.nn

nn.conf.mat <- caret::confusionMatrix(table.nn)
nn.conf.mat

accuracy.nn <- (107 + 128)/(107 + 128 + 24 + 16)
accuracy.nn

recall.nn <- caret::recall(table.nn)
recall.nn

precision.nn <- caret::precision(table.nn)
precision.nn

f1_score.nn <- 2*(precision.nn * recall.nn)/ (precision.nn + recall.nn)
f1_score.nn

## Ada Boost Classifier ##

library(adabag)

train_control <- trainControl(method = 'cv', number = 10)

adaboost.model <- train(HeartDisease ~ ., data = train.cp, method = 'AdaBoost.M1', 
                        tr.control = train_control)

adaboost.model

ada.pred <- predict(adaboost.model, test[1:18])

table.ada <- table(ada.pred, test$HeartDisease)

ada.conf.mat <- caret::confusionMatrix(table.ada)
ada.conf.mat

accuracy.ada <- (194 + 45)/(194 + 45 + 32 + 29)
accuracy.ada

recall.ada <- caret::recall(table.ada) 
recall.ada

precision.ada <- caret::precision(table.ada)
precision.ada

f1_score.ada <- 2*(precision.ada * recall.ada)/(precision.ada + recall.ada)
f1_score.ada

## Stochastic Gradient Boosting Algorithm ##

library(gbm)

stgb.model <- gbm(HeartDisease ~ ., data = train.cp, distribution = 'bernoulli', n.trees = 1000,
                    interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.7, cv.folds = 5)

stgb.pred <- predict(stgb.model, newdata = test.cp[1:18], ntree = 1000)
stgb.predict <- ifelse(stgb.pred > 0.5, 0, 1)

table.stgb <- table(stgb.predict, test.cp$HeartDisease)
table.stgb

stgb.conf.mat <- caret::confusionMatrix(table.stgb)
stgb.conf.mat

recall.stgb <- caret::recall(table.stgb)
recall.stgb 

precision.stgb <- caret::precision(table.stgb)
precision.stgb

# Tuning the parameters #

gbm.grid <- expand.grid(n.trees = c(1000, 5000, 10000), interaction.depth = c(1,2,3,4,5), 
                        shrinkage = c(0.001, 0.01), n.minobsinnode = c(5,10))

tr.control <- trainControl(method = 'cv', number = 10, verboseIter = F)

gbm_tuned <- train(HeartDisease ~ ., data = train.cp, method = 'gbm', trControl = tr.control,
                   tuneGrid = gbm.grid, verbose = F)

gbm_tuned$bestTune

sgbm_tuned.predict <- predict(gbm_tuned, test.cp[1:18]) 
sgbm_tuned.pred <- ifelse(sgbm_tuned.predict > 0.5, 1, 0)

table.tuned_stgb <- table(sgbm_tuned.pred, test.cp$HeartDisease)
table.tuned_stgb

tuned_stgb.conf.mat <- caret::confusionMatrix(table.tuned_stgb)
tuned_stgb.conf.mat

accuracy.tuned_stgb <- (104 + 141)/(104 + 141 + 19 + 11)
accuracy.tuned_stgb

recall.tuned_stgb <- caret::recall(table.tuned_stgb)
recall.tuned_stgb 

precision.tuned_stgb <- caret::precision(table.tuned_stgb)
precision.tuned_stgb

f1_score.tuned_stgb <- 2*(precision.tuned_stgb * recall.tuned_stgb)/(precision.tuned_stgb + recall.tuned_stgb)
f1_score.tuned_stgb

## XGBoost Algorithm ##

library(xgboost)

train.matrix <- as.matrix(unlist(train.cp))
dim(train.matrix) <- c(643, 19)
train.label <-  train.cp$HeartDisease

test.matrix <- as.matrix(as.numeric(unlist(test)))
dim(test.matrix) <- c(275, 19)
test.label <- test.cp$HeartDisease

dtrain <- xgb.DMatrix(data = train.matrix[, 1:18], label = train.cp$HeartDisease)
dtest <- xgb.DMatrix(data = test.matrix[, 1:18], label = test.cp$HeartDisease)

num_class <- length(levels(train$HeartDisease))

params <- list(booster = 'gbtree', eta = 0.001, max_depth = 5, gamma = 3, subsample = 0.75, 
               objective = 'binary:logistic', eval_metric = 'logloss', num_class = 1) 


xgb.model <- xgboost(data = dtrain, params = params, nrounds = 1000, verbose = 0)

xgb.pred <- predict(xgb.model, dtest[1:275])

xgb.predict <- ifelse(xgb.pred > 0.5, 1, 0)

table.xgb <- table(xgb.predict, test.cp$HeartDisease)
table.xgb

xgb.conf.mat <- caret::confusionMatrix(table.xgb)
xgb.conf.mat

accuracy.xgb <- (101 + 137)/(101 + 137 + 22 + 15)
accuracy.xgb

recall.xgb <- caret::recall(table.xgb)
recall.xgb

precision.xgb <- caret::precision(table.xgb)
precision.xgb

f1_score.xgb <- 2*(recall.xgb * precision.xgb)/(recall.xgb + precision.xgb)
f1_score.xgb

### Final Scores ###

accuracy_scores <- c(accuracy.log, accuracy.tree, accuracy.rf, accuracy.knn, accuracy.nn, 
                     accuracy.svm, accuracy.tuned_svm, accuracy.tuned_stgb, accuracy.ada, 
                     accuracy.xgb)

recall_scores <- c(recall.log, recall.tree, recall.rf, recall.knn, recall.nn, recall.svm, 
                   recall.tuned_svm, recall.tuned_stgb, recall.ada, recall.xgb)

precision_scores <- c(precision.log, precision.tree, precision.rf, precision.knn, precision.nn,
                      precision.svm, precision.tuned_svm, precision.tuned_stgb, precision.ada, 
                      precision.xgb)

f1_scores <- c(f1_score.log, f1_score.tree, f1_score.rf, f1_score.knn, f1_score.nn, f1_score.svm,
               f1_score.tuned_svm, f1_score.tuned_stgb, f1_score.ada, f1_score.xgb)

models <- c('Logarithmic', 'Decision Tree', 'Random Forest', 'KNN', 'Neural Nets', 'SVM', 
            'Tuned SVM', 'Tuned Stochastic Gradient Booster', 'Ada Booster', 'XGBooster')

df.scores <- data.frame(Models = models, Accuracy = accuracy_scores, Recall = recall_scores,
                           Precision = precision_scores, F1_Score = f1_scores)

df.scores

# Here, we need to minimize the false positives, so we will take precision scores as the primary
# metric to choose the model. We can opt for Ada Booster or Tuned Stochastic Gradient Booster for 
# our final model. Ada Booster also provides higher F1_score, so we can use that model. 





























