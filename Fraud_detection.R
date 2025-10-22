df <- read.csv('~/Documents/Udemy/Data Science with R/Projects/insurance_claims.csv')

head(df)

df$collision_type[df$collision_type == '?'] <- NA
df$property_damage[df$property_damage == '?'] <- NA
df$police_report_available[df$police_report_available == '?'] <- NA

# Removing impertinent columns

df <- subset(df, select = -c(policy_number, policy_bind_date, policy_state, insured_hobbies, 
                             X_c39, auto_year, auto_make, auto_model, insured_zip, incident_location,
                             incident_date))


# Making certain numeric columns factorised

df$policy_csl <- factor(df$policy_csl)
df$policy_deductable <- factor(df$policy_deductable)
df$insured_sex <- factor(df$insured_sex)
df$insured_education_level <- factor(df$insured_education_level)
df$insured_occupation <- factor(df$insured_occupation)
df$insured_relationship <- factor(df$insured_relationship)
df$incident_type <- factor(df$incident_type)
df$collision_type <- factor(df$collision_type)
df$incident_severity <- factor(df$incident_severity)
df$authorities_contacted <- factor(df$authorities_contacted)
df$incident_state <- factor(df$incident_state)
df$incident_city <- factor(df$incident_city)
df$number_of_vehicles_involved <- factor(df$number_of_vehicles_involved)
df$property_damage <- factor(df$property_damage)
df$bodily_injuries <- factor(df$bodily_injuries)
df$witnesses <- factor(df$witnesses)
df$police_report_available <- factor(df$police_report_available)
df$fraud_reported <- factor(df$fraud_reported)

summary(df)

# Correlation plot - Checking for multicollinearity

library(ggplot2)
library(corrplot)
library(gridExtra)

num.cols <- sapply(df, is.numeric)
corr.data <- cor(df[, num.cols])
corrplot(corr.data, method = 'color')

# We see that age and months_as_customer, and injury_claim, property_claim and vehicle_claim and 
# total_claim_amount are highly correlated. So, we will take off age and total_claim_amount.

df <- subset(df, select = -c(age, total_claim_amount))

### EDA ###

## 1-D Plots ##

# Density Plots

num.cols <- sapply(df, is.numeric)
num.col.names <- names(num.cols[num.cols == T])

cat.cols <- num.cols[num.cols == F]
cat.col.names <- names(cat.cols)

df.num <- df[, num.col.names]
df.cat <- df[, cat.col.names]

den.list <- list()

for (m in num.col.names){
  den <- ggplot(df.num, aes_string(m)) + geom_density(fill = "lightblue") + theme_bw()
  den.list[[m]] <- den
}

grid.arrange(grobs = den.list, ncol = 3)

# Bar Plots #

bar.list <- list()

for (c in cat.col.names){
  bar <- ggplot(df.cat, aes_string(c)) + geom_bar(fill = "lightblue", color = 'black') + theme_bw() + 
         theme(axis.text.x = element_text(angle = 15))
  bar.list[[c]] <- bar
}

grid.arrange(grobs = bar.list, ncol = 3)

# Box plots #

box_list <- list()

for (i in names(df.num)){
  box <- ggplot(df.num, aes_string(x = i)) + geom_boxplot(fill = 'lightblue') + theme_bw()
  box_list[[i]] <- box
}

grid.arrange(grobs = box_list, ncol = 2)

## 2-D Plots ##

den.1 <- ggplot(df, aes(x = months_as_customer)) + geom_density(fill = 'lightblue') + 
       facet_wrap(~ insured_sex, ncol = 2)
den.1

den.2 <- ggplot(df, aes(x = policy_annual_premium)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ policy_csl, ncol = 2)
den.2

den.3 <- ggplot(df, aes(x = injury_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ incident_type, ncol = 2)
den.3

den.4 <- ggplot(df, aes(x = injury_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ incident_severity, ncol = 2)
den.4

den.5 <- ggplot(df, aes(x = injury_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ number_of_vehicles_involved, ncol = 2)
den.5

den.6 <- ggplot(df, aes(x = injury_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ incident_state, ncol = 2)
den.6

den.7 <- ggplot(df, aes(x = property_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ policy_csl, ncol = 2)
den.7

den.8 <- ggplot(df, aes(x = property_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ incident_type, ncol = 2)
den.8

den.9 <- ggplot(df, aes(x = property_claim)) + geom_density(fill = 'lightblue') + 
         facet_wrap(~ incident_severity, ncol = 2)
den.9

den.10 <- ggplot(df, aes(x = property_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ number_of_vehicles_involved, ncol = 2)
den.10

den.11 <- ggplot(df, aes(x = property_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ incident_state, ncol = 2)
den.11

den.12 <- ggplot(df, aes(x = vehicle_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ policy_csl, ncol = 2)
den.12

den.13 <- ggplot(df, aes(x = vehicle_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ incident_type, ncol = 2)
den.13

den.14 <- ggplot(df, aes(x = vehicle_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ incident_severity, ncol = 2)
den.14

den.15 <- ggplot(df, aes(x = vehicle_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ number_of_vehicles_involved, ncol = 2)
den.15

den.16 <- ggplot(df, aes(x = vehicle_claim)) + geom_density(fill = 'lightblue') + 
          facet_wrap(~ incident_state, ncol = 2)
den.16

### Missing Value Imputation ###

missing.per.column <- colSums(is.na(df))
missing.per.column

library(VIM)

df.imputed <- kNN(df, k = 5)

colSums(is.na(df.imputed))

### Data Encoding ###

df.cat <- df.imputed[, cat.col.names]

head(df.cat)
summary(df.cat)

sex.func <- function(x){
  if(x == 'M'){
    return(0)
  }else{
    return(1)
  }
}

df.cat$insured_sex <- sapply(df.cat$insured_sex, sex.func)

property.func <- function(x){
  if (x == 'NO'){
    return(0)
  }else{
    return(1)
  }
}

df.cat$property_damage <- sapply(df.cat$property_damage, property.func)
df.cat$police_report_available <- sapply(df.cat$police_report_available, property.func)

df.cat <- subset(df.cat, select = -fraud_reported)


df.cat <- fastDummies::dummy_cols(df.cat)
df.cat <- subset(df.cat, select = -c(policy_csl, policy_deductable, insured_education_level,
                                     insured_occupation, insured_relationship, incident_type,
                                     collision_type, incident_severity, authorities_contacted,
                                     incident_state, incident_city, number_of_vehicles_involved,
                                     bodily_injuries, witnesses))


library(dplyr)

df.new <- bind_cols(df.num, df.cat)
df.new$fraud_reported <- df$fraud_reported

### Numerical Data Scaling ###

df.new$months_as_customer <- scale(df.new$months_as_customer, center = T, scale = T)
df.new$policy_annual_premium <- scale(df.new$policy_annual_premium, center = T, scale = T)
df.new$umbrella_limit <- scale(df.new$umbrella_limit, center = T, scale = T)
df.new$capital.gains <- scale(df.new$capital.gains, center = T, scale = T)
df.new$capital.loss <- scale(df.new$capital.loss, center = T, scale = T)
df.new$incident_hour_of_the_day <- scale(df.new$incident_hour_of_the_day, center = T, scale = T)
df.new$injury_claim <- scale(df.new$injury_claim, center = T, scale = T)
df.new$property_claim <- scale(df.new$property_claim, center = T, scale = T)
df.new$vehicle_claim <- scale(df.new$vehicle_claim, center = T, scale = T)

### Machine Learning Algorithms ###

library(caTools)

set.seed(101)

sample <- sample.split(df.new$fraud_reported, SplitRatio = 0.7)
train <- subset(df.new, sample == T)
test <- subset(df.new, sample == F)

## Logarithmic Classifier ##

log.model <- glm(fraud_reported ~ ., data = train, family = binomial('logit'))
summary(log.model)

log.pred <- predict(log.model, test[1:86])
log.pred.values <- 1*(log.pred > 0.5)

fraud.func <- function(x){
  if(x == 'N'){
    return(0)
  }else{
    return(1)
  }
}

test$fraud_report <- sapply(test$fraud_reported, fraud.func)

log.table <- table(test$fraud_report, log.pred.values)

library(caret)
library(Metrics)

log.conf.mat <- caret::confusionMatrix(log.table)
log.conf.mat

accuracy.log <- (201 + 27)/(201 + 27 + 47 + 25)
accuracy.log

recall.log <- caret::recall(log.table)
recall.log

prec.log <- caret::precision(log.table)
prec.log

f1_score.log <- 2*(prec.log * recall.log)/(prec.log + recall.log)
f1_score.log

### Decision Tree ###

library(rpart)

tree.model <- rpart(fraud_reported ~ ., data = train, method = 'class')
summary(tree.model)

tree.pred <- predict(tree.model, test[1:86])
tree.pred <- as.data.frame(tree.pred)

joiner <- function(x){
  if (x >= 0.5){
    return(1)
  }else{
    return(0)
  }
}

tree.pred$Yes <- sapply(tree.pred$Y, joiner)

tree.table <- table(test$fraud_report, tree.pred$Yes)

tree.conf.mat <- caret::confusionMatrix(tree.table)
tree.conf.mat

accuracy.tree <- (195 + 35)/(195 + 35 + 31 + 39)
accuracy.tree

recall.tree <- caret::recall(tree.table)
recall.tree

precision.tree <- caret::precision(tree.table)
precision.tree

f1_score.tree <- 2*(precision.tree * recall.tree)/(precision.tree + recall.tree)
f1_score.tree

### Random Forest ###

library(randomForest)

names(train) <- make.names(names(train))
rf.model <- randomForest(fraud_reported ~ ., importance = T, data = train)

rf.model$confusion

names(test) <- make.names(names(test))

rf.pred <- predict(rf.model, test[1:86])
rf.table <- table(rf.pred, test$fraud_reported)

accuracy.rf <- (201 + 28)/(201 + 28 + 46 + 25)
accuracy.rf

rf.conf.mat <- caret::confusionMatrix(rf.table)
rf.conf.mat

recall.rf <- caret::recall(rf.table)
recall.rf

precision.rf <- caret::precision(rf.table)
precision.rf

f1_score.rf <- 2*(precision.rf * recall.rf)/(precision.rf + recall.rf)
f1_score.rf

### Support Vector Classifier ###

library(e1071)

svm.model <- svm(fraud_reported ~ ., data = train)
summary(svm.model)

svm.pred <- predict(svm.model, test[1:86])
svm.table <- table(svm.pred, test$fraud_reported)

svm.conf.mat <- caret::confusionMatrix(svm.table)
svm.conf.mat

accuracy.svm <- (205 + 19)/(205 + 19 + 55 + 21)
accuracy.svm

recall.svm <- caret::recall(svm.table)
recall.svm

precision.svm <- caret::precision(svm.table)
precision.svm

f1_score.svm <- 2*(precision.svm * recall.svm)/(precision.svm + recall.svm)
f1_score.svm

## Tuning the Hyperparameters ##

svm.tune <- tune(svm, train.x = train[1:86], train.y = train[, 87], kernel = 'radial', 
                 ranges = list(cost = c(0.1,1,10), gamma = c(0.5,1,2)))
summary(svm.tune)

tuned.svm <- svm(fraud_reported ~ ., data = train, cost = 0.1, gamma = 0.5)
summary(tuned.svm)


tuned.svm.pred <- predict(tuned.svm, test[1:86])
tuned.svm.table <- table(tuned.svm.pred, test$fraud_reported)

tuned.svm.conf.mat <- caret::confusionMatrix(tuned.svm.table)
tuned.svm.conf.mat

accuracy.tuned_svm <- (226)/(226 + 74)
accuracy.tuned_svm

recall.tuned.svm <- caret::recall(tuned.svm.table)
recall.tuned.svm

precision.tuned.svm <- caret::precision(tuned.svm.table)
precision.tuned.svm

f1_score.tuned.svm <- 2*(precision.tuned.svm * recall.tuned.svm)/(precision.tuned.svm + recall.tuned.svm)
f1_score.tuned.svm

### Neural Net Classifier ###

library(neuralnet)

n <- names(train)
f <- as.formula(paste('fraud_reported ~ ', paste(n[!n %in% 'fraud_reported'], collapse = '+')))

nn.model <- neuralnet(f, data = train, hidden = c(5,3), linear.output = F)

#plot(nn.model)

nn.pred <- compute(nn.model, test[1:86])
true.predictions <- nn.pred$net.result * (max(test$fraud_report) - min(test$fraud_report)) +
                    min(test$fraud_report)

test.r <- test$fraud_report * (max(test$fraud_report) - min(test$fraud_report)) + 
          min(test$fraud_report)

MSE.nn <- sum((test.r - true.predictions)^2/nrow(test))
MSE.nn

error.df.nn <- data.frame(test.r, true.predictions)
head(error.df.nn)

error.df.nn$Y <- sapply(error.df.nn$X2, joiner)

table.nn <- table(error.df.nn$Y, test$fraud_report)
table.nn

nn.conf.mat <- caret::confusionMatrix(table.nn)
nn.conf.mat

accuracy.nn <- (142 + 45)/(142 + 45 + 29 + 84)
accuracy.nn

recall.nn <- caret::recall(table.nn)
recall.nn

precision.nn <- caret::precision(table.nn)
precision.nn

f1_score.nn <- 2*(precision.nn * recall.nn)/ (precision.nn + recall.nn)
f1_score.nn

### ADA Boost Classifier ###

library(adabag)

train_control <- trainControl(method = 'cv', number = 10)

adaboost.model <- train(fraud_reported ~ ., data = train, method = 'AdaBoost.M1', 
                        tr.control = train_control)

adaboost.model

ada.pred <- predict(adaboost.model, test[1:86])

table.ada <- table(ada.pred, test$fraud_reported)

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

### XGBoost Classifier ###

library(xgboost)

train.matrix <- as.matrix(as.numeric(unlist(train)))
train.label <- as.integer(train$fraud_reported) - 1


test.matrix <- as.matrix(as.numeric(unlist(test)))
dim(test.matrix) <- c(300, 88)
test.label <- as.integer(test$fraud_reported) - 1

train.cp <- data.frame(train)

train.cp$fraud_report <- sapply(train.cp$fraud_reported, fraud.func)
  
dtrain <- xgb.DMatrix(data = as.matrix(train[,-87]), label = train.cp$fraud_report)
dtest <- xgb.DMatrix(data = as.matrix(test[,-c(87,88)]), label = test$fraud_report)

num_class <- length(levels(train$fraud_reported))

params <- list(booster = 'gbtree', eta = 0.001, max_depth = 5, gamma = 3, subsample = 0.75, 
               objective = 'binary:logistic', eval_metric = 'logloss', num_class = 1) 


xgb.model <- xgboost(data = dtrain, params = params, nrounds = 1000, verbose = 0)

xgb.pred <- predict(xgb.model, dtest[1:300])

xgb.predict <- ifelse(xgb.pred > 0.5, 1, 0)

table.xgb <- table(xgb.predict, test$fraud_report)
table.xgb

xgb.conf.mat <- caret::confusionMatrix(table.xgb)
xgb.conf.mat

accuracy.xgb <- (192 + 43)/(192 + 43 + 31 + 34)
accuracy.xgb

recall.xgb <- caret::recall(table.xgb)
recall.xgb

precision.xgb <- caret::precision(table.xgb)
precision.xgb

f1_score.xgb <- 2*(recall.xgb * precision.xgb)/(recall.xgb + precision.xgb)
f1_score.xgb

### Stochastic Gradient Boosting Classifier ###

st_gbm.model <- gbm(fraud_report ~ ., data = train.cp[,-87], distribution = 'bernoulli', n.trees = 1000,
                    interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.7, cv.folds = 5)

st_gb.pred <- predict(st_gbm.model, newdata = test[1:86], ntree = 1000)
st_gb.predict <- ifelse(st_gb.pred > 0.5, 0, 1)

table.st_gbm <- table(st_gb.predict, test$fraud_report)
table.st_gbm

stgb.conf.mat <- caret::confusionMatrix(table.st_gbm)
stgb.conf.mat

recall.stgb <- caret::recall(table.st_gbm)
recall.stgb 

precision.stgb <- caret::precision(table.st_gbm)
precision.stgb

## Tuning Parameters ##

gbm.grid <- expand.grid(n.trees = c(1000, 5000, 10000), interaction.depth = c(1,2,3,4,5), 
                        shrinkage = c(0.001, 0.01), n.minobsinnode = c(5,10))

tr.control <- trainControl(method = 'cv', number = 10, verboseIter = T)

gbm_tuned <- train(fraud_report ~ ., data = train.cp[,-87], method = 'gbm', trControl = tr.control,
                   tuneGrid = gbm.grid, verbose = F)

gbm_tuned$bestTune

sgbm_tuned.predict <- predict(gbm_tuned, test[1:86]) 
sgbm_tuned.pred <- ifelse(sgbm_tuned.predict > 0.5, 1, 0)

table.tuned_stgb <- table(sgbm_tuned.pred, test$fraud_report)
table.tuned_stgb

tuned_stgb.conf.mat <- caret::confusionMatrix(table.tuned_stgb)
tuned_stgb.conf.mat

accuracy.tuned_stgb <- (188 + 51)/(188 + 51 + 23 + 38)
accuracy.tuned_stgb

recall.tuned_stgb <- caret::recall(table.tuned_stgb)
recall.tuned_stgb 

precision.tuned_stgb <- caret::precision(table.tuned_stgb)
precision.tuned_stgb

f1_score.tuned_stgb <- 2*(precision.tuned_stgb * recall.tuned_stgb)/(precision.tuned_stgb + recall.tuned_stgb)
f1_score.tuned_stgb

########### Final Scores ###############

accuracy_scores <- c(accuracy.log, accuracy.tree, accuracy.rf, accuracy.svm, accuracy.tuned_svm, 
                     accuracy.nn, accuracy.ada, accuracy.xgb, accuracy.tuned_stgb)

recall_scores <- c(recall.log, recall.tree, recall.rf, recall.svm, recall.tuned.svm, recall.nn,
                   recall.ada, recall.xgb, recall.tuned_stgb)

precision_scores <- c(prec.log, precision.tree, precision.rf, precision.svm, 
                      precision.tuned.svm, precision.nn, precision.ada, precision.xgb, 
                      precision.tuned_stgb)

f1_scores <- c(f1_score.log, f1_score.tree, f1_score.rf, f1_score.svm, f1_score.tuned.svm, 
               f1_score.nn, f1_score.ada, f1_score.xgb, f1_score.tuned_stgb)

models <- c('Logarithmic', 'Decision Tree', 'Random Forest', 'Support Vector Machines', 'Tuned_SVM',
            'Neural Nets', 'ADA Boosting', 'XGBoosting', 'Tuned Stochastic Gradient Boosting')
df.score <- data.frame(Models = models, Accuracy = accuracy_scores, Recall = recall_scores, 
                       Precision = precision_scores, F1 = f1_scores)

df.score

# Since in the case of Fraud detection, the cost of false negatives is high. We need to determine
# all the possible fraudulent cases. So, we would prefer recall scores over precision. So, we can
# use SVM as our final model, or Random Forest to determine if we have a fraudulent claim.




















































































































































































