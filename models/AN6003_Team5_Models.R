library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(randomForest)
library(e1071)

cols <-
  c(
    "age",
    "sex",
    "cp",
    "trestbps",
    "chol",
    "fbs",
    "restecg",
    "thalach",
    "exang",
    "oldpeak",
    "slope",
    "ca",
    "thal",
    "num"
  )

data <-
  read.table(
    "processed.cleveland.data",
    sep = ",",
    col.names = cols,
    na.strings = c("?")
  )

data = na.omit(data)

data$num[data$num > 1] <- 1
# Relabelling of Columns  -----------------------------------------------------------------
# # Relabel sex
# data$sex[data$sex == 0] = "female"
# data$sex[data$sex == 1] = "male"
# data$sex = factor(data$sex)
# 
# # Relabel cp
# data$cp[data$cp == 1] = "typical angina"
# data$cp[data$cp == 2] = "atypical angina"
# data$cp[data$cp == 3] = "non-anginal pain"
# data$cp[data$cp == 4] = "asymptomatic"
# data$cp = factor(data$cp)
# 
# # Relabel fbs
# data$fbs[data$fbs == 0] = "false"
# data$fbs[data$fbs == 1] = "true"
# data$fbs = factor(data$fbs)
# 
# # Relabel exang
# data$exang[data$exang == 0] = "no"
# data$exang[data$exang == 1] = "yes"
# data$exang = factor(data$exang)
# 
# # Relabel slope
# data$slope[data$slope == 1] = "upsloping"
# data$slope[data$slope == 2] = "flat"
# data$slope[data$slope == 3] = "downsloping"
# data$slope = factor(data$slope)
# 
# # Relabel ca
# data$ca = factor(data$ca)
# 
# # Relabel thal
# data$thal[data$thal == 3] = "normal"
# data$thal[data$thal == 6] = "fixed defect"
# data$thal[data$thal == 7] = "reversable defect"
# data$thal = factor(data$thal)
# 
# # Relabel num
# # data$num[data$num == 0] = "NO"
# # data$num[data$num == 1] = "YES"
# # data$num[data$num == 2] = "YES"
# # data$num[data$num == 3] = "YES"
# # data$num[data$num == 4] = "YES"
# data$num[data$num > 1] <- 1
# data$num = factor(data$num)

# summary(data)
# str(data)

# Factor  ----------
for (i in c(2, 3, 6, 7, 9, 11, 12, 13, 14)) {
  data[, i] <- factor(data[, i])
}

# Train Test Split Stratified by Gender------

set.seed(139)

selected <- sample.split(Y = data$sex, SplitRatio = 0.3)
test = subset(data, selected == T)
train = subset(data, selected == F)

table(data$sex)
table(train$sex)
table(test$sex)

# Model------------------------------------------------------------------------------------
set.seed(5071)

m.forest = randomForest(num ~ .,
                        data = train,
                        na.action = na.omit,
                        importance = T)

m.forest

# OOB error rates
# plot(m.forest)

# Variable Importance
var.impt = importance(m.forest)

# varImpPlot(m.forest, type = 1)

# Accuracy ------
predict_forest <-
  predict(
    m.forest,
    newdata = test,
    type = "class",
    ntree = 500,
    mtry = 6,
    nodesize = 3
  )
confusionMatrix(predict_forest, test$num)$overall[[1]]
# Accuracy: 0.9213483

## Logistic ------
m.logistic <- glm(num ~ ., family = binomial, data = train)
prob <- predict(m.logistic, test, type = "response")
threshold <- 0.5
predict_logistic <- ifelse(prob > threshold, 1, 0)
confusionMatrix(factor(predict_logistic), test$num)$overall[[1]]
# Accuracy: 0.8988764

## CART Decision Tree ------
getOptCp <- function(m) {
  CVerror.cap <-
    m$cptable[which.min(m$cptable[, "xerror"]), "xerror"] +
    m$cptable[which.min(m$cptable[, "xerror"]), "xstd"]
  i <- 1
  j <- 4
  while (m$cptable[i, j] > CVerror.cap) {
    i <- i + 1
  }
  ifelse(i > 1, sqrt(m$cptable[i, 1] * m$cptable[i - 1, 1]), 1)
}
m1.cart <- rpart(num ~ .,
                 data = train,
                 method = "class",
                 cp = 0)
cp.opt <- getOptCp(m1.cart)
m.cart <- prune(m1.cart, cp = cp.opt)
m.cart.rules <- rpart.rules(m.cart, nn = T, cover = T)
# View(m.cart.rules)
predict_cart <- predict(m.cart, type = "class", newdata = test)
confusionMatrix(test$num, factor(predict_cart))$overall[[1]]
# Accuracy: 0.7865169

## Support Vector Machine ------
m.svm <- svm(num ~ .,
             data = train)
predict_svm <- predict(m.svm, newdata = test)
confusionMatrix(predict_svm, test$num)$overall[[1]]
# Accuracy: 0.8988764

# newData <-
#   data.frame(
#     "age" = 37,
#     "sex" = factor(1),
#     "cp" = factor(3),
#     "trestbps" = 130,
#     "chol" = 250,
#     "fbs" = factor(0),
#     "restecg" = factor(0),
#     "thalach" = 187,
#     "exang" = factor(0),
#     "oldpeak" = 3.5,
#     "slope" = factor(3),
#     "ca" = factor(0),
#     "thal" = factor(3),
#     "num" = factor(0)
#   )

getMyRfProb <- function(nd) {
  nd <- rbind(data[1, ], nd)
  nd <- nd[-1, ]
  predict(m.forest, newdata = nd, type = "prob")[2]
}
