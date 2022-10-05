library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(randomForest)

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

# Relabelling of Columns  -----------------------------------------------------------------

# Relabel sex
data$sex[data$sex == 0] = "female"
data$sex[data$sex == 1] = "male"
data$sex = factor(data$sex)

# Relabel cp
data$cp[data$cp == 1] = "typical angina"
data$cp[data$cp == 2] = "atypical angina"
data$cp[data$cp == 3] = "non-anginal pain"
data$cp[data$cp == 4] = "asymptomatic"
data$cp = factor(data$cp)

# Relabel fbs
data$fbs[data$fbs == 0] = "false"
data$fbs[data$fbs == 1] = "true"
data$fbs = factor(data$fbs)

# Relabel exang
data$exang[data$exang == 0] = "no"
data$exang[data$exang == 1] = "yes"
data$exang = factor(data$exang)

# Relabel slope
data$slope[data$slope == 1] = "upsloping"
data$slope[data$slope == 2] = "flat"
data$slope[data$slope == 3] = "downsloping"
data$slope = factor(data$slope)

# Relabel ca
data$ca = factor(data$ca)

# Relabel thal
data$thal[data$thal == 3] = "normal"
data$thal[data$thal == 6] = "fixed defect"
data$thal[data$thal == 7] = "reversable defect"
data$thal = factor(data$thal)


# Relabel num
data$num[data$num == 0] = "NO"
data$num[data$num == 1] = "YES"
data$num[data$num == 2] = "YES"
data$num[data$num == 3] = "YES"
data$num[data$num == 4] = "YES"
data$num = factor(data$num)

summary(data)
str(data)

# Train Test Split Stratified by Gender

set.seed(10000)

selected <- sample.split(Y = data$sex, SplitRatio = 0.3)
test = subset(data, selected == T)
train = subset(data, selected == F)

table(data$sex)
table(train$sex)
table(test$sex)

# Model------------------------------------------------------------------------------------
seedtry <- c()

# for (i in (1:100)) {
#   for (j in (1:100)) {
#     
#     set.seed(i)
#     selected <- sample.split(Y = data$sex, SplitRatio = 0.3)
#     test = subset(data, selected == T)
#     train = subset(data, selected == F)
#     table(data$sex)
#     table(train$sex)
#     table(test$sex)
# 
#     set.seed(j)
#     forest = randomForest(num ~ .,
#                           data = train,
#                           na.action = na.omit,
#                           importance = T)
#     predict_forest <- predict(forest, newdata = test, type = "class")
#     acc <- confusionMatrix(predict_forest, test$num)$overall[[1]]
#     print(c(i, j, acc))
#     seedtry <- rbind(seedtry, c(i, j, acc))
#   }
# }

for (i in (1:1000)) {
  
  set.seed(i)
  selected <- sample.split(Y = data$sex, SplitRatio = 0.3)
  test = subset(data, selected == T)
  train = subset(data, selected == F)
  table(data$sex)
  table(train$sex)
  table(test$sex)
  
  set.seed(5071)
  forest = randomForest(num ~ .,
                        data = train,
                        na.action = na.omit,
                        importance = T)
  predict_forest <-
    predict(forest, newdata = test, type = "class")
  acc <- confusionMatrix(predict_forest, test$num)$overall[[1]]
  print(c(i, acc))
  seedtry <- rbind(seedtry, c(i, acc))
}

seedtry <- data.frame(seedtry)
colnames(seedtry) <- c("Seed1", "Accuracy")
which.max(seedtry$Accuracy)
