library(caret)
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
data$num[data$num > 1] <- 1

# Factor  ----------
for (i in c(2, 3, 6, 7, 9, 11, 12, 13, 14)) {
  data[, i] <- factor(data[, i])
}
# Train Test Split Stratified by Gender------

set.seed(139)

selected <- sample.split(Y = data$sex, SplitRatio = 0.3)
test = subset(data, selected == T)
train = subset(data, selected == F)

# Model------------------------------------------------------------------------------------
set.seed(5071)

m.forest = randomForest(num ~ .,
                        data = train,
                        na.action = na.omit,
                        importance = T)

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
# Accuracy: 0.9325843

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
#
# predict(m.forest, newdata = newData, type = "prob")[2]
# predict(m.logistic, newdata = newData, type = "response")[[1]]

getMyRfProb <- function(nd) {
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
  for (i in c(2, 3, 6, 7, 9, 11, 12, 13, 14)) {
    data[, i] <- factor(data[, i])
  }
  nd <- rbind(data[1,], nd)
  nd <- nd[-1,]
  m.forest = randomForest(num ~ .,
                          data = data)
  predict(m.forest, newdata = nd, type = "prob")[2]
}

saveRDS(getMyRfProb, "getMyRfProb.rds")
#
# getProb <- readRDS("getMyRfProb.rds")

# getProb(newData)
# predict(m.forest, newdata = test[1,], type = "prob")

# getMyRfProb(test[1,])
