library(caTools)
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

df <-
  read.table(
    "/Users/jiangyifan/Desktop/AN6003/Team Project/processed.cleveland.data",
    sep = ",",
    col.names = cols,
    na.strings = c("?")
  )

df$ca[which(is.na(df$ca))] <- 2
df$thal[which(is.na(df$thal))] <- 3
df$num[df$num > 1] <- 1

for (i in c(2, 3, 6, 7, 9, 11, 12, 13, 14)) {
  df[, i] <- factor(df[, i])
}

set.seed(2022)
split <- sample.split(df$num)
trainSet <- subset(df, split == TRUE)
testSet <- subset(df, split == FALSE)

# trainSet[, c(1, 4, 5, 8, 10)] <-
#   scale(trainSet[, c(1, 4, 5, 8, 10)])
# testSet[, c(1, 4, 5, 8, 10)] <- scale(testSet[, c(1, 4, 5, 8, 10)])

m.logistic <- glm(num ~ ., family = binomial, data = trainSet)

H <-
  data.frame(
    "age" = 30,
    "sex" = factor(1),
    "cp" = factor(1),
    "trestbps" = 131,
    "chol" = 241,
    "fbs" = factor(1),
    "restecg" = factor(1),
    "thalach" = 150,
    "exang" = factor(0),
    "oldpeak" = 1,
    "slope" = factor(1),
    "ca" = factor(0),
    "thal" = factor(3)
  )

round(predict(m.logistic, H, type = "response"))