getMyRfProb <- readRDS("getMyRfProb.rds")

newData <-
  data.frame(
    "age" = 37,
    "sex" = factor(1),
    "cp" = factor(3),
    "trestbps" = 130,
    "chol" = 250,
    "fbs" = factor(0),
    "restecg" = factor(0),
    "thalach" = 187,
    "exang" = factor(0),
    "oldpeak" = 3.5,
    "slope" = factor(3),
    "ca" = factor(0),
    "thal" = factor(3),
    "num" = factor(0)
  )

getMyRfProb(newData)