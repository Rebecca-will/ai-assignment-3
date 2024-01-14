library(stats) # preinstalled with R generally
library(FactoMineR)
library(factoextra)
library(cluster)
library(mclust)
library(mlbench)
library(randomForest)
library(rpart)
library(entropy)
library(pROC)
library(ggplot2)
library(cluster)
library(caret)


attach(Assignment_section_3_data_set)
data <- Assignment_section_3_data_set[-1]
#optional
library(randomForestSRC) 

## hopefully this too
library(entropy)
library(rpart.plot)

train <- subset(data, set == 'train')
test <- subset(data, set == 'test')

#building the tree
model = rpart(as.factor(diagnosis) ~ ., data = (train[-1]), method = 'class')
par(xpd = T)
plot(model)
text(model, use.n = TRUE)
summary(model)

test$diagnosis <- factor(test$diagnosis, levels = levels(train$diagnosis))

table(as.factor(test[-1]$diagnosis), predict(object = model, newdata = test[-1], type = 'class'))



levels(as.factor(train$diagnosis))
levels(as.factor(test$diagnosis))
sum(is.na(test))

table(train$diagnosis)
table(test$diagnosis)

mc = Mclust(data)
dr = MclustDR(object = mc, lambda = 1, normalized = T)
plot(dr, main = "MclustDR Plot for Data", legend = TRUE) # check options 1, 5 for sure


rf = randomForest(as.factor(diagnosis) ~ ., data = (train), ntree = 10)
table(as.factor(test$diagnosis), predict(object = rf, newdata = test, type = 'class'))
summary(rf)

#Understanding the importance of the model. 

#entropy
repEn <- function(k) {
  t1 = sum(table(data$diagnosis[data$concave.points_se > k]))
  t2 = sum(table(data$diagnosis[data$concave.points_se <= k]))
  en1 = entropy(table(data$diagnosis[data$concave.points_se > k]), unit = 'log2')
  en2 = entropy(table(data$diagnosis[data$concave.points_se <= k]), unit = 'log2')
  return((en1*t1/(t1+t2)) + (en2*t2/(t1+t2)))
}

repEn(0.01013)


imp_rf <- importance(rf)
imp_rf 

imp_mod <- varImp(model)
imp_mod

mc1 = Mclust(test[])
dr1 = MclustDR(object = mc1, lambda = 1, normalized = T)
plot(dr1) # check options 1, 5 for sure




