boston <- read.csv("boston.csv")
latlonlm = lm(MEDV ~ LAT + LON, data=boston)

library(rpart)
library(rpart.plot)

latlontree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)

fittedvalues = predict(latlontree)
latlontree <- rpart(MEDV~LAT + LON, data = boston, minbucket = 50)

plot(latlontree)
text(latlontree)

library(caTools)
set.seed(123)
split=sample.split(boston$MEDV, SplitRatio=.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

#Basic linear regression model
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)

linreg.pred <- predict(linreg, newdata=test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2)

#Regression Tree model

tree <-  rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
tree.pred <- predict(tree, newdata=test)
tree.sse <- sum((tree.pred-test$MEDV)^2)

#Cross Validation

library(caret)
library(e1071)

tr.control = trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp=(0:10)*0.0001)

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

best.tree <- tr$finalModel
prp(best.tree)


best.tree.pred <- predict(best.tree, newdata=test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)