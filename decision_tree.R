library(C50)
data(churn)
treeModel <- C5.0(x = churnTrain[,-20], y = churnTrain$churn)
treeModel
summary(treeModel)
ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)
ruleModel
summary(ruleModel)
plot(treeModel)

