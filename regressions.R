#REGRESSIONS#

'Simple linear Regression logprice ~ logsqft'
Model1=lm(housing$logPrice ~ housing$logSqft)
plot(housing$logPrice, housing$logSqft, main="Model1", xlab="Log Price", ylab ="Log SqFt", las =1 ,col="blue")

summary(Model1)

library(lmtest)
bptest(Model1)
plot(Model1)

'Split dataset in test und trainset '
smp_size <- floor(0.75*nrow(housing))
set.seed(123)
train_ind <- sample(seq_len(nrow(housing)), size = smp_size)
train <- housing[train_ind, ]
test <- housing[-train_ind, ]

'Multivariant Regressionmodel 2'
Model2=lm(logPrice ~ logSqft + Baths +Bedrooms, data=train)
summary(Model2)
bptest(Model2)


library(robustbase)
Model3=lmrob(logPrice ~ logSqft + Baths + Bedrooms + logExists + Garage + GarCap, data=train)
summary(Model3)
bptest(Model3)

library(robustbase)
Model3T=lmrob(logPrice ~ logSqft + Baths + Bedrooms + logExists + Garage + GarCap, data=test)
summary(Model3T)

'Testing the data - predict'
Y_predict <- predict(Model3, newdate=test)
head(Y_predict)
head(test$logPrice)