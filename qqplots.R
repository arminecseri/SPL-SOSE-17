"QQNorm of price, log-transformation and another QQNorm of LogPrice"
qqnorm(housing$Price, xlab="Price", col = "blue")
housing$logPrice = log(housing$Price)
qqnorm(housing$logPrice, xlab="Log Price", col = "blue")

'QQNorm of SqFTotFn and its log-transformation logSqft'
qqnorm(housing$SqFtTotFn, xlab="SqFtTotFn", col = "blue") 
housing$logSqft = log(1+housing$SqFtTotFn)
qqnorm(housing$logSqft, xlab="Log SqFt", col = "blue")

'QQNorm of other variables'
qqnorm(housing$Rooms, xlab="Rooms", col = "blue") 
qqnorm(housing$Built, xlab="Built", col = "blue")
qqnorm(housing$Bedrooms, xlab="Bedrooms", col = "blue")
qqnorm(housing$Baths, xlab="Baths", col = "blue")