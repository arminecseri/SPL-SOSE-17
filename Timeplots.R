######################Time Plots#########################################
#########################################################################


#Plot Price per YearClosed
table2 = aggregate(housing[, ], list(housing$YearClosed), mean)
Plot1=plot(table2$Group.1, table2$Price,las=1, type="o", cex=2, pch=20, main="Average Price per Year",xlab="Year Closed", ylab="Mean Price", col="blue", bg="blue")
grid(nx=NULL, ny=NULL, col="lightgray", lty="solid")
par(new=TRUE)
Plot1=plot(table2$Group.1, table2$Price,las=1, type="o", cex=2, pch=20, main="Average Price per Year",xlab="Year Closed", ylab="Mean Price", col="blue", bg="blue")

#Price per Square Feet
housing$price_per_sqft = housing$Price/housing$SqFtTotFn
summary(housing$price_per_sqft, complete.obs)

'Plot Price per SqFt per YearClosed WITH grid'
table3=aggregate(housing[, 24], list(housing$YearClosed), mean)
Plot2 = plot(table3$Group.1, table3$price_per_sqft,las=1, type="o", cex=3, pch=20, main="Average Price per SqFt per Year",xlab="Year Closed", ylab="Mean Price per SqFt", col="blue", bg="blue")
grid(nx=NULL, ny=NULL, col="lightgray", lty="solid")
par(new=TRUE)
Plot2 = plot(table3$Group.1, table3$price_per_sqft,las=1, type="o", cex=3, pch=20, main="Average Price per SqFt per Year",xlab="Year Closed", ylab="Mean Price per SqFt", col="blue", bg="blue")

#########################################################################
#########################################################################
