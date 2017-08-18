###CORRELATION MATRIX ###


sub_num = housing[sapply(housing,is.numeric)]

sub_num$Built = NULL
sub_num$Price = NULL
sub_num$SqFtTotFn = NULL
sub_num$YearCL = NULL
sub_num$Exists = NULL
sub_num$YearClosed = NULL
sub_num$price_per_sqft = NULL

sub_num = sub_num[c(8,10,9,2,3,5,7,1,4)]

H = cor(sub_num, use = "complete.obs", method = "pearson")
print(H) # CORRELATION VALUES #
library("corrplot")
corrplot(H, method="circle") # CORRELATION PLOT #