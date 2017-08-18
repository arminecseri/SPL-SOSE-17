install.packages("robustbase")
install.packages("lmtest")
install.packages("corrplot")

data = read.csv(file="/Users/raphaelhanke/desktop/HUBerlin/SPL/Vermont.csv")
housing = Vermont

'Deleting variables'
housing$`Listing Office - Office Name` = NULL
housing$`List Agent - Agent Name` = NULL
housing$Status = NULL
housing$County = NULL
housing$DOM = NULL
housing$`Assessment Amount` = NULL
housing$`Assessment Year` = NULL
housing$`Price - List` = NULL
housing$`Price - Closed` = NULL
housing$`Price - Original` = NULL
housing$PicCount = NULL
housing$`Tax - Gross Amount` = NULL
housing$Surveyed = NULL
housing$`Showing Service`= NULL
housing$Seasonal = NULL
housing$`Short Sale` = NULL
housing$Rented = NULL
housing$`Foreclosed/Bank-Owned/REO`= NULL
housing$Easements = NULL
housing$`Current Use` = NULL
housing$Covenants = NULL
housing$`Basement Access Type` = NULL
housing$`Development / Subdivision` = NULL
housing$`MLS #`= NULL
housing$`Common Land Acres`= NULL

'Rename Variables'
names(housing)[names(housing)=="Water Body Type"] <- "WBT"
names(housing)[names(housing)=="Water Frontage Length"] <- "WFT"
names(housing)[names(housing)=="Date - MLS List"] <- "DateMLS"
names(housing)[names(housing)=="Date - Closed"] <- "DateCL"
names(housing)[names(housing)=="Baths - Total"] <- "Baths"
names(housing)[names(housing)=="Bedrooms - Total"] <- "Bedrooms"
names(housing)[names(housing)=="Year Built"] <- "Built"
names(housing)[names(housing)=="Rooms - Total"] <- "Rooms"
names(housing)[names(housing)=="Garage Capacity"] <- "GarCap"
names(housing)[names(housing)=="Garage Type"] <- "GarType"
names(housing)[names(housing)=="Flood Zone"] <- "Flood"
names(housing)[names(housing)=="Total Stories"] <- "Stories"
names(housing)[names(housing)=="Property Type"] <- "PropType"
names(housing)[names(housing)=="Lot - Acres"] <- "LotAcr"


'order coloumns'
housing = housing[c(1,6,4,9,17,7,8,12,13,21,18,11,14,15,16,19,12,3,2,5,20,10)]

'changing the date'
housing$DateCL=data$Date...Closed

'Data overview'
print(str(housing))

'Delete wrong rows' #Listwise
housing = housing[!(housing$Rooms>30),]
housing = housing[!(housing$SqFtTotFn < 1),]

'Adjustment of Data Types'
housing$Stories = as.numeric(gsub("[+]", "", housing$Stories))
housing$PropType = as.factor(housing$PropType) 
housing$City = as.factor(housing$City) 
housing$State = as.factor(housing$State)
housing$WBT = as.factor(housing$WBT)

'Data overview'
print(str(housing))

'Sorting price'
housing = housing[order(housing$Price, decreasing = TRUE),] 


#########################################################################
#DESCRIPTIVE PART
#########################################################################
##########################complete##########################################

'Summary of number of houses in different states and cities' 
library(MASS) 
summary(housing$State, use = complete.obs)
'Split in the two States and create new files'
print(str(housing$State))
housing$State = as.character(housing$State)
NH = subset(housing, State == "NH")
NH$City = factor(NH$City) #drops unused levels
VT = subset(housing, State == "VT")
VT$City = factor(VT$City)
summary(NH$City)
summary(VT$City)
'Display relative distribution of NH'
NHcity = NH$City     
NHcity.freq = table(NHcity) 
NHcity.relfreq = NHcity.freq / nrow(NH)
old = options(digits=1) 
cbind(NHcity.relfreq) 
'Display relative distribution of VT'
VTcity = VT$City     
VTcity.freq = table(VTcity) 
VTcity.relfreq = VTcity.freq / nrow(VT)
old = options(digits=1) 
cbind(VTcity.relfreq) 
'Plot relative distributions'
#plot(NH$City, legend.text = levels(y),  las=2) aboslut
#plot(VT$City, legend.text = levels(y),  las=2) absolut
par(mar=c(5,6,4,1)+.5)
barplot(prop.table(table(VT$City)), horiz=TRUE, las=1, 
        main = "Relative distribution of Houses in different cities of Vermont", col="blue", bg="blue")
par(mar=c(5,6,4,1)+.5)
barplot(prop.table(table(NH$City)), horiz=TRUE, las=1, 
        main = "Relative distribution of Houses in different cities of New Hampshire", col="blue", bg="blue")

#########################################################################
#########################################################################

'Five number summary of Price'
summary(housing$Price)

'Average Price of States'
mean(NH$Price)
mean(VT$Price)

#Relative distribution in 200000 steps 
u =c(0, 200000, 400000, 600000, 800000, 1000000, +Inf)
round(table(cut(housing$Price,u, labels=c("<200000$","<400000$", "<600000$", "<800000$","<1000000$", ">1000000"))) /length(data$Price), 3)

#Subset with Prices below 1000000$
subgroup_price = subset(housing, housing$Price <1000000)
summary(subgroup_price$Price)

#Graph of cumulative distribution function 
plot.ecdf(housing$Price, xlab = "price", las=1,
          main = "cumulative distribution function of prices", col="red") 
plot.ecdf(subgroup_price$Price, xlab = "price", las=1,
          main = "cumulative distribution function of price < 1.000.000", col="red") 

#Boxplot of price and subgroup_price'
options(scipen=8) 
boxplot(housing$Price, las=1, main = "Boxplot of Price", col = "grey")
boxplot(subgroup_price$Price, las=1, main = "Boxplot of Price < 1000000", col = "grey")


#########################################################################
#########################################################################
########################################################################
#########################################################################
'TIME'

#housing$DateCL=data$Date...Closed #changing date
housing$YearClosed = format(as.Date(housing$DateCL, format="%d/%m/%Y"),"%Y")
print(str(housing$YearClosed))
housing$YearClosed = as.numeric(housing$YearClosed)

summary(housing$YearClosed)

barplot(table(housing$YearClosed), col="blue", las=1, main="Objects listed / Year")

barplot(prop.table(table(housing$YearClosed)), las=1, 
        col="blue", las=1, main="Objects listed / Year")


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

'Five number summaries'
housing1 = housing

'Addtional summaries'
summary(housing$Stories)
summary(housing$GarCap)

'Bedrooms Summary'
housing1 = housing[!(housing$Bedrooms == 0),]
summary(housing1$Bedrooms, use = complete.obs)

'Rooms Summary'
housing1 = housing[!(housing$Baths == 0),]
summary(housing1$Baths, use = complete.obs)

'Rooms Summary'
housing1 = housing[!(housing$Rooms == 0),]
summary(housing1$Rooms, use = complete.obs)

'Area Summary'
housing1 = housing[!(housing$SqFtTotFn == 0),]
summary(housing$SqFtTotFn, use = complete.obs)


##################################################################
##################################################################
#EXPLORATORY
##################################################################


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

"Calculating the years an object exists & qqnorm"
#housing$DateCL=data$Date...Closed #adjust date
housing$YearClosed = format(as.Date(housing$DateCL, format="%d/%m/%Y"),"%Y")
print(str(housing$YearClosed))
housing$YearClosed = as.numeric(housing$YearClosed)

'create variable exists'
housing$Exists = housing$YearClosed-housing$Built
qqnorm(housing$Exists, xlab="Exists", col = "blue")

'create log exists'
housing$logExists = log(1+housing$Exists)
qqnorm(housing$logExists, xlab="Log Exists", col = "blue")


##########################################################
##########################################################
##########################################################


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


##########################################################
##########################################################
##########################################################

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




