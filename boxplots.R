options(scipen=8) 
boxplot(housing$Price, las=1, main = "Boxplot of Price", col = "grey")
boxplot(subgroup_price$Price, las=1, main = "Boxplot of Price < 1000000", col = "grey")