install.packages("XLConnect") #installs package to read Excel spreadsheets
install.packages("tidyverse") #installs very powerful visualization package

library(readxl) 
library(tidyverse)
library(plyr)
library(car)
library(dplyr)

BigMartData <- read.csv("Big Mart Sale.csv")  #puts data in Global Environment#
View(BigMartData) #to view the data
dim(BigMartData) #to get the shape of dataframe
names(BigMartData)# to get the column names in the dataset
str(BigMartData)# to get the structure of the dataset

################# summary of all variables #################
#Summary of continuous variables before data cleaning
summary(BigMartData[, c('Item_Weight', 'Item_Visibility', 'Outlet_Establishment_Year', 'Item_MRP', 'Item_Outlet_Sales')])

#Summary of categorical variables before data cleaning
table(BigMartData$Item_Fat_Content, useNA = "always")
table(BigMartData$Item_Type, useNA = "always")
table(BigMartData$Outlet_Identifier, useNA = "always")
table(BigMartData$Outlet_Size, useNA = "always")
table(BigMartData$Outlet_Location_Type, useNA = "always")
table(BigMartData$Outlet_Type, useNA = "always")

################# Data exploration for continuous variables #################

################# Item_Outlet_Sales #################
# Histogram of target variable
ggplot(BigMartData) + 
  geom_histogram(aes(BigMartData$Item_Outlet_Sales), binwidth = 50, fill = "blue") +  xlab("Item_Outlet_Sales")

################# Item_Weight #################
# missing value treatment
item_weight_mean_df = aggregate(Item_Weight~Item_Identifier, data = BigMartData, FUN = "mean")
names(item_weight_mean_df)[2] = "item_weight_mean"
BigMartData<-merge(x=BigMartData,y=item_weight_mean_df,by="Item_Identifier",all.x=TRUE)
BigMartData$Item_Weight_trf <- ifelse(is.na(BigMartData$Item_Weight), BigMartData$item_weight_mean, BigMartData$Item_Weight)
BigMartData$Item_Weight_trf <- ifelse(is.na(BigMartData$Item_Weight_trf), mean(BigMartData$Item_Weight_trf, na.rm = TRUE), BigMartData$Item_Weight_trf)

# histogram of Item weight after treatment
ggplot(BigMartData) + geom_histogram(aes(Item_Weight_trf), binwidth = 0.4, fill = "orange")

# bivariate plot after treatment
ggplot(BigMartData) + geom_point(aes(Item_Weight_trf, Item_Outlet_Sales), colour = "magenta")

################# Item_Visibility #################
# no feature transformation

# histogram of Item_Visibility
ggplot(BigMartData) + geom_histogram(aes(Item_Visibility), binwidth = 0.01, fill = "orange")

# bivariate plot
ggplot(BigMartData) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "magenta")

################# Item_MRP #################
# no feature transformation

# histogram of Item MRP
ggplot(BigMartData) + geom_histogram(aes(Item_MRP), binwidth = 10, fill = "orange")

# bivariate plot
ggplot(BigMartData) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "magenta")

################# Data exploration for Categorical variables #################

################# Item_Fat_content #################
# Collapsing Fat content into two categories
BigMartData$Item_Fat_Content_trf<-factor(BigMartData$Item_Fat_Content)
BigMartData$Item_Fat_Content_trf <- car::recode(BigMartData$Item_Fat_Content_trf, 'c("low fat", "LF","Low Fat") = "Low Fat"; c("Regular","reg") = "Regular"')

# Barplot of Item_Fat_content after collapsing
IFC_DF_cat <- as.data.frame(table(BigMartData$Item_Fat_Content_trf))
barplot(IFC_DF_cat$Freq, names.arg=IFC_DF_cat$Var1, xlab="Item_Fat_Content",col='orange')

# bivariate plot
boxplot(Item_Outlet_Sales~Item_Fat_Content_trf, data = BigMartData, col = 'magenta', las = 2)

################# Item_Type #################
# Collapse Item type into categories#
BigMartData$Item_Type_trf<-factor(BigMartData$Item_Type)
BigMartData$Item_Type_trf<- car::recode(BigMartData$Item_Type_trf, 'c("Breakfast", "Others","Seafood") = "Others"; c("Dairy") = "Dairy";
                                      c("Soft Drinks") = "Soft Drinks"; c("Meat") = "Meat"; c("Fruits and Vegetables") = "Fruits and Vegetables";
                                      c("Snack Foods") = "Snack Foods";c("Frozen Foods") = "Frozen Foods";
                                      c("Household") = "Household";c("Health and Hygiene") = "Health and Hygiene";c("Hard Drinks") = "Hard Drinks";
                                      c("Canned") = "Canned";c("Starchy Foods") = "Starchy Foods";
                                       c("Baking Goods","Breads") = "Baking Goods"')

# Barplot of Item_Type after collapsing
IT_DF_cat <- as.data.frame(table(BigMartData$Item_Type_trf))
barplot(IT_DF_cat$Freq, names.arg=IT_DF_cat$Var1, xlab="Item_Type",col='orange',las=2)

# bivariate plot
boxplot(Item_Outlet_Sales~Item_Type_trf, data = BigMartData, col = 'magenta', las = 2)

################# Outlet_Identifier #################
# no feature transformation

# Barplot of Outlet_Identifier
OI_DF <- as.data.frame(table(BigMartData$Outlet_Identifier))
barplot(OI_DF$Freq, names.arg=OI_DF$Var1, xlab="Outlet_Identifier",col='orange',las=2)

# bivariate plot
boxplot(Item_Outlet_Sales~Outlet_Identifier, data = BigMartData, col = 'magenta', las = 2)

################# Outlet_size #################
# missing value treatment
BigMartData$Outlet_Size_trf <- ifelse(as.character(BigMartData$Outlet_Size) == "", names(which.max(table(BigMartData$Outlet_Size))), as.character(BigMartData$Outlet_Size))

# Barplot of Outlet_size
OS_DF <- as.data.frame(table(BigMartData$Outlet_Size_trf))
barplot(OS_DF$Freq, names.arg=OS_DF$Var1, xlab="Outlet_Size_trf",col='orange',las=2)

# bivariate plot
boxplot(Item_Outlet_Sales~Outlet_Size_trf, data = BigMartData, col = 'magenta', las = 2)

################# Outlet_Establishment_year #################
# no feature transformation

# Barplot of Outlet_Establishment_year
OEY_DF <- as.data.frame(table(BigMartData$Outlet_Establishment_Year))
barplot(OEY_DF$Freq, names.arg=OEY_DF$Var1, xlab="Outlet_Establishment_Year",col='orange',las=2)

# bivariate plot
boxplot(Item_Outlet_Sales~Outlet_Establishment_Year, data = BigMartData, col = 'magenta', las = 2)

################# Outlet_Type #################
# no feature transformation

#Barplot of Outlet_Type
OT_DF <- as.data.frame(table(BigMartData$Outlet_Type))
barplot(OT_DF$Freq, names.arg=OT_DF$Var1, xlab="Outlet_Type",col='orange')

# bivariate plot
boxplot(Item_Outlet_Sales~Outlet_Type, data = BigMartData, col = 'magenta')

################# Outlet_Location_Type #################
# no feature transformation

#Barplot of Outlet_Location_Type
OLT_DF <- as.data.frame(table(BigMartData$Outlet_Location_Type))
barplot(OLT_DF$Freq, names.arg=OLT_DF$Var1, xlab="Outlet_Location_Type",col='orange',las=2)

# bivariate plot
boxplot(Item_Outlet_Sales~Outlet_Location_Type, data = BigMartData, col = 'magenta', las = 2)

################# write cleaned csv #################
#to write the cleaned dataset to csv
write.csv(BigMartData,"BigMartData_cleaned.csv",row.names = FALSE)

################# Linear regression model #################
SalesModel=lm(Item_Outlet_Sales~Item_Weight_trf+Item_Visibility+Item_MRP+Item_Fat_Content_trf+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year+Outlet_Type+Outlet_Location_Type, data=BigMartData)
summary(SalesModel)
vif(SalesModel)
SalesModel1=lm(Item_Outlet_Sales~Item_Weight_trf+Item_Visibility+Item_MRP+Item_Fat_Content_trf+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel1)
vif(SalesModel1)

#######Partial F-Tests#######
SalesModel2=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Item_Fat_Content_trf+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel2)
anova(SalesModel1, SalesModel2) 
#p>0.05, hence Item_Weight has no significance in the model #removing it from model
SalesModel3=lm(Item_Outlet_Sales~Item_MRP+Item_Fat_Content_trf+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel3)
anova(SalesModel1, SalesModel3)
#p<0.05, hence Item_Visibility has significance in the model #keeping it in model
SalesModel4=lm(Item_Outlet_Sales~Item_Visibility+Item_Fat_Content_trf+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel4)
anova(SalesModel1, SalesModel4)
#p<0.05, hence Item_MRP has significance in the model #keeping it in model
SalesModel5=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Item_Type_trf+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel5)
anova(SalesModel1, SalesModel5)
#p>0.05, hence Item_Fat_content_trf has no significance in the model #removing it
SalesModel6=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel6)
anova(SalesModel1, SalesModel6)
#p>0.05, hence Item_Type_trf has no significance in the model #removing it
SalesModel7=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel7)
anova(SalesModel1, SalesModel7)
#p<0.05, hence Outlet_Size_trf has significance in the model #keeping it in model
SalesModel8=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Outlet_Size_trf, data=BigMartData)
summary(SalesModel8)
anova(SalesModel1, SalesModel8)
#p<0.05, hence Outlet_Establishment_Year has significance in the model #keeping it in model
##below is the parsimonious model##
SalesModel9=lm(Item_Outlet_Sales~Item_Visibility+Item_MRP+Outlet_Size_trf+Outlet_Establishment_Year, data=BigMartData)
summary(SalesModel9)
##################
##Residual plots##
##################
#Residual plots of untransformed variables#
Item_Outlet_Sales.stdres <-residuals(SalesModel9)
ggplot(data=BigMartData, aes(x=Item_Visibility, y=Item_Outlet_Sales.stdres))+geom_point()
ggplot(data=BigMartData, aes(x=Item_MRP, y=Item_Outlet_Sales.stdres))+geom_point()
ggplot(data=BigMartData, aes(x=Outlet_Size_trf, y=Item_Outlet_Sales.stdres))+geom_point()
ggplot(data=BigMartData, aes(x=Outlet_Establishment_Year, y=Item_Outlet_Sales.stdres))+geom_point()

#Log transformation of variables#
BigMartData$LOGItem_Outlet_Sales <- log(BigMartData$Item_Outlet_Sales)
BigMartData$LOGItem_Visibility <- log(BigMartData$Item_Visibility)
BigMartData$LOGItem_MRP <- log(BigMartData$Item_MRP)
BigMartData$LOGOutlet_Establishment_Year <- log(BigMartData$Outlet_Establishment_Year)

#Bivariate plot of transformed variables#
ggplot(data=BigMartData, aes(x=LOGItem_Visibility, y=Item_Outlet_Sales)) + geom_point(size=2)
ggplot(data=BigMartData, aes(x=LOGItem_MRP, y=Item_Outlet_Sales)) + geom_point(size=2)
ggplot(data=BigMartData, aes(x=LOGOutlet_Establishment_Year, y=Item_Outlet_Sales))

#removing nulls#
BigMartData[BigMartData==0] <- NA
BigMartData <-  BigMartData[complete.cases(BigMartData), ]

#parsimonious model#
SalesModel10<-lm(Item_Outlet_Sales~LOGItem_Visibility+LOGItem_MRP+Outlet_Size_trf+LOGOutlet_Establishment_Year,na.action="na.omit", data=BigMartData)
summary(SalesModel10) 
Item_Outlet_SalesLOG.stdres <-residuals(SalesModel10)

#residual plots with transformed variables
ggplot(data=BigMartData, aes(x=LOGItem_Visibility, y=Item_Outlet_SalesLOG.stdres))+geom_point()
ggplot(data=BigMartData, aes(x=LOGItem_MRP, y=Item_Outlet_SalesLOG.stdres))+geom_point()
ggplot(data=BigMartData, aes(x=LOGOutlet_Establishment_Year, y=Item_Outlet_SalesLOG.stdres))+geom_point()
