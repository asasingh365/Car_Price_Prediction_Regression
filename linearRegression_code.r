library(tidyr)
library(MASS)
library(car)


# Linear Regression Assignment
# Objective is to model the price of cars with the available independent variables.
# 

# Will be performing below 3 steps before starting with modelling :

#  1.) Data Sourcing
#  2.) Data cleaning
#  3.) Derived Metrics

#  -> 1. DATA SOURCING

# First we set working directory
# Imported the csv file into the data frame 'geely_auto' 

geely_auto <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

# Checked the structure of the data frame

str(geely_auto)


# -> DATA CLEANING

# Went through the Data cleaning Data Cleaning_checklist to ensure data is ready for analysis
# Checklist consists of 

# 1. Fix rows and columns    

# a. Incorrect rows   
# There were no headers or footer rows
                   
# b. Summary rows
# There were no rows that indicated summarised values for example Total or Average

# c. Extra rows
# There were no extra rows like blank rows, column numbers etc

# d. Missing Column Names
# All the 6 columns have appropriate names

# e. Inconsistent column names
# All the 6 columns have appropriate names

# f. Unnecessary columns
# Found one column car_ID to be un-necessary here as there won't be impact of this column anywhere so removing this column
geely_auto <- geely_auto[,-1]

# g. Columns containing Multiple data values
# No such columns

# h. No Unique Identifier
# No action needs to be performed like combining of columns etc as columns are sufficiently present

# i. Misaligned columns
# No misalignment of columns found


# As per given information, splitted the column CarName into 2 columns 'car_company' and 'car_model'

geely_auto <-  separate(geely_auto,CarName,into = c("car_company","car_name"),sep = " ")

# Since car_name needs not to be used in modelling, so removed this column

geely_auto <- geely_auto[,-3]

# Checked for duplicate values in the column car_brand
unique(geely_auto$car_company)

# Could see lot of duplicates , checked for lower case and spelling mistakes
# Cleaned this column thoroughly

geely_auto$car_company <- tolower(geely_auto$car_company)
geely_auto[which(geely_auto$car_company == "maxda"),"car_company"] <- "mazda"
geely_auto[which(geely_auto$car_company == "porcshce"),"car_company"] <- "porsche"
geely_auto[which(geely_auto$car_company == "toyouta"),"car_company"] <- "toyota"
geely_auto[which(geely_auto$car_company == "vw"),"car_company"] <- "volkswagen"
geely_auto[which(geely_auto$car_company == "vokswagen"),"car_company"] <- "volkswagen"

# Now focussing on other columns

# Checked for NA values
# None found
sum(is.na(geely_auto))

# Now, creating dummy variables for categorical variables

geely_auto$fueltype <- factor(geely_auto$fueltype)
levels(geely_auto$fueltype) <- c(0,1)
geely_auto$fueltype <- as.numeric(levels(geely_auto$fueltype))[geely_auto$fueltype]

geely_auto$aspiration <- factor(geely_auto$aspiration)
levels(geely_auto$aspiration) <- c(0,1)
geely_auto$aspiration <- as.numeric(levels(geely_auto$aspiration))[geely_auto$aspiration]

geely_auto$doornumber <- factor(geely_auto$doornumber)
levels(geely_auto$doornumber) <- c(4,2)
geely_auto$doornumber <- as.numeric(levels(geely_auto$doornumber))[geely_auto$doornumber]

levels(factor(geely_auto$carbody))
dummy_1 <- data.frame(model.matrix(~carbody,geely_auto))
dummy_1 <- dummy_1[,-1]
geely_auto <- cbind(geely_auto[,-6],dummy_1)

dummy_2 <- data.frame(model.matrix(~drivewheel,geely_auto))
dummy_2 <- dummy_2[,-1]
geely_auto <- cbind(geely_auto[,-6],dummy_2)

geely_auto$enginelocation <- factor(geely_auto$enginelocation)
levels(geely_auto$enginelocation) <- c(0,1)
geely_auto$enginelocation <- as.numeric(levels(geely_auto$enginelocation))[geely_auto$enginelocation]

dummy_3 <- data.frame(model.matrix(~enginetype,geely_auto))
dummy_3 <- dummy_3[,-1]
geely_auto <- cbind(geely_auto[,-12],dummy_3)

geely_auto$cylindernumber <- factor(geely_auto$cylindernumber)
dummy_4 <- data.frame(model.matrix(~cylindernumber,geely_auto))
dummy_4 <- dummy_4[,-1]
geely_auto <- cbind(geely_auto[,-12],dummy_4)

geely_auto$fuelsystem <- factor(geely_auto$fuelsystem)
dummy_5 <- data.frame(model.matrix(~fuelsystem,geely_auto))
dummy_5 <- dummy_5[,-1]
geely_auto <- cbind(geely_auto[,-13],dummy_5)

geely_auto$car_company <- factor(geely_auto$car_company)
dummy_6 <- data.frame(model.matrix(~car_company,geely_auto))
dummy_6 <- dummy_6[,-1]
geely_auto <- cbind(geely_auto[,-2],dummy_6)

## Completed with the creation of the dummy variables

## Now started with outliers removal of numeric variables

quantile(geely_auto$wheelbase,seq(0,1,.01))

quantile(geely_auto$carlength,seq(0,1,.01))

quantile(geely_auto$carwidth,seq(0,1,.01))

quantile(geely_auto$carheight,seq(0,1,.01))

quantile(geely_auto$curbweight,seq(0,1,.01))
geely_auto$curbweight[which(geely_auto$curbweight < 1819.72)] <- 1819.72
geely_auto$curbweight[which(geely_auto$curbweight > 3376.080)] <- 3376.080

quantile(geely_auto$enginesize,seq(0,1,.01))
geely_auto$enginesize[which(geely_auto$enginesize > 209)] <- 209

quantile(geely_auto$boreratio,seq(0,1,.01))
geely_auto$boreratio[which(geely_auto$boreratio < 2.9100)] <- 2.9100
geely_auto$boreratio[which(geely_auto$boreratio > 3.8000)] <- 3.8000

quantile(geely_auto$stroke,seq(0,1,.01))
geely_auto$stroke[which(geely_auto$stroke < 2.6400)] <- 2.6400
geely_auto$stroke[which(geely_auto$stroke > 3.9000)] <- 3.9000

quantile(geely_auto$compressionratio,seq(0,1,.01))
geely_auto$compressionratio[which(geely_auto$compressionratio > 10.9400)] <- 10.9400

quantile(geely_auto$horsepower,seq(0,1,.01))
geely_auto$horsepower[which(geely_auto$horsepower > 207)] <- 207

quantile(geely_auto$peakrpm,seq(0,1,.01))

quantile(geely_auto$citympg,seq(0,1,.01))
geely_auto$citympg[which(geely_auto$citympg > 38.00)] <- 38.00

quantile(geely_auto$highwaympg,seq(0,1,.01))
geely_auto$highwaympg[which(geely_auto$highwaympg > 49.88)] <- 49.88

str(geely_auto)


# Data modelling

# Created training and test dataset from current dataset.
# Took 70% of rows for training and remaining for test data set

set.seed(100)
train_indices <- sample(1:nrow(geely_auto),0.7*nrow(geely_auto))
train.geely_auto <- geely_auto[train_indices,]
test.geely_auto <- geely_auto[-train_indices,]

model_1 <- lm(price~.,train.geely_auto)
summary(model_1)
stepAIC(model_1,direction = "both")

model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + carbodywagon + 
                  drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + car_companyaudi + 
                  car_companybmw + car_companybuick + car_companydodge + car_companyhonda + 
                  car_companyisuzu + car_companyjaguar + car_companymazda + 
                  car_companymercury + car_companymitsubishi + car_companynissan + 
                  car_companyplymouth + car_companyrenault + car_companysaab + 
                  car_companytoyota + car_companyvolkswagen + car_companyvolvo, 
              data = train.geely_auto)
			  
			  

summary(model_2)
vif(model_2)

# model_3 -- removed carcompany_buick

model_3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + carbodywagon + 
                  drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + car_companyaudi + 
                  car_companybmw + car_companydodge + car_companyhonda + 
                  car_companyisuzu + car_companyjaguar + car_companymazda + 
                  car_companymercury + car_companymitsubishi + car_companynissan + 
                  car_companyplymouth + car_companyrenault + car_companysaab + 
                  car_companytoyota + car_companyvolkswagen + car_companyvolvo, 
              data = train.geely_auto)			  

summary(model_3)			  
vif(model_3)

cor(train.geely_auto$curbweight,train.geely_auto$enginesize)
cor(train.geely_auto$carwidth,train.geely_auto$curbweight)
cor(train.geely_auto$enginesize,train.geely_auto$carwidth)

			 
model_4 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                  carwidth + enginesize + stroke + peakrpm + carbodywagon + 
                  drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + car_companyaudi + 
                  car_companybmw + car_companydodge + car_companyhonda + 
                  car_companyisuzu + car_companyjaguar + car_companymazda + 
                  car_companymercury + car_companymitsubishi + car_companynissan + 
                  car_companyplymouth + car_companyrenault + car_companysaab + 
                  car_companytoyota + car_companyvolkswagen + car_companyvolvo, 
              data = train.geely_auto)


# model_4 --  removed curbweight because of correlation with enginesize

summary(model_4)
vif(model_4)

model_5 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                  carwidth + enginesize + stroke + peakrpm + carbodywagon + 
                  enginetypedohcv + enginetypel + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + car_companyaudi + 
                  car_companybmw + car_companydodge + car_companyhonda + 
                  car_companyisuzu + car_companyjaguar + car_companymazda + 
                  car_companymercury + car_companymitsubishi + car_companynissan + 
                  car_companyplymouth + car_companyrenault + car_companysaab + 
                  car_companytoyota + car_companyvolkswagen + car_companyvolvo, 
              data = train.geely_auto)
			  
			  

# model_5  -- removed drivewheelrwd

summary(model_5)
vif(model_5)

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke + peakrpm + carbodywagon + 
                  enginetypedohcv + enginetypel + enginetypeohcf + 
                  enginetyperotor + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + car_companyaudi + 
                  car_companybmw + car_companydodge + car_companyhonda + 
                  car_companyisuzu + car_companyjaguar + car_companymazda + 
                  car_companymercury + car_companymitsubishi + car_companynissan + 
                  car_companyplymouth + car_companyrenault + car_companysaab + 
                  car_companytoyota + car_companyvolkswagen + car_companyvolvo, 
              data = train.geely_auto)



summary(model_6)
vif(model_6)

model_7 <- lm(formula = price ~ aspiration + enginelocation +
carwidth + enginesize + peakrpm + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + cylindernumberthree + car_companyaudi +
car_companybmw + car_companydodge + car_companyhonda +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companytoyota + car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)

summary(model_7)
vif(model_7)


model_8 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + peakrpm + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + cylindernumberthree + car_companyaudi +
car_companybmw + car_companydodge + car_companyhonda +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companytoyota + car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)

summary(model_8)

vif(model_8)
cor(train.geely_auto$car_companytoyota,train.geely_auto$cylindernumbersix)
cor(train.geely_auto$enginetypeohcf,train.geely_auto$cylindernumbersix)
cor(train.geely_auto$enginetypeohcf,train.geely_auto$cylindernumberfour)
cor(train.geely_auto$enginetypeohcf,train.geely_auto$car_companytoyota)
cor(train.geely_auto$car_companydodge,train.geely_auto$car_companytoyota)
cor(train.geely_auto$car_companymazda,train.geely_auto$car_companytoyota)
cor(train.geely_auto$car_companysaav,train.geely_auto$car_companytoyota)
cor(train.geely_auto$car_companysaab,train.geely_auto$car_companytoyota)

model_9 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + peakrpm + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + cylindernumberthree + car_companyaudi +
car_companybmw + car_companydodge + car_companyhonda +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)

summary(model_9)
vif(model_9)

model_10 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + cylindernumberthree + car_companyaudi +
car_companybmw + car_companydodge + car_companyhonda +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)

summary(model_10)
vif(model_10)



model_11 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + car_companyaudi +
car_companybmw + car_companydodge + car_companyhonda +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)


summary(model_11)
vif(model_11)

model_12 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + car_companyaudi +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen + car_companyvolvo,
data = train.geely_auto)

summary(model_12)
vif(model_12)

model_13 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix + car_companyaudi +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_13)

vif(model_13)

model_14 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi + car_companynissan +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_14)
vif(model_14)

model_15 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi  +
car_companyplymouth + car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_15)
vif(model_15)

model_16 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  + carbodywagon +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_16)
vif(model_16)

model_17 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw + car_companydodge  +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_17)
vif(model_17)


model_18 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymercury + car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_18)
vif(model_18)

model_19 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  +
enginetypedohcv + enginetypel + enginetypeohcf +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_19)
vif(model_19)

model_20 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  +
enginetypedohcv + enginetypel +
enginetyperotor + cylindernumberfive + cylindernumberfour +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_20)
vif(model_20)


cor(train.geely_auto$cylindernumberfour,train.geely_auto$cylindernumbersix)

model_21 <- lm(formula = price ~ aspiration + enginelocation +
carwidth  +
enginetypedohcv + enginetypel +
enginetyperotor + cylindernumberfive +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_21)
vif(model_21)

model_22 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypedohcv + enginetypel +
enginetyperotor + cylindernumberfive +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_22)
vif(model_22)

model_23 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
enginetyperotor + cylindernumberfive +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_23)
vif(model_23)

model_24 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
enginetyperotor  +
cylindernumbersix  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_24)
vif(model_24)

model_25 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
enginetyperotor  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault + car_companysaab +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_25)
vif(model_25)

model_26 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
enginetyperotor  +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault  +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_26)
vif(model_26)

model_27 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyisuzu + car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault  +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_27)
vif(model_27)

model_28 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyjaguar + car_companymazda +
car_companymitsubishi  +
car_companyrenault  +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_28)
vif(model_28)


model_29 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyjaguar + car_companymazda +
car_companyrenault  +
car_companyvolkswagen,
data = train.geely_auto)

summary(model_29)

model_30 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyjaguar + car_companymazda +
car_companyrenault
,
data = train.geely_auto)

summary(model_30)

model_31 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyjaguar  +
car_companyrenault
,
data = train.geely_auto)

summary(model_31)


model_32 <- lm(formula = price ~ enginelocation +
carwidth  +
enginetypel +
car_companybmw   +
car_companyjaguar
,
data = train.geely_auto)

summary(model_32)

model_33 <- lm(formula = price ~ enginelocation +
carwidth  +
car_companybmw   +
car_companyjaguar
,
data = train.geely_auto)
summary(model_33)



# 1 Multiple R-squared:  0.977,	Adjusted R-squared:  0.9629 
# 2 Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9677 
# 3 Multiple R-squared:  0.9752,	Adjusted R-squared:  0.9675
# 4 Multiple R-squared:  0.9738,	Adjusted R-squared:  0.9659 
# 5 Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9655  
# 6 Multiple R-squared:  0.971,	Adjusted R-squared:  0.963 
# 7 Multiple R-squared:  0.9705,	Adjusted R-squared:  0.9626
# 8 Multiple R-squared:  0.9663,	Adjusted R-squared:  0.9577 
# 9 Multiple R-squared:  0.9548,	Adjusted R-squared:  0.9437 
# 10 Multiple R-squared:  0.9543,	Adjusted R-squared:  0.9436
# 11 Multiple R-squared:  0.9465,	Adjusted R-squared:  0.9345
# 12 Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9349 
# 13 Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9355
# 14 Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9351 
# 15 Multiple R-squared:  0.9454,	Adjusted R-squared:  0.9353 
# 16 Multiple R-squared:  0.9444,	Adjusted R-squared:  0.9353 
# 17 Multiple R-squared:  0.9444,	Adjusted R-squared:  0.9353
# 18 Multiple R-squared:  0.9438,	Adjusted R-squared:  0.9351
# 19 Multiple R-squared:  0.9433,	Adjusted R-squared:  0.9351
# 20 Multiple R-squared:  0.9421,	Adjusted R-squared:  0.9342
# 21 Multiple R-squared:  0.8655,	Adjusted R-squared:  0.8485 
# 22 Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8486 
# 23 Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8498 
# 24 Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8509 
# 25 Multiple R-squared:  0.8644,	Adjusted R-squared:  0.8518 
# 26 Multiple R-squared:  0.8637,	Adjusted R-squared:  0.8522 
# 27 Multiple R-squared:  0.8619,	Adjusted R-squared:  0.8514
# 28 Multiple R-squared:  0.8589,	Adjusted R-squared:  0.8493 
# 29 Multiple R-squared:  0.8547,	Adjusted R-squared:  0.8461 
# 30 Multiple R-squared:  0.8503,	Adjusted R-squared:  0.8426
# 31 Multiple R-squared:  0.8452,	Adjusted R-squared:  0.8383 
# 32 Multiple R-squared:  0.8403,	Adjusted R-squared:  0.8344 
# 33 Multiple R-squared:  0.835,	Adjusted R-squared:  0.8302 

# Final : Multiple R-squared:  0.835,	Adjusted R-squared:  0.8302 

# Now will use  test data set to predict or check our linear model

which(colnames(test.geely_auto) == "price")
Predict_1 <- predict(model_33,test.geely_auto[,-19])

test.geely_auto$test_price <- Predict_1
rsquared <- cor(test.geely_auto$test_price,test.geely_auto$price)^2

# Rsquared is coming out to be 0.7785859


# Final model equation :   -172954.8 + (24703.3)(enginelocation) + 2812(carwidth) + 9990(car_companybmw) + (11140.1)(car_companyjaguar)

# So, the final model contains below predictor variables to predict the price of the car :

# 1. enginelocation        # Location of the engine
# 2. carwidth              # Width of the car
# 3. car_companybmw        # Brand has a significant role in predicting price of a car
# 4. car_companyjaguar     # Brand has a significant role in predicting price of a car



	