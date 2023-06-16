# Install all needed packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Load all needed packages
library(tidyverse)
library(caret)
library(haven)
library(ggrepel)
library(broom)
library(readr)
library(lubridate)

# Note: This code assumes the user has downloaded and loaded the "CensusData" file from the GitHub link provided, https://github.com/jacobsteimer/USBankruptcyZIP.git
# This code will automatically download and load the other necessary dataset

Brup_file_url <- "https://www.fjc.gov/sites/default/files/idb/datasets/cpbankdec22.zip"
Brup_local_zip <- "cpbankdec22.zip"
download.file(Brup_file_url, Brup_local_zip)
unzip(Brup_local_zip, exdir = "data_directory")
Brup_data <- read_sas("data_directory/cpbankdec22.sas7bdat")
head(Brup_data)

#Getting started, let's ensure there are no duplicate cases
Brup_No_Dup <- Brup_data %>% distinct(CASEKEY, .keep_all = TRUE)

#This dataset includes all cases that were either filed in 2022 or were still open then. Let's cut it down to just cases filed that year.
Brup_No_Dup_2022 <- Brup_No_Dup %>% filter((year(FILEDATE) == 2022))

#Then, let's clean up the ZIP column
Brup_No_Dup_2022$GEOID <- substr(Brup_No_Dup_2022$D1ZIP, 1, 5)

#Then, let's count up the number of bankruptcies in each ZIP and the number of Chapter 13 Bankruptcies in each ZIP
BrupZIPcounts <- Brup_No_Dup_2022 %>% group_by(GEOID) %>% summarize(Bankruptcies = n())
ThirteenZIPcounts <- Brup_No_Dup_2022 %>% filter(ORGFLCHP == 13) %>% group_by(GEOID) %>% summarize(Thirteens = n())
BrupAndThirteenZIP <- inner_join(BrupZIPcounts,ThirteenZIPcounts, by = "GEOID")

#Let's also calculate the percentage of Bankruptcies that are Chapter 13
BrupAndThirteenZIP <- BrupAndThirteenZIP %>% mutate(ThirteenPerc = (Thirteens/Bankruptcies))

#Now, let's join that with our census data
Brup_w_Census <- inner_join(BrupAndThirteenZIP,CensusData,by = "GEOID")

#Before doing any analysis, let's split it into a holdout set, a testing set and a training set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = Brup_w_Census$Bankruptcies, times = 1, p = 0.1, list = FALSE)
BrupCensusUse <- Brup_w_Census[-test_index,]
BrupCensusHoldout <- Brup_w_Census[test_index,]

test_index2 <- createDataPartition(y = BrupCensusUse$Bankruptcies, times = 1, p = 0.1, list = FALSE)
BrupCensusTrain <- BrupCensusUse[-test_index2,]
BrupCensusTest <- BrupCensusUse[test_index2,]

#To help my laptop run better, I'm going to go ahead and remove "Brup_data" and "Brup_No_Dup"
rm(Brup_data)
rm(Brup_No_Dup)

#Now, let's analyze!
#Let's try an lm with all six variables
fitlm <- lm(Bankruptcies ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm)
#What an interesting result! It looks like race, labor force participate rate and the homeownership rate are the strongest predictors. 
#I would have thought median income would be a better predictor than at least a couple of those.

#Now, let's try it with just BlackPercent, LabForceParticipation and HomeOwnRate
fitlm2 <- lm(Bankruptcies ~ BlackPercent + LabForceParticipation + HomeOwnRate, data = BrupCensusTrain)
tidy(fitlm2)
#The only real difference is that Labor Force Participate rate appears more weighty in this simplified linear model. That's probably because of its correlation with unemployment.

#Let's get an RMSE for both models
predictions_fitlm <- predict(fitlm, newdata = BrupCensusTest)
rmse_fitlm <- RMSE(predictions_fitlm, BrupCensusTest$Bankruptcies, na.rm = TRUE)
predictions_fitlm2 <- predict(fitlm2, newdata = BrupCensusTest)
rmse_fitlm2 <- RMSE(predictions_fitlm2, BrupCensusTest$Bankruptcies, na.rm = TRUE)
#They're roughly the same! Both around 23.1. Later on, we'll see if we can do better. 

#Now, let's try it on the number of Ch 13s
fitlm3 <- lm(Thirteens ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm3)
fitlm4 <- lm(Thirteens ~ BlackPercent + LabForceParticipation + HomeOwnRate, data = BrupCensusTrain)
tidy(fitlm4)
# The percentage of Black residents and the labor force participation rate remain extremely powerful predictors. However, the homeownership rate begins to have a confusing effect (positive in the larger model, negative in the simplified one).
# This makes some sense. Chapter 13s are often used to save homes from foreclosure but is also commonly used by poor renters. 
# Interestingly, median income has a much stronger effect this time around. 
# Let's run the RMSEs.
predictions_fitlm3 <- predict(fitlm3, newdata = BrupCensusTest)
rmse_fitlm3 <- RMSE(predictions_fitlm3, BrupCensusTest$Thirteens, na.rm = TRUE)
predictions_fitlm4 <- predict(fitlm4, newdata = BrupCensusTest)
rmse_fitlm4 <- RMSE(predictions_fitlm4, BrupCensusTest$Thirteens, na.rm = TRUE)
#These RMSEs are really low. That makes sense. I would have thought they would be harder to predict since there are fewer of them.

#Now, the percentage of 13s.
fitlm5 <- lm(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm5)
fitlm6 <- lm(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate, data = BrupCensusTrain)
tidy(fitlm6)
#This is interesting, homeownership rate and the percentage of Black residents both drive up the usage of Chapter 13 (despite those things being negatively correlated with each other)
# Rising incomes drive down the percentage of Chapter 13s, which is not how it's supposed to work
#Now find RMSEs
predictions_fitlm5 <- predict(fitlm5, newdata = BrupCensusTest)
rmse_fitlm5 <- RMSE(predictions_fitlm5, BrupCensusTest$ThirteenPerc, na.rm = TRUE)
predictions_fitlm6 <- predict(fitlm6, newdata = BrupCensusTest)
rmse_fitlm6 <- RMSE(predictions_fitlm6, BrupCensusTest$ThirteenPerc, na.rm = TRUE)
#WOAH!! WHAT A LOW RMSE FOR BOTH OF THESE. .24!!!

#Let's now try some other types of models. Let's start with Random Forest.
ctrl <- trainControl(method = "cv", number = 5)
fitrf <- train(Bankruptcies ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance_rf <- varImp(fitrf)
print(var_importance_rf)
# This makes it extremely clear that the percentage of Black residents is our most important variable, with homeownership after that. 
# Interestingly, this has the labor force participation rate as more similar in importance to unemployment than the two top variables.
# Also, it sees no importance in median income.

#Thirteens and ThirtennPerc
fitrf13 <- train(Thirteens ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance_rf_13 <- varImp(fitrf13)
print(var_importance_rf)
fitrf13perc <- train(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance_rf_13perc <- varImp(fitrf13perc)
print(var_importance_rf)

#Now for 13s
ctrl <- trainControl(method = "cv", number = 5)


BrupCensusTestAdjusted <- na.omit(BrupCensusTest)
predictions_fitrf <- predict(fitrf, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitrf <- RMSE(predictions_fitrf, BrupCensusTestAdjusted$Bankruptcies, na.rm = TRUE)
#This is our lowest RMSE yet! (Roughly 21)

#Let's give KNN a shot
fitknn <- train(Bankruptcies ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "knn", trControl = ctrl, na.action = na.omit)
predictions_fitknn <- predict(fitknn, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitknn <- RMSE(predictions_fitknn, BrupCensusTestAdjusted$Bankruptcies, na.rm = TRUE)
#That's our highest RMSE yet. So, we'll largely ignore KNN.

#(JACOB DO THIS:)Now that we've run our analysis using only the census data and the BrupZIPcounts, let's try to predict whether a bankruptcy is a Chapter 7 or Chapter 13, using more parts of the Bankruptcy dataset