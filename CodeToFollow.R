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

# This code will automatically download and load the two necessary datasets

Brup_file_url <- "https://www.fjc.gov/sites/default/files/idb/datasets/cpbankdec22.zip"
Brup_local_zip <- "cpbankdec22.zip"
download.file(Brup_file_url, Brup_local_zip)
unzip(Brup_local_zip, exdir = "data_directory")
Brup_data <- read_sas("data_directory/cpbankdec22.sas7bdat")
head(Brup_data)
Census_file_url <- "https://github.com/jacobsteimer/USBankruptcyZIP/raw/main/CensusData.csv"
census_data <- readr::read_csv(Census_file_url)

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
Brup_w_Census <- inner_join(BrupAndThirteenZIP,census_data,by = "GEOID")

# And let's go ahead and turn Bankruptcies and Thirteens to "per thousand residents" statistics
Brup_w_Census <- Brup_w_Census %>% mutate(BrupPerThou = Bankruptcies/Population*1000) %>% mutate(ThirteenPerThou = Thirteens/Population*1000)

# Also, let's divide MedIncome by 10,000. A difference in $10,000 between ZIP codes is a more understandable change than a difference of $1.
Brup_w_Census$MedIncome <- Brup_w_Census$MedIncome/10000 

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
fitlm <- lm(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm)
#What an interesting result! It looks like race, college education and the homeownership rate are the strongest predictors. 
#I would have thought median income would be a better predictor than at least a couple of those.

#Now, let's try it with just BlackPercent, BachPercent and HomeOwnRate
fitlm2 <- lm(BrupPerThou ~ BlackPercent + BachPercent + HomeOwnRate, data = BrupCensusTrain)
tidy(fitlm2)
#The biggest difference is the Bachelor Degree rate appears more weighty in this simplified linear model. That's probably because of its correlation with unemployment.

#Let's get an RMSE for both models
predictions_fitlm <- predict(fitlm, newdata = BrupCensusTest)
rmse_fitlm <- RMSE(predictions_fitlm, BrupCensusTest$BrupPerThou, na.rm = TRUE)
predictions_fitlm2 <- predict(fitlm2, newdata = BrupCensusTest)
rmse_fitlm2 <- RMSE(predictions_fitlm2, BrupCensusTest$BrupPerThou, na.rm = TRUE)
#Both are quite low. The first landed around 1.6. The second around 2.2. So we lose a little when deleting variables. 

#Now, let's try it on the number of Ch 13s
fitlm3 <- lm(ThirteenPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm3)
fitlm4 <- lm(ThirteenPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + BachPercent, data = BrupCensusTrain)
tidy(fitlm4)
# The percentage of Black residents and the homeownership rate remain extremely powerful predictors.
# I used the top four predictors this time instead of the top three in the simplified model. Labor Force Participation Rate becomes much more important, probably because of its correlation with unemployment and median income.
# Let's run the RMSEs.
predictions_fitlm3 <- predict(fitlm3, newdata = BrupCensusTest)
rmse_fitlm3 <- RMSE(predictions_fitlm3, BrupCensusTest$ThirteenPerThou, na.rm = TRUE)
predictions_fitlm4 <- predict(fitlm4, newdata = BrupCensusTest)
rmse_fitlm4 <- RMSE(predictions_fitlm4, BrupCensusTest$ThirteenPerThou, na.rm = TRUE)
#These RMSEs are a little lower but about the same. 

#Now, the percentage of 13s.
fitlm5 <- lm(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain)
tidy(fitlm5)
fitlm6 <- lm(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate, data = BrupCensusTrain)
tidy(fitlm6)
#This is interesting, homeownership rate and the percentage of Black residents both drive up the usage of Chapter 13 (despite those things being negatively correlated with each other)
# Rising incomes drive down the percentage of Chapter 13s, which is not how it's supposed to work
# College education stops being nearly as predictive. 

#Now find RMSEs
predictions_fitlm5 <- predict(fitlm5, newdata = BrupCensusTest)
rmse_fitlm5 <- RMSE(predictions_fitlm5, BrupCensusTest$ThirteenPerc, na.rm = TRUE)
predictions_fitlm6 <- predict(fitlm6, newdata = BrupCensusTest)
rmse_fitlm6 <- RMSE(predictions_fitlm6, BrupCensusTest$ThirteenPerc, na.rm = TRUE)
#WOAH!! WHAT A LOW RMSE FOR BOTH OF THESE. CLOSE TO .24!!!
#Finally, it didn't matter too much to reduce our number of predictors.

#Let's now try some other types of models. Let's start with Random Forest.
ctrl <- trainControl(method = "cv", number = 5)
fitrf <- train(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance_rf <- varImp(fitrf)
print(var_importance_rf)
# This is fascinating. This analysis turns up college education and race as the two most important variables, with median income in third. 
# Also, it sees no importance in homeownership rate, despite our linear model showing it had a large and statistically significant effect.
BrupCensusTestAdjusted <- na.omit(BrupCensusTest)
predictions_fitrf <- predict(fitrf, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitrf <- RMSE(predictions_fitrf, BrupCensusTestAdjusted$BrupPerThou, na.rm = TRUE)
# This RMSE is roughly the same as our lm

#Thirteens ... original attempt was too big to run, so I cut down the control number and used only the four most predictive variables from the lm
ctrl13 <- trainControl(method = "cv", number = 3)
fitrf13 <- train(ThirteenPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl13, na.action = na.omit)
var_importance_rf_13 <- varImp(fitrf13)
print(var_importance_rf_13)
predictions_fitrf13 <- predict(fitrf13, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitrf13 <- RMSE(predictions_fitrf13, BrupCensusTestAdjusted$ThirteenPerThou, na.rm = TRUE)
#Woah! Race lost all of its importance. Labor force participation becomes most important and college education becomes #2. 
# Relatively wild results, based on our linear models. I don't exactly know how to interpret them, other than to take labor force participation more seriously and be a little more skeptical of the race effect.
#Slightly lower RMSE than for the total bankruptcies random forest. However, only a little bit better than the lms for Chapter 13s.

# Now for ThirteenPerc, using all the variables 
fitrf13perc <- train(ThirteenPerc ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "rf", trControl = ctrl13, na.action = na.omit)
var_importance_rf_13perc <- varImp(fitrf13perc)
print(var_importance_rf_13perc)
predictions_fitrf13perc <- predict(fitrf13perc, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitrf13perc <- RMSE(predictions_fitrf13perc, BrupCensusTestAdjusted$ThirteenPerc, na.rm = TRUE)
# Race returns to near the top, with homeownership just ahead of it. 
# Race remains fascinating, as it's been the subject of much discussion. It's also fascinating that random forest analyses sees little importance for race in the number of Chapter 13 filings but great importance for a ZIP's percentage of filings that are Ch. 13.
# Homeownership makes sense, as homeowners are far more likely to file a Chapter 13 than a Chapter 7.
# Also interesting that it relegates Median Income to having no importance.
# This is our lowest RMSE yet, although not that much lower than the linear models for the percentage of chapter 13s. 

#Let's give KNN a shot
fitknn <- train(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = BrupCensusTrain, method = "knn", trControl = ctrl, na.action = na.omit)
predictions_fitknn <- predict(fitknn, newdata = BrupCensusTestAdjusted, na.action = na.omit)
rmse_fitknn <- RMSE(predictions_fitknn, BrupCensusTestAdjusted$BrupPerThou, na.rm = TRUE)
# this is our highest RMSE for total bankruptcies per thousand. Because of this, I feel comfortable ignoring KNN and sticking with our linear and random forest analyses.

#Now that we've run our analysis using only the census data and the BrupZIPcounts, let's try to predict whether a bankruptcy is a Chapter 7 or Chapter 13, using more parts of the Bankruptcy dataset
#First, let's pare it down a bit and filter it to just chapter 7s and chapter 13s
Brup_Pared <- Brup_No_Dup_2022 %>% select(ORGFLCHP, TOTASSTS, REALPROP, PERSPROP, TOTLBLTS, SECURED, UNSECPR, UNSECNPR, TOTDBT, CNTMNTHI)
Brup_Pared <- Brup_Pared %>% filter(ORGFLCHP %in% c(7,13))
#Now, let's make 7 vs. 13 a binary result
Brup_Pared$Binary <- ifelse(Brup_Pared$ORGFLCHP == "13", 1, 0)
#And let's get rid of NAs
Brup_Pared_Adj <- na.omit(Brup_Pared)

#Then, let's subset out a final holdout set, a testing set, a training set, and a small training set
set.seed(1, sample.kind="Rounding")
test_index3 <- createDataPartition(y = Brup_Pared_Adj$ORGFLCHP, times = 1, p = 0.1, list = FALSE)
Brup_Pared_Use <- Brup_Pared_Adj[-test_index3,]
Brup_Pared_Holdout <- Brup_Pared_Adj[test_index3,]

test_index4 <- createDataPartition(y = Brup_Pared_Use$ORGFLCHP, times = 1, p = 0.1, list = FALSE)
BrupParedTrain <- Brup_Pared_Use[-test_index4,]
BrupParedTest <- Brup_Pared_Use[test_index4,]

test_index5 <- createDataPartition(y = Brup_Pared_Use$ORGFLCHP, times = 1, p = 0.1, list = FALSE)
ParedSmallTrain <- BrupParedTrain[test_index5,]
#Let's run an lm on the small version, to help us figure out which variables are important.
fitlm7 <- lm(Binary ~ TOTASSTS + REALPROP + PERSPROP + TOTLBLTS + SECURED + UNSECPR + UNSECNPR + TOTDBT + CNTMNTHI, data = ParedSmallTrain, na.action = na.omit)
tidy(fitlm7)
#REALPROP, CNTMNTHI, and SECURED are by far the most predictive variables

#Let's go back and work the whole dataset
fitlm8 <- lm(Binary ~ REALPROP + SECURED + CNTMNTHI, data = BrupParedTrain, na.action = na.omit)
tidy(fitlm8)
predictions_fitlm8 <- predict(fitlm8, newdata = BrupParedTest)
rmse_fitlm8 <- RMSE(predictions_fitlm8, BrupParedTest$Binary, na.rm = TRUE)
#That rmse is pretty great! About .49. 