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

#Then, let's count up the number of bankruptcies in each ZIP 
BrupZIPcounts <- Brup_No_Dup_2022 %>% group_by(GEOID) %>% summarize(Bankruptcies = n())

#Now, let's join that with our census data
Brup_w_Census <- inner_join(BrupZIPcounts,CensusData,by = "GEOID")

#Before doing any analysis, let's split it into testing and training sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = Brup_w_Census$Bankruptcies, times = 1, p = 0.1, list = FALSE)
BrupCensusTrain <- Brup_w_Census[-test_index,]
BrupCensusTest <- Brup_w_Census[test_index,]

#Now, let's analyze!
#Starting with a linear model


#(JACOB DO THIS:)Now that we've run our analysis using only the census data and the BrupZIPcounts, let's try to predict whether a bankruptcy is a Chapter 7 or Chapter 13, using more parts of the Bankruptcy dataset