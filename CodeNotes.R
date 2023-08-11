#5-year estimates for total population, Median income, and number of Black Residents
MedianIncome <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B19013_001"
)
TotalPopulation <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B01003_001" 
) 
BlackPopulation <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B02001_003" 
)

#Combine and add BlackPercent
IncomeAndRace <- left_join(MedianIncome, TotalPopulation, by = "GEOID","NAME")
IncomeAndRace <- left_join(IncomeAndRace, BlackPopulation, by = "GEOID","NAME")
IncomeAndRace <- IncomeAndRace %>% rename(MedIncome = estimate.x) %>% rename(Population = estimate.y) %>% rename(BlackPop = estimate)
IncomeAndRace <- IncomeAndRace %>% mutate(BlackPercent = BlackPop/Population)

#Data necessary for educational Bach degree Rate
BachDegreeOrHigher <-  get_acs(
    geography = "zcta", year = 2021,
    variables = "B16010_041E" 
  )
Population25Higher <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B15003_001" 
)

#Combine and calculate BachPercent
BachDegreeData <- left_join(BachDegreeOrHigher,Population25Higher, by = "GEOID","NAME")
View(BachDegreeData)
BachDegreeData <- BachDegreeData %>% rename(BachDegrees = estimate.x) %>% rename(Pop25Higher = estimate.y)
BachDegreeData <- BachDegreeData %>% mutate(BachPercent = BachDegrees/Pop25Higher)

#Data necessary for Home Ownership Rate
HomesOwned <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B25003_002" 
)
TotalHomes <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B25003_001" 
)

#Combine and calculate HomeOwnRate
Homeownership <- left_join(HomesOwned,TotalHomes,by = "GEOID","NAME")
Homeownership <- Homeownership %>% rename(OwnedHomes = estimate.x) %>% rename(AllHomes = estimate.y)
Homeownership <- Homeownership %>% mutate(HomeOwnRate = OwnedHomes/AllHomes)

#Employment Data
Population16Higher <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B23025_001" 
)
InCivilianLaborForce <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B23025_003" 
)
Unemployed <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B23025_005" 
)

#Combine and calculate LabForceParticipation and UnRate
Employment <- left_join(Population16Higher,InCivilianLaborForce,by = "GEOID","NAME")
Employment <- left_join(Employment,Unemployed,by = "GEOID","NAME")
Employment <- Employment %>% rename(Pop16Higher = estimate.x) %>% rename(LaborForce = estimate.y) %>% rename(Unemploy = estimate)
Employment <- Employment %>% mutate(LabForceParticipation = LaborForce/Pop16Higher) %>% mutate(UnRate = Unemploy/LaborForce)

#Combine all relevant data
CensusData <- bind_cols(select(Employment, LabForceParticipation, UnRate, GEOID), select(Homeownership,HomeOwnRate), select(BachDegreeData,BachPercent), select(IncomeAndRace,MedIncome,BlackPercent,Population))

#Save as CSV
current_dir <- getwd()
file_path <- file.path(current_dir, "CensusData.csv")
write_csv(CensusData, file_path)