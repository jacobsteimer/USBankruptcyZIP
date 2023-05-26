MedianIncome <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B19013_001")
)
TotalPopulation <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B01003_001" 
) 
BlackPopulation <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B02001_003" 
)
#5-year estimates for total population, Median income, and number of Black Residents

BachDegreeOrHigher <-  get_acs(
    geography = "zcta", year = 2021,
    variables = "B16010_041E" 
  )
Population25Higher <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B15003_001" 
)
#Data necessary for educational Bach degree Rate

HomesOwned <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B25003_002" 
)
TotalHomes <- get_acs(
  geography = "zcta", year = 2021,
  variables = "B25003_001" 
)
#Data necessary for Home Ownership Rate

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
#Employment Data

