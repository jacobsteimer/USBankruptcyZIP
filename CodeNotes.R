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
#Data necessary for educational Bach degree %



