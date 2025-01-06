
# clear workspace
rm( list = ls() )

# load libraries
#library( dplyr )        # used for wrangling the data
#library( tidyr )        # used for wrangling the data
#library( here )         # for referencing the local directory
#library( tidycensus )   # getting data from the census API
#library( zoo )          # to help with interpolation


# set the api key
census_api_key( "8f1ce150e65b8cba01951fbcbbe65ebbb9409638" )

# set the years
years <- c( seq( from = 2016, to = 2025, by = 1 ) )

# drop years that are not available.
years <- years[!years %in% c( 2020, 2024, 2025 )]

# create a list of populations
phoenixPop <- NA

# create a loop to update each year

for( i in seq_along( years ) ){

  azDat <- get_acs(
    geography = "place",
    variables = "B01003_001", # Total population variable
    year = years[i],
    state = "AZ",
    place = "55000", # Phoenix FIPS code
    survey = "acs1" # 1-year ACS estimates
  )

  phoenixPop[i] <- azDat$estimate[ azDat$GEOID == "0455000" ]
     
}

# bind them together
phoenixPop <- data.frame( year = years, pop = phoenixPop )


# add the missing years
# Population data from 2016 to 2022 (NA for missing years)
years <- 2016:2025
population <- c(
  phoenixPop$pop[phoenixPop$year == 2016], 
  phoenixPop$pop[phoenixPop$year == 2017], 
  phoenixPop$pop[phoenixPop$year == 2018], 
  phoenixPop$pop[phoenixPop$year == 2019], 
  NA, 
  phoenixPop$pop[phoenixPop$year == 2021], 
  phoenixPop$pop[phoenixPop$year == 2022], 
  phoenixPop$pop[phoenixPop$year == 2023], 
  NA,
  NA
  )

# Perform linear interpolation to estimate missing values
intPop <- na.approx(population, x = years, rule = 2)
phoenixPop2 <- data.frame( year = years, pop = intPop )


# attach years to the population data
phoenixPopDat <- data.frame(
  year = as.character( years ),
  population = intPop,
  stringsAsFactors = FALSE
)

