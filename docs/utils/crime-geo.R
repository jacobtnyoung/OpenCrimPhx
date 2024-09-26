
# clear workspace
rm( list = ls() )

# load libraries
library( dplyr )        # used for wrangling the data
library( tidyr )        # used for wrangling the data
library( here )         # for referencing the local directory
library( tidygeocoder ) # help with geocoding
library( tidycensus )   # getting data from the census API


# get the data
url <- "https://www.phoenixopendata.com/dataset/cc08aace-9ca9-467f-b6c1-f0879ab1a358/resource/0ce3411a-2fc6-4302-a33f-167f68608a20/download/crime-data_crime-data_crimestat.csv"
crimeData <- read.csv( url, as.is = TRUE, header = TRUE )
crimeData <- na.omit( crimeData )

# remove duplicate ids
duplicateIds <- crimeData$INC.NUMBER[ duplicated( crimeData$INC.NUMBER )]
crimeData <- crimeData[ !crimeData$INC.NUMBER %in% duplicateIds, ]
rm( duplicateIds )

# clean up the dates
date.vec <- strptime( crimeData$OCCURRED.ON, format="%m/%d/%Y %H:%M" )
crimeData$year   <- format( date.vec, format="%Y" )

# set the api key
census_api_key( "8f1ce150e65b8cba01951fbcbbe65ebbb9409638" )


# write the function to get the geocoded data

crimeGeo <- function( crimeDat, filterYear ){
  
  # select by year
  crimeDat <- 
    crimeDat %>% 
    filter( year == filterYear )
  
    # fill these two digits with 50 and then append the zip code
  # we use 50 since that is midway between the street segment
  crimeDat$Address.adj <- gsub( "XX", "50", crimeDat$X100.BLOCK.ADDR )
  
  # append the zip code
  crimeDat$Address.zip <- paste( crimeDat$Address.adj, crimeDat$ZIP )

    
  # ----
  # the api only takes 10k cases at a time, so we have to do it in batches
  
  # number of cases
  n <- dim( crimeDat )[1]
  
  # number of batches
  batches <- n / 10000
  
  # round it and add 1 because it rounds down
  batches <- round( batches ) + 1
  
  # create the group ids
  groups <- gl( 
    n = batches, 
    k = ceiling( length( crimeDat$Address.zip ) / batches ), 
    length = length( crimeDat$Address.zip ) )
  
  # Split the character vector into separate groups
  grouped_char_vectors <- split( crimeDat$Address.zip, groups )
  
  # Create a loop to dynamically assign each group to a separate object
  for ( i in seq_along( grouped_char_vectors ) ) {
    assign( paste0( "group", i ), grouped_char_vectors[[i]] )
  }
  
  # Initialize an empty list to store the group objects
  group_list <- list()
  
  # Loop to dynamically add each group to the list
  for ( i in 1:batches ) {
    group_list[[i]] <- get( paste0("group", i ) )
  }
  
  # Initialize a list to store the results
  spatial_results <- list()
  
  # Loop over the groups and apply the geo() function
  for ( i in seq_along( group_list ) ) {
    spatial_results[[i]] <- geo( 
      group_list[[i]],               # use the current group of addresses
      full_results = TRUE,           # request all data from the geocoding service
      method = "census",             # specify the census method
      api_options = list( census_return_type = "geographies" )  # set API options
    )
  }
  
  # take the results and create a dataframe
  results <- data.frame( 
    Address.zip = character(), 
    Latitude = numeric(),
    Longitude = numeric(),
    stringsAsFactors = FALSE 
  )
  
  # Loop through the list and add rows to the data frame
  for ( i in seq_along( spatial_results ) ) {
    # Extract the address and lat elements
    temp_df <- data.frame(
      Address.zip = spatial_results[[i]]$address,
      lat = spatial_results[[i]]$lat,
      long = spatial_results[[i]]$long
    )
    
    # Add the rows to the existing data frame
    results <- rbind( results , temp_df )
  }
  
  # pull the id for joining the data
  results$INC.NUMBER <- crimeDat$INC.NUMBER
  
  return( results )
}


# run the function over the years
# this is a bit ugly but the loop I came up with
# created a list and I would have to unlist it
# so i just went C&P here
crimeDat2016 <- crimeGeo( crimeData, 2016 )
crimeDat2017 <- crimeGeo( crimeData, 2017 )
crimeDat2018 <- crimeGeo( crimeData, 2018 )
crimeDat2019 <- crimeGeo( crimeData, 2019 )
crimeDat2020 <- crimeGeo( crimeData, 2020 )
crimeDat2021 <- crimeGeo( crimeData, 2021 )
crimeDat2022 <- crimeGeo( crimeData, 2022 )
crimeDat2023 <- crimeGeo( crimeData, 2023 )
crimeDat2024 <- crimeGeo( crimeData, 2024 )

# bind them all together
crimeDatGeo <- bind_rows(
  crimeDat2016, crimeDat2017, crimeDat2018, crimeDat2019,
  crimeDat2020, crimeDat2021, crimeDat2022, crimeDat2023,
  crimeDat2024
)

# save the geo data
saveRDS( crimeDatGeo , file = here( "data/data-geo/crimeDatGeo2016-2024.rds" ) )
