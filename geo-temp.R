
# clear workspace
rm( list = ls() )


# load libraries
library( dplyr )        # used for wrangling the data
library( tidyr )        # used for wrangling the data
library( openxlsx )     # for opening an excel file
library( here )         # for referencing the local directory
library( tidygeocoder ) # help with geocoding
library( tidycensus )   # getting data from the census API


# get the data
url <- "https://www.phoenixopendata.com/dataset/cc08aace-9ca9-467f-b6c1-f0879ab1a358/resource/0ce3411a-2fc6-4302-a33f-167f68608a20/download/crime-data_crime-data_crimestat.csv"
crimeData <- read.csv( url, as.is = TRUE, header = TRUE )
crimeData <- na.omit( crimeData )

# remove duplicate ids
duplicate_ids <- crimeData$$INC.NUMBER[duplicated(crimeData$$INC.NUMBER)]

# remove all instances of duplicates
crimeData <- crimeData[!crimeData$INC.NUMBER %in% duplicate_ids, ]

# drop the object with the duplicated ids
rm( duplicated_ids )

# clean up the dates
date.vec <- strptime( crimeData$OCCURRED.ON, format="%m/%d/%Y %H:%M" )
crimeData$year   <- format( date.vec, format="%Y" )
crimeData$month  <- format( date.vec, format="%B" )
crimeData$day365 <- format( date.vec, format="%j" )
crimeData$week   <- format( date.vec, format="%V" )

# clean up the variable classifying the cases
crimeData <- 
  crimeData %>% 
  mutate( crime.type = case_when( 
    UCR.CRIME.CATEGORY == "AGGRAVATED ASSAULT" ~ "Assault",
    UCR.CRIME.CATEGORY == "ARSON" ~ "Arson",
    UCR.CRIME.CATEGORY == "BURGLARY" ~ "Burglary",
    UCR.CRIME.CATEGORY == "DRUG OFFENSE" ~ "Drugs",
    UCR.CRIME.CATEGORY == "LARCENY-THEFT" ~ "Theft",
    UCR.CRIME.CATEGORY == "MURDER AND NON-NEGLIGENT MANSLAUGHTER" ~ "Homicide",
    UCR.CRIME.CATEGORY == "MOTOR VEHICLE THEFT" ~ "MV Theft",
    UCR.CRIME.CATEGORY == "RAPE" ~ "Rape",
    UCR.CRIME.CATEGORY == "ROBBERY" ~ "Robbery" ) )



# drop cases from 2015
# these are dropped because the 2015 cases begin in December
crimeData <- 
  crimeData %>% 
  filter( year != 2015 )


# get 2016
crimeData2016 <- 
  crimeData %>% 
  filter( year == 2016 )






# set the api key
census_api_key( "8f1ce150e65b8cba01951fbcbbe65ebbb9409638" )

# fill these two digits with 50 and then append the zip code
# we use 50 since that is midway between the street segment
crimeData2016$Address.adj <- gsub( "XX", "50", crimeData2016$X100.BLOCK.ADDR )

# append the zip code
crimeData2016$Address.zip <- paste( crimeData2016$Address.adj, crimeData2016$ZIP )


# the api only takes 10k cases at a time, so we have to do it in batches

# number of cases
n <- dim( crimeData2016 )[1]

# number of batches
batches <- n / 10000

# round it and add 1 because it rounds down
batches <- round( batches ) + 1

# create the group ids
groups <- gl( n = batches, k = ceiling(length(crimeData2016$Address.zip)/batches), length = length(crimeData2016$Address.zip))

# Split the character vector into separate groups
grouped_char_vectors <- split( crimeData2016$Address.zip, groups )

# Create a loop to dynamically assign each group to a separate object
for (i in seq_along(grouped_char_vectors)) {
  assign(paste0("group", i), grouped_char_vectors[[i]])
}


# 
# group_list <- list(group1, group2, group3, group4, group5,group6,group7)

# Initialize an empty list to store the group objects
group_list <- list()

# Loop to dynamically add each group to the list
for (i in 1:batches) {
  group_list[[i]] <- get( paste0("group", i ) )
}


# Initialize a list to store the results
spatial_results <- list()

# Loop over the groups and apply the geo() function
for (i in seq_along(group_list)) {
  spatial_results[[i]] <- geo( 
    group_list[[i]],               # use the current group of addresses
    full_results = TRUE,           # request all data from the geocoding service
    method = "census",             # specify the census method
    api_options = list(census_return_type = "geographies")  # set API options
  )
}


# take the results and create a dataframe
results <- data.frame( 
  Address.zip = character(), 
  lat = numeric(),
  long = numeric(),
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


# added there here because you are trying to work with the object
# and it keeps getting written over
results2 <- results

# pull the id for joining the data
results$INC.NUMBER <- crimeData2016$INC.NUMBER


! the problem you run into here is duplicate ids
Need to go to the top and make sure you are dropping the duplicate
ids before you run this


crimeData2016 <- left_join( crimeData2016, results, by = "INC.NUMBER" )






Need to think about how you want this structured,
so you pull the years, code them, and then create a data object?
  Do you then call that to tie into the file in the other pre-processing?




# pull the spatial geographic data
spatial <- geo( 
  crimeData2016$Address.zip,  # the addresses with zip codes
  full_results = TRUE,         # we want all of the data from the geocoding service
  method = "census",           # we want the information from the census
  api_options = list( census_return_type = "geographies" )
)

