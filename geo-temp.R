
# clear workspace
rm( list = ls() )

# load libraries
library( dplyr )    # used for wrangling the data
library( tidyr )    # used for wrangling the data
library( ggplot2 )  # for plotting
library( scales )   # for formatting the text
library( forecast ) # for working with time series data
library( here )     # for referencing the local directory
library( sf )       # spatial objects
library( tigris )   # to get geographic boundaries
library( leaflet )  # for producing the leaflet plot


# define the objects
crimeData       <- readRDS( here( "data/crimeData.rds" ) )
crimesByDay     <- readRDS( here( "data/crimesByDay.rds" ) )
crimesByMonth   <- readRDS( here( "data/crimesByMonth.rds" ) )
crimesByYear    <- readRDS( here( "data/crimesByYear.rds" ) )
crimeRatesMonth <- readRDS( here( "data/crimeRatesMonth.rds" ) )
crimeRatesYear  <- readRDS( here( "data/crimeRatesYear.rds" ) )

crimeRatesMonthType  <- readRDS( here( "data/crimeRatesMonthType.rds" ) )



# set Phoenix boundary ----------------------------------------------------

# Some of the lat/long is outside the boundary of phoenix
# That throws of the plot, so you need to constrain those

# Set tigris options to cache data for faster loading
options( tigris_use_cache = TRUE )

# Get the boundary for the city of Phoenix, Arizona
phx_boundary <- places( state = "AZ", cb = TRUE ) %>%
  filter( NAME == "Phoenix" ) %>%
  st_transform(4326)  # Make sure it’s in the correct CRS (longitude/latitude)

# View the coordinates of the boundary
phx_coords <- st_coordinates( phx_boundary )



# filter cases with phoenix coordinates -----------------------------------

# go through and take those cases that meet the coordinates

df2 <- crimeData %>%
  filter( !is.na( long ) & !is.na( lat ) )

df <- st_as_sf( df2, coords = c("long", "lat"), crs = 4326 )

options(tigris_use_cache = TRUE)

phx_boundary <- places(state = "AZ", cb = TRUE) %>%
  filter(NAME == "Phoenix") %>%
  st_transform(4326)

df$within_phx <- st_within(df, phx_boundary, sparse = FALSE)

df$long <- df2$long
df$lat  <- df2$lat

# Recode lat/lon to NA if the point is outside the Phoenix boundary
df <- df %>%
  mutate(lon = ifelse(within_phx, long, NA),
         lat = ifelse(within_phx, lat, NA))


# Create the leaflet map
leaflet(df) %>%
  addTiles() %>%  # Adds the default OpenStreetMap tiles
  addCircleMarkers(~long, ~lat, popup = ~paste("Latitude:", lat, "<br>Longitude:", long)) 



Stiil here with working through this


