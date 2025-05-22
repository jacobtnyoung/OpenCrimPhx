# ------------------------------------------- #
# ?


# ----
# Setup

# clear the workspace
rm( list=ls() )


# set the libraries to use
library( network ) # for working with the network object
library( here )    # for accessing local directory
library( dplyr )   # for working with the data
library( network ) # for building the networks
library( sna )     # for working with the network data


# ----
# Download the traffic data from the portal

url <- paste(
  "https://www.phoenixopendata.com/dataset/1e0ea85f-3aed-4f1f-93e8-ada3c15db86c/",
  "resource/7725bbf3-7829-4f57-8cc2-0faac51b90de/download/",
  "citations_traffic-citations-details_citationdetail.csv",
  sep = ""
)

dat <- read.csv(
  url,
  as.is = TRUE,
  header = TRUE
)


# ----

dat_race <- dat  |>
  group_by( UNIQUE_NAME_ID )  |>  # group by unique id
  filter( n() > 1 ) |>  # drop cases that have only 1 incident
  filter( UNIQUE_NAME_ID != "MNI-" ) |> # drop cases that have an incorrect id
  arrange( UNIQUE_NAME_ID ) # arrange by unique id


race_gt1_incident <- dat_race |>
  summarize( n_race_codes = n_distinct( SIMPLE_SUBJ_RE_GRP ), .groups = "drop" )

race_variation <- dat_race |>
  summarize( n_race_codes = n_distinct( SIMPLE_SUBJ_RE_GRP ), .groups = "drop" ) |>
  filter( n_race_codes > 1 )

df_inconsistent <- dat_race %>%
  filter( UNIQUE_NAME_ID %in% race_variation$UNIQUE_NAME_ID ) 

100 * ( dim( race_variation )[1] / dim( race_gt1_incident )[1] )






race_switchers <- dat %>%
  group_by( UNIQUE_NAME_ID ) %>%
  filter( UNIQUE_NAME_ID != "MNI-" ) |> # drop cases that have an incorrect id
  filter( n_distinct( SUBJ_RACE ) > 1 ) %>%
  ungroup()


race_switchers %>%
  count( UNIQUE_NAME_ID ) %>%
  summarise( mean_n = mean( n ), max_n = max( n ) )

race_switchers %>%
  count( UNIQUE_NAME_ID, SUBJ_RACE ) %>%
  group_by( UNIQUE_NAME_ID ) %>%
  summarise( races = toString( unique( SUBJ_RACE ) ) ) %>%
  head()




# IV approach

ambiguous_individuals <- dat %>%
  group_by( UNIQUE_NAME_ID ) %>%
  filter( n_distinct( SIMPLE_SUBJ_RE_GRP ) > 1 ) %>%
  ungroup()

dat_iv <- ambiguous_individuals %>%
  left_join( officer_race_tendency, by = "ISSUING_OFFICER" ) %>%
  mutate( race_is_black = as.integer( SIMPLE_SUBJ_RE_GRP == "Black" ) )

first_stage <- feols(
  race_is_black ~ pct_black + HOUR_OF_DAY,
  data = dat_iv
)

summary( feols(
  CRIMINAL_TRAFFIC_VIOLATIONS ~ 1 | race_is_black ~ pct_black + HOUR_OF_DAY,
  data = dat_iv
) )



Think about including controls here
Work through the logic, does this make sense?
  Think about what you are trying to get at





# ----
# Download the arrest data from the portal

url <- paste(
  "https://www.phoenixopendata.com/dataset/6f58a024-6fc2-4405-9306-15f2021c3c06/resource/",
  "1eaee7f1-ccd0-4057-af55-e5749a934258/download/arrests_adult-arrests-details_arrestdetail.csv",
  sep = ""
)

dat <- read.csv(
  url,
  as.is = TRUE,
  header = TRUE
)


# ----

dat_race <- dat  |>
  select( SIMPLE_SUBJ_RE_GRP, UNIQUE_NAME_ID, ARST_OFFICER )  |>  # take the variables you need
  group_by( UNIQUE_NAME_ID )  |>  # group by unique id
  filter( n() > 1 ) |>  # drop cases that have only 1 incident
  filter( UNIQUE_NAME_ID != "MNI-" ) |> # drop cases that have an incorrect id
  arrange( UNIQUE_NAME_ID ) # arrange by unique id


race_gt1_incident <- dat_race |>
  summarize( n_race_codes = n_distinct( SIMPLE_SUBJ_RE_GRP ), .groups = "drop" )

race_variation <- dat_race |>
  summarize(n_race_codes = n_distinct( SIMPLE_SUBJ_RE_GRP ), .groups = "drop") |>
  filter( n_race_codes > 1 )

df_inconsistent <- dat_race %>%
  filter(UNIQUE_NAME_ID %in% race_variation$UNIQUE_NAME_ID)

100 * ( dim( race_variation )[1] / dim( race_gt1_incident )[1] )





