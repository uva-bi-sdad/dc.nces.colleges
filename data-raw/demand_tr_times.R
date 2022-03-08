# packages
library(RPostgreSQL)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)

library(tidygeocoder)
library(osrm)

################# HEALTH DISTRICTS
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")
# read in health districts
health_district <- dbGetQuery(con, "SELECT * FROM dc_common.va_hdct_sdad_2021_health_district_counties")

#health_district <- fread("/project/biocomplexity/sdad/projects_data/vdh/va_county_to_hd.csv")
health_district$county_id <- as.character(health_district$geoid_county)

############ TRACT AND COUNTY DEMAND
# census geographies
# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# get
reg.tracts <- get_acs(geography = "tract",
                      year = 2019,
                      variables = c(tpop = "B01003_001"), # tot population
                      state = "VA",
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE)

# tract centroids
tr_centroid <- reg.tracts %>%
  st_centroid()

# get county population and shapes
reg.counties <- get_acs(geography = "county",
                        year = 2019,
                        variables = c(tpop = "B01003_001"), # tot population
                        state = "VA",
                        survey = "acs5",
                        output = "wide",
                        geometry = TRUE)

ct_centroid <- reg.counties %>%
  st_centroid()

# Reproject
reg.ct.utm <- st_transform(reg.counties, crs = "+proj=longlat +datum=WGS84")

# add health districts column to geometries
#demand <- left_join(reg.ct.utm, health_district[, c("geoid", "geoid_county", "region_name")], by = c("GEOID" = "geoid_county"))

#demand <- demand  %>%
#  group_by(geoid) %>%
#  summarise(geometry = sf::st_union(geometry),
#            tpop = sum(tpopE)) %>%
#  ungroup()

# convert it make to sf (I don't think this is actually necessary)
#demand <- st_sf(geoid = demand$geoid,
#                #health_district = demand$region_name,
#                tpop = demand$tpop, geometry = demand$geometry) %>%
#  mutate(centroid = st_centroid(geometry))

# get longitude and latitude
#demand$longitude = st_coordinates(demand$centroid)[,1]
#demand$latitude = st_coordinates(demand$centroid)[,2]

#demand <- tr_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE)
#demand <- as.data.frame(demand)
demand <- ct_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE)
demand <- as.data.frame(demand)

####################################### SUPPLY (ONE BY ONE) ########################################
# read in supply file
#supply <- read_csv("college_data/two_year_tr_supply.csv")
#supply <- read_csv("college_data/two_year_ct_supply.csv")
supply <- read_csv("college_data/four_year_ct_supply.csv")
#supply <- read_csv("college_data/four_year_tr_supply.csv")
#supply <- read_csv("college_data/two_year_tr_supply.csv")

supply$GEOID <- as.character(supply$GEOID)
# add health districts column to geometries
#new_supply <- left_join(supply, health_district[, c("geoid_county", "geoid")],
#                        by = c("GEOID" = "geoid_county"))

#new_supply <- new_supply  %>%
#  group_by(geoid) %>%
#  summarise(tot_cap = sum(capacity)) %>%
#  ungroup()

#save supply for the health disctrict
#write_csv(new_supply, '/college_data/less_two_hd_newsupply.csv')

####################################### DRIVING TIME ############################################
# options for OSRM
options(osrm.server = Sys.getenv("OSRM_SERVER"), osrm.profile = "car")

start.time <- Sys.time() # using this to see run-time
all_data <- matrix(, nrow = 0, ncol = nrow(supply))

# maximum number of requests that OSRM can handle at a time -
# I don't know if there is still a limit on this, but I still use 1 million as the upper bound
max.size <- 100

# based on the max.size, I calculate the maximum number of block groups we can create time matrix for at a time
n <- floor(max.size / nrow(supply))
chunks <- ceiling((nrow(demand)) / n)

for (i in 1 : chunks)
{
  # if not at the final chunk
  if (i != chunks)
  {
    matrix <- osrmTable(src = demand[(1 + n * (i - 1)):(n * i), c("GEOID", "lon", "lat")],
                        dst = supply[, c("GEOID", "lon", "lat")])$durations
  }
  # if at final chunk, only go until final row
  else
  {
    #matrix <- osrmTable(src = demand[(1 + n * (i - 1)):nrow(demand), c("geoid_bg", "closest_property_lon", "closest_property_lat")],
    #                    dst = new_supply[, c("GEOID", "lon", "lat")])$durations

    matrix <- osrmTable(src = demand[(1 + n * (i - 1)):nrow(demand), c("GEOID", "lon", "lat")],
                        dst = supply[, c("GEOID", "lon", "lat")])$durations
  }
  # show percentage completion
  if (i == ceiling(chunks / 4)) {print( "25%" )}
  if (i == ceiling(chunks / 2)) {print( "50%" )}
  if (i == ceiling(3 * chunks / 4)) {print( "75%" )}
  all_data <- rbind(all_data, matrix)
}

# convert data to times dataframe with origin, dest, and cost columns (needed for floating catchment areas)
#colnames(all_data) <- new_supply$GEOID
colnames(all_data) <- supply$GEOID
times <- as.data.frame(as.table(all_data))
colnames(times) <- c("origin", "dest", "cost")
write_csv(times, '/college_data/four_year_ct_drive_times.csv')

# merge block group centroid with geom
#demand <- merge(reg.bg.utm, demand, by.x = "GEOID", by.y = "geoid_bg")
write_csv(demand, '/college_data/four_year_ct_demand.csv')

################# GO TO PYTHON NOTEBOOK TO CALCULATE THE ACCESS SCORES
