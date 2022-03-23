# 4-year, 2-year Colleges and Trade Schools Access Scores in VA at the census tract level
# packages
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(tidycensus)
library(osrm)
library(catchment)
library(RPostgreSQL)
library(reshape2)

###################
# VA COLLEGEs
###################

# load college data
four_year_lonlat <- read_csv("college_data/four_year_lonlat.csv")
two_year_lonlat <- read_csv("college_data/two_year_lonlat.csv")
less_two_lonlat <- read_csv("college_data/less_two_lonlat.csv")

# VA counties geoids
tracts <- st_as_sf(tracts(state="VA"))

# drop where coordinates are missing
four_year_lonlat <- four_year_lonlat[is.na(four_year_lonlat$latitude) == F,]
four_year_lonlat <- four_year_lonlat[is.na(four_year_lonlat$longitude) == F,]

two_year_lonlat <- two_year_lonlat[is.na(two_year_lonlat$latitude) == F,]
two_year_lonlat <- two_year_lonlat[is.na(two_year_lonlat$longitude) == F,]

less_two_lonlat <- less_two_lonlat[is.na(less_two_lonlat$latitude) == F,]
less_two_lonlat <- less_two_lonlat[is.na(less_two_lonlat$longitude) == F,]

# lon and lat to geo-points
geopts_4year <- four_year_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_2year <- two_year_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

geopts_less2 <- less_two_lonlat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)

# indeces of bgs which contain a geopoint
inds_4year <- st_within(geopts_4year$geometry, tracts$geometry, sparse=T)
inds_2year <- st_within(geopts_2year$geometry, tracts$geometry, sparse=T)
inds_less2 <- st_within(geopts_less2$geometry, tracts$geometry, sparse=T)

ct_list_4year <- c()
ct_list_2year <- c()
ct_list_less2 <- c()

for (i in inds_4year){
  if (identical(tracts$NAME[i],character(0))){
    ct_list_4year<- append(ct_list_4year, NA)}
  else{
    ct_list_4year <- append(ct_list_4year, tracts$GEOID[i])}
}
for (i in inds_2year){
  if (identical(tracts$NAME[i],character(0))){
    ct_list_2year<- append(ct_list_2year, NA)}
  else{
    ct_list_2year <- append(ct_list_2year, tracts$GEOID[i])}
}
for (i in inds_less2){
  if (identical(tracts$NAME[i],character(0))){
    ct_list_less2<- append(ct_list_less2, NA)}
  else{
    ct_list_less2 <- append(ct_list_less2, tracts$GEOID[i])}
}

four_year_lonlat['geoid'] <- ct_list_4year
two_year_lonlat['geoid'] <- ct_list_2year
less_two_lonlat['geoid'] <- ct_list_less2

# drop if capacity is missing
four_year_lonlat <- four_year_lonlat[is.na(four_year_lonlat$tot_enrol) == F,]
two_year_lonlat <- two_year_lonlat[is.na(two_year_lonlat$tot_enrol) == F,]
less_two_lonlat <- less_two_lonlat[is.na(less_two_lonlat$tot_enrol) == F,]

#######################
# SUPPLY
#######################
# create new supply
supply_4year <- data.frame(four_year_lonlat$longitude, four_year_lonlat$latitude,
                           four_year_lonlat$geoid, four_year_lonlat$tot_enrol,
                           four_year_lonlat$...1)

supply_2year <- data.frame(two_year_lonlat$longitude, two_year_lonlat$latitude,
                           two_year_lonlat$geoid, two_year_lonlat$tot_enrol,
                           two_year_lonlat$...1)

supply_less2 <- data.frame(less_two_lonlat$longitude, less_two_lonlat$latitude,
                           less_two_lonlat$geoid, less_two_lonlat$tot_enrol,
                           less_two_lonlat$...1)

colnames(supply_4year) <- c("lon", "lat", "GEOID", "capacity", "id")
colnames(supply_2year) <- c("lon", "lat", "GEOID", "capacity", "id")
colnames(supply_less2) <- c("lon", "lat", "GEOID", "capacity", "id")
#write.csv(supply, '~/projects_data/dc_education/supply.csv')

#####################
# DEMAND
#####################

# installed census api key
readRenviron("~/.Renviron/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# population under 15 years in NCR
va.ct <- get_acs(geography = "tract",
                 year = 2019,
                 variables = c("B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010",
                               "B01001_011",  "B01001_012", "B01001_013",  "B01001_014",  "B01001_015",
                               "B01001_016", "B01001_017", "B01001_018", "B01001_019",  "B01001_020",
                               "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", # male 15 to 85+
                               "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034",
                               "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039",
                               "B01001_040", "B01001_041", "B01001_042", "B01001_043", "B01001_044",
                               "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"), # female 15 to 85+
                 state = c("VA"),
                 survey = "acs5",
                 output = "wide",
                 geometry = TRUE)

va.ct <- va.ct %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  pop_over15 = B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
    B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E +
    B01001_017E + B01001_018E + B01001_019E + B01001_020E + B01001_021E +  B01001_022E +
    B01001_023E +  B01001_024E + B01001_025E + B01001_030E +B01001_031E + B01001_032E +
    B01001_033E +  B01001_034E + B01001_035E + B01001_036E + B01001_037E + B01001_038E +
    B01001_039E + B01001_040E +  B01001_041E + B01001_042E + B01001_043E + B01001_044E +
    B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E,
  geometry = geometry
)

ct_centroid <- va.ct %>%
  st_centroid()

demand <- ct_centroid %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)',
                                  convert = TRUE)
demand <- as.data.frame(demand)

# drop where the block group coordinates are NaN
demand <- demand[is.na(demand$lon) == FALSE,]
demand <- demand[is.na(demand$lat) == FALSE,]

##################
# DRIVE TIMES
##################

supply_4year$id <- as.character(supply_4year$id)
supply_2year$id <- as.character(supply_2year$id)
supply_less2$id <- as.character(supply_less2$id)

# OSRM travel times from case study
options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "car")
traveltimes_4year <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_4year[, c("id", "lon", "lat")]
)$duration

traveltimes_2year <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_2year[, c("id", "lon", "lat")]
)$duration

traveltimes_less2 <- osrmTable(
  src = demand[, c("GEOID", "lon", "lat")],
  dst = supply_less2[, c("id", "lon", "lat")]
)$duration

traveltimes_4year[is.na(traveltimes_4year)] <- 0
traveltimes_2year[is.na(traveltimes_2year)] <- 0
traveltimes_less2[is.na(traveltimes_less2)] <- 0

# drive times to 5 nearest colleges
# 4 year colleges
# find the 5 largest values
list_5_top_4year <- apply(traveltimes_4year, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_4year <- apply(list_5_top_4year, 2, function(x) (mean(x)))
median_low_five_4year <- apply(list_5_top_4year, 2, function(x) (median(x)))

out_df_4year <- data.frame(geoid=names(mean_low_five_4year),
                           mean_drive_time_top5=mean_low_five_4year, row.names=NULL)
med_df_4year <- data.frame(geoid=names(median_low_five_4year),
                           median_drive_time_top5=median_low_five_4year, row.names=NULL)

out_df_4year <- left_join(out_df_4year, med_df_4year, by="geoid")

# drive times to 5 nearest colleges
# 2-year colleges
# find the 5 largest values
list_5_top_2year <- apply(traveltimes_2year, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_2year <- apply(list_5_top_2year, 2, function(x) (mean(x)))
median_low_five_2year <- apply(list_5_top_2year, 2, function(x) (median(x)))

out_df_2year <- data.frame(geoid=names(mean_low_five_2year),
                           mean_drive_time_top5=mean_low_five_2year, row.names=NULL)
med_df_2year <- data.frame(geoid=names(median_low_five_2year),
                           median_drive_time_top5=median_low_five_2year, row.names=NULL)

out_df_2year <- left_join(out_df_2year, med_df_2year, by="geoid")

# drive times to 5 nearest colleges
# trade schools
# find the 5 largest values
list_5_top_less2 <- apply(traveltimes_less2, 1, function(x) sort(x, decreasing = F)[1:5])
# mean and median value
mean_low_five_less2 <- apply(list_5_top_less2, 2, function(x) (mean(x)))
median_low_five_less2 <- apply(list_5_top_less2, 2, function(x) (median(x)))

out_df_less2 <- data.frame(geoid=names(mean_low_five_less2),
                           mean_drive_time_top5=mean_low_five_less2, row.names=NULL)
med_df_less2 <- data.frame(geoid=names(median_low_five_less2),
                           median_drive_time_top5=median_low_five_less2, row.names=NULL)

out_df_less2 <- left_join(out_df_less2, med_df_less2, by="geoid")


# format for database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
dbDisconnect(con)

tract_names <- geo_names %>% filter(region_type=="tract")

out_df_4year <- left_join(out_df_4year, tract_names, by=c("geoid"))
out_df_2year <- left_join(out_df_2year, tract_names, by=c("geoid"))
out_df_less2 <- left_join(out_df_less2, tract_names, by=c("geoid"))

out_df_4year["year"] <- 2019
out_df_2year["year"] <- 2019
out_df_less2["year"] <- 2019

# long format
df_long_4year <- melt(out_df_4year,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

# long format
df_long_2year <- melt(out_df_2year,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

# long format
df_long_less2 <- melt(out_df_less2,
                      id.vars=c("geoid", "region_type", "region_name", "year"),
                      variable.name="measure",
                      value.name="value"
)

df_long_4year['measure_type'] = ""
indx1 <- grepl('mean', df_long_4year$measure)
indx2 <- grepl('median', df_long_4year$measure)

df_long_4year$measure_type[indx1] <- 'mean'
df_long_4year$measure_type[indx2] <- 'median'

df_long_2year['measure_type'] = ""
indx1 <- grepl('mean', df_long_2year$measure)
indx2 <- grepl('median', df_long_2year$measure)

df_long_2year$measure_type[indx1] <- 'mean'
df_long_2year$measure_type[indx2] <- 'median'

df_long_less2['measure_type'] = ""
indx1 <- grepl('mean', df_long_less2$measure)
indx2 <- grepl('median', df_long_less2$measure)

df_long_less2$measure_type[indx1] <- 'mean'
df_long_less2$measure_type[indx2] <- 'median'

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_4year_colleges"),
             df_long_4year,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_2year_colleges"),
             df_long_2year,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_trade_schools"),
             df_long_less2,  row.names = F)

dbRemoveTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_4year_colleges"))
dbRemoveTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_2year_colleges"))
dbRemoveTable(con, c("dc_education_training", "va_tr_osrm_2021_drive_times_nearest_trade_schools"))

dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_osrm_2021_drive_times_nearest_4year_colleges
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_osrm_2021_drive_times_nearest_2year_colleges
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_osrm_2021_drive_times_nearest_trade_schools
                    OWNER TO data_commons")

dbDisconnect(con)

##################
# FCA
##################
step_weights_30 <- list(c(30, .22), c(20, .68), c(10, 1))
step_weights_60 <- list(c(60, .042), c(30, .377), c(20, .704), c(10, .962))

demand_4year <- demand
demand_2year <- demand
demand_less2 <- demand

# 4-year colleges
demand_4year['e2sfca'] <- catchment_ratio(
  demand_4year, supply_4year,
  cost=traveltimes_4year,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_4year['fca2s'] <- catchment_ratio(
  demand_4year, supply_4year,
  cost=traveltimes_4year,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_4year['fca3s'] <- catchment_ratio(
  demand_4year, supply_4year,
  cost=traveltimes_4year,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# 2 year colleges
demand_2year['e2sfca'] <- catchment_ratio(
  demand_2year, supply_2year,
  cost=traveltimes_2year,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_2year['fca2s'] <- catchment_ratio(
  demand_2year, supply_2year,
  cost=traveltimes_2year,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_2year['fca3s'] <- catchment_ratio(
  demand_2year, supply_2year,
  cost=traveltimes_2year,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# trade and vocational schools
demand_less2['e2sfca'] <- catchment_ratio(
  demand_less2, supply_less2,
  cost=traveltimes_less2,
  step_weights_30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_less2['fca2s'] <- catchment_ratio(
  demand_less2, supply_less2,
  cost=traveltimes_less2,
  30,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

demand_less2['fca3s'] <- catchment_ratio(
  demand_less2, supply_less2,
  cost=traveltimes_less2,
  step_weights_30,
  normalize_weight = TRUE,
  consumers_value = "pop_over15",
  providers_id = "id",
  providers_value = "capacity", verbose = TRUE
) * 1000

# DB formatting
# add total college capacity
inter_4year <- left_join(demand_4year, supply_4year[,c("GEOID", "capacity")], by = "GEOID")
inter_2year <- left_join(demand_2year, supply_2year[,c("GEOID", "capacity")], by = "GEOID")
inter_less2 <- left_join(demand_less2, supply_less2[,c("GEOID", "capacity")], by = "GEOID")

inter_4year$capacity <- as.numeric(as.character(inter_4year$capacity ))
inter_2year$capacity <- as.numeric(as.character(inter_2year$capacity ))
inter_less2$capacity <- as.numeric(as.character(inter_less2$capacity ))

# calculate the tot capacity by GEOID
df2_4year <- inter_4year %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df2_2year <- inter_2year %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df2_less2 <- inter_less2 %>%  group_by(GEOID) %>%
  summarise(capacity = sum(capacity)) %>%
  as.data.frame()

df3_4year <- left_join(demand_4year, df2_4year, by=("GEOID"))
df3_2year <- left_join(demand_2year, df2_2year, by=("GEOID"))
df3_less2 <- left_join(demand_less2, df2_less2, by=("GEOID"))

# add normalized fca capacities values
df3_4year$norm_fca3s <- (df3_4year$fca3s -
                           min(df3_4year$fca3s, na.rm=TRUE)) /
  (max(df3_4year$fca3s, na.rm=TRUE)
   -min(df3_4year$fca3s, na.rm=TRUE))* 100

df3_2year$norm_fca3s <- (df3_2year$fca3s -
                           min(df3_2year$fca3s, na.rm=TRUE)) /
  (max(df3_2year$fca3s, na.rm=TRUE)
   -min(df3_2year$fca3s, na.rm=TRUE))* 100

df3_less2$norm_fca3s <- (df3_less2$fca3s -
                           min(df3_less2$fca3s, na.rm=TRUE)) /
  (max(df3_less2$fca3s, na.rm=TRUE)
   -min(df3_less2$fca3s, na.rm=TRUE))* 100

# add normalized fca capacities values
df3_4year$norm_fca2s <- (df3_4year$fca2s -
                           min(df3_4year$fca2s, na.rm=TRUE)) /
  (max(df3_4year$fca2s, na.rm=TRUE)
   -min(df3_4year$fca2s, na.rm=TRUE))* 100

df3_2year$norm_fca2s <- (df3_2year$fca2s -
                           min(df3_2year$fca2s, na.rm=TRUE)) /
  (max(df3_2year$fca2s, na.rm=TRUE)
   -min(df3_2year$fca2s, na.rm=TRUE))* 100

df3_less2$norm_fca2s <- (df3_less2$fca2s -
                           min(df3_less2$fca2s, na.rm=TRUE)) /
  (max(df3_less2$fca2s, na.rm=TRUE)
   -min(df3_less2$fca2s, na.rm=TRUE))* 100

# add normalized fca capacities values
df3_4year$norm_efca2s <- (df3_4year$e2sfca -
                            min(df3_4year$e2sfca, na.rm=TRUE)) /
  (max(df3_4year$e2sfca, na.rm=TRUE)
   -min(df3_4year$e2sfca, na.rm=TRUE))* 100

df3_2year$norm_efca2s <- (df3_2year$e2sfca -
                            min(df3_2year$e2sfca, na.rm=TRUE)) /
  (max(df3_2year$e2sfca, na.rm=TRUE)
   -min(df3_2year$e2sfca, na.rm=TRUE))* 100

df3_less2$norm_efca2s <- (df3_less2$e2sfca -
                            min(df3_less2$e2sfca, na.rm=TRUE)) /
  (max(df3_less2$e2sfca, na.rm=TRUE)
   -min(df3_less2$e2sfca, na.rm=TRUE))* 100

# select columns to keep
df3_4year <- df3_4year %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

df3_2year <- df3_2year %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

df3_less2 <- df3_less2 %>% select(c("GEOID", "e2sfca", "fca2s",
                                    "fca3s", "capacity", "norm_fca3s",
                                    "norm_fca2s", "norm_efca2s"))

# add region names
out_df_4year <- left_join(df3_4year, tract_names, by=c("GEOID"="geoid"))
out_df_2year <- left_join(df3_2year, tract_names, by=c("GEOID"="geoid"))
out_df_less2 <- left_join(df3_less2, tract_names, by=c("GEOID"="geoid"))

out_df_4year["year"] <- 2019
out_df_2year["year"] <- 2019
out_df_less2["year"] <- 2019

names(out_df_4year)[1] <- "geoid"
names(out_df_2year)[1] <- "geoid"
names(out_df_less2)[1] <- "geoid"

# long format
out_df_long_4year <- melt(out_df_4year,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_2year <- melt(out_df_2year,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_less2 <- melt(out_df_less2,
                          id.vars=c("geoid", "region_type", "region_name", "year"),
                          variable.name="measure",
                          value.name="value"
)

out_df_long_4year['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_4year$measure)
indx2 <- grepl('norm', out_df_long_4year$measure)
indx3 <- grepl('capacity', out_df_long_4year$measure)
out_df_long_4year$measure_type[indx1] <- 'index'
out_df_long_4year$measure_type[indx2] <- 'normalized index'
out_df_long_4year$measure_type[indx3] <- 'count'

out_df_long_2year['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_2year$measure)
indx2 <- grepl('norm', out_df_long_2year$measure)
indx3 <- grepl('capacity', out_df_long_2year$measure)
out_df_long_2year$measure_type[indx1] <- 'index'
out_df_long_2year$measure_type[indx2] <- 'normalized index'
out_df_long_2year$measure_type[indx3] <- 'count'

out_df_long_less2['measure_type'] = ""
indx1 <- grepl('fca', out_df_long_less2$measure)
indx2 <- grepl('norm', out_df_long_less2$measure)
indx3 <- grepl('capacity', out_df_long_less2$measure)
out_df_long_less2$measure_type[indx1] <- 'index'
out_df_long_less2$measure_type[indx2] <- 'normalized index'
out_df_long_less2$measure_type[indx3] <- 'count'

# replace NAs in capacity with 0
out_df_long_4year$value[is.na(out_df_long_4year$value)]<-0
out_df_long_2year$value[is.na(out_df_long_2year$value)]<-0
out_df_long_less2$value[is.na(out_df_long_less2$value)]<-0


# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_4year_colleges_access_scores"),
             out_df_long_4year,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_2year_colleges_access_scores"),
             out_df_long_2year,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_trade_schools_access_scores"),
             out_df_long_less2,  row.names = F)

# dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_4year_colleges_access_scores"))
# dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_2year_colleges_access_scores"))
# dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_trade_schools_access_scores"))

dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_4year_colleges_access_scores
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_2year_colleges_access_scores
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_trade_schools_access_scores
                    OWNER TO data_commons")

dbDisconnect(con)
