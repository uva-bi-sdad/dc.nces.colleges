# packages

library(RPostgreSQL)
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)

# connect to database
con <- dbConnect(PostgreSQL(), 
                 dbname = "sdad",
                 host = "postgis1", 
                 port = 5432, 
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

tables_dc_com <- dbGetQuery(con, "SELECT * FROM information_schema.tables 
WHERE table_schema = 'dc_common'")

dbDisconnect(con)

counties <- dbGetQuery(con, "SELECT * FROM dc_common.va_ct_sdad_2021_virginia_county_geoids")
tracts <- dbGetQuery(con, "SELECT * FROM dc_common.va_tr_sdad_2021_virginia_tract_geoids") 
health_dist <- dbGetQuery(con, "SELECT * FROM dc_common.va_hdct_sdad_2021_health_district_counties")

# read in access file
#fca <- read.csv("/college_data/3sfca_less_two_tr.csv")
#fca <- read.csv("/college_data/3sfca_less_two_ct.csv")
fca <- read.csv("/college_data/3sfca_two_year_ct.csv")
#fca <- read.csv("/college_data/3sfca_four_year_ct.csv")
#fca <- read.csv("/college_data/3sfca_two_year_tr.csv")
#fca <- read.csv("/college_data/3sfca_four_year_tr.csv")
fca$GEOID <- as.character(fca$GEOID) 

# read in supply for capacities
#supply <- read.csv("/college_data/less_two_tr_supply.csv")
#supply <- read.csv("/college_data/less_two_ct_supply.csv")
supply <- read.csv("/college_data/two_year_ct_supply.csv")
#supply <- read.csv("/college_data/four_year_ct_supply.csv")
#supply <- read.csv("/college_data/two_year_tr_supply.csv")

#supply <- read.csv("/college_data/four_year_tr_supply.csv")
supply$GEOID <- as.character(supply$GEOID) 
# add total college capacity 
inter_df <- left_join(fca, supply[,c("GEOID", "capacity")], by = "GEOID")

# calculate the tot capacity by GEOID
df2 <- inter_df %>%  group_by(GEOID) %>%
  dplyr::summarise(capacity = sum(capacity)) %>% 
  as.data.frame()
df3 <- left_join(fca, df2, by=("GEOID"))

# add normalized fca capacities values
df3$norm_3sfca <- (df3$X3sfca_capacity - 
  min(df3$X3sfca_capacity, na.rm=TRUE)) / 
  (max(df3$X3sfca_capacity, na.rm=TRUE)
   -min(df3$X3sfca_capacity, na.rm=TRUE))* 100

# add normalized fca capacities values
df3$norm_2sfca <- (df3$X2sfca30_capacity - 
                     min(df3$X2sfca30_capacity, na.rm=TRUE)) / 
  (max(df3$X2sfca30_capacity, na.rm=TRUE)
   -min(df3$X2sfca30_capacity, na.rm=TRUE))* 100

# add census tract/counties names
#va_data <- left_join(df3, tracts, by=c("GEOID"= "geoid"))
va_data <- left_join(df3, counties, by=c("GEOID"= "geoid"))
# rearrange columns
#va_data_tr <- va_data_tr[, c(1, 7, 2, 3, 5,6,4)]
va_data <- va_data[, c(1, 7, 2, 3, 5,6,4)]
# rename columns
names(va_data)[1] <- 'geoid'
names(va_data)[3] <- '3sfca_capacity'
names(va_data)[4] <- '2sfca30_capacity'
names(va_data)[7] <- 'college_capacity'

# save 
# write_csv(va_data, "/college_data/va_ct_nces_2019_2year_colleges.csv")

# Add to DB
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

two_year_ct <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_ct_sdad_2019_2year_colleges_access_scores")
two_year_tr <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_sdad_2019_2year_colleges_access_scores")
two_year_hd <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_hd_sdad_2019_2year_colleges_access_scores")

four_year_ct <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_ct_sdad_2019_4year_colleges_access_scores")
four_year_tr <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_sdad_2019_4year_colleges_access_scores")
four_year_hd <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_hd_sdad_2019_4year_colleges_access_scores")

trade_ct <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_ct_sdad_2019_trade_schools_access_scores")
trade_tr <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_tr_sdad_2019_trade_schools_access_scores")
trade_hd <- dbGetQuery(con, "SELECT * FROM dc_education_training.va_hd_sdad_2019_trade_schools_access_scores")

health_dist <- dbGetQuery(con, "SELECT * FROM dc_geographies.va_hd_vdh_2021_health_district_geo_names")

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")

dbRemoveTable(con, c("dc_education_training", "va_ct_sdad_2019_2year_colleges_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_2year_colleges_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_hd_sdad_2019_2year_colleges_access_scores"))

dbRemoveTable(con, c("dc_education_training", "va_ct_sdad_2019_4year_colleges_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_4year_colleges_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_hd_sdad_2019_4year_colleges_access_scores"))

dbRemoveTable(con, c("dc_education_training", "va_ct_sdad_2019_trade_schools_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_tr_sdad_2019_trade_schools_access_scores"))
dbRemoveTable(con, c("dc_education_training", "va_hd_sdad_2019_trade_schools_access_scores"))

dbWriteTable(con, c("dc_education_training", "va_ct_sdad_2019_2year_colleges_access_scores"),
             two_year_ct,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_sdad_2019_2year_colleges_access_scores
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_2year_colleges_access_scores"),
             two_year_tr,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_2year_colleges_access_scores
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_hd_sdad_2019_2year_colleges_access_scores"),
             two_year_hd,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_hd_sdad_2019_2year_colleges_access_scores
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_ct_sdad_2019_4year_colleges_access_scores"),
             four_year_ct,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_sdad_2019_4year_colleges_access_scores
                OWNER TO data_commons")
dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_4year_colleges_access_scores"),
             four_year_tr,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_4year_colleges_access_scores
                OWNER TO data_commons")
dbWriteTable(con, c("dc_education_training", "va_hd_sdad_2019_4year_colleges_access_scores"),
             four_year_hd,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_hd_sdad_2019_4year_colleges_access_scores
                OWNER TO data_commons")


dbWriteTable(con, c("dc_education_training", "va_ct_sdad_2019_trade_schools_access_scores"),
             trade_ct,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_sdad_2019_trade_schools_access_scores
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_tr_sdad_2019_trade_schools_access_scores"),
             trade_tr,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_sdad_2019_trade_schools_access_scores
                OWNER TO data_commons")

dbWriteTable(con, c("dc_education_training", "va_hd_sdad_2019_trade_schools_access_scores"),
             trade_hd,  row.names = F)
dbSendStatement(con, "ALTER TABLE dc_education_training.va_hd_sdad_2019_trade_schools_access_scores
                OWNER TO data_commons")

dbDisconnect(con)

# county geoids and names
counties <- geo_names %>% filter(region_type=="county")

# remove 'measure units' columns
four_year_ct <- four_year_ct %>% select(-measure_units) 
four_year_tr <- four_year_tr %>% select(-measure_units) 
four_year_hd <- four_year_hd %>% select(-measure_units)

two_year_ct <- two_year_ct %>% select(-measure_units) 
two_year_tr <- two_year_tr %>% select(-measure_units) 
two_year_hd <- two_year_hd %>% select(-measure_units)

trade_ct <- trade_ct %>% select(-measure_units) 
trade_tr <- trade_tr %>% select(-measure_units) 
trade_hd <- trade_hd %>% select(-measure_units)

# multiply access scores by a 1000
# county
four_year_ct$value[four_year_ct$measure == "3sfca_capacity"] <-
  four_year_ct$value[four_year_ct$measure == "3sfca_capacity"]*1000

four_year_ct$value[four_year_ct$measure == "2sfca30_capacity"] <-
  four_year_ct$value[four_year_ct$measure == "2sfca30_capacity"]*1000

# tract 
four_year_tr$value[four_year_tr$measure == "3sfca_capacity"] <-
  four_year_tr$value[four_year_tr$measure == "3sfca_capacity"]*1000

four_year_tr$value[four_year_tr$measure == "2sfca30_capacity"] <-
  four_year_tr$value[four_year_tr$measure == "2sfca30_capacity"]*1000

# health dists
four_year_hd$value[four_year_hd$measure == "2sefca30_tot_enrol"] <-
  four_year_hd$value[four_year_hd$measure == "2sefca30_tot_enrol"]*1000

four_year_hd$value[four_year_hd$measure == "3sfca_tot_enrol"] <-
  four_year_hd$value[four_year_hd$measure == "3sfca_tot_enrol"]*1000

# two year 
# county
two_year_ct$value[two_year_ct$measure == "3sfca_capacity"] <-
  two_year_ct$value[two_year_ct$measure == "3sfca_capacity"]*1000

two_year_ct$value[two_year_ct$measure == "2sfca30_capacity"] <-
  two_year_ct$value[two_year_ct$measure == "2sfca30_capacity"]*1000

# tract 
two_year_tr$value[two_year_tr$measure == "3sfca_capacity"] <-
  two_year_tr$value[two_year_tr$measure == "3sfca_capacity"]*1000

two_year_tr$value[two_year_tr$measure == "2sfca30_capacity"] <-
  two_year_tr$value[two_year_tr$measure == "2sfca30_capacity"]*1000

# health dists
two_year_hd$value[two_year_hd$measure == "3sfca_tot_enrol"] <-
  two_year_hd$value[two_year_hd$measure == "3sfca_tot_enrol"]*1000

two_year_hd$value[two_year_hd$measure == "2sefca30_tot_enrol"] <-
  two_year_hd$value[two_year_hd$measure == "2sefca30_tot_enrol"]*1000

# trade schools
# county
trade_ct$value[trade_ct$measure == "3sfca_capacity"] <-
  trade_ct$value[trade_ct$measure == "3sfca_capacity"]*1000

trade_ct$value[trade_ct$measure == "2sfca30_capacity"] <-
  trade_ct$value[trade_ct$measure == "2sfca30_capacity"]*1000

# tract 
trade_tr$value[trade_tr$measure == "3sfca_capacity"] <-
  trade_tr$value[trade_tr$measure == "3sfca_capacity"]*1000

trade_tr$value[trade_tr$measure == "2sfca30_capacity"] <-
  trade_tr$value[trade_tr$measure == "2sfca30_capacity"]*1000

# health dists
trade_hd$value[trade_hd$measure == "3sfca_tot_enrol"] <-
  trade_hd$value[trade_hd$measure == "3sfca_tot_enrol"]*1000

trade_hd$value[trade_hd$measure == "2sefca30_tot_enrol"] <-
  trade_hd$value[trade_hd$measure == "2sefca30_tot_enrol"]*1000

# replace NAs in counts with 0s
four_year_ct <- within(four_year_ct, value[(is.na(value) & measure == 'college_capacity')] <- 0)
four_year_tr <- within(four_year_tr, value[(is.na(value) & measure == 'college_capacity')] <- 0)
four_year_hd <- within(four_year_hd, value[(is.na(value) & measure == 'capacity')] <- 0)

two_year_ct <- within(two_year_ct, value[(is.na(value) & measure == 'college_capacity')] <- 0)
two_year_tr <- within(two_year_tr, value[(is.na(value) & measure == 'college_capacity')] <- 0)
two_year_hd <- within(two_year_hd, value[(is.na(value) & measure == 'capacity')] <- 0)

trade_ct <- within(trade_ct, value[(is.na(value) & measure == 'college_capacity')] <- 0)
trade_tr <- within(trade_tr, value[(is.na(value) & measure == 'college_capacity')] <- 0)
trade_hd <- within(trade_hd, value[(is.na(value) & measure == 'capacity')] <- 0)

# remove old county names
four_year_ct <- four_year_ct %>% select(-c("region_name","region_type"))
two_year_ct <- two_year_ct %>% select(-c("region_name","region_type"))
trade_ct <- trade_ct %>% select(-c("region_name","region_type"))

# add new county names
four_year_ct <- left_join(four_year_ct, counties, by="geoid")
two_year_ct <- left_join(two_year_ct, counties, by="geoid")
trade_ct <- left_join(trade_ct, counties, by="geoid")

# remove old health dsitricts ids
four_year_hd <- four_year_hd %>% select(-c("geoid","region_type"))
two_year_hd <- two_year_hd %>% select(-c("geoid","region_type"))
trade_hd <- trade_hd %>% select(-c("geoid","region_type"))

# add new health distrcits geoids
four_year_hd <- left_join(four_year_hd, health_dist, by="region_name")
two_year_hd <- left_join(two_year_hd, health_dist, by="region_name")
trade_hd <- left_join(trade_hd, health_dist, by="region_name")

# re-arrange columns
four_year_ct <- four_year_ct[, c(1, 7,6, 2, 3, 4, 5)]
two_year_ct <- two_year_ct[, c(1, 7,6, 2, 3, 4, 5)]
trade_ct <- trade_ct[, c(1, 7,6, 2, 3, 4, 5)]

four_year_hd <- four_year_hd[, c(6, 7, 1, 2, 3, 4, 5)]
two_year_hd <- two_year_hd[, c(6, 7, 1, 2, 3, 4, 5)]
trade_hd <- trade_hd[, c(6, 7, 1, 2, 3, 4, 5)]