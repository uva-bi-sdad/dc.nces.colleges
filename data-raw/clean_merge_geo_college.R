# packages
library(dplyr)
library(data.table)
library(stringr)
library(tidygeocoder)

# read in college data

four_year_college <- read.csv('/college_data/four-year-colleges.csv', 
                              stringsAsFactors = FALSE)
two_year_college <- read.csv('/college_data/two-year-colleges.csv', 
                             stringsAsFactors = FALSE)
less_two_college <- read.csv('/college_data/less_2_year_colleges.csv', 
                             stringsAsFactors = FALSE)

# clean address
less_two_college$address = str_remove(string = less_two_college$address, pattern = less_two_college$name)
less_two_college$address <- gsub('(,)', '', less_two_college$address)

two_year_college$address = str_remove(string = two_year_college$address, pattern = two_year_college$name)
two_year_college$address <- gsub('(,)', '', two_year_college$address)

four_year_college$address = str_remove(string = four_year_college$address, pattern = four_year_college$name)
four_year_college$address <- gsub('(,)', '', four_year_college$address)

# append together 
college <- do.call("rbind", list(four_year_college,two_year_college,less_two_college))
# save csv 
# write.csv(college,'/college_data/college.csv')

# clean address
college$address = str_remove(string = college$address, pattern = college$name)
college$address <- gsub('(,)', '', college$address)

# total enrolment vs undergrad enrolment 
college$tot_enrol <- str_extract(college$student_population, '[0-9]+(?= \\()')
college$tot_enrol <- as.numeric(college$tot_enrol)
college$undergrad_enrol <- str_extract(college$student_population, '(?<=\\()[0-9]+|(?<=\\()all')

less_two_college$tot_enrol <- str_extract(less_two_college$student_population, '[0-9]+(?= \\()')
less_two_college$tot_enrol <- as.numeric(less_two_college$tot_enrol)
less_two_college$undergrad_enrol <- str_extract(less_two_college$student_population, '(?<=\\()[0-9]+|(?<=\\()all')

two_year_college$tot_enrol <- str_extract(two_year_college$student_population, '[0-9]+(?= \\()')
two_year_college$tot_enrol <- as.numeric(two_year_college$tot_enrol)
two_year_college$undergrad_enrol <- str_extract(two_year_college$student_population, '(?<=\\()[0-9]+|(?<=\\()all')

four_year_college$tot_enrol <- str_extract(four_year_college$student_population, '[0-9]+(?= \\()')
four_year_college$tot_enrol <- as.numeric(four_year_college$tot_enrol)
four_year_college$undergrad_enrol <- str_extract(four_year_college$student_population, '(?<=\\()[0-9]+|(?<=\\()all')


# geocode addresses
# installed google api key
readRenviron("~/.Renviron")
Sys.getenv("GOOGLEGEOCODE_API_KEY")

#geocode the addresses
college_lonlat <- college %>%
  geocode(address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = FALSE)

#geocode the addresses
two_year_lonlat <- two_year_college %>%
  geocode(address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = FALSE)

#geocode the addresses
less_two_lonlat <- less_two_college %>%
  geocode(address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = FALSE)

#geocode the addresses
four_year_lonlat <- four_year_college %>%
  geocode(address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = FALSE)

# save into RDS and csv files
write.csv(college_lonlat, '/college_data/college_lonlat.csv')
write.csv(less_two_lonlat, '/college_data/less_two_lonlat.csv')
write.csv(two_year_lonlat, '/college_data/two_year_lonlat.csv')
write.csv(four_year_lonlat, '/college_data/four_year_lonlat.csv')