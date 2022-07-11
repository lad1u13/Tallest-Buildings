
### Read in csv data files
# Read countries file and perform data cleaning
countries <-
  read.csv('data/countries of the world.csv', # name of csv file to read
           dec = ',', # Decimals are currently represented with commas, replace with periods using dec argument
           check.names = FALSE # setting to FALSE  improves formatting of column names
  ) %>%
  mutate(Country = str_squish(Country)) %>% # Remove whitespace to allow later join by country
  mutate(Country = case_when(Country == 'Korea, South' ~ 'South Korea',
                             Country == 'United States' ~ 'USA',
                             TRUE ~ Country))

# Read buildings file with data cleaning and using world.cities dataset from maps package to find countries for missing cities
buildings <-
  read.csv('data/tallest_completed_buildings.csv') %>%
  rename_with(tolower) %>% # Convert column names to camel case to allow later join by city
  rename_with(str_to_title) %>%
  mutate(Height = as.numeric(as.character(gsub(" .*", " ", Height))), # Keep only metres
         City = case_when(City == 'St. Petersburg' ~ 'Saint Petersburg',
                          TRUE ~ as.character(City))) %>%
  mutate(City = str_squish(str_replace(City, 'City', ''))) %>%
  left_join(world.cities %>% #  Using world.cities dataframe from map package to retrieve countries where missing
              rename(City = 'name',
                     Country = 'country.etc') %>%
              filter(!(City == 'Saint Petersburg' & Country == 'USA'), # Remove unwanted city duplicates (e.g a saint petersburg also exists in the US)
                     !(City == 'Los Angeles' & Country == 'Chile'),
                     !(City == 'Suzhou' & Country == 'China' & pop < 1000000) # Two versions of Suzhou - we want the one with the larger population
              )
  ) %>% 
  # Manually deal with cases that did not map
  mutate(Country = case_when(City == 'Hong Kong' ~ 'Hong Kong',
                             City == 'Kuwait' ~ 'Kuwait',
                             City == 'Busan' ~ 'South Korea',
                             City == 'Ho Chi Minh' ~ 'Vietnam',
                             TRUE ~ Country),
         lat = case_when(City == 'Hong Kong' ~ 22.31, # lat = latitude
                         City == 'Kuwait' ~ 47.48,
                         City == 'Busan' ~ 31.18,
                         City == 'Ho Chi Minh' ~ 10.82,
                         City == 'Los Angeles' ~ 34.11,
                         TRUE ~ lat
         ),
         long = case_when(City == 'Hong Kong' ~ 114.17, # long = longitude
                          City == 'Kuwait' ~ 47.48,
                          City == 'Busan' ~ 129.08,
                          City == 'Ho Chi Minh' ~ 106.63,
                          City == 'Los Angeles' ~ -118.41,
                          TRUE ~ long),
         Country = case_when(Country == 'Korea South' ~ 'South Korea',
                             TRUE ~ Country)
  ) 

# Read in cities dataset - note many of these cities do not contain top 100 tallest buildings so these rows will be dropped when joined
cities <-
  read.csv('data/The Most Largest Cities in the World 1950 - 2035.csv',
           check.names = FALSE) %>%
  dplyr::select(- one_of("Countries' Flags", "From 1950 - 2035")) # Drop redundant columns - flag images and empty column

# Join the three datasets together into one complete one
full_data <- buildings %>%
  left_join(cities) %>%
  left_join(countries) %>%
  left_join(iso3166 %>% # Include country codes from the map package for later visualising countries in map
              rename(Country = 'mapname',
                     CODE = 'a3') %>% 
              dplyr::select(Country, CODE))

# Add country level column with number of top 100 tallest buildings in countryfull  
full_data <- full_data %>%
  left_join(full_data %>%
              group_by(Country) %>%
              summarise(`Number of top 100 tallest buildings` = n())
  )

# dim(full_data) # Check there are still 100 rows

