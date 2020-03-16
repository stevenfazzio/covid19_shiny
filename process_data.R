library(tidyverse)
library(lubridate)

path <- '../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'

us_state_abb <- tibble(
  state_abb = state.abb,
  state_name = state.name
)
ca_state_abb <- tibble(
  state_abb = c('NL', 'PE', 'NS', 'NB', 'QC', 'ON', 'MB', 'SK', 'AB', 'BC', 'YT', 'NT', 
                'NU'),
  state_name = c('Newfoundland and Labrador', 'Prince Edward Island', 'Nova Scotia', 
                 'New Brunswick', 'Quebec', 'Ontario', 'Manitoba', 'Saskatchewan',
                 'Alberta', 'British Columbia', 'Yukon', 'Northwest Territories',
                 'Nunavut')
)
state_abb <- bind_rows(us_state_abb, ca_state_abb)


file_reader <- function(filename, path) {
  date <- str_extract(filename, '\\d{2}-\\d{2}-\\d{4}') %>% 
    mdy()
  
  str_c(path, filename) %>% 
    read_csv() %>% 
    mutate(date = date) %>% 
    select(
      state = `Province/State`,
      country = `Country/Region`,
      date,
      confirmed = Confirmed,
      deaths = Deaths,
      recovered = Recovered
    )
}

raw_data <- list.files(path) %>% 
  str_subset('.csv') %>% 
  map(file_reader, path = path) %>% 
  bind_rows()

all_data <- raw_data %>% 
  replace_na(list(confirmed = 0, deaths = 0, recovered = 0)) %>% 
  mutate(active = confirmed - (deaths + recovered)) %>% 
  pivot_longer(-(1:3), names_to = 'statistic', values_to = 'num_people') %>% 
  mutate(
    country = if_else(country == 'Mainland China', 'China', country),
    country = case_when(
      country == 'Mainland China' ~ 'China',
      country == 'Korea, South' ~ 'South Korea',
      country == 'Republic of Korea' ~ 'South Korea',
      TRUE ~ country
    ),
    state = case_when(
      state == 'Virgin Islands, U.S.' ~ 'Virgin Islands',
      state == 'France' ~ NA_character_,
      str_detect(state, 'Princess') ~ 'Cruise Ship',
      TRUE ~ state
    ),
    state = str_replace(state, '.*, ', '')
  ) %>% 
  left_join(state_abb, by = c('state' = 'state_abb')) %>% 
  mutate(state = coalesce(state_name, state))

by_state <- all_data %>% 
  filter(!is.na(state)) %>% 
  mutate(region = str_c(state, ', ', country, ' (State/Province)')) %>% 
  group_by(region_type = 'state', region, date, statistic) %>% 
  summarise(num_people = sum(num_people))

by_country <- all_data %>% 
  mutate(region = str_c(country, ' (Country)')) %>% 
  group_by(region_type = 'country', region, date, statistic) %>% 
  summarise(num_people = sum(num_people))

non_china <- all_data %>% 
  filter(country != 'China') %>% 
  mutate(region = 'Non-China (International)') %>% 
  group_by(region_type = 'international', region, date, statistic) %>% 
  summarise(num_people = sum(num_people))

global <- all_data %>% 
  mutate(region = 'Global (International)') %>% 
  group_by(region_type = 'international', region, date, statistic) %>% 
  summarise(num_people = sum(num_people))

processed_data <- bind_rows(
  by_state,
  by_country,
  non_china,
  global
)

write_csv(processed_data, 'data/processed_data.csv')


