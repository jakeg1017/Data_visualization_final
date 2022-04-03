####setup####
library(tidyverse)
setwd("C:/Local/Docs/School/4th Year/Spring/Descriptive Analytics/Data")
cords<- read.csv("world_coordinates.csv")
covid<- read.csv("owid-covid-data.csv")
index<- read.csv("Indexes.csv")
country<- read.csv("countries.csv")
####Making the country_data dataset####
index = index[c(1:3,9,10,11,22,30,31,34,38,39,43,49,50,52,53)]

blacklist = c("World","Upper middle income","Lower middle income","Low income","European Union","Europe","Asia","Africa","High income","International","North America","South America","Oceania")

bl_cords <- covid %>%
  filter(!location %in% cords$Country) %>%
  select(location) %>%
  unique()

bl_country <- covid %>%
  filter(!location %in% country$Country) %>%
  select(location) %>%
  unique()

bl_index <- covid %>%
  filter(!location %in% index$Id) %>%
  select(location) %>%
  unique()

blacklist <- blacklist %>%
  append(bl_index) %>%
  append(bl_country) %>%
  append(bl_cords)
  
whitelist <- covid$location %>%
  unique()

index <- index %>%
  rename("Country" = Id)
  
country_data <- country %>%
  left_join(cords,by = "Country") %>%
  left_join(index,by = "Country") %>%
  filter(Country %in% whitelist)
####Functions####
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
sum_na <- function(v){
  v %>%
    is.na() %>%
    sum()
}
####looking at the covid data####

covid <- covid %>%
  filter(!location %in% blacklist)

covid <- covid %>%
  filter(location %in% country_data$Country) 

covid <- covid %>%
  transmute(
    location,
    continent,
    date = as.Date(date),
    total_cases,
    new_cases,
    total_deaths,
    new_deaths,
    total_cases_per_million,
    total_deaths_per_million,
    new_cases_per_million,
    new_deaths_per_million,
    reproduction_rate,
    icu_patients,
    icu_patients_per_million,
    hosp_patients,
    hosp_patients_per_million,
    new_tests,
    new_tests_per_thousand,
    total_tests,
    total_tests_per_thousand,
    positive_rate,
    total_vaccinations,
    total_vaccinations_per_hundred,
    people_vaccinated,
    people_vaccinated_per_hundred,
    people_fully_vaccinated,
    people_fully_vaccinated_per_hundred,
    total_vaccinations,
    total_vaccinations_per_hundred,
    total_boosters,
    total_boosters_per_hundred,
    stringency_index,
    year = substr(covid$date,1,4),
    season = getSeason(covid$date),
    period = paste(season, year)
    )

covid %>%
  transmute(location,continent,x = 1) %>%
  group_by(location,continent) %>%
  summarise("rows" = sum(x)) %>%
  arrange(desc(rows))
  

country_data %>%
  select(Country,Population) %>%
  arrange(desc(Population)) 

covid %>%
  select(period) %>%
  unique()

covid %>%
  transmute(location,period,x = 1) %>%
  group_by(location,period) %>%
  summarise("rows" = sum(x))

covid %>%
  group_by(location) %>%
  summarize()
  

covid <- covid %>%
  mutate(nulls = apply(covid,1,sum_na),
         x = 1)

data_quality <- covid %>%
  group_by(location) %>%
  summarise("rows" = sum(x),
            "avg_nulls" = mean(nulls)) %>%
  mutate(total_cols = dim(covid)[2],
         total_rows = dim(covid)[1]) %>%
  mutate(pct_of_rows = rows / total_rows,
         pct_of_cols_null = avg_nulls / total_cols) %>%
  arrange(desc(pct_of_cols_null)) %>%
  data.frame()

filt <- data_quality %>%
        filter(avg_nulls < 16) %>%
        select(location)

covid <- covid %>%
  filter(location %in% filt$location)

covid %>%
  apply(2,is.na) %>%
  data.frame() %>%
  mutate(location = covid$location,
         x = covid$x) %>%
  group_by(location) %>%
  summarise("total_cases" = sum(total_cases),
            "new_cases" = sum(new_cases),
            "total_deaths" = sum(total_deaths),
            "new_deaths" = sum(new_deaths),
            "total_cases_per_million" = sum(total_cases_per_million),
            "total_deaths_per_million" = sum(total_deaths_per_million),
            "new_cases_per_million" = sum(new_cases_per_million),
            "new_deaths_per_million" = sum(new_deaths_per_million),
            "reproduction_rate" = sum(reproduction_rate),
            "icu_patients" = sum(icu_patients),
            "icu_patients_per_million" = sum(icu_patients_per_million),
            "hosp_patients" = sum(hosp_patients),
            "hosp_patients_per_million" = sum(hosp_patients_per_million),
            "new_tests" = sum(new_tests),
            "new_tests_per_thousand" = sum(new_tests_per_thousand),
            "total_tests" = sum(total_tests),
            "total_tests_per_thousand" = sum(total_tests_per_thousand),
            "positive_rate" = sum(positive_rate),
            "total_vaccinations" = sum(total_vaccinations),
            "total_vaccinations_per_hundred" = sum(total_vaccinations_per_hundred),
            "people_vaccinated" = sum(people_vaccinated),
            "people_vaccinated_per_hundred" = sum(people_vaccinated_per_hundred),
            "people_fully_vaccinated" = sum(people_fully_vaccinated),
            "stringency_index" = sum(stringency_index),
            "total_rows" = sum(x)) %>%
             data.frame() %>%
             summary()

 covid <- covid %>%
  select(location:reproduction_rate,
         new_tests:people_fully_vaccinated_per_hundred,
         stringency_index:period)

 covid <- covid %>%
                select(location:date,
                       year:period,
                       total_cases,
                       total_deaths,
                       reproduction_rate,
                       positive_rate,
                       stringency_index,
                       contains("per"))

####Trimming country_data####

 #filtering the country data
 country_data <- country_data %>%
   filter(Country %in% unique(covid$location))
 
 #adding continent and getting rid of columns
 continents <- covid %>% 
   select(Country = location,
          continent) %>%
   unique()
 
 country_data <- country_data %>%
   merge(continents, by = "Country") %>%
   transmute(country = Country,
             region = Region,
             continent,
             latitude,
             longitude,
             population = Population,
             area_sq_mi = Area..sq..mi..,
             pop_density_per_sq_mi = as.numeric(gsub(",", ".",Pop..Density..per.sq..mi..)),
             net_migration = as.numeric(gsub(",", ".",Net.migration)),
             GDP_per_capita = GDP....per.capita.,
             HDI = Human.Development.Index.HDI.2014,
             gini_coefficient = Gini.coefficient.2005.2013,
             consumer_price_index = Consumer.price.index.2013,
             gender_inequality_index = Gender.Inequality.Index.2014,
             infant_mortality_per_1k = as.numeric(gsub(",", ".", Infant.mortality..per.1000.births.)),
             physicians_per_10k = Physicians.per.10k.people,
             pub_health_spend_pct_of_gdp = Public.health.expenditure.percentage.of.GDP.2013,
             avg_years_of_education = Mean.years.of.schooling...Years,
             pub_education_spend_pct_of_gdp = Public.expenditure.on.education.Percentange.GDP,
             phones_per_1k = as.numeric(gsub(",", ".", Phones..per.1000.)),
             pct_of_internet_users = Internet.users.percentage.of.population.2014,
             prison_pop_per_100k = Prison.population.per.100k.people)
 
# finding the countries with null values
  country_data %>%
   apply(1,sum_na) %>%
   data.frame() %>%
   mutate(i = 1:124) %>%
   filter(.>0)
# dropping those countries
 country_data = country_data[-c(28,48,111,31),]
 
# filtering and renaming the covid data again
 covid <- covid %>%
 filter(location %in% country_data$country) %>%
 rename(country = location)
 
####Uploading final data####
 
 write.csv(covid,"final_data/covid_data.csv")
 write.csv(country_data,"final_data/country_data.csv")
 
 
 
####Grouping the covid data####
covid %>%
  group_by(location,period) %>%
  filter(year < 2022) %>%
  summarise("total_cases" = max(total_cases),
            "total_deaths" = max(total_deaths),
            "avg_positive_test_rate" = mean(positive_rate),
            "avg_reproduction_rate" = mean(reproduction_rate),
            "avg_stringency_index" = mean(stringency_index),
            "total_cases_per_million" = max(total_cases_per_million),
            "total_deaths_per_million" = mean(total_deaths_per_million),
            "avg_new_cases_per_million"  = mean(new_cases_per_million),
            "avg_new_deaths_per_million" = mean(new_deaths_per_million),
            "avg_new_tests_per_thousand" = mean(new_tests_per_thousand),
            "total_tests_per_thousand" = max(total_tests_per_thousand),
            "total_vaccinations_per_hundred" = max(total_vaccinations_per_hundred),
            "people_vaccinated_per_hundred" = max(people_vaccinated_per_hundred),
            "people_fully_vaccinated_per_hundred" = max(people_fully_vaccinated_per_hundred)) %>%
             data.frame() %>%
             view()
 
 apply(covid,2,sum_na)

