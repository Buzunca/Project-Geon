###Brandon Hoang
##Specific Question Analyses



library("readxl")
library("dplyr")
library("ggplot2")
library("stringr")
library("tidyr")

wa_county_pop_df <- read.csv("data/wa_county_population.csv")
wa_covid_deaths_df <- read_excel("data/WA_COVID19_Cases_Hospitalizations_Deaths.xlsx") 
wa_covid_cases_df <- read.csv("data/WA_COVID19_Cases.csv")
wa_unemployment_insurance_claims_df <- read.csv("data/washington_unemployment_data.csv")


#Data Wrangling
county_in_question <- c("King County", "Clark County", "Pierce County", "Snohomish County", "Spokane County")

county_covid_cases <- wa_covid_cases_df %>% 
  #rename(County = ?..County) %>%
  filter(County %in% county_in_question) %>%   #https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr
  filter(WeekStartDate > "2019-12-31") %>% 
  mutate(
    month = months(as.Date(WeekStartDate))
  )

county_covid_cases$County = str_sub(county_covid_cases$County, start = 1, end = -8)

county_covid_cases_by_month <- county_covid_cases %>%
  mutate(
    month_and_county = paste(County,month)
  ) %>% 
  group_by(County, month, month_and_county) %>%
  summarise(
    cases = sum(ConfirmedCases)
  )


####
county_pop_cases <- wa_county_pop_df %>% 
  #rename(CTYNAME = ?..CTYNAME) %>%
  filter(CTYNAME %in% county_in_question)

county_pop_cases$CTYNAME = str_sub(county_pop_cases$CTYNAME, start = 1, end = -8)
county_pop_cases
#duptimes <- 12
#dup_index <- rep(1:nrow(county_pop_cases), duptimes)
#dup_county_pop_df <- county_pop_cases[dup_index,]

dup_county_pop_with_county_df <- county_pop_cases %>% 
  mutate(
    County.x = CTYNAME
  )

pop <- function(county_name) {county_pop_cases %>% 
  filter(CTYNAME == county_name) %>% 
  pull(pop2021)
}

king_county_pop <- pop("King")
pierce_county_pop <- pop("Pierce")
snohomish_county_pop <- pop("Snohomish")
spokane_county_pop <- pop("Spokane")
clark_county_pop <- pop("Clark")



#
unemployment_claims_with_date <- wa_unemployment_insurance_claims_df %>% 
  rename("2020-1-1" = "X1") %>% 
  rename("2020-2-1" = "X2") %>% 
  rename("2020-3-1" = "X3") %>% 
  rename("2020-4-1" = "X4") %>% 
  rename("2020-5-1" = "X5") %>% 
  rename("2020-6-1" = "X6") %>% 
  rename("2020-7-1" = "X7") %>% 
  rename("2020-8-1" = "X8") %>% 
  rename("2020-9-1" = "X9") %>% 
  rename("2020-10-1" = "X10") %>% 
  rename("2020-11-1" = "X11") %>% 
  rename("2020-12-1" = "X12") %>% 
  pivot_longer(!County, 
               names_to = "Date",
               values_to = "Claims"
  ) %>% 
  mutate(
    month = months(as.Date(Date))
  ) %>% 
  mutate(
    month_and_county = paste(County, month)
  )

cases_unemployment_df <- right_join(county_covid_cases_by_month, unemployment_claims_with_date, by = c("month_and_county"))

cases_unemployment_pop_df <- left_join(cases_unemployment_df, dup_county_pop_with_county_df, by = "County.x")

# Question 1
#Do COVID-19 infection rates impact the rate of unemployment in King, Snohomish, Pierce, Clark and Spokane counties?

q_covid_infection_rate_df <- cases_unemployment_pop_df %>% 
  mutate(
    covid_rate = cases/pop2021,
    unemployment_rate = Claims/pop2021
  )

###What kind of qualitative variable should I make here?
highest_covid_infection_rate_unemployment <- q_covid_infection_rate_df %>% 
  filter(covid_rate == max(covid_rate)) %>% 
  pull(unemployment_rate)

highest_covid_infection_rate_County <-
  q_covid_infection_rate_df %>% 
  filter(covid_rate == max(covid_rate)) %>% 
  pull(County.x)

####

covid_unemployment_rate_v_covid_rate <- ggplot(data = q_covid_infection_rate_df)+
  geom_point(mapping = aes(x = unemployment_rate, y = covid_rate, color = County.x))+
  labs(
    title = "Unemployment Rate v. Covid Rate by County",
    x = "Unemployment Rate",
    y = "Covid Infection Rate",
    color = "County"
  )


covid_unemployment_rate_v_covid_rate
# Question 2
#Does a lower rate of unemployment lead to a higher number of COVID cases?
#### Qualatative
highest_covid_cases_unemployment <- q_covid_infection_rate_df %>% 
  filter(cases == max(cases)) %>% 
  pull(unemployment_rate)

highest_covid_cases_county <- q_covid_infection_rate_df %>% 
  filter(cases == max(cases)) %>% 
  pull(County.x)
  


#### Quantitative
covid_unemployment_rate_v_cases <- ggplot(data = q_covid_infection_rate_df)+
  geom_point(mapping = aes(x = unemployment_rate, y = cases, color = County.x))+
  labs(
    title = "Unemployment Rate v. Number of Covid Cases",
    x = "Unemployment Rate",
    y = "Covid Cases",
    color = "County"
  )

covid_unemployment_rate_v_cases

