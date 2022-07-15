library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(maps)

covid_df <- read.csv(file = "data/WA_COVID19_Cases.csv") %>%
  replace(is.na(.),0)
  ##rename(County = ?..County)

unemployment_df <- read.csv(file = "data/washington_unemployment_data.csv") %>%
  rename(Janurary = X1, Feburary = X2, March = X3, April = X4, May = X5, June = X6, July = X7, August = X8, September = X9, October = X10, November = X11, December = X12) %>%
  replace(is.na(.),0)

group_avg_unemployment <- unemployment_df %>%
  pivot_longer(!County, names_to = "Month") %>%
  group_by(Month) %>%
  summarise(Average = mean(value), Sum = sum(value))

total_groups_unemployment <- unemployment_df %>%
  pivot_longer(!County, names_to = "Month") %>%
  group_by(County) %>%
  summarise(Sum = sum(value))

sum_unemployment_circle <- ggplot(total_groups_unemployment, aes(x = "", y = Sum, fill = County)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Total Number of Reported Unemployed in 2020")

clean_covid_line <- covid_df %>%
  select(County, WeekStartDate, ConfirmedCases)

line_covid_plt <- ggplot(clean_covid_line, aes(x = WeekStartDate, y= ConfirmedCases, color = County)) +
  geom_point() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "Number of Confirmed Cases in WA by County", x = "Time (Weeks)", y = "Confirmed Cases")

covid_box_data <- covid_df %>%
  select(County, ConfirmedCases)

covid_box_plt <- ggplot(covid_box_data, aes(x=ConfirmedCases, y=County)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "Distribution of COVID Cases")

max_covid_county <- covid_df %>%
  slice_max(ConfirmedCases) %>%
  select(County)%>%
  unlist(use.names = FALSE)

max_unemployment <- unemployment_df %>%
  pivot_longer(!County, names_to = "Month") %>%
  slice_max(value) %>%
  unlist(use.names = FALSE)

min_unemployment <- unemployment_df %>%
  pivot_longer(!County, names_to = "Month") %>%
  slice_min(value) %>%
  unlist(use.names = FALSE)


### Part 1 ###

unemployment_df <- read_csv("data/washington_unemployment_data_2.csv")
covid_cases_df <- read_csv("data/WA_COVID19_Cases.csv")

options(scipen = 999)

##Standardizing the Month column using the months() function.
unemployment_df <- unemployment_df %>%
  select(-Month) %>%
  mutate(
    Month=months(unemployment_df$Month, abbr=TRUE)
  ) %>%
  select(County, Month, Unemployment_Claims = Unemployment_Claim)


##Standardizing the Month column using months() function.
covid_cases_df  <- mutate(
  covid_cases_df,
  Month=months(covid_cases_df$WeekStartDate, abbr=TRUE)
) %>%
  select(County, Month, Total_Covid_Cases = TotalCases) %>%
  filter(County=="King County" | County=="Pierce County"
         | County=="Snohomish County" | County=="Clark County"
         | County=="Spokane County")


unemployment_monthly_df <- unemployment_df %>%
  group_by(Month) %>%
  summarize(across(where(is.numeric), list(sum=sum)))

covid_cases_monthly_df <- covid_cases_df %>%
  group_by(Month) %>%
  summarize(across(where(is.numeric), list(sum=sum)))


unemployment_vs_covid_df <- left_join(covid_cases_monthly_df, unemployment_monthly_df, by="Month") %>%
  select("Total COVID-19 Cases" = Total_Covid_Cases_sum,
         "Total Unemployment Claims" = Unemployment_Claims_sum, Month) %>%
  pivot_longer(!Month, names_to = "category")


ordered_months<- factor(unemployment_vs_covid_df$Month, ordered=TRUE, level = c("Jan", "Feb", "Mar", "Apr", "May", 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))


unemployment_vs_covid_plot <- ggplot(unemployment_vs_covid_df) +
  geom_point(
    mapping = aes(x=ordered_months, y=value, color=category)
  ) +
  geom_line(
    mapping = aes(x=ordered_months, y=value, color=category)
  ) +
  labs(
    title = "COVID-19 Cases Compared With Unemployment Claims in WA",
    y = "Number of Cases/Claims",
    x = "Month"
  ) +
  scale_color_brewer(palette = "Dark2")

print(unemployment_vs_covid_plot)

### Part 2 ###

covid_cases_df <- read_csv("data/WA_COVID19_Cases.csv")

wa_county_pop_df <- read.csv("data/wa_county_population.csv")%>%
  #rename(CTYNAME = ?..CTYNAME) %>%
  select(County=CTYNAME, population=pop2021)


covid_infection_rate_df <- left_join(covid_cases_df, wa_county_pop_df, by="County") %>%
  select(County, Population = population, TotalCases, Date=WeekStartDate) %>%
  filter(County=="King County" | County=="Pierce County"
         | County=="Snohomish County" | County=="Clark County"
         | County=="Spokane County")

covid_infection_rate_df  <- mutate(
  covid_infection_rate_df,
  Month=months(covid_infection_rate_df$Date, abbr=TRUE)
) %>%
  select(County, Month, TotalCases, Population)

covid_sum_months <- covid_infection_rate_df %>%
  group_by(Month, County) %>%
  summarize(Cases = sum(TotalCases))
#filter(County=="Snohomish County" | County=="Clark County") 

covid_monthly_counties <- ggplot(covid_sum_months) +
  geom_point(
    mapping = aes(x=Month, y=Cases, color=County)
  ) +
  geom_line(
    mapping = aes(x=Month, y=Cases, color=County)
  ) +
  labs(
    title = "COVID-19 cases throughout WA in 2020",
    y = "Number of Cases",
    x = "Month"
  ) +
  scale_color_brewer(palette = "Dark2")


