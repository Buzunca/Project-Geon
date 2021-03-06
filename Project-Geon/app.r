library("shiny")
library("ggplot2")
library("dplyr")


source("questions1-2.R")
source("analysis.r")


#Data Wrangling

mean_covid_cases <- mean(covid_df$ConfirmedCases)
sum_covid_cases <- sum(covid_df$ConfirmedCases)

covid_df <- q_covid_infection_rate_df %>%
  select(cases, covid_rate, unemployment_rate, County.x)


#UI
covid_county_selection_1 <- selectInput(
  inputId = "county_selection_1",
  label = "Which County?",
  choices = unique(covid_df$County.x)
)
  
trend_line_selection_1 <- checkboxInput(
  inputId = "trendline_choice_1",
  label = "Show Trendline"  
)

covid_county_selection_2 <- selectInput(
  inputId = "county_selection_2",
  label = "Which County?",
  choices = unique(covid_df$County.x)
)

trend_line_selection_2 <- checkboxInput(
  inputId = "trendline_choice_2",
  label = "Show Trendline"  
)

month_choices <- unique(unemployment_vs_covid_df$Month)

month_selection_q3 <- selectInput(
  inputId = "q3_month",
  label = "Which Month?",
  choices = month_choices
)

month_selection_q4 <- selectInput(
  inputId = "q4_month",
  label = "Which Month?",
  choices = month_choices
)


my_ui <- fluidPage(
  
  titlePanel("Covid To Unemployment Analysis"),
  tabsetPanel(
    tabPanel( "Data Introduction",
      tags$h2("Problem Domain"),
      p("Our project is about unemployment rates in King, Snohomish, Pierce, Clark, and Spokane counties in Washington compared to their respective coronavirus infection rates by month in 2020.The coronavirus is the most dangerous virus in 2020 and it affects not only people's lives but also the economy. People who work in entertainment industry (restaurants, cinemas, etc.) may lose their jobs because of the quarantine that prevents people from going outdoors. Higher infection rates may cause stricter quarantine policies and will cause higher unemployment rates as a result. We will analyze if there is actually a connection between the unemployment rates and the infection rates in the community."),
      p("We analyze unemployment rates with respective to infection rates because unemployment does not only affect those who have jobs, it also affects those people who are searching for jobs and the community's economy. The unemployment can cause a chain reaction that makes more people lose jobs and shortens up the service provided in the community, which as a result affect people's lives in a negative way. If we can find out a positive linear connection between unemployment rates and infection rates, we may be able to control unemployment rate by reducing infection rates through better practical policies."),
      p("This site gave us a massive datatset including statistics on COVID-19 infections, infections rates, death, etc. throughout Washington State during the year 2020."),
      tags$a(href = "https://www.doh.wa.gov/Emergencies/COVID19/DataDashboard",
             "WA COVID-19 Data Dashboard"),
      p("This site provided valuable statistics on Washington State's monthly unemployment insurance claims throughout 2020."),
      tags$a(href = "https://esd.wa.gov/labormarketinfo/unemployment-insurance-data",
             "WA Labor Market Info"),
      p("Finally, this is our data report published on GitHub Pages."),
      tags$a(href = "https://info201b-wi21.github.io/project-geon0514/",
             "INFO 201 Data Report"),
      p("This app was created by the genuises in group H6: Geon Kim, Brandon Hoang, Fanfei Zhang, Berk Uzuncaova. Hope you enjoy perusing our work and thanks for an amazing quarter through these difficult times!")
    ),
    tabPanel(
      #Section 2
      "Data Representation",
      h3("What is the data?"),
      p("The infection rate or volume and unemployment claims for the chosen 5 counties (King, Pierce, Spokane, Clark, Snohomish) from Washington State."),
      h3("Sample of the Data"),
      tableOutput(outputId = "table_covid_df"),
      tableOutput(outputId = "table_unemployment_df"),
      h3("What are the Columns"),
      tags$ul(
        tags$li("(Note: all labels are in raw formatting)"),
        tags$li("cases - New COVID-19 in collection period"),
        tags$li("covid_rate - COVID-19 rate of increase in collection period"),
        tags$li("unemployment_rate - Unemployment rate of increase in collection period"),
        tags$li("County.x - Name of the county"),
        tags$li("Month - The collection period"),
        tags$li("Unemployment_Claims - Total number of unemployment claims in collection period")
        
      ),
      h2("Summary Analysis"),
      h3("Summary of Data"),
      p(paste0("In the first data set highlighting COVID infections, the total number of Confirmed Covid Cases throughout Washington State is ", sum_covid_cases, ". With the average for one single week being ", format(mean_covid_cases, digits=4), ". Furthermore, as most of the data is complete or is filled with a 0, a simple replacement of NA values with 0 cleans up the data.")),
      h3("Example Plots"),
      plotOutput(outputId = "pieplot"),
      plotOutput(outputId = "covid_line"),
      plotOutput(outputId = "box_plot"),
      h3("Notable Outliers"),
      p(paste0("Most of the collected data is quite clean and therefore don't have many outliers, especially for the unemployment dataframe. However, it can be noted that the county with the highest reported covid cases is " ,max_covid_county, " as also evident in the Distribution of COVID Cases Box Plot. Furthermore, the maximum for unemployment in WA is ", max_unemployment[[1]]," in the month of ",max_unemployment[[2]]," at a volume of ",max_unemployment[[3]]," the lowest being ",min_unemployment[[1]]," in the month of ",min_unemployment[[2]], " at a volume of ", min_unemployment[[3]], ".")),
      #Question 1
      h3("Do COVID-19 infection rates impact the rate of unemployment in King, Snohomish, Pierce, Clark and Spokane counties?"),
      p("1.We have decided to use population data, covid case data, and unemployment claim data. Using this we could determine the infection rates in each county by dividing the number of confirmed cases by the population number in 2021. Then we were able to find the rate of unemployment similarly by dividing the number of unemployment claims by the population."),
      p("2.My data analysis method started with a lot of data wrangling. Because my original data was very different to be able to join them together. I had to do things like get rid of ???County??? at the end of each County for a few datasets, I also had to try to join a data set together by two columns, but because I couldn???t figure out how to do that I created an extra column with both of those values in each dataset and then joined using that column. To calculate my infection rate, and unemployment rate I mutated the data and then divided the number of cases or claims by the population in each county in 2021. This left we with a data frame of all the information I needed to start creating my plots."),
      p("3."),
      plotOutput(outputId = "q1"),
      p(paste("The unemployment rate with highest covid infection rate is", highest_covid_infection_rate_unemployment, "and the county with highest infection rate is", highest_covid_infection_rate_County, ".")),
      p("4.The analysis shows the result that Covid-19 infection rate is not impacting the rate of unemployment in the 5 counties in washington state.If the impact exists  between infection rates and rate of unemployment,there should be a positive or negative linear relationship.This means that the plot will show 5 lines going up or down if the impact exists.However,we can see that most points are clustering around the rectangle formed by covid  infection rate=0.005 and unemployment rate=0.02.If we treat this rectangle as initial condition that the covid has just started and not much restrictions are made on jobs,the trend of other data should be heading for one direction instead of two as shown in the plot.For some of the data outside the rectangle,the unemployment rate remains around 0.01 when the infection rate is going up.For other data outside the rectangle,the infection rate remains around 0.002 when the unemployment rate is going up.This suggests that there is no actual connection between infection rates and the rate of unemployment.I think the main reason is that when the covid first happens,the infection rate is increasing rapidly.However,the government is not taking serious measures against it and most employees are still working in the companies.However,when problem is getting more serious and government has taken actions,the companies are shut down to cut expenditure and people lose their jobs and the unemployment rate rises.Under the restrictions,the infection rate is controlled.However,the market is still down and companies are not making enough money to hire new employees.The companies are still preventing more losses from the covid-19,which causes more people to lose jobs and make it harder for graduates to find jobs.This causes the unemployment rate to rise.I think this situation can only be changed when the covid-19 is cured and the world's economy is back onto its track."),
      h3("Does a lower rate of unemployment lead to higher COVID cases in King, Snohomish, Pierce, Clark and Spokane counties?"),
      #Question 2
      p("1.Similar to the first question, except the change is whether or not it leads to a higher number of actual cases rather than infection rates. So ???cases??? in this case were confirmed covid cases in each county."),
      p("2.My data analysis method actually used the same data as the first one. But I used the amount of Covid cases to compare my results. So when I called the ggplot function instead of using covid_rate I would call cases instead. This would allow me to compare a rate to a number rather than two rates together. Although the graphs were similar testing both hypotheses allowed for different implications based on the outcome."),
      p("3."),
      plotOutput(outputId = "q2"),
      p("The unemployment rate with highest covid case is", highest_covid_cases_unemployment, "and the county with highest covid cases is", highest_covid_cases_county,"."),
      p("4.I think a lower rate of unemployment does not lead to higher covid cases in the 5 counties of washington state.This plot is pretty similar to question 1 because covid cases leads to infection rates.For example,if we assume the cases we test in a county is x,then covid cases is equal to infection rate mutilply x.Then the plot we actually get is just multiply y-axis by 1/infetion rate.We can see from the plot that the covid cases is reaming around 2000 when the unemployment rate is going down.I think the covid is what causes the lower unemployment rate.However,the unemployment is not happening as soon as the covid comes out.It go through a process along with government restrictions and citizen's awareness of the virus."),
      h3("How has the total number of unemployment claims corresponded with the number of COVID-19 cases in Washington?"),
      #Question 3
      p("1. This question is looking at if the data can show us a distinct relationship between the number of cases and claims per month. The data will be taken from the 5 most populous counties in Washington (Clark, King, Pierce, Snohomish, and Spokane)."),
      p("2. My analysis method was oriented around bringing the data to the same scale, monthly, and then finding the monthly total for both categories. I started by adjusting the data sets and adding a Month column, I then group by Month and summed up the totals for each county every month. Then, I created a scatterplot comparing the two dataframes of the unemployment and covid-19 data."),
      p("3"),
      plotOutput(outputId = "q3"),
      p("There were four points in time I thought to be significant, the beginning, cases peak, claims peak, and end. In January, there were 21046 claims and 24992 cases, which are very similar. The claims peak, April, had 722037 claims but only 5035 cases, which is a huge disparity. Washington's cases peaked in July, where there were 73330 claims and 11659 cases, not too close but a higher ratio than in April. Finally, in December, claims were at 42601 and cases were at 39579. "),
      p("4. My initial hypothesis was that cases would have a strong correlation with claims, but I found that not to be true, obvious in the large disparity throughout the middle of the year. However, in the early and late months of the year, the ratio of cases to claims is high, 0.93 in December and 1.19 in January. This led me to the hypothesis that this is because as COVID-19 first hit, we lacked preparation and as a result, cases were able to hit at full force. This lead to jobs shutting down, but the infection rate was maximized. As the year went on, jobs remained closed and more shut down, increasing the claim count to a maximum of 722037 claims in April. However, with jobs and schools being shut down, the infection rate was mitigated, making it so cases did not increase at the same rate as the unemployment claims. Then as time passed, jobs opened up and restrictions loosened, letting people get employment, decreasing the claims back down in Decemeber, however this led to a similar context as in the beginning of the year, where cases matched claims because of loose restrictions. As we entered 2021, restrictions came back and so I would expect the same pattern to arise from it."),
      h3("How does the COVID-19 infection rate compare across counties in WA?"),
      #Question 4
      p("1. This question is about comparing the infection rates across 5 counties in Washington (Clark, King, Pierce, Snohomish, and Spokane). The two features to notice are total amount of cases and the trends of each county, whether they are increasing or decreasing over certain periods of time."),
      p("2. My data analysis method was to get the data for each county and sum up the data so that there were totals for each month for every county. I then plotted this in a scatter plot. This format seems messy at first but I think it is a great way to see all of the data from a raw standpoint."),
      plotOutput(outputId = "q4"),
      p("A table displaying quarterly case data on 2 of the counties. King being most populated and Clark being one of the least populated."),
      tableOutput(outputId = "table"),
      p("4. Looking at the data, one thing that jumps out is the variation between counties. King County is significantly higher than any county, reaching a peak in November, being over 2.5x the closest monthly total in cases throughout the entire year. The data is oriented around two peaks, at the beginning of the year and at the end, with a dip in the middle. I think this is related to how the government handled the pandemic, with it first catching us off guard, then shutting down, and then trying to reopen and causing another spike in infections. The biggest correlation among the data is how population compares to infections. While no county is significantly less in population, King County is by far the most populated and that is reflected in how many more cases it has consistently over the course of 2020.")
    ),
    tabPanel("Question 1",
             sidebarLayout(
               sidebarPanel(
                 h2("Choices"),
                 covid_county_selection_1,
                 trend_line_selection_1
               ),
               mainPanel(
                 fluidRow(plotOutput("covid_rate_graph"), 
                          textOutput("covid_rate_text"))
               )
             )
    ),
    tabPanel("Question 2",
             sidebarLayout(
               sidebarPanel(
                 h2("Choices"),
                 covid_county_selection_2,
                 trend_line_selection_2
               ),
               mainPanel(fluidRow(plotOutput("covid_case_graph"), 
                                             textOutput("covid_case_text")))
             )
    ),
    tabPanel("Question 3",
             sidebarLayout(
               sidebarPanel(
                 h2("Choices"),
                 month_selection_q3
               ),
               mainPanel(fluidRow(plotOutput("unemployment_vs_covid_plot"), 
                                             textOutput("unemployment_vs_covid_text")))
             )
    ),
    tabPanel("Question 4",
             sidebarLayout(
               sidebarPanel(
                 h2("Choices"),
                 month_selection_q4
               ),
               mainPanel(fluidRow(plotOutput("covid_wa_plot"), 
                                  textOutput("covid_wa_text")))
             )
    )
  )
)

#Server

my_server <- function(input, output){
  
  ##Data Representation
  output$table_covid_df <- renderTable(head(covid_df))
  output$table_unemployment_df <- renderTable(head(unemployment_df))
  output$pieplot <- renderPlot(sum_unemployment_circle)
  output$covid_line <- renderPlot(line_covid_plt)
  output$box_plot <- renderPlot(covid_box_plt)
  output$q1 <- renderPlot(covid_unemployment_rate_v_covid_rate)
  output$q2 <- renderPlot(covid_unemployment_rate_v_cases)
  output$q3 <- renderPlot(unemployment_vs_covid_plot)
  output$q4 <- renderPlot(covid_monthly_counties)
  
  ##Question 1
  output$covid_rate_graph <- renderPlot({
    
    clean_rate_df <- covid_df %>%
      filter(County.x == input$county_selection_1) %>%
      select(covid_rate, unemployment_rate)
    
    clean_rate_plot <- ggplot(data = clean_rate_df, aes(x = unemployment_rate, y = covid_rate)) +
      {if(input$trendline_choice_1)geom_smooth(method = "lm")}+
      geom_point(color = "darkred") +
      labs(title = "Unemployment Rate v. Covid Rate By County", x = "Unemployment Rate", y = "Covid Infection Rate")
           
    plot(clean_rate_plot)
  })
  
  output$covid_rate_text <- renderText({
    
    message <- paste0("This plot displays the Covid rate vs unemployment rate for ", input$county_selection_1, " County.")
    return(message)
    
  })
  
  ##Question 2
  output$covid_case_graph <- renderPlot({
    
    clean_case_df <- covid_df %>%
      filter(County.x == input$county_selection_2) %>%
      select(cases, unemployment_rate)
    
    clean_case_plot <- ggplot(data = clean_case_df, aes(x = unemployment_rate, y = cases)) +
      {if(input$trendline_choice_2)geom_smooth(method = "lm")}+
      geom_point(color = "darkred") +
      labs(title = "Unemployment Rate v. Number of Covid Cases", x = "Unemployment Rate", y = "Covid Cases")
    
    plot(clean_case_plot)
  })
  
  output$covid_case_text <- renderText({
    
    message <- paste0("This plot displays the covid cases vs unemployment rate for ", input$county_selection_2, " County.")
    return(message)
    
  })
  
  ##Question 3
  output$unemployment_vs_covid_plot <- renderPlot({
    
    clean_unemp_covid_plot_df <- unemployment_vs_covid_df %>% 
        filter(Month==input$q3_month)
    
    p <- ggplot(clean_unemp_covid_plot_df) +
      geom_col(
        mapping = aes(x=category, y=value, fill=category)
      ) +
      labs(
        title = "COVID-19 Cases Compared With Unemployment Claims in WA",
        y = "Number of Cases/Claims",
        x = "Month"
      ) +
      scale_color_brewer(palette = "Pastel1")
    
    plot(p)
  })
  
  output$unemployment_vs_covid_text <- renderText({
    msg <- paste0(input$q3_month, ". in ")
    
    message <- paste0("This plot displays the covid cases
                      vs unemployment claims for all of WA State during ",
                      msg, "2020. Keep in mind the scaling between unemployment and covid cases.")
    return(message)
    
  })
  
  ##Question 4
  output$covid_wa_plot <- renderPlot({
    
    clean_covid_wa_plot_df <- covid_sum_months %>% 
      filter(Month==input$q4_month)
    
    p <- ggplot(clean_covid_wa_plot_df) +
      geom_col(
        mapping = aes(x=County, y=Cases, fill=County)
      ) +
      labs(
        title = "COVID-19 Cases in WA",
        y = "Number of Cases",
        x = "County"
      ) +
      scale_color_brewer(palette = "Pastel1")
    
    plot(p)
  })
  
  output$covid_wa_text <- renderText({
    msg <- paste0(input$q4_month, ". in ")
    
    message <- paste0("This plot displays the covid cases
                      in the top 5 most populous counties of WA State during ",
                      msg, "2020.")
    return(message)
    
  })
}


#Run Code


shinyApp(ui = my_ui, server = my_server)



