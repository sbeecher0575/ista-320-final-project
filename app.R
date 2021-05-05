#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(usmap)
library(scales)


household_income <- read_csv('data/income_by_race_1967-2019.csv')

household_income <- household_income %>%
    clean_names() %>%
    rename(year = race_and_hispanic_origin_of_householder_and_year) %>%
    filter(!(race %in% c('BLACK ALONE OR IN COMBINATION', 'ASIAN ALONE OR IN COMBINATION', 'WHITE', 'WHITE ALONE'))) %>%
    mutate(year = parse_number(year),
           number_thousands = parse_number(number_thousands),
           mean_income_estimate = parse_number(mean_income_estimate),
           mean_margin_of_error1 = parse_number(mean_margin_of_error1),
           race = ifelse(race == 'WHITE ALONE, NOT HISPANIC', 'WHITE, NOT HISPANIC', race),
           race = ifelse(race == 'BLACK ALONE', 'BLACK', race),
           race = ifelse(race == 'ASIAN ALONE', 'ASIAN AND PACIFIC ISLANDER', race),
           )
    
races1 <- unique(household_income[,'race'])
income_choices <- c('Median', 'Mean')

income_distr <- household_income %>%
    select(-mean_income_estimate, 
           -mean_margin_of_error1, 
           -median_income_estimate_dollars, 
           -median_margin_of_error1, 
           -total_percent) %>%
    pivot_longer(cols = colnames(household_income)[4:12], 
                 names_to = 'distribution', 
                 names_prefix = 'percent_',
                 values_to = 'percent') %>%
    mutate(distribution = gsub('_to_', '-', distribution),
           distribution = gsub('under_', 'less than ', distribution),
           distribution = gsub('_and_over', ' or more', distribution),
           distribution = gsub('_', ',', distribution),
           distribution = factor(distribution, levels = c('less than 15,000', 
                                                          '15,000-24,999', 
                                                          '25,000-34,999',
                                                          '35,000-49,999',
                                                          '50,000-74,999',
                                                          '75,000-99,999',
                                                          '100,000-149,999',
                                                          '150,000-199,999',
                                                          '200,000 or more')),
           number = round(number_thousands * percent / 100)
           )

years <- sort(unique(household_income$year), decreasing = T)

homelessness <- read_csv('data/2019-Point-in-Time-Estimates-by-state.csv')[,c(1,2,3,4,12,13,14,15,16,17,18,19)]

homelessness <- homelessness %>%
    clean_names() %>%
    select(-number_of_co_cs) %>% 
    pivot_longer(cols = c("overall_homeless_non_hispanic_non_latino_2019",                  
                          "overall_homeless_hispanic_latino_2019",                          
                          "overall_homeless_white_2019",                                    
                          "overall_homeless_black_or_african_american_2019",                
                          "overall_homeless_asian_2019",                                    
                          "overall_homeless_american_indian_or_alaska_native_2019",         
                          "overall_homeless_native_hawaiian_or_other_pacific_islander_2019",
                          "overall_homeless_multiple_races_2019"), 
                 names_to = 'race',
                 names_prefix = 'overall_homeless_',
                 values_to = 'number') %>% 
    mutate(race = gsub('_2019|_', ' ', race))

races2 <- data.frame('Race' = c('all', homelessness$race))
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Income and Homelessness in the United States by Race and Ethnicity"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Each of these three tabs shows different information about income and homelessness in the United States. The first shows the change in household income over time, separated by race/ethnicity. The second compares the distributions of household incomes by race/ethnicity. The third visualization is a map that shows the relative percentage of people (per state population) of homeless people for each race/ethnic identity."),
            HTML('State Population, Income, and Poverty data is from the US Census Bureau: <a href="https://www.census.gov/newsroom/press-kits/2019/national-state-estimates.html">2019 National and State Population Estimates </a> and <a href="https://www.census.gov/data/tables/2020/demo/income-poverty/p60-270.html">Income and Poverty in the United States</a>. Homelessness data is from the Annual Homeless Assessment Report to Congress (AHAR) published by the United States Department of Housing and Urban Development: <a href="https://www.hudexchange.info/resource/5948/2019-ahar-part-1-pit-estimates-of-homelessness-in-the-us/">2019 AHAR: Part 1 - PIT Estimates of Homelessness in the U.S.</a>')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Timeline", 
                                 selectInput("metric",
                                             "Choose Household Income metric:",
                                             choices = income_choices),
                                 plotOutput("race_year_plot",
                                            height = '450px')),
                        tabPanel("Income Distribution", 
                                 selectInput("race1",
                                             "Choose Race/Ethnicity combination:",
                                             choices = races1),
                                 selectInput("years",
                                             "Select a Year:",
                                             choices = years),
                                 plotOutput("income_bar",
                                            width = '800px')),
                        tabPanel("Homelessness Map", 
                                 selectInput("race2",
                                             "Choose Race/Ethnicity combination:",
                                             choices = races2),
                                 plotOutput("mapplot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$race_year_plot <- renderPlot({

        household_income %>%
            filter(!(race %in% c('ALL RACES'))) %>%
            ggplot(aes(x = year,
                       if(input$metric == 'Median'){y = median_income_estimate_dollars}
                       else{y = mean_income_estimate},
                       color = race)) +
            geom_line(size = .8) + 
            geom_point() +
            theme(legend.position = "bottom",
                  plot.title = element_text(size = 14)) +
            labs(y = paste(input$metric, 'household income'),
                 x = 'Year',
                 title = 'Household Income in the United States over Time',
                 caption = 'NOTE: Not all races/ethnicities have data the full range 1967-2019') +
            scale_y_continuous(label = comma) +
            scale_x_continuous(breaks = breaks_extended(10))
    })
    
    output$income_bar <- renderPlot({
        
        if(input$race1 == 'ALL RACES'){
            income_distr %>%
                filter(!(race %in% c('ALL RACES'))) %>%
                filter(year == input$years) %>%
                ggplot(aes(x = distribution,
                           y = percent,
                           fill = race, percent)) +
                geom_col() + 
                theme(legend.position = "bottom",
                      plot.title = element_text(size = 14)) +
                labs(title = paste('Percent of Population in different income classes by Race/Ethnicity in', input$years),
                     caption = 'NOTE: This is not a histogram--not all bars represent the same range of household incomes',
                     x = 'Annual Household Income',
                     fill = 'Race/Ethnicity',
                     y = 'Percent')
        }
        else{
            income_distr %>%
                filter(race == input$race1) %>%
                filter(year == input$years) %>%
                ggplot(aes(x = distribution,
                           y = percent)) +
                theme(plot.title = element_text(size = 14)) +
                geom_col() +
                labs(title = paste('Percent of population of', input$race1, 'people in different income classes in', input$years),
                     caption = 'NOTE: This is not a histogram--not all bars represent the same range of household income',
                     x = 'Annual Household Income',
                     y = 'Percent')
        }
    })
    
    output$mapplot <- renderPlot({
        
        if(input$race2 == 'all'){
            plot_data <- homelessness %>%
                mutate(percent = overall_homeless_2019 / total_population * 100)}
        else{
            plot_data <- homelessness %>%
                filter(race == input$race2) %>% 
                mutate(percent = number / total_population * 100)}
            
            plot_data %>%
                plot_usmap(data = .,
                           values = "percent") +
                theme(legend.position = "bottom",
                      plot.title = element_text(size = 14, hjust = 0.5)) +
                scale_fill_continuous(name = "Percent",
                                      low = "gray85",
                                      high = "gray0") +
                labs(title = 'Percent Homeless by State in 2019',
                     caption = 'NOTE: The scales change for each race/ethnicity choice, so colors cannot be compared between maps, only between states in the same map')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
