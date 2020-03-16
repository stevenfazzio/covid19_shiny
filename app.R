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
library(lubridate)
library(scales)

all_data <- read_csv('data/processed_data.csv')

regions <- all_data %>% 
    filter(
        statistic == 'confirmed',
        date == ymd('2020-03-15')
    ) %>% 
    group_by(region) %>% 
    summarise(num_people = sum(num_people)) %>% 
    arrange(desc(num_people)) %>% 
    select(region) %>% 
    distinct() %>% 
    pull()

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
        
        # Application title
        titlePanel("COVID-19 Forecasting"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            
            sidebarPanel(
                dateRangeInput("date_range",
                               "Date Range",
                               start = ymd('2020-01-15'),
                               end = today() + months(1)),
                selectInput("region",
                            "Region",
                            choices = regions,
                            selected = 'Non-China (International)'),
                checkboxGroupInput("statistics",
                                   "Statistics",
                                   choices = c('confirmed', 'active', 
                                               'recovered', 'deaths'),
                                   selected = 'confirmed'),
                numericInput('threshold',
                             'Volume Threshold',
                             value = 10,
                             min = 1,
                             step = 1)
                
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("forecastPlot")
            )
        ),
        bookmarkButton()
    )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    plot_data <- reactive({
        actual_data <- all_data %>% 
            filter(
                region == input$region,
                num_people > 0
            ) %>% 
            mutate(type = 'actual') %>% 
            select(type, date, statistic, num_people)
        
        stat_model <- function(df) {
            lm(log(num_people) ~ date, data = df)
        }
        
        date_range <- seq(input$date_range[[1]], input$date_range[[2]], by = '1 day')
        
        predicted_data <- actual_data %>% 
            filter(num_people > input$threshold) %>% 
            group_by(statistic) %>% 
            nest() %>% 
            mutate(
                model = map(data, stat_model),
                date = list(tibble(date = date_range)),
                log_num_people = map2(model, date, ~predict(.x, newdata = .y))
            ) %>% 
            unnest(c(date, log_num_people)) %>%
            mutate(
                type = 'predicted',
                num_people = exp(log_num_people)
            ) %>%
            select(type, date, statistic, num_people)
        
        df <- bind_rows(
            actual_data,
            predicted_data
        ) %>% 
            filter(statistic %in% input$statistics) 
    })

    output$forecastPlot <- renderPlot({
        
        num_breaks <- ceiling(log10(max(plot_data()$num_people))) - 
            floor(log10(min(plot_data()$num_people))) + 1
        
        plot_data() %>% 
            ggplot(aes(x = date, y = num_people, color = statistic)) +
            geom_line(aes(linetype = type)) +
            scale_y_log10(label = label_comma(accuracy = 1), breaks = breaks_log(num_breaks)) +
            scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
            ggtitle(input$region)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
