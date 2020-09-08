# global ------------------------------------------------------------------
options(scipen = 123) 

library(shiny)
library(shinydashboard)
library(dplyr)
library(glue)
library(plotly)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)

corona <- read_csv("covid_19_clean_complete (21)1.csv")
states <- geojsonio::geojson_read("custom.geo.json", what = "sp")

corona$Date <- mdy(corona$Date)

corona1 <- corona %>% 
  select(-`Province/State`) %>% 
  mutate(country = `Country/Region`,
         country = case_when(country =="Mainland China" ~ "China",
                             T ~ country),
         area = ifelse(country == "China", "China", "Not China"),
         area = as.factor(area))

corona1 <- corona1 %>% 
  mutate(country = case_when(country == "Czech Republic" ~ "Czech Rep.",
                             country == "Dominican Republic" ~ "Dominican Rep.",
                             country == "South Korea" ~ "Korea",
                             country == "United Arab Emirates" ~ "United Arab Emirate",
                             country == "UK" ~ "United Kingdom",
                             country == "US" ~ "United States",
                             T ~ country),
         name = as.factor(country),
         country = as.factor(country))

corona2 <- corona1 %>% 
  group_by(name) %>% 
  summarise(total_confirmed = sum(Confirmed),
            total_death = sum(Deaths),
            total_recovered = sum(Recovered)) %>% 
  ungroup() 

states@data <- left_join(x = states@data, y = corona2, by = "name")

bins <- c(1, 10, 100, 1000, 10000, Inf)
pal <- colorBin("YlOrRd", domain = states$total_confirmed, bins = bins)


# ui ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "menu1", 
               text = "Total COVID-19 Case"),
      menuItem(tabName = "menu2",
               text = "Ratio of COVID-19"),
      menuItem(tabName = "menu3",
               text = "Map of COVID-19 Spreading")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu1",
              fluidRow(
                column(width = 4,
                       selectInput(inputId = "area",
                                   label = "Choose area",
                                   choices = levels(corona1$area),
                                   multiple = F)),
                column(width = 5,
                       "The picture shows the development of the corona virus from 22 January 2020 to 15 March 2020. The trend shows that corona sufferers are increasing from time to time, but the number of victims who died is far less than the total sufferers.
If you see an increase in the number of people affected by corona in China has decreased over time. The number of victims affected by corona outside of China has also increased, although the number of people recovering has also increased over time. ")),
              fluidRow(width=12,
                       plotlyOutput(outputId = "plot1"))),
      
      tabItem(tabName = "menu2",
                fluidRow(
                  column(width = 4,
                         selectInput(inputId = "country",
                                     label = "Choose country",
                                     choices = levels(corona1$country),
                                     multiple = F,
                                     selected = "China")),
                  column(width = 5,
                         "The following figure depicts the comparison between heal ratio and death ratio of COVID-19 by country. The data shows that heal ratio of COVID-19 is higher than the death ratio.")),
                fluidRow(width=12,
                         plotlyOutput(outputId = "plot2"))),
      
      tabItem(tabName = "menu3",
              fluidRow(
                column(width = 4,
                       sliderInput(inputId = "Date",
                                   label = "Select Date",
                                   min = as.Date("2020-01-22","%Y-%m-%d"),
                                   max = as.Date("2020-03-21","%Y-%m-%d"),
                                   value = as.Date("2020-03-21")
                       )),
                column(width = 3,
                       valueBoxOutput(outputId = "confirmed",
                                      width = NULL
                        )),
                column(width = 5,
                       "The map shows the spreading of COVID-19. It shows that China with the highest of Corona case, and it spreads more to almost all country in the world. However, Corona penetration in Africa contingent is rarely found")
              ),
              fluidRow(width=12,
                       leafletOutput(outputId = "m"))
           
      ))))
  
# server ------------------------------------------------------------------



server <- function(input, output, session) {

  output$plot1 <- renderPlotly({
    corona11 <- corona1 %>% 
      filter(area %in% c(input$area)) %>% 
      group_by(Date) %>% 
      summarise(total_confirmed = sum(Confirmed),
                total_death = sum(Deaths),
                total_recovered = sum(Recovered)) %>% 
      ungroup() 
    
    corona11_long <- pivot_longer(data = corona11, cols = -Date) %>% 
      mutate(case = name,
             text = glue(
               "Type Case: {case}
     Total Case = {value}"
             ))
    
    
    plot1 <- ggplot(data = corona11_long, aes(x = Date, y = value)) +
      geom_line(aes(color = case), ) +
      geom_point(aes(color = case, text = text, show.legend= T)) +
      theme()+
      labs(title = "Corona Case February- March 2020",
           x = "Date",
           y = "Total Case")
    
    ggplotly(plot1, tooltip = "text")
    
  })
  
  output$plot2 <- renderPlotly({
    corona3 <- corona1 %>%
      filter(country %in% c(input$country)) %>% 
      group_by(country,Date) %>% 
      summarise(total_confirmed= sum(Confirmed),
                total_death = sum(Deaths),
                total_recovered = sum(Recovered)) %>% 
      ungroup %>% 
      filter(Date == "2020-03-21") %>% 
      mutate(death_ratio = total_death/total_confirmed,
             heal_ratio = total_recovered/total_confirmed) %>% 
      select(Date, heal_ratio, death_ratio)
    
    corona3_long <- pivot_longer(data = corona3, cols = -Date) %>%
      mutate(ratio = name,
             text = glue(
               "Type Case: {ratio}
     Total Case = {value}"
             ))
    
    
    plot2 <- ggplot(data = corona3_long, aes(x = Date, y = value, text=text)) +
      geom_col(aes(fill= ratio), show.legend = T)+
      coord_flip()+
      theme()+
       labs(title = "Death ratio vs Heal Ratio of COVID-19",
           subtitle = "Ratio by Country",
           x = "Date",
           y = "Case_ratio")
    
    ggplotly(plot2, tooltip = "text")
    
  })

  
  output$confirmed <- renderValueBox({
    agg1 <- corona11 %>% 
      filter(Date == input$Date)
    
    valueBox(value = agg1$total_confirmed,
             caption = "Total Corona")
  })
  
  output$m <- renderLeaflet({
    m <- leaflet(states) %>% 
      addTiles() %>% 
      setView(0,0,1.5) %>% 
      addPolygons(
        fillColor = ~pal(total_confirmed),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7) %>% 
      addLegend(pal = pal, values = ~total_confirmed, opacity = 0.7, title = "Total Corona Confirmed",
                position = "bottomright")
    
    
  })
}

shinyApp(ui, server)

