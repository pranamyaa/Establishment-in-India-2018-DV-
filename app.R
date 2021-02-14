library(readxl)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(shinythemes)
library(plotly)

#importing data into R
data <- read_excel("datafile1.xlsx")
data <- data %>%select(c (`State/UT`,`Rural Own Account Enterprises`,`Rural Establishment`,`Urban Own Account Enterprises`,`Urban Establishment`,`All Rural + All Urban`))
data<-  data %>%rename(
                        State = `State/UT`,
                        Rural_Enterprises = `Rural Own Account Enterprises`,
                        Rural_Establishment = `Rural Establishment`,
                        Urban_Enterprises = `Urban Own Account Enterprises`,
                        Urban_Establishment = `Urban Establishment`,
                        Total = `All Rural + All Urban`,
                      )
states <- data$State



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
        dashboardHeader(title = "Establishment/Enterprises in India by States and Union Territories in year 2018",
                        titleWidth = 900),
        
        dashboardSidebar(disable = TRUE),
        
        dashboardBody(
          fluidRow(
             box(
                  hr(),
                  p("This dashboard shows the industrial establishments and enterprises in India by its States and Union Territories."),
                  p("The dashboard consists of 2 main graphs."),
                  p("The first graph shows the comparison of establishment and enterprises counts between 2 selected states."),
                  p("The second graph shows Top 5 and Least 5 states/UTs in India according to the establishment/enterprise count of the selected type."),
                  p("Establishment/Enterprise are devided into 5 groups : Rural Establishment, Rural Enterprises, Urban Establishment, Urban Enterprises and total count."),
                  p("We can find out the states in India, where more industrial establishment took place in year 2018."),
                  hr(),
                  p("This data is sourced from :",
                  a("Indian government data", href = "https://data.gov.in/catalog/estimated-number-enterprises-different-statesuts")),                    width = 12
            )
          ),
          fluidRow(
            box(
                  title = "Comparison between the states.",solidHeader = TRUE, status = "primary",
                  helpText("Please Select two states from below for comparison."),
                  hr(),
                  selectInput("State1", "First State: ", choices = states, selected = "Maharashtra"),
                  selectInput("State2", "Second State: ", choices = states, selected = "Kerala"),
                  width = 4
            ),
            box(
                  title = "State comparison Bar Graph", solidHeader = TRUE, status = "primary",
                  plotlyOutput("Barplot"),
                  width = 8
            )
          ),
          
          fluidRow(
            box(
                  title = "campare top 5 and least 5 states.", solidHeader = TRUE, status = "primary",
                  helpText("The graph shows Top 5 or Least 5 states in India according to the Establishment type selected below"),
                  hr(),
                  
                  radioButtons("TP", "Graph type:",
                               c("TOP 5" = "TF",
                                 "LEAST 5" = "LF",
                                 selected = NULL )),
                  
                  radioButtons("EST", "Establishment type:",
                               c("Rural Establishment" = "RE",
                                 "Urban Establishment" = "UE",
                                 "Rural Enterprises" = "RN",
                                 "Urban Enterprises" = "UN",
                                 "Total" = "total",
                                 selected = NULL )),
                  width = 4
            ),
            box(
                  title = "Bar Graph top 5 or least 5 States.", solidHeader = TRUE,status = "primary",
                  plotlyOutput("Barplott"),
                  width = 8
            )
          )
        )
      )




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # get the reactive data to use in the graphs.
    CurrentData <- reactiveValues()
    observe( {
          CurrentData$x <- data %>% filter(State %in% c(input$State1, input$State2))%>% gather(key = "types", value = "count", 2:6)
          b <- switch(input$EST,
                       RE = data %>% select(c(State, Rural_Establishment)),
                       UE = data %>% select(c(State, Urban_Establishment)),
                       RN = data %>% select(c(State, Rural_Enterprises)),
                       UN = data %>% select(c(State, Urban_Enterprises)),
                       total = data %>% select(c(State, Total))
                     )
          b <- b[-36,]
          sort_data <- switch(input$TP,
                              TF = b[order(-b[,2]),],
                              LF = b[order(b[,2]),]
                              ) 
          names(sort_data)[2]<- "counts"
          CurrentData$a <- sort_data
          })
    
    # Plot a grouped bar graph to compare the establishment of seleccted states.
    output$Barplot <- renderPlotly({
      plot_ly(data = CurrentData$x,
              x = ~ types,
              y= ~ count,
              type = "bar",
              color = ~State,
              colors = c("#fec44f", "#7fcdbb")
              ) %>%
              layout(
                    yaxis = list(zeroline = FALSE, title = "Count"),
                   xaxis = list(zeroline = FALSE, title = "Types", categoryorder = "array", categoryarray = ~types)
              )
    })
    
    
    #Plot a bar graph to compare top 5 or least 5 states by establishment types.
    output$Barplott <- renderPlotly({
      
        
        plot_ly(data = CurrentData$a[1:5,],
                x = ~ State,
                y = ~ counts,
                type = "bar",
                color = I("#7fcdbb")
        ) %>%
        layout(
          yaxis = list(zeroline = FALSE, title = " Establishment Count"),
          xaxis = list(zeroline = FALSE, title = "States", categoryorder = "array", categoryarray = ~counts)
                )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
