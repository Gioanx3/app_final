library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(plotly)
library(formattable)
library(readr)

flights<- read.csv("flights.csv",stringsAsFactors = F)
flights<-flights[1:1000,]
data2<-flights%>%select(AIRLINE,AIR_SYSTEM_DELAY,SECURITY_DELAY,AIRLINE_DELAY,LATE_AIRCRAFT_DELAY,WEATHER_DELAY)
data2[is.na(data2)]<-0
data3<-data2%>%
  group_by(AIRLINE)%>%
  summarize(AIR_SYSTEM_DELAY=sum(AIR_SYSTEM_DELAY),
            SECURITY_DELAY=sum(SECURITY_DELAY),
            AIRLINE_DELAY=sum(AIRLINE_DELAY),
            LATE_AIRCRAFT_DELAY=sum(LATE_AIRCRAFT_DELAY),
            WEATHER_DELAY=sum(WEATHER_DELAY))
col<-data3$AIRLINE
data4<-as.data.frame(t(data3[,-1]))
colnames(data4)<-col
data5<- data.frame("Reason"=rownames(data4),data4)
delayReason <- function(x){
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  data6<-data5[,c('Reason',x)]
  library(plotly)
  p<-plot_ly(data6,labels=data6$Reason,values=data6[,x],type = 'pie',
             textposition="inside",
             textinfo="label+percent",
             hoverinfo="text",
             text=paste(data6[,x],"times"),
             insidetextfont=list(color="#FFFFFF"),
             marker=list(colors=c("red","yellow","green","blue","purple"))
  )%>%
    layout(title=paste("What are the reasons for delay in",x,"in 2015 ?"),showlegend=TRUE,xaxis = ax, yaxis = ax)
  return(p)
}

airports <- read.csv("airports.csv", stringsAsFactors = F)
colnames(airports)[1] <- "ORIGIN_AIRPORT"
flights<- read.csv("flights.csv",stringsAsFactors = F, text = raw)
flights<-flights[1:1000,]
flights_airports <- select(flights, ORIGIN_AIRPORT, DEPARTURE_DELAY) 
total_flights_num <- group_by(flights_airports, ORIGIN_AIRPORT) %>% summarise(total_num = n())
delay_flight_num <- filter(flights_airports, DEPARTURE_DELAY > 0) %>% group_by(ORIGIN_AIRPORT) %>% summarise(delay_num = n())
flight_info <- left_join(total_flights_num, delay_flight_num, by = "ORIGIN_AIRPORT")
flight_info <- mutate(flight_info, delay_rate = percent(delay_num / total_num))

flight_airports_info <- left_join(airports, flight_info, by = "ORIGIN_AIRPORT")
mapplot <- function(state){
  data_select <- filter(flight_airports_info, STATE == state)
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  
  x <- plot_geo(data_select, lat = ~LATITUDE, lon = ~LONGITUDE,colors = c("green","red")) %>% 
    add_markers(
      color = ~delay_rate, 
      symbol = I("circle"), 
      size = I(10), 
      hoverinfo = "text",
      text = paste(data_select$AIRPORT, data_select$CITY, data_select$STATE, sep = "<br />")
    ) %>%
    colorbar(title = "Delay Rate") %>%
    layout(
      title = "US Airport Delay Rate", geo = g
    )
  return(x)
}
airport_code <- function(airport_name_origin,airport_name_destination){
  parsed.data.origin <- filter(airports,airports$AIRPORT==airport_name_origin) 
  result.data.origin <- parsed.data.origin$ORIGIN_AIRPORT
  parsed.data.destination <- filter(airports,airports$AIRPORT==airport_name_destination) 
  result.data.destination <- parsed.data.destination$ORIGIN_AIRPORT
  table_data <- flights[which((flights$DEPARTURE_DELAY<=0) & (flights$ARRIVAL_DELAY<=0) &(flights$ORIGIN_AIRPORT==result.data.origin) & (flights$DESTINATION_AIRPORT==result.data.destination)),]
  results <- sort(table(table_data$AIRLINE_NAME),decreasing = TRUE)
  return(results)
}






UI<-(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("ALLFlights",
             
             tabPanel("Airports and Delays",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("state", h3("Choose State"), 
                                      choices = c(state.name),selected = "Washington")
                        ),
                        
                        mainPanel(
                          plotlyOutput("delay_plot"),
                          
                        ))),
             
             tabPanel("Reasons for Delays",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("airlines", h3("Choose Airlines"), 
                                      choices = c("United Airlines Inc."= "UA", "American Airlines Inc." = "AA","US Airways Inc."="US","Frontier Airlines Inc."="F9",
                                                  "JetBlue Airways"="B6","Skywest Airlines Inc."="OO","Alaska Airlines Inc."="AS","Spirit Air Lines"="NK","Southwest Airlines Co."="WN",
                                                  "Delta Air Lines Inc."="DL","Atlantic Southeast Airlines"="EV","Hawaiian Airlines Inc."="HA","American Eagle Airlines Inc"="MQ","Virgin America"="VX"),selected = "AS")
                        ),
                        
                        mainPanel(
                          plotlyOutput("delay_reason"),
                          
                        ))),
             tabPanel("Arrival and Departure delays",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("state1", "Choose State of Origin Airport", 
                                      choices = c(state.name),selected = "Washington"),
                          uiOutput("origin_airport"),
                          selectInput("state2", "Choose State of Destination Airport", 
                                      choices = c(state.name),selected = "California"),
                          uiOutput("destination_airport"),
                          actionButton("submit","Find Flight")),
                        mainPanel(
                          tableOutput("table_info")
                          
                        ))
             )
             
             
  )
))
# Define server logic required to draw a histogram
Server<-(function(input, output) {
  value1 <- reactiveVal("Seattle-Tacoma International Airport")
  value2 <- reactiveVal("Oakland International Airport")
  output$delay_plot <- renderPlotly({
    name <- state.abb[[grep(input$state,state.name)]]
    return(mapplot(name))
  })
  
  output$delay_reason <- renderPlotly({
    return(delayReason(input$airlines))
  })
  output$origin_airport <- renderUI({
    if (is.null(input$state1))
      return()
    name1 <- state.abb[[grep(input$state1,state.name)]]
    airport.state.origin <- filter(airports,STATE == name1)
    selectInput("origin.airport","Choose Origin Airport",choices = c(airport.state.origin$AIRPORT),selected = value1())
  })
  output$destination_airport <- renderUI({
    if (is.null(input$state2))
      return()
    name <- state.abb[[grep(input$state2,state.name)]]
    airport.state <- filter(airports,STATE == name)
    selectInput("destination.airport","Choose Destination Airport",choices = c(airport.state$AIRPORT),selected = value2())
  })
  results <- table(unique(flights$AIRLINE_NAME))
  
  observeEvent(
    input$submit,{
      airport_code(input$origin.airport,input$destination.airport)
      output$table_info <- renderTable({
        airport_code(input$origin.airport,input$destination.airport)
      })
    }
  )
  
})
shinyApp(UI, Server)
