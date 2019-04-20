#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

airplane<-read.csv("/Users/fudanvip/project2_IE6600/final-project-VS/Airplane_Crashes_and_Fatalities_Since_1908.csv",sep =",",
                   stringsAsFactors = FALSE)
cleaned_trend<-subset(airplane, !is.na(Fatalities) & !is.na(Aboard) & Operator != "" & Type != "")
cleaned_trend$Date<-as.Date(cleaned_trend$Date, "%m/%d/%Y")
cleaned_trend$Year<- as.numeric(format(cleaned_trend$Date, format = "%Y"))
cleaned_trend$Month<- as.numeric(format(cleaned_trend$Date, format = "%m"))
cleaned_trend$Day<- as.numeric(format(cleaned_trend$Date, format = "%d"))
top10<-arrange(summarise(group_by(cleaned_trend,Operator),total_happen = n()),desc(total_happen))[1:10,]
top10data<-subset(cleaned_trend,Operator %in% top10$Operator)
year<-unique(top10data$Year)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Aircrash Data"),
   
   sidebarLayout(
     sidebarPanel(
              h4("Filter"),
  
              selectInput("year1", "min year",
                          year,selected = 1943),
              selectInput("year2", "max year",
                          year,selected = 2008),
              selectInput("operator", "Operator for flight",
                        top10$Operator,selected = "Aeroflot"
              ),
              selectInput("yvar", "y-axis variable", c("Fatality","aircrash number"), 
                          selected = "Fatality")
     ),
   mainPanel( plotOutput("plot")))
   
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  crash<-reactive({
    #minyear<-as.numeric(input$year1)
    #maxyear<-as.numeric(input$year2)
    #operator<-input$operator
    subset(top10data,Year >=input$year1 & Year <= input$year2 & Operator == input$operator )
    
  })
  
  summ<-reactive({
    if(input$yvar == "Fatality"){
      summarise(group_by(crash(),Year),death=sum(Fatalities))
    }
    else{
      summarise(group_by(crash(),Year),crash_total=n())
    }
  })
  output$plot<-renderPlot({
    if(input$yvar == "Fatality"){
      p<-ggplot(summ())+geom_line(aes(x=Year, y=death, color="pcs"))
    }
    else{
      p<-ggplot(summ())+geom_line(aes(x=Year, y=crash_total, color="pcs"))
    }
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

