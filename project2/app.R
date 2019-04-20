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

military<-filter(cleaned_trend, grepl("Military", Operator, fixed = TRUE))
military_sum<-arrange(summarise(group_by(military,Operator),total_happen = n()),desc(total_happen))[1:20,]
top20military<-subset(cleaned_trend,Operator %in% military_sum$Operator)

normal<-filter(cleaned_trend, !grepl("Military", Operator, fixed = TRUE))
normal_sum<-arrange(summarise(group_by(normal,Operator),total_happen = n()),desc(total_happen))[1:20,]
top20normal<-subset(cleaned_trend,Operator %in% normal_sum$Operator)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Aircrash Data"),
   
   sidebarLayout(
     sidebarPanel(
              h4("Filter"),
              
              sliderInput("year","Year",1943,2008,c(1956,1995)),
              
              selectInput(
                "subsetType", "",
                c("Both Military and normal Operator"="both",
                  "Select Military Operator" = "military",
                  "Select Normal Operator" = "normal"),
                selected = "both"),
              
              conditionalPanel(
                "input.subsetType == 'military'",
                uiOutput("militaryTypeUi")
              ), br(),
              
              conditionalPanel(
                "input.subsetType == 'normal'",
                uiOutput("normalTypeUi")
              ), br(),
              
              selectInput("yvar", "y-axis variable", c("Fatality number","Aircrash number"), 
                          selected = "Fatality"),
              selectInput("xvar", "x-axis variable", c("Year","Month"), 
                          selected = "Year"),
              
              shiny::hr(),
              actionButton("updateBtn", "Update Data")
     ),
   mainPanel( 
     h3(textOutput("summaryText")),
     tabsetPanel(type = "tabs",
                 tabPanel("Plot",plotOutput("plot")),
                 tabPanel("Table", tableOutput("table")))
     )
   
   ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$militaryTypeUi <- renderUI({
    selectizeInput("militaryType", "",
                   military_sum$Operator,
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select military operator"))
  })
  
  output$normalTypeUi <- renderUI({
    selectizeInput("normalType", "",
                   normal_sum$Operator,
                   selected = NULL, multiple = TRUE,
                   options = list(placeholder = "Select normal operator"))
  })	
  
  
  crash<-reactive({
    input$updateBtn
    isolate({
    data<-subset(cleaned_trend,Year >=input$year[1] & Year <= input$year[2])
    if(input$subsetType =="military" & !is.null(input$militaryType)){
      data<-filter(data, Operator %in% input$militaryType)
    }  
    if(input$subsetType =="normal" & !is.null(input$normalType)){
      data<-filter(data, Operator %in% input$normalType)
    }
    })
    data
  })
  
  output$summaryText <- renderText({
    input$updateBtn
    isolate({
    numOptions <- nrow(crash())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    })
    paste0("We found ", numOptions, " records for you")
  })
  

  output$plot<-renderPlot({
    input$updateBtn
    isolate({
    if(nrow(crash())==0){
      p<-NULL
    }
    else{
      if(input$yvar == "Aircrash number" & input$xvar == "Year"){
        p<-ggplot(crash())+geom_histogram(aes(Year, fill=I("red"),color=I("grey")))+labs(title="Accident vs Year ",ylab="number of death")+theme(plot.title = element_text(hjust = 0.5))
      }
      else if (input$yvar == "Fatality number" & input$xvar == "Year"){
        p<-ggplot(crash())+geom_histogram(aes(x=Year,weights=Fatalities, fill=I("blue"),color=I("grey")))+labs(title="Death vs Year ",ylab="number of crash")+theme(plot.title = element_text(hjust = 0.5))
      }
      else if (input$yvar =="Fatality number" & input$xvar =="Month"){
        p<-ggplot(crash())+geom_histogram(aes(x=Month,weights=Fatalities, fill=I("blue"),color=I("grey")))+labs(title="Death vs Year ",ylab="number of crash")+scale_x_continuous(breaks = seq(1,12,by=1))+theme(plot.title = element_text(hjust = 0.5))
      }
      else if(input$yvar == "Aircrash number" & input$xvar == "Month"){
        p<-ggplot(crash())+geom_histogram(aes(Month, fill=I("red"),color=I("grey")))+labs(title="Accident vs Year ",ylab="number of death")+scale_x_continuous(breaks = seq(1,12,by=1))+theme(plot.title = element_text(hjust = 0.5))
      }
    }
    })
    print(p)
   })
  output$table <- renderTable({
    crash()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

