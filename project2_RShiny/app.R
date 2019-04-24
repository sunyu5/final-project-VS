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

airplane<-read.csv("/Users/fudanvip/IE6600_project2/final-project-VS/Airplane_Crashes_and_Fatalities_Since_1908.csv",sep =",",
                   stringsAsFactors = FALSE)
cleaned_trend<-subset(airplane, !is.na(Fatalities) & !is.na(Aboard) & Operator != "" & Type != "")
Date<-as.Date(cleaned_trend$Date, "%m/%d/%Y")
cleaned_trend$Year<- as.numeric(format(Date, format = "%Y"))
cleaned_trend$Month<- as.numeric(format(Date, format = "%m"))
cleaned_trend$Day<- as.numeric(format(Date, format = "%d"))
top30<-arrange(summarise(group_by(cleaned_trend,Operator),total_happen = n()),desc(total_happen))[1:30,]
top30data<-subset(cleaned_trend,Operator %in% top30$Operator)
#year<-unique(top10data$Year)

military<-filter(cleaned_trend, grepl("Military", Operator, fixed = TRUE))
militaylist<-unique(military$Operator)
military_sum<-arrange(summarise(group_by(military,Operator),total_happen = n()),desc(total_happen))
militarydata<-subset(cleaned_trend,Operator %in% military_sum$Operator)

normal<-filter(cleaned_trend, !grepl("Military", Operator, fixed = TRUE))
normalist<-unique(normal$Operator)
normal_sum<-arrange(summarise(group_by(normal,Operator),total_happen = n()),desc(total_happen))
normaldata<-subset(cleaned_trend,Operator %in% normal_sum$Operator)


# Define UI for application that draws a histogram
ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "cerulean",  
    "Air Crash Analysis",
    tabPanel("Total Trend ",
             sidebarPanel(
               h4("Filter"),
               
               sliderInput("year","Year",1943,2008,c(1956,1995)),
               
               selectInput(
                 "subsetType", "Select what to show:",
                 c("Show all data in both military and normal operator"="both",
                  "Military Operator" = "military",
                  "Normal Operator" = "normal"),
                 selected = "both"),
              #checkboxGroupInput("subsetType","Operator Type to Show",c("Show all data togather"="both",
                                                                       # "Military and all"="compare1",
                                                                        #"Normal and all"="compare2",
                                                                        #"Military and Operator as group"="group",
                                                                         #"Only Military Operator" = "military",
                                                                         #"Only Normal Operator" = "normal"),selected = "both"),
              
               
               selectInput("yvar", "y-axis variable", c("Fatality number","Aircrash number"), 
                           selected = "Fatality"),
               selectInput("xvar", "x-axis variable", c("Year","Month"), 
                           selected = "Year"),
               
               shiny::hr(),
               actionButton("updateBtn", "Update Data")
             ),
             mainPanel(
               
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot",h3(textOutput("summaryText")),br(),plotOutput("plot")),
                             tabPanel("Table", tableOutput("table")))
               
             )
    ),
    tabPanel("Top 30 Dangerous Operator ",
             sidebarPanel(
      selectizeInput("top30Operator", "",
                     choices=list(Top10=top30$Operator[1:10],Top11_20=top30$Operator[11:20], Top21_30=top30$Operator[21:30]),
                     selected = NULL, multiple = TRUE,
                     options = list(placeholder = "Select from top 30 operators which happened accident most ")),
      br(),
      selectInput("yvar2", "y-axis variable", c("Fatality number","Aircrash number"), 
                  selected = "Fatality"),
      selectInput("xvar2", "x-axis variable", c("Year","Month"), 
                  selected = "Year")
    ),mainPanel(tabsetPanel(type = "tabs",
                            tabPanel("Plot",plotOutput("plot2"),br(),h4("Summary"),
                                     verbatimTextOutput("summary")),
                            tabPanel("Table", tableOutput("table2"))
                            ))
    
  )
))

server <- function(input, output) {
   
  crash<-reactive({
    input$updateBtn
    isolate({
      data<-subset(cleaned_trend,Year >=input$year[1] & Year <= input$year[2])
      if(input$subsetType =="military"){
        data<-filter(data, Operator %in% militaylist)
      }  
      if(input$subsetType =="normal" ){
        data<-filter(data, Operator %in% normalist)
      }
    })
    data
  })
  
  crash2<-reactive({
    data<-top30data
    if(!is.null(input$top30Operator)){
    data<-filter(cleaned_trend,Operator %in% input$top30Operator)}
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
            data<-summarise(group_by(crash(),Year),total_crash=n())
            #p<-ggplot(crash())+geom_histogram(aes(Year, fill=I("red"),color=I("grey")))+labs(title="Accident vs Year ",ylab="number of crash")+theme(plot.title = element_text(hjust = 0.5))
             p<-ggplot(data)+ geom_line(aes(x=Year, y=total_crash,color="pcs")) +labs(title="Crash vs Year",ylab="number of crash")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(min(data$Year),max(data$Year),by=1))
          }
          else if (input$yvar == "Fatality number" & input$xvar == "Year"){
            data<-summarise(group_by(crash(),Year),total_death=sum(Fatalities))
            p<-ggplot(data)+ geom_line(aes(x=Year, y=total_death, color="pcs")) +labs(title="Death vs Year",ylab="number of death")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(min(data$Year),max(data$Year),by=1))
            #p<-ggplot(crash())+geom_histogram(aes(x=Year,weights=Fatalities, fill=I("blue"),color=I("grey")))+labs(title="Death vs Year ",ylab="number of crash")+theme(plot.title = element_text(hjust = 0.5))
          }
          else if (input$yvar =="Fatality number" & input$xvar =="Month"){
            data<-summarise(group_by(crash(),Month),total_death=sum(Fatalities))
            p<-ggplot(data)+ geom_line(aes(x=Month, y=total_death, color="pcs")) +labs(title="Death vs Month",ylab="number of death")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(1,12,by=1))
            #p<-ggplot(crash())+geom_histogram(aes(x=Month,weights=Fatalities, fill=I("blue"),color=I("grey")))+labs(title="Death vs Year ",ylab="number of crash")+scale_x_continuous(breaks = seq(1,12,by=1))+theme(plot.title = element_text(hjust = 0.5))
          }
          else if(input$yvar == "Aircrash number" & input$xvar == "Month"){
            data<-summarise(group_by(crash(),Month),total_crash=n())
            p<-ggplot(data)+ geom_line(aes(x=Month, y=total_crash, color="pcs")) +labs(title="Crash vs Month",ylab="number of crash")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(1,12,by=1))
            # p<-ggplot(crash())+geom_histogram(aes(Month, fill=I("red"),color=I("grey")))+labs(title="Accident vs Year ",ylab="number of death")+scale_x_continuous(breaks = seq(1,12,by=1))+theme(plot.title = element_text(hjust = 0.5))
          }
      }
    })
    print(p)
  })
  output$table <- renderTable({
    crash()
    
  })
  
  output$plot2<-renderPlot({
    if(nrow(crash2())==0){
      p<-NULL
    }
    else{
      if(input$yvar2 == "Aircrash number" & input$xvar2 == "Year"){
        data<-summarise(group_by(crash2(),Year,Operator),total_crash=n())
        p<-ggplot(data)+ geom_line(aes(x=Year, y=total_crash, color=as.factor(Operator))) +labs(title="Crash vs Year",ylab="number of crash")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(min(data$Year),max(data$Year),by=5))
      }
      else if (input$yvar2 == "Fatality number" & input$xvar2 == "Year"){
        data<-summarise(group_by(crash2(),Year,Operator),total_death=sum(Fatalities))
        p<-ggplot(data)+ geom_line(aes(x=Year, y=total_death,color=as.factor(Operator))) +labs(title="Death vs Year",ylab="number of death")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(min(data$Year),max(data$Year),by=5))
      }
      else if (input$yvar2 =="Fatality number" & input$xvar2 =="Month"){
        data<-summarise(group_by(crash2(),Month,Operator),total_death=sum(Fatalities))
        p<-ggplot(data)+ geom_line(aes(x=Month, y=total_death, color=as.factor(Operator))) +labs(title="Death vs Month",ylab="number of death")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(1,12,by=5))
      }
      else if(input$yvar2 == "Aircrash number" & input$xvar2 == "Month"){
        data<-summarise(group_by(crash2(),Month,Operator),total_crash=n())
        p<-ggplot(data)+ geom_line(aes(x=Month, y=total_crash, color=as.factor(Operator))) +labs(title="Crash vs Month",ylab="number of crash")+theme(axis.text.x = element_text(angle=90),plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks = seq(1,12,by=5))
      }
    }
    print(p)
  })
  
  output$table2 <- renderTable({
    crash2()
  })
  output$summary <- renderPrint({
    if(input$yvar2 == "Aircrash number" & input$xvar2 == "Year"){
      data<-summarise(group_by(crash2(),Year,Operator),total_crash=n())
      data<-arrange(data,desc(total_crash))
    }
    else if (input$yvar2 == "Fatality number" & input$xvar2 == "Year"){
      data<-summarise(group_by(crash2(),Year,Operator),total_death=sum(Fatalities))
      data<-arrange(data,desc(total_death))
    }
    else if (input$yvar2 =="Fatality number" & input$xvar2 =="Month"){
      data<-summarise(group_by(crash2(),Month,Operator),total_death=sum(Fatalities))
      data<-arrange(data,desc(total_death))
    }
    else if(input$yvar2 == "Aircrash number" & input$xvar2 == "Month"){
     data<-summarise(group_by(crash2(),Month,Operator),total_crash=n())
     data<-data<-arrange(data,desc(total_crash))
    }
    data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

