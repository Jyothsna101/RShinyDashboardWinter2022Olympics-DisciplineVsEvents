library(shiny)
library(pheatmap)
library(ggplot2)
library(DataExplorer)
library(scales)
library(dplyr)
library(plotly)
library(leaflet)
library(shinydashboard)


medals_data <- read.csv("/Users/jyothsnaakula/Documents/6070/Data/medals.csv")
medals_total <- read.csv("/Users/jyothsnaakula/Documents/6070/Data/medals_total.csv")
Events<- read.csv("/Users/jyothsnaakula/Documents/6070/Data/events.csv")
Displine<- read.csv("/Users/jyothsnaakula/Documents/6070/Data/entries_discipline.csv")
D1 <- Displine[-c(16),]
D1

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

EE1<- Events %>% group_by(event_stage, discipline) %>% 
  summarise(Count = table(event_stage))

ui <- dashboardPage(
  dashboardHeader(title = "DISCIPLINE VS EVENTS", titleWidth = 1450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(
   
    box(
      title = "Total Participants in Each Discipline",
      status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("plot", height = "300px")),
    
    box(
      title = "Total Events Per Discipline",
      status= "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("plot1",height="300px")),
    
 box(
   title = "Medals Won",
   status = "primary",
   solidHeader = TRUE,
   collapsible = TRUE,
   plotOutput("plot2", height = "300px")
 ) )
 ))


server <- function(input, output) {
  
  EE1<- Events %>% group_by(event_stage, discipline) %>% 
    summarise(Count = table(event_stage))
  
  output$plot <- renderPlot(
    ggplot(data = D1,   
           aes(x= Discipline , y= Total, fill=factor(Discipline))) + 
      geom_histogram(position = "dodge", stat = "identity") + ylab("Total Participants") + 
      xlab("Discipline") + theme(legend.position="None" 
                                ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Total Participants in Disciplines")+theme(axis.text.x = element_text(angle = 90))
  )
  
  output$plot1 <- renderPlot(
    ggplot(data = EE1,
           aes(x= discipline, y = Count, fill = event_stage))+
      geom_bar(stat="identity", position = "stack")+
      xlab("Gender")+ theme(legend.position="None" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Events per Discipline")+theme(axis.text.x = element_text(angle = 90))
    
  )
  
 m<- head(medals_total, 10)
  
  output$plot2 <- renderPlot(
    ggplot(data = m,
           aes(x= Country, y = Total, fill = Country))+
      geom_point(stat="identity", position = "stack")+
      xlab("Countries")+ theme(legend.position="None" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 10 countries in medals count")+theme(axis.text.x = element_text(angle = 90))
    
  )}

shinyApp(ui = ui, server = server)

