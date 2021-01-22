# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library("jsonlite")
library(ggplot2)
library(dplyr)
library(lubridate)

base="https://coronavirusapi-france.now.sh/AllDataByDepartement?"

endpoint="Departement"
library(jsonlite)
stock=c("Rhône","Doubs","Bas-Rhin")


  #url[i]=c(paste(base,endpoint,"=",stock[i],sep=""),paste(base,endpoint,"=",stock[i],sep=""),paste(base,endpoint,"=",stock[i],sep=""))
  url=c(paste(base,endpoint,"=",stock[1],sep=""),paste(base,endpoint,"=",stock[2],sep=""),paste(base,endpoint,"=",stock[3],sep=""))
  #print(url[i])
  book_data=c(fromJSON(url[1], flatten = TRUE), fromJSON(url[2],flatten = TRUE), fromJSON(url[3],flatten = TRUE) )
  #print(book_data[i])
  data=rbind(book_data[1],book_data[2], book_data[3])
  
  ##data=book_data$allDataByDepartement
  data2=bind_rows(data[[1]],data[[2]],data[[3]])
  

data2=as.data.frame(data2)


# ui ----------------------------------------------------------------------


## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Dashboard_Covid"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Onglet Situation", tabName = "situation",icon=icon("situation")),
      menuItem("Onglet Historique", tabName = "historique",icon = icon("historique"))),
    
    # Choix du département 
    selectInput("departement",
                "Choisissez votre departement:",
                choices = unique(data2$nom),
                selected = 'Rhône'),
    
    
    
    #Choix de la date
    selectInput("date","Choix de la date",choices=unique(data2$date),selected = '2020-11-26'),
    
    
    
    ##action button 
    #actionButton(inputId = 'action','Lancer l analyse'),
    #actionButton(inputId = "bouton", label = "Ceci est un bouton"),
    
    actionButton("button", "button"),
    
    
    
    
    
    
    #########2 eme onglet########"
    
    #Choix de la date
    selectInput("date_debut","Choix de la date de début",choices=unique(data2$date),selected = '2020-11-26'),
    #Choix de la date
    selectInput("date_fin","Choix de la date de fin",choices=unique(data2$date),selected = '2020-11-26')
    
    
    
    
    
  ),
  
  
  
  dashboardBody(
    
    
    tabItems(
      #Premier onglet : mon departement
      
      
      tabItem('situation',fluidRow(infoBoxOutput("valuebox1"),
                                   infoBoxOutput("valuebox2"),
                                   infoBoxOutput("valuebox3")),downloadLink('downloadData', 'Telecharger Onglet 1')),
      
      #Premier onglet : mon departement
      tabItem('historique',fluidRow(infoBoxOutput("valuebox6"),infoBoxOutput("valuebox7"),infoBoxOutput("valuebox8")),
              
              plotlyOutput('gueris'),plotlyOutput('casConfirmes'),plotlyOutput('desces')
              ,plotlyOutput('reanimation'),plotlyOutput('hospitalises'),plotlyOutput('nouvellesHospitalisations'),
              plotlyOutput('nouvellesReanimations'))
      
      
      
      
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  ),
  
  skin="yellow"
  
)

server <- function(input, output, session){
  
  
  
  #observeEvent(input$do, {
  #session$sendCustomMessage(type = 'testmessage',
  #message = 'Thank you for clicking')
  #})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep=",")
    },
    content = function(con) {
      write.csv(data, con)
    }
  )  
  
  
  observeEvent(input$button, {
    cat(" recours API ")
    output$gueris <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=gueris)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
    
    output$casConfirmes <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=casConfirmes)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
    
    output$desces <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=deces)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
    
    output$reanimation <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=reanimation)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    })
    
    output$hospitalises <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=hospitalises)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
    
    output$nouvellesHospitalisations <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=nouvellesHospitalisations)) + 
        geom_point(color="blue")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
    
    output$nouvellesReanimations <- renderPlotly({
      
      d=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
      Date=as.Date(d$date)
      df=ggplot(d, aes(x=Date, y=nouvellesReanimations)) + 
        geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
      out=ggplotly(df)
      out
      
    }) 
  })
  #eventReactive(input$bouton, {
  
  #})
  
  
  
  
  
  output$valuebox1<-renderValueBox({
    hospitalises=data2%>%filter(data2$date==input$date & data2$nom==input$departement)
    
    valueBox(value=max(paste(hospitalises$hospitalises)),"Hospitalisés",
             icon = icon("stethoscope"),color = "blue")
    
    
  })
  
  output$valuebox2<-renderValueBox({
    hospitalises=data2%>%filter(data2$date==input$date & data2$nom==input$departement)
    
    valueBox(value=max(paste(hospitalises$casConfirmes)),"Cas Confirmés",
             icon = icon("stethoscope"),color = "purple")
    
    
  })
  
  output$valuebox3<-renderValueBox({
    hospitalises=data2%>%filter(data2$date==input$date & data2$nom==input$departement)
    
    valueBox(value=max(paste(hospitalises$nouvellesReanimations)),"Nouvelles Réanimations",
             icon = icon("stethoscope"),color = "orange")
    
    
  })
  
  
  
  
  
  output$valuebox6<-renderValueBox({
    hospitalises=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
    
    valueBox(value=paste(sum(hospitalises$hospitalises,na.rm=TRUE)),"Hospitalisés",
             icon = icon("stethoscope"),color = "blue")
    
    
  })
  
  output$valuebox7<-renderValueBox({
    hospitalises=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
    
    valueBox(value=paste(sum(hospitalises$casConfirmes,na.rm=TRUE)),"Cas Confirmés",
             icon = icon("stethoscope"),color = "purple")
    
    
  })
  
  output$valuebox8<-renderValueBox({
    hospitalises=data2%>%filter(data2$date <=input$date_fin & data2$date >=input$date_debut & data2$nom==input$departement)
    
    valueBox(value=paste(sum(hospitalises$nouvellesReanimations,na.rm=TRUE)),"Nouvelles Réanimations",
             icon = icon("stethoscope"),color = "orange")
    
    
  })
  
  
  
}

shinyApp(ui, server)