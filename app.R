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


url="https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
book_data <- fromJSON(url)

data=book_data$allDataByDepartement


# ui ----------------------------------------------------------------------


## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title="Dashboard Covid"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Onglet Situation", tabName = "situation",icon=icon("situation")),
            menuItem("Onglet Historique", tabName = "historique",icon = icon("historique"))),
    
        
        
        #Choix de la date
        selectInput("date","Choix de la date",choices=unique(data$date),selected = '2020-11-26'),
        
        
        
        ##action button 
        #actionButton(inputId = 'action','Lancer l analyse'),
        
        
        downloadLink('downloadData', 'Telecharger'),
        
        
        #########2 eme onglet########"
        
        #Choix de la date
        selectInput("date_debut","Choix de la date de début",choices=unique(data$date),selected = '2020-11-26'),
        #Choix de la date
        selectInput("date_fin","Choix de la date de fin",choices=unique(data$date),selected = '2020-11-26')
        
        
        
        
        
    ),
    
    
    
    dashboardBody(
    
        
        tabItems(
            #Premier onglet : mon departement
            
            
            tabItem('situation',class='dynamic',valueBoxOutput("valuebox1"),
                    valueBoxOutput("valuebox2"),
                    valueBoxOutput("valuebox3"))
            
            
            
            
        ),
        
        
        
        
        
        tabItems(
            #Premier onglet : mon departement
            tabItem('historique',plotlyOutput('gueris'))
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    ),
    
    skin="purple"
    
))

server <- function(input, output){
    
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep=",")
        },
        content = function(con) {
            write.csv(data, con)
        }
    )  
    
    
    
    output$valuebox1<-renderValueBox({
        hospitalises=data%>%filter(data$date==input$date)
        
        valueBox(value=paste(hospitalises$hospitalises),"Hospitalisés",
                 icon = icon("stethoscope"),color = "blue")
        
        
    })
    
    output$valuebox2<-renderValueBox({
        hospitalises=data%>%filter(data$date==input$date)
        
        valueBox(value=paste(hospitalises$casConfirmes),"Cas Confirmés",
                 icon = icon("stethoscope"),color = "purple")
        
        
    })
    
    output$valuebox3<-renderValueBox({
        hospitalises=data%>%filter(data$date==input$date)
        
        valueBox(value=paste(hospitalises$nouvellesReanimations),"Nouvelles Réanimations",
                 icon = icon("stethoscope"),color = "orange")
        
        
    })
    
    
    output$gueris <- renderPlotly({
        
        d=data%>%filter(data$date <=input$date_fin & data$date >=input$date_debut)
        Date=as.Date(d$date)
        df=ggplot(d, aes(x=Date, y=gueris)) + 
            geom_point(color="magenta")+scale_x_date(date_labels = "%Y %b %d")
        out=ggplotly(df,width = 700,height = 500)
        out
        
    }) 
    
    
    
    
    
    
    
}    








shinyApp(ui, server)