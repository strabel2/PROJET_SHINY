# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library("jsonlite")


url_req <- "https://coronavirusapi-france.now.sh/AllLiveData"
book_data <- fromJSON(url_req)
data<-book_data$allLiveFranceData


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
        
        # Choix du département 
        selectInput("departement",
                    "Choisissez votre departement:",
                    choices = unique(data$nom),
                    selected = 'Doubs'),
        
        
        #Choix de la date
        selectInput("date","Choix de la date",choices=unique(data$date),selected = '2020-11-26'),
        
 
        
        ##action button 
        #actionButton(inputId = 'action','Lancer l analyse'),
        
        
        downloadLink('downloadData', 'Telecharger')
        
        
        
        
    ),
    
    
    
    dashboardBody(
        fluidRow(
            
            # Dynamic valueBoxes
            valueBoxOutput("valuebox1")
            
        ),
        
        tabItems(
            #Premier onglet : mon departement
          
            
            tabItem('situation')
            
            
            
            
            ),
            
            
            
            
            
            tabItems(
                #Premier onglet : mon departement
                tabItem('historique'))
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
        ),
        
        skin="purple"
        
    )

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
      hospitalises=data%>%filter(data$nom==input$departement & data$date==input$date)
      
      valueBox(value=paste(hospitalises$hospitalises),"Hospitalisés",
                 icon = icon("stethoscope"),color = "blue")
        
        
    }
    
    
    
    
    
    )
    
    
    
    
    
    
    
    
}    








shinyApp(ui, server)