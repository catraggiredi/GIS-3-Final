#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#NOTES TO SELF
# ADD AFRICA ONLY OPTION?
# SHOW PROPORtioN ACCEPTED?
# NET VALUE?

library(shiny)
library(leaflet)
library(spData)
library(spDataLarge)
library(dplyr)
library(tmap)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Originating Countries of UNHCR Populations of Concern in Uganda"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("year",
                  "Year:",
                  min = 2000,
                  max = 2017,
                  step = 1,
                  sep="",
                  value = 2000),
      selectInput("type", "Choose a type:",
                  choices = c("refugees", "asylum-seekers", "all")),
      selectInput("where", "From or To Uganda",
                  choices = c("from", "to"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map"),
      dataTableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    if (input$where == "to"){
      migrants<-read.csv("C:/Users/catra/OneDrive/Documents/GIS3/FinalDataOptions/timebased.csv")
      id = "Origin"
      
      if (input$type == "refugees"){
        pop <- migrants[migrants$Population.type=="Refugees (incl. refugee-like situations)" , ]
      } else if (input$type == "asylum-seekers"){
        pop <- migrants[migrants$Population.type=="Asylum-seekers" , ]
      } else{
        pop <- migrants
      }
    
    } else{
      migrants<-read.csv("C:/Users/catra/OneDrive/Documents/GIS3/FinalDataOptions/RefFromUganda.csv")
      id <- "migrants.Residence"
      
      if (input$type == "refugees"){
        pop<- data.frame("Year"= migrants$Year, migrants$Residence, migrants$Origin, "Value"=migrants$Refugees)
      } else if (input$type == "asylum-seekers"){
        pop <- data.frame("Year"= migrants$Year, migrants$Residence, migrants$Origin, "Value"=migrants$Asylum)
      } else{
        pop <- data.frame("Year" =migrants$Year, migrants$Residence, migrants$Origin, "Value"=migrants$Total)
      }
    }
    dataset <- pop[pop$Year==input$year, ]
    final <- merge(world,dataset, by.x = "name_long", by.y = id)
    final$Value <- as.integer(final$Value)
    tm<- tm_shape(final) + tm_fill(alpha = 0.3, "Value") + tm_shape(world) +  tm_borders()
    tmap_leaflet(tm)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

