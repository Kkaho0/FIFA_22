library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(plotly)

data <- read.csv("players_22.csv", header = TRUE, sep = ",")
columns1 <- c('short_name','long_name','overall','potential','pace','shooting',
             'passing',	'dribbling',	'defending',	'physic')
sdata <- data[,columns1]
columns2 <- c('short_name','long_name','overall','age','dob','height_cm',
              'weight_kg','nationality_name','player_tags','player_traits')
adata <- data[,columns2]

ui = dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(
    minified = TRUE,
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Player Scores", tabName = "page1",icon=icon("tachometer-alt")),
      menuItem("Player Attributes", tabName = "page2", icon = icon("th")),
      menuItem("Score Plots", tabName = "page3", icon = icon("chart-area"))
    )
    
  ),
  body = dashboardBody(tabItems(
    tabItem(
      tabName = "page1",
      fluidRow(
        column(3,h2("Player scores"),),
        column(9,checkboxInput("checkbox", label = h3("All Records - NO filters"), value = FALSE)),
               ), 
      fluidRow(
        column(3,sliderInput("slider1", label = h4("Overall Score Range"), min = 0, 
                           max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider2", label = h4("Potential Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider3", label = h4("Pace Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider4", label = h4("Shooting Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
      ),
      fluidRow(
        column(3,sliderInput("slider5", label = h4("Passing Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider6", label = h4("Dribbling Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider7", label = h4("Defending Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
        column(3,sliderInput("slider8", label = h4("Physic Score Range"), min = 0, 
                             max = 100, value = c(0, 100)),),
      ),
      dataTableOutput("table1")
    ),
    tabItem(
      tabName = "page2",
      h2("Player Attributes"),
      fluidRow(
        column(4,sliderInput("slider9", label = h4("Age Range (in years)"), min = 16, 
                             max = 54, value = c(16, 54)),),
        column(4,sliderInput("slider10", label = h4("Height Range (in cm)"), min = 155, 
                             max = 206, value = c(155, 206)),),
        column(4,sliderInput("slider11", label = h4("Weight Range (in kg)"), min = 49, 
                             max = 110, value = c(49, 110)),)
      ),
      dataTableOutput("table2")
      
    ),
    tabItem(tabName = "page3",
            h1("Players Score Plots"),
            fluidRow(
              column(4,plotlyOutput("plot1")),
              column(4,plotlyOutput("plot2")),
              column(4,plotlyOutput("plot3"))
            )
            )
  ), ),
  controlbar = dashboardControlbar()
)
server = function(input, output) {
  
  output$table1 = renderDataTable({
    if(input$checkbox)
    {
      return(datatable(sdata, options = list(scrollX = TRUE)))  
    }  
    else
    {
      sdata_1 <- sdata
      sdata_1 <- sdata_1[sdata_1$overall>=input$slider1[1],]
      sdata_1 <- sdata_1[sdata_1$overall<=input$slider1[2],]
      
      sdata_1 <- sdata_1[sdata_1$potential>=input$slider2[1],]
      sdata_1 <- sdata_1[sdata_1$potential<=input$slider2[2],]
      
      sdata_1 <- sdata_1[sdata_1$pace>=input$slider3[1],]
      sdata_1 <- sdata_1[sdata_1$pace<=input$slider3[2],]
      
      sdata_1 <- sdata_1[sdata_1$shooting>=input$slider4[1],]
      sdata_1 <- sdata_1[sdata_1$shooting<=input$slider4[2],]
      
      sdata_1 <- sdata_1[sdata_1$passing>=input$slider5[1],]
      sdata_1 <- sdata_1[sdata_1$passing<=input$slider5[2],]
      
      sdata_1 <- sdata_1[sdata_1$dribbling>=input$slider6[1],]
      sdata_1 <- sdata_1[sdata_1$dribbling<=input$slider6[2],]
      
      sdata_1 <- sdata_1[sdata_1$defending>=input$slider7[1],]
      sdata_1 <- sdata_1[sdata_1$defending<=input$slider7[2],]
      
      sdata_1 <- sdata_1[sdata_1$physic>=input$slider8[1],]
      sdata_1 <- sdata_1[sdata_1$physic<=input$slider8[2],]
      
      return(datatable(na.omit(sdata_1), options = list(scrollX = TRUE)))
    }
    
  })
  
  output$table2 = renderDataTable({
    
    adata_1 <- adata
    adata_1 <- adata_1[adata_1$age>=input$slider9[1],]
    adata_1 <- adata_1[adata_1$age<=input$slider9[2],]
    
    adata_1 <- adata_1[adata_1$height_cm>=input$slider10[1],]
    adata_1 <- adata_1[adata_1$height_cm<=input$slider10[2],]
    
    adata_1 <- adata_1[adata_1$weight_kg>=input$slider11[1],]
    adata_1 <- adata_1[adata_1$weight_kg<=input$slider11[2],]
    
    return(datatable(adata_1, options = list(scrollX = TRUE)))
  })
  
  sdata_2 <- na.omit(sdata)
  output$plot1 = renderPlotly({
    p = sdata_2 %>%
      ggplot(mapping = aes(x = pace, y = shooting)) +
      geom_smooth() +
      geom_point()
    return(ggplotly(p))
  })
    
  output$plot2 = renderPlotly({
    p = sdata_2 %>%
      ggplot(mapping = aes(x = passing, y = dribbling)) +
      geom_smooth() +
      geom_point()
    return(ggplotly(p))
  })
  
  output$plot3 = renderPlotly({
    p = sdata_2 %>%
      ggplot(mapping = aes(x = defending, y = physic)) +
      geom_smooth() +
      geom_point()
    return(ggplotly(p))
  })
  }
shinyApp(ui = ui,  server = server)
