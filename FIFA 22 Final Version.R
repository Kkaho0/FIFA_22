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
  header = dashboardHeader(
    title = tags$strong("FIFA 22 Player Search"),
    titleWidth = 280
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Homepage", tabName = "page1",icon=icon("home")),
      menuItem("Player Scores", tabName = "page2",icon=icon("tachometer-alt")),
      menuItem("Player Attributes", tabName = "page3", icon = icon("th")),
      menuItem("Score Plots", tabName = "page4", icon = icon("chart-area"))
    )
    
  ),
  body = dashboardBody(tabItems(
    tabItem(
      tabName="page1",
      HTML(
        '<iframe width="100%" height="500"
                  src="https://www.youtube.com/embed/o1igaMv46SY"
                  frameborder="0" allowfullscreen></iframe>'),
      fluidRow(
        box(
          title = "Overview",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          collapsible = TRUE,
          column(12,
                 tags$div(
                   p("When playing FIFA 22 Career Mode, there is a common complaint about the limited player attributes in the searching function, which only contains information such as player names, position, league, etc. 

With such limited attributes, finding the right players to assemble the ideal squad is complex and time-consuming.  In this case, our team aims to create a web that helps users find their desired players easily and accurately, by adding more searching attributes and multiple players comparison functions."
                   ),
                   
                 ))
        )
      ),
      fluidRow(
        box(
          title = "Team Member",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          collapsible = TRUE,
          column(
            12,
            tags$li("1.", tags$strong("Kaho Mai"), "(jmai7@jh.edu) "),
            tags$li("2.", tags$strong("Jiaqi Fang"), "(jfang33@jh.edu)"),
            
          )
        )
      )
    ),
    
    
    
    tabItem(
      tabName = "page2",
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
      tabName = "page3",
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
    tabItem(tabName = "page4",
            h1("Players Score Plots"),
            fluidRow(
              column(4,plotlyOutput("plot1")),
              column(4,plotlyOutput("plot2")),
              column(4,plotlyOutput("plot3")),
            fluidRow(
              box(
                  title = "Explaination",
                  solidHeader = TRUE,
                  status = "success",
                  width = 12,
                  collapsible = TRUE,
                  column(
                    12,
                    tags$div(
                      p("After selecting Player Scores and Player Attributes in Page 2&3, the Score Plots will visualize slected players attributions in Page4 "),
            )
            )
  ), 
 
   
        
      )
    )
  )
   )
  
  
  ),
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
