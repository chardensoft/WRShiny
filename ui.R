#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Wood Report Data"),
  
  mainPanel(
    column(12,
    tabsetPanel(
      tabPanel("Manual Update", 
               fluidRow(
                 column(6, 
                        
                        selectInput("data_choice",
                                    "Choose Your Data",
                                    c("Runners", "Teams", "Races"), 
                                    selected = "Runners", 
                                    selectize = TRUE),
                        selectInput("gender",
                                    "Gender",
                                    c("Mens", "Womens"), 
                                    selected = "Mens", 
                                    selectize = TRUE),
                 ),
                 column(6, 
                        actionButton("recalc", "Recalculate (Local)", icon = icon("sync")),
                        actionButton("update", "Update Firebase (Website)", icon = icon("sync")),
                        actionButton("reset", "Reset (Remove Changes)", icon = icon("sync")),
                        actionButton("refresh", "New Week", icon = icon("sync")),
                 )
               ),
               
               # Sidebar with a slider input for number of bins
               # sidebarLayout(
               #     sidebarPanel(
               
               # ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 DT::dataTableOutput("table_output")
               )
              ),
      tabPanel("Instructions", 
               
               )
    ))
  )
    # Application title
    
    # )
))
