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
                                    selectize = TRUE)
                        
                 ),
                 column(6, 
                        actionButton("recalc", "Recalculate (Locally)" 
                                     # icon = icon("sync")
                                     ),
                        actionButton("update", "Update Website (Also recalculates)"),
                        actionButton("reset", "Reset to Website (Removes all local changes)"),
                        actionButton("refresh", "Get Chris' Update")
                 )
               ),
               
               # Sidebar with a slider input for number of bins
               # sidebarLayout(
               #     sidebarPanel(
               
               # ),
               conditionalPanel(
                 condition = "input.data_choice == 'Runners'",
                 fluidRow(
                    actionButton("add", "Add New Runner", 
                              style = "margin-bottom: 10px"), 
                    actionButton("deleteRows", "Delete Selected Rows", 
                              style = "margin-bottom: 10px"), 
                    style = "margin-left: 2.5px"
                 )
               ),
               
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
