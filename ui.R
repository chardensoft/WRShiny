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
                                    "Data Type",
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
                        actionButton("recalc", "Recalculate (Locally)",
                                     icon = icon("calculator"),
                                     style = "margin: 2px"
                                     ),
                        actionButton("update", "Update Website (Also recalculates)", 
                                     icon = icon("arrow-up"),
                                     style = "margin: 2px"),
                        actionButton("reset", "Reset to Website (Removes all local changes)", 
                                     icon = icon("broom"),
                                     style = "margin: 2px"),
                        actionButton("refresh", "Get Chris' Update", 
                                     icon = icon("arrow-down"),
                                     style = "margin: 2px")
                 ), style = "margin-top: 10px"
               ),
               
               # Sidebar with a slider input for number of bins
               # sidebarLayout(
               #     sidebarPanel(
               
               # ),
               conditionalPanel(
                 condition = "input.data_choice == 'Runners'",
                 fluidRow(
                    actionButton("add", "Add New Runner", 
                                 icon = icon("plus"),
                              style = "margin-bottom: 10px; color: #0a5431; background-color: #5bd39b"), 
                    actionButton("deleteRows", "Delete Selected Rows", 
                                 icon = icon("trash"),
                              style = "margin-bottom: 10px; color: #6d1a1a; background-color: #f79696"), 
                    style = "margin-left: 2.5px"
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 DT::dataTableOutput("table_output")
               )
              ),
      tabPanel("Instructions", 
               br(), 
               strong("A few initial things"), 
               tags$li("Let me know how this can improve!"),
               tags$li("If you notice you're doing the same thing over and over, we can probably automate it, change the setting, etc."),
               tags$li("You can edit data by double-clicking on the cell"),
               tags$li("The full instructions for setting up this app are in 'Instructions.docx' which you should've saved in your 'WR' folder"),
               br(),
               icon("caret-down"), strong("Data Type"),
               p("The ", strong("Data Type"), " drop down lets you select runners, teams or races depending on which table you want to look at.", br(),
                 "The Races table is not editable, it's more for reference and if a large error is found, we'll need to fix it on my end.", br(), 
                 "The Runners and Teams tables are editable but only the columns that are informational + the override_rank column."),
               br(),
               icon("caret-down"), strong("Gender"),
               p("The ", strong("Gender"), " drop down lets you select which gender you want to edit."),
               br(),
               icon("calculator"), strong("Recalculate"),
               p("The ", strong("Recalculate (Locally)"), " button replaces each runner's rank with their override_rank and then re-calculates the runners and teams tables."),
               br(),
               icon("arrow-up"), strong("Update Website"),
               p("The ", strong("Update Website"), " button first runs the same process as the recalculate button, then sends the new data to the website and updates the website. It will replace the saved data in the app, so if you need to undo changes after pushing this button, you'll have to do them manually, or get the old data again from me. In other words, the ", 
                 strong("Reset to Website"), " button will now just get this data."),
               br(),
               icon("broom"), strong("Reset"),
               p("The ", strong("Reset to Website"), " button will remove any changes you have done since the last time you clicked the ", 
                 strong("Update Website"), " button. Be cautious with this - if you have run the ", 
                 strong("Recalculate"), " button a few times and then you click this without updating the website, all changes will be lost."),
               br(),
               icon("arrow-down"), strong("Get Chris' Update"),
               p("The ", strong("Get Chris' Update"), " button will get any new data Chris has created from recent meets. I'll let you know when new data is available, and then you can come and click this button. No worries if you click it whenever, there's no harmful affects to doing so if I haven't added any new data."),
               br(),
               icon("plus"), strong("Add New Runner"), 
               p("The ", strong("Add New Runner"), " button will add a new row at the top of the runners table which is editable just like the other rows. When you later click the ", 
                 strong("Recalculate"), " button, these new runners will be factored in."),
               br(), 
               icon("trash"), strong("Delete Selected Rows"), 
               p("The ", strong("Delete Selected Rows"), " button will delete all selected rows. You select a row by clicking on it and it will highlight blue."),
               br(),
               p("From time to time I'm sure we'll have errors. Usually if something goes wrong, some red text will show up in the Console area (usually bottom left) of the R Studio window. If that's the case, send me a screenshot, but if not no worries, we should still be able to fix most errors quite quickly."),
               )
    ))
  )
    # Application title
    
    # )
, style = "background-color: #edf8ff"))
