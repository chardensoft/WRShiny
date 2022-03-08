#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(uuid)
library(DT)
library(jsonlite)
source("database2 (team).R")
source("update.R")
source("pullGit.R")

runners_table <- bind_rows(read_json(path = "www/runners.json"))
teams_table <- bind_rows(read_json(path = "www/teams.json"))
Wteams_table <- bind_rows(read_json(path = "www/Wteams.json"))
Wrunners_table <- bind_rows(read_json(path = "www/Wrunners.json"))
Wraces_table <- bind_rows(read_json(path = "www/Wraces.json"))
races_table <- bind_rows(read_json(path = "www/races.json"))

if ("RUNNER.ID" %in% colnames(runners_table)) {
  runners_table <- runners_table[-which(colnames(runners_table) == "RUNNER.ID")]
}
if ("RUNNER.ID" %in% colnames(Wrunners_table)) {
  Wrunners_table <- Wrunners_table[-which(colnames(Wrunners_table) == "RUNNER.ID")]
}

runners_table$uniqueTableID <- rownames(runners_table)
Wrunners_table$uniqueTableID <- rownames(Wrunners_table)

races_table$date <- as.Date(races_table$date, format = "%m/%d/%y")
Wraces_table$date <- as.Date(Wraces_table$date, format = "%m/%d/%y")

Wrunners_table <- Wrunners_table %>% arrange(desc(rank))
runners_table <- runners_table %>% arrange(desc(rank))
Wraces_table <- Wraces_table %>% arrange(desc(date))
races_table <- races_table %>% arrange(desc(date))

runners_table$override_rank <- as.numeric(NA)
Wrunners_table$override_rank <- as.numeric(NA)


MensRunners <- runners_table[c("place", "last", "first", "year", "team", "override_rank", 
                               "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
WomensRunners <- Wrunners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                  "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
MensTeams <- teams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                           "fourthrank", "fifthrank", "div", "conf", "reg")]
WomensTeams <- Wteams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                              "fourthrank", "fifthrank", "div", "conf", "reg")]
WomensRaces <- Wraces_table[c("dist", "type", "race", "date", "PL", "NAME", "YEAR", "TEAM", "TIME", "TIME.IN.S", "Rank")]
MensRaces <- races_table[c("dist", "type", "race", "date", "PL", "NAME", "YEAR", "TEAM", "TIME", "TIME.IN.S", "Rank")]

Medited <- data.frame(uniqueTableID = NA)
Wedited <- data.frame(uniqueTableID = NA)

Mdeleted <- data.frame(team = NA)
Wdeleted <- data.frame(team = NA)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  disableCol <- reactive({
    if (input$data_choice == "Runners") {
      disable <- c(1, 7, 8, 9, 11)
    } else if (input$data_choice == "Teams") {
      disable <- c(1:10)
    } else {
      disable <- c(1:11)
    }
    disable - 1
  })
  
  observeEvent(input$recalc, {
    showModal(modalDialog(
      title = "Recalculating data!",
      "This typically takes about 2 minutes.... please wait...."
    ))
    # print(length(runners_table$last))
    runners_table$last <<- rv[["MensRunners"]]$last
    # print('here')
    runners_table$first <<- rv[["MensRunners"]]$first
    runners_table$year <<- rv[["MensRunners"]]$year
    runners_table$team <<- rv[["MensRunners"]]$team
    runners_table$override_rank <<- rv[["MensRunners"]]$override_rank
    runners_table$active <<- rv[["MensRunners"]]$active
    Wrunners_table$last <<- rv[["WomensRunners"]]$last
    Wrunners_table$first <<- rv[["WomensRunners"]]$first
    Wrunners_table$year <<- rv[["WomensRunners"]]$year
    Wrunners_table$team <<- rv[["WomensRunners"]]$team
    Wrunners_table$override_rank <<- rv[["WomensRunners"]]$override_rank
    Wrunners_table$active <<- rv[["WomensRunners"]]$active
    
    newData <- recalculateTeams(runners_table, teams_table, Wrunners_table, Wteams_table, Medited, Wedited, Mdeleted, Wdeleted)
    
    runners_table <<- newData[[1]]
    teams_table <<- newData[[2]]
    Wrunners_table <<- newData[[3]]
    Wteams_table <<- newData[[4]]
    
    races_table$date <- as.Date(races_table$date, format = "%m/%d/%y")
    Wraces_table$date <- as.Date(Wraces_table$date, format = "%m/%d/%y")
    
    Wrunners_table <<- Wrunners_table %>% arrange(desc(rank))
    runners_table <<- runners_table %>% arrange(desc(rank))
    Wraces_table <<- Wraces_table %>% arrange(desc(date))
    races_table <<- races_table %>% arrange(desc(date))
    
    rv[["MensRunners"]] <- runners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                           "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    rv[["MensTeams"]] <- teams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                                       "fourthrank", "fifthrank", "div", "conf", "reg")]
    rv[["WomensRunners"]] <- Wrunners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                              "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    rv[["WomensTeams"]] <- Wteams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                                          "fourthrank", "fifthrank", "div", "conf", "reg")]
    
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = T, rownames = F)
    showModal(modalDialog(
      title = "Recalculating data!",
      "This typically takes about 2 minutes.... please wait.... 
        RECALCULATION COMPLETE!!"
    ))
  })
  
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Refreshing",
      "Refreshing.... please wait.... "
    ))
    runners_table <<- bind_rows(read_json(path = "www/runners.json"))
    teams_table <<- bind_rows(read_json(path = "www/teams.json"))
    Wteams_table <<- bind_rows(read_json(path = "www/Wteams.json"))
    Wrunners_table <<- bind_rows(read_json(path = "www/Wrunners.json"))
    Wraces_table <<- bind_rows(read_json(path = "www/Wraces.json"))
    races_table <<- bind_rows(read_json(path = "www/races.json"))
    
    runners_table$uniqueTableID <<- rownames(runners_table)
    Wrunners_table$uniqueTableID <<- rownames(Wrunners_table)
    
    races_table$date <<- as.Date(races_table$date, format = "%m/%d/%y")
    Wraces_table$date <<- as.Date(Wraces_table$date, format = "%m/%d/%y")
    
    Wrunners_table <<- Wrunners_table %>% arrange(desc(rank))
    runners_table <<- runners_table %>% arrange(desc(rank))
    Wraces_table <<- Wraces_table %>% arrange(desc(date))
    races_table <<- races_table %>% arrange(desc(date))
    
    runners_table$override_rank <<- as.numeric(NA)
    Wrunners_table$override_rank <<- as.numeric(NA)
    
    MensRunners <<- runners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                   "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    WomensRunners <<- Wrunners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                      "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    MensTeams <<- teams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                               "fourthrank", "fifthrank", "div", "conf", "reg")]
    WomensTeams <<- Wteams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                                  "fourthrank", "fifthrank", "div", "conf", "reg")]
    WomensRaces <<- Wraces_table[c("dist", "type", "race", "date", "PL", "NAME", "YEAR", "TEAM", "TIME", "TIME.IN.S", "Rank")]
    MensRaces <<- races_table[c("dist", "type", "race", "date", "PL", "NAME", "YEAR", "TEAM", "TIME", "TIME.IN.S", "Rank")]
    
    rv[["MensRunners"]] <- MensRunners
    rv[["MensTeams"]] <- MensTeams
    rv[["WomensRunners"]] <- WomensRunners
    rv[["WomensTeams"]] <- WomensTeams
    
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = F, rownames = F)
    showModal(modalDialog(
      title = "Refreshing",
      "DONE!"
    ))
  })
  
  
  rv <- reactiveValues(MensTeams = MensTeams, MensRunners = MensRunners, MensRaces = MensRaces, 
                           WomensTeams = WomensTeams, WomensRunners = WomensRunners, WomensRaces = WomensRaces)
  
  output$table_output <- DT::renderDT(isolate(rv[[paste0(input$gender, input$data_choice)]]), editable = list(
    target = 'cell', disable = list(columns = disableCol())
  ), rownames = F, options = list(pageLength = 25))
  
  proxyTeams <- DT::dataTableProxy("table_output")
  
  observeEvent(input$table_output_cell_edit, {
    #get values
    req(input$table_output_cell_edit)
    info <- input$table_output_cell_edit
    
    # i <- info$row
    # # print(i)
    # j <- info$col + 1L
    # # print(j)
    # k <- info$value
    # print(length(info))
    # print('here1')
    if (length(info) > 0) {
      if (input$data_choice == "Runners") {
        # print('here2')
          if ((info$col + 1L) %in% c(5, 6, 10)) {
          # print('here3')
          if (input$gender == "Mens") {
            # print('here4')
            newEdit <- data.frame(uniqueTableID = rv[[paste0(input$gender, input$data_choice)]][info$row, length(rv[[paste0(input$gender, input$data_choice)]])])
            Medited <<- rbind(newEdit, Medited)
          } else {
            newEdit <- data.frame(uniqueTableID = rv[[paste0(input$gender, input$data_choice)]][info$row, length(rv[[paste0(input$gender, input$data_choice)]])])
            Wedited <<- rbind(newEdit, Wedited)
          }
        }
      }
      # 
      
      #write values to reactive
      # rv[[paste0(input$gender, input$data_choice)]][i,j] <- coerceValue(k, data.frame(rv[[paste0(input$gender, input$data_choice)]])[i,j])
      rv[[paste0(input$gender, input$data_choice)]] <<- editData(rv[[paste0(input$gender, input$data_choice)]], info, proxyTeams, resetPaging = FALSE, rownames = FALSE)
    }
    # replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$gender, {
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$data_choice, {
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$update, {
    showModal(modalDialog(
      title = "Updating Firebase",
      "Please wait while we update the website.... This takes about 3 min....."
    ))
    
    runners_table$last <<- rv[["MensRunners"]]$last
    runners_table$first <<- rv[["MensRunners"]]$first
    runners_table$year <<- rv[["MensRunners"]]$year
    runners_table$team <<- rv[["MensRunners"]]$team
    runners_table$override_rank <<- rv[["MensRunners"]]$override_rank
    runners_table$active <<- rv[["MensRunners"]]$active
    
    Wrunners_table$last <<- rv[["WomensRunners"]]$last
    Wrunners_table$first <<- rv[["WomensRunners"]]$first
    Wrunners_table$year <<- rv[["WomensRunners"]]$year
    Wrunners_table$team <<- rv[["WomensRunners"]]$team
    Wrunners_table$override_rank <<- rv[["WomensRunners"]]$override_rank
    Wrunners_table$active <<- rv[["WomensRunners"]]$active
    
    newData <- recalculateTeams(runners_table, teams_table, Wrunners_table, Wteams_table, Medited, Wedited, Mdeleted, Wdeleted)
    
    runners_table <<- newData[[1]]
    teams_table <<- newData[[2]]
    Wrunners_table <<- newData[[3]]
    Wteams_table <<- newData[[4]]
    
    updateFirebase(runners_table, Wrunners_table, teams_table, Wteams_table)
    
    races_table$date <<- as.Date(races_table$date, format = "%m/%d/%y")
    Wraces_table$date <<- as.Date(Wraces_table$date, format = "%m/%d/%y")
    
    Wrunners_table <<- Wrunners_table %>% arrange(desc(rank))
    runners_table <<- runners_table %>% arrange(desc(rank))
    Wraces_table <<- Wraces_table %>% arrange(desc(date))
    races_table <<- races_table %>% arrange(desc(date))
    
    runners_table$override_rank <<- as.numeric(NA)
    Wrunners_table$override_rank <<- as.numeric(NA)
    
    rv[["MensRunners"]] <- runners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                           "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    rv[["MensTeams"]] <- teams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                                       "fourthrank", "fifthrank", "div", "conf", "reg")]
    rv[["WomensRunners"]] <- Wrunners_table[c("place", "last", "first", "year", "team", "override_rank", 
                                              "rank", "previous_rank", "previous_place", "active", "uniqueTableID")]
    rv[["WomensTeams"]] <- Wteams_table[c("rank", "prev_rank", "name", "teamrank", "prev_rate", "firstrank", "secondrank", "thirdrank", 
                                          "fourthrank", "fifthrank", "div", "conf", "reg")]
    
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = T, rownames = F)

    showModal(modalDialog(
      title = "Updating Firebase",
      "DONE!"
    ))
  })
  
  observeEvent(input$refresh, {
    showModal(modalDialog(
      title = "Updating", 
      "updating...."
    ))
    pullFromGit()
    showModal(modalDialog(
      title = "Updating", 
      "updating....Done!"
    ))
  })
  
  observeEvent(input$add, {
    newRunner <- data.frame(place = NA, gender = ifelse(input$gender == "Mens", "M", "W"), last = "", first = "", year = "", 
                             team = "", rid = "", tid = "", RUNNERID = "", rank = NA, previous_rank = NA, 
                             previous_place = NA, active = "active", override_rank = NA, 
                            uniqueTableID = as.character(max(as.numeric(rv[[paste0(input$gender, input$data_choice)]]$uniqueTableID)) + 1))
    rv[[paste0(input$gender, input$data_choice)]] <- rbind(newRunner[c("place", "last", "first", "year", "team", "override_rank", 
                                     "rank", "previous_rank", "previous_place", "active", "uniqueTableID")], 
                         rv[[paste0(input$gender, input$data_choice)]])
    if (input$gender == "Mens") {
      runners_table <<- rbind(newRunner, runners_table)
    } else {
      Wrunners_table <<- rbind(newRunner, Wrunners_table)
    }
   
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = F, rownames = F)
  })
  
  observeEvent(input$deleteRows, {
    if (!is.null(input$table_output_rows_selected)) {
      
      if (input$gender == "Mens") {
        runners_table <<- runners_table[-which(runners_table$uniqueTableID %in% 
                         rv[["MensRunners"]][as.numeric(input$table_output_rows_selected),]$uniqueTableID),]
        newDelete <- rv[["MensRunners"]][as.numeric(input$table_output_rows_selected), 5]
        Mdeleted <<- rbind(newDelete, Mdeleted)
      } else {
        Wrunners_table <<- Wrunners_table[-which(Wrunners_table$uniqueTableID %in% 
                          rv[["WomensRunners"]][as.numeric(input$table_output_rows_selected),]$uniqueTableID),]
        newDelete <- rv[["WomensRunners"]][as.numeric(input$table_output_rows_selected), 5]
        Wdeleted <<- rbind(newDelete, Wdeleted)
      }
      rv[[paste0(input$gender, input$data_choice)]] <- rv[[paste0(input$gender, 
                                    input$data_choice)]][-as.numeric(input$table_output_rows_selected),]
    }
    replaceData(proxyTeams, rv[[paste0(input$gender, input$data_choice)]], resetPaging = F, rownames = F)
  })

})
