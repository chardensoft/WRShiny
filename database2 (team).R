### Database - just teams
library(uuid)
library(dplyr)

recalculateTeams <- function(runners_table, teams_table, Wrunners_table, Wteams_table, Medited, Wedited, Mdeleted, Wdeleted) {
  Mrunners <- runners_table
  # oldMrunners <- read.csv("database files/Mrunners.csv")
  Mteams <- teams_table
  
  Mrunners$place[Mrunners$place == ""] <- NA
  Mrunners$RUNNERID <- paste0(Mrunners$last, Mrunners$first, Mrunners$team)
  Mrunners$rid[Mrunners$rid == 0] <- NA
  Mrunners$tid[Mrunners$tid == 0] <- NA
  # Mrunners$override_rank[Mrunners$override_rank == ""] <- NA
  Mrunners$rank[Mrunners$rank == ""] <- NA
  Mrunners$previous_rank[Mrunners$previous_rank == ""] <- NA
  Mrunners$previous_place[Mrunners$previous_place == ""] <- NA
  Mrunners$active[Mrunners$active == ""] <- "active"
  Mrunners$rank <- ifelse(is.na(Mrunners$override_rank), Mrunners$rank, Mrunners$override_rank)
  Mrunners$override_rank <- NA
  # Mteams$prev_rate <- Mteams$teamrank
  # Mteams$prev_rank <- Mteams$rank
  # Mrunners$previous_rank <- Mrunners$rank
  # Mrunners$previous_place <- Mrunners$place
  
  if (length(which(Mrunners$team == "")) > 0) {
    Mrunners <- Mrunners[-which(Mrunners$team == ""),]
  }
  if (length(which(tolower(Mrunners$team) == "unattached")) > 0) {
    Mrunners <- Mrunners[-which(tolower(Mrunners$team) == "unattached"),]
  }
  
  if (length(which(Mrunners$active == "inactive")) > 0) {
    inactives <- Mrunners[which(Wrunners$active == "inactive"),]  
  }
  Mrunners <- Mrunners[which(Mrunners$active == "active"),] %>% 
    arrange(desc(rank))
  
  for (i in c(which(is.na(Mrunners$rid)), which(Mrunners$rid == ""))) {
    Mrunners$rid[i] <- UUIDgenerate()
  }
  theEditedTeams <- data.frame(team = Mrunners$team[which(Mrunners$uniqueTableID %in% Medited$uniqueTableID)])
  MeditedTeams <- data.frame(team = unique(c(theEditedTeams$team, Medited$team))[-which(is.na(unique(c(theEditedTeams$team, Medited$team))))])
  MdeletedTeams <- Mteams$name[which(Mteams$name %in% Mdeleted$team)]
  
  listOfEdits <- which(Mteams$name %in% c(MeditedTeams, MdeletedTeams))
  
  if (length(listOfEdits) > 0) {
    Mteams$firstrank[listOfEdits] <- NA
    Mteams$firstid[listOfEdits] <- NA
    Mteams$secondrank[listOfEdits] <- NA
    Mteams$secondid[listOfEdits] <- NA
    Mteams$thirdrank[listOfEdits] <- NA
    Mteams$thirdid[listOfEdits] <- NA
    Mteams$fourthrank[listOfEdits] <- NA
    Mteams$fourthid[listOfEdits] <- NA
    Mteams$fifthrank[listOfEdits] <- NA
    Mteams$fifthid[listOfEdits] <- NA
    
    no_runners <- data.frame(NA)
    for (i in listOfEdits) {
      if (tolower(Mteams$name[i]) %in% tolower(Mrunners$team)) {
        Mteams$prev_rate[i] <- Mteams$teamrank[i]
        Mteams$prev_rank[i] <- Mteams$rank[i]
        team <- Mrunners[which(tolower(Mrunners$team) == tolower(Mteams$name[i])),]
        team <- team %>% arrange(desc(rank))
        Mteams$teamrank[i] <- sum(as.numeric(team$rank[1:5]))
        if (length(team$rank) >= 5) {
          Mteams$firstrank[i] <- team$rank[1]
          Mteams$firstid[i] <- team$rid[1]
          Mteams$secondrank[i] <- team$rank[2]
          Mteams$secondid[i] <- team$rid[2]
          Mteams$thirdrank[i] <- team$rank[3]
          Mteams$thirdid[i] <- team$rid[3]
          Mteams$fourthrank[i] <- team$rank[4]
          Mteams$fourthid[i] <- team$rid[4]
          Mteams$fifthrank[i] <- team$rank[5]
          Mteams$fifthid[i] <- team$rid[5]
        } else if (length(team$rank) == 4) {
          Mteams$firstrank[i] <- team$rank[1]
          Mteams$firstid[i] <- team$rid[1]
          Mteams$secondrank[i] <- team$rank[2]
          Mteams$secondid[i] <- team$rid[2]
          Mteams$thirdrank[i] <- team$rank[3]
          Mteams$thirdid[i] <- team$rid[3]
          Mteams$fourthrank[i] <- team$rank[4]
          Mteams$fourthid[i] <- team$rid[4]
        } else if (length(team$rank) == 3) {
          Mteams$firstrank[i] <- team$rank[1]
          Mteams$firstid[i] <- team$rid[1]
          Mteams$secondrank[i] <- team$rank[2]
          Mteams$secondid[i] <- team$rid[2]
          Mteams$thirdrank[i] <- team$rank[3]
          Mteams$thirdid[i] <- team$rid[3]
        } else if (length(team$rank) == 2) {
          Mteams$firstrank[i] <- team$rank[1]
          Mteams$firstid[i] <- team$rid[1]
          Mteams$secondrank[i] <- team$rank[2]
          Mteams$secondid[i] <- team$rid[2]
        } else if (length(team$rank) == 1) {
          Mteams$firstrank[i] <- team$rank[1]
          Mteams$firstid[i] <- team$rid[1]
        }
        
      } else {
        no_runners <- rbind(no_runners, Mteams$name[i])
      }
    }
  
    for (i in listOfEdits) {
      if (is.na(Mteams$tid[i])) {
        Mteams$tid[i] <- UUIDgenerate()
      }
    }
  }
  
  new_teams <- data.frame(name = unique(MeditedTeams[which(!(MeditedTeams %in% Mteams$name))]))
  
  if (length(!is.na(new_teams$name)) > 0) {
    new_teams <- new_teams[!is.na(new_teams$name),]
  }
  
  if (length(new_teams$name) > 0) {
    new_teams$firstrank <- NA
    new_teams$firstid <- NA
    new_teams$secondrank <- NA
    new_teams$secondid <- NA
    new_teams$thirdrank <- NA
    new_teams$thirdid <- NA
    new_teams$fourthrank <- NA
    new_teams$fourthid <- NA
    new_teams$fifthrank <- NA
    new_teams$fifthid <- NA
    new_teams$teamrank <- NA
    
    for (i in 1:length(new_teams$name)) {
      team <- Mrunners[which(tolower(Mrunners$team) == tolower(new_teams$name[i])),]
      team <- team %>% arrange(desc(rank))
      new_teams$teamrank[i] <- sum(as.numeric(team$rank[1:5]))
      if (length(team$rank) >= 5) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
        new_teams$fourthrank[i] <- team$rank[4]
        new_teams$fourthid[i] <- team$rid[4]
        new_teams$fifthrank[i] <- team$rank[5]
        new_teams$fifthid[i] <- team$rid[5]
      } else if (length(team$rank) == 4) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
        new_teams$fourthrank[i] <- team$rank[4]
        new_teams$fourthid[i] <- team$rid[4]
      } else if (length(team$rank) == 3) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
      } else if (length(team$rank) == 2) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
      } else if (length(team$rank) == 1) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
      }
    }
    
    new_teams$gender <- "M"
    new_teams$rank <- 0
    new_teams$tid <- NA
    new_teams$div <- NA
    new_teams$conf <- NA
    new_teams$reg <- NA
    new_teams$prev_rate <- NA
    new_teams$prev_rank <- NA
    
    Mteams <- data.frame(rbind(Mteams, new_teams))
  }
  
  for (i in which(is.na(Mteams$tid))) {
    Mteams$tid[i] <- UUIDgenerate()
  }
    
  if (length(which(tolower(Mteams$name) == "unattached")) > 0) {
    Mteams <- Mteams[-which(tolower(Mteams$name) == "unattached"),]
  }
  if (length(which(!is.na(Mteams$firstrank))) > 0) {
    Mteams <- Mteams[which(!is.na(Mteams$firstrank)),]
  }

  Mteams$newcount <- 5
  for (i in 1:length(Mteams$rank)) {
    if (length(which(is.na(Mteams[i,c(4,6,8,10,12)]))) > 0) {
      if (length(which(is.na(Mteams[i,c(4,6,8,10)]))) > 0) {
        if (length(which(is.na(Mteams[i,c(4,6,8)]))) > 0) {
          if (length(which(is.na(Mteams[i,c(4,6)]))) > 0) {
            Mteams$newcount[i] <- 1
          } else {
            Mteams$newcount[i] <- 2
          }
        } else {
          Mteams$newcount[i] <- 3
        }
      } else {
        Mteams$newcount[i] <- 4
      }
    }
  }
  
  Mteams <- Mteams %>% arrange(desc(teamrank), desc(newcount), 
                               desc(firstrank), desc(secondrank), desc(thirdrank), desc(fourthrank), desc(fifthrank))
  Mrunners$rank <- as.numeric(Mrunners$rank)
  
  Mrunners <- Mrunners %>% 
    arrange(desc(rank))
  
  for (i in 1:length(Mteams$rank)) {
    Mteams$rank[i] <- i
  }
  for (i in 1:length(Mrunners$place)) {
    Mrunners$place[i] <- i
  }
  
  Mteams <- Mteams %>% select(-newcount)
  
  for (i in which(is.na(Mrunners$tid))) {
    Mrunners$tid[i] <- ifelse(length(Mteams[which(tolower(Mrunners[i,]$team) == tolower(Mteams$name)),]$tid)>0,
                              Mteams[which(tolower(Mrunners[i,]$team) == tolower(Mteams$name)),]$tid, NA)
  }
  
  Mrunners <- Mrunners %>% 
    arrange(desc(rank)) %>% 
    arrange(active) %>% 
    arrange(team)
  
  Mrunners$uniqueTableID <- rownames(Mrunners)
  
  runners_table <- Mrunners
  teams_table <- Mteams
  
  # no_runners <- data.frame(team = no_runners[-1,])
  # write.csv(no_runners, "errors/Mempty_teams.csv", row.names = FALSE)
  
  
  ###################### WOMEN ##############################
  
  Wrunners <- Wrunners_table
  # oldWrunners <- read.csv("database files/Wrunners.csv")
  Wteams <- Wteams_table
  
  Wrunners$place[Wrunners$place == ""] <- NA
  Wrunners$RUNNERID <- paste0(Wrunners$last, Wrunners$first, Wrunners$team)
  Wrunners$rid[Wrunners$rid == 0] <- NA
  Wrunners$tid[Wrunners$tid == 0] <- NA
  # Wrunners$override_rank[Wrunners$override_rank == ""] <- NA
  Wrunners$rank[Wrunners$rank == ""] <- NA
  Wrunners$previous_rank[Wrunners$previous_rank == ""] <- NA
  Wrunners$previous_place[Wrunners$previous_place == ""] <- NA
  Wrunners$active[Wrunners$active == ""] <- "active"
  Wrunners$rank <- ifelse(is.na(Wrunners$override_rank), Wrunners$rank, Wrunners$override_rank)
  Wrunners$override_rank <- NA
  # Wteams$prev_rate <- Wteams$teamrank
  # Wteams$prev_rank <- Wteams$rank
  # Wrunners$previous_rank <- Wrunners$rank
  # Wrunners$previous_place <- Wrunners$place
  
  if (length(which(Wrunners$team == "")) > 0) {
    Wrunners <- Wrunners[-which(Wrunners$team == ""),]
  }
  if (length(which(tolower(Wrunners$team) == "unattached")) > 0) {
    Wrunners <- Wrunners[-which(tolower(Wrunners$team) == "unattached"),]
  }
  
  if (length(which(Wrunners$active == "inactive")) > 0) {
    inactives <- Wrunners[which(Wrunners$active == "inactive"),]  
  }
  
  Wrunners <- Wrunners[which(Wrunners$active == "active"),] %>% 
    arrange(desc(rank))
  
  for (i in c(which(is.na(Wrunners$rid)), which(Wrunners$rid == ""))) {
    Wrunners$rid[i] <- UUIDgenerate()
  }
  
  theEditedTeams <- data.frame(team = Wrunners$team[which(Wrunners$uniqueTableID %in% Wedited$uniqueTableID)])
  WeditedTeams <- data.frame(team = unique(c(theEditedTeams$team, Wedited$team))[-which(is.na(unique(c(theEditedTeams$team, Wedited$team))))])
  WdeletedTeams <- Wteams$name[which(Wteams$name %in% Wdeleted$team)]
  
  listOfEdits <- which(Wteams$name %in% c(WeditedTeams, WdeletedTeams))
  
  if (length(listOfEdits) > 0) {
    Wteams$firstrank[listOfEdits] <- NA
    Wteams$firstid[listOfEdits] <- NA
    Wteams$secondrank[listOfEdits] <- NA
    Wteams$secondid[listOfEdits] <- NA
    Wteams$thirdrank[listOfEdits] <- NA
    Wteams$thirdid[listOfEdits] <- NA
    Wteams$fourthrank[listOfEdits] <- NA
    Wteams$fourthid[listOfEdits] <- NA
    Wteams$fifthrank[listOfEdits] <- NA
    Wteams$fifthid[listOfEdits] <- NA
    
    no_runners <- data.frame(NA)
    for (i in listOfEdits) {
      if (tolower(Wteams$name[i]) %in% tolower(Wrunners$team)) {
        team <- Wrunners[which(tolower(Wrunners$team) == tolower(Wteams$name[i])),]
        team <- team %>% arrange(desc(rank))
        Wteams$teamrank[i] <- sum(as.numeric(team$rank[1:5]))
        if (length(team$rank) >= 5) {
          Wteams$firstrank[i] <- team$rank[1]
          Wteams$firstid[i] <- team$rid[1]
          Wteams$secondrank[i] <- team$rank[2]
          Wteams$secondid[i] <- team$rid[2]
          Wteams$thirdrank[i] <- team$rank[3]
          Wteams$thirdid[i] <- team$rid[3]
          Wteams$fourthrank[i] <- team$rank[4]
          Wteams$fourthid[i] <- team$rid[4]
          Wteams$fifthrank[i] <- team$rank[5]
          Wteams$fifthid[i] <- team$rid[5]
        } else if (length(team$rank) == 4) {
          Wteams$firstrank[i] <- team$rank[1]
          Wteams$firstid[i] <- team$rid[1]
          Wteams$secondrank[i] <- team$rank[2]
          Wteams$secondid[i] <- team$rid[2]
          Wteams$thirdrank[i] <- team$rank[3]
          Wteams$thirdid[i] <- team$rid[3]
          Wteams$fourthrank[i] <- team$rank[4]
          Wteams$fourthid[i] <- team$rid[4]
        } else if (length(team$rank) == 3) {
          Wteams$firstrank[i] <- team$rank[1]
          Wteams$firstid[i] <- team$rid[1]
          Wteams$secondrank[i] <- team$rank[2]
          Wteams$secondid[i] <- team$rid[2]
          Wteams$thirdrank[i] <- team$rank[3]
          Wteams$thirdid[i] <- team$rid[3]
        } else if (length(team$rank) == 2) {
          Wteams$firstrank[i] <- team$rank[1]
          Wteams$firstid[i] <- team$rid[1]
          Wteams$secondrank[i] <- team$rank[2]
          Wteams$secondid[i] <- team$rid[2]
        } else if (length(team$rank) == 1) {
          Wteams$firstrank[i] <- team$rank[1]
          Wteams$firstid[i] <- team$rid[1]
        }
        
      } else {
        no_runners <- rbind(no_runners, Wteams$name[i])
      }
    }
    
  }
  
  new_teams <- data.frame(name = unique(WeditedTeams[which(!(WeditedTeams %in% Wteams$name))]))
  
  if (length(!is.na(new_teams$name)) > 0) {
    new_teams <- new_teams[!is.na(new_teams$name),]
  }
  
  if (length(new_teams$name) > 0) {
    new_teams$firstrank <- NA
    new_teams$firstid <- NA
    new_teams$secondrank <- NA
    new_teams$secondid <- NA
    new_teams$thirdrank <- NA
    new_teams$thirdid <- NA
    new_teams$fourthrank <- NA
    new_teams$fourthid <- NA
    new_teams$fifthrank <- NA
    new_teams$fifthid <- NA
    new_teams$teamrank <- NA
    
    for (i in 1:length(new_teams$name)) {
      team <- Wrunners[which(tolower(Wrunners$team) == tolower(new_teams$name[i])),]
      team <- team %>% arrange(desc(rank))
      new_teams$teamrank[i] <- sum(as.numeric(team$rank[1:5]))
      if (length(team$rank) >= 5) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
        new_teams$fourthrank[i] <- team$rank[4]
        new_teams$fourthid[i] <- team$rid[4]
        new_teams$fifthrank[i] <- team$rank[5]
        new_teams$fifthid[i] <- team$rid[5]
      } else if (length(team$rank) == 4) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
        new_teams$fourthrank[i] <- team$rank[4]
        new_teams$fourthid[i] <- team$rid[4]
      } else if (length(team$rank) == 3) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
        new_teams$thirdrank[i] <- team$rank[3]
        new_teams$thirdid[i] <- team$rid[3]
      } else if (length(team$rank) == 2) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
        new_teams$secondrank[i] <- team$rank[2]
        new_teams$secondid[i] <- team$rid[2]
      } else if (length(team$rank) == 1) {
        new_teams$firstrank[i] <- team$rank[1]
        new_teams$firstid[i] <- team$rid[1]
      }
    }
    
    new_teams$gender <- "W"
    new_teams$rank <- 0
    new_teams$tid <- NA
    new_teams$div <- NA
    new_teams$conf <- NA
    new_teams$reg <- NA
    new_teams$prev_rate <- NA
    new_teams$prev_rank <- NA
    
    Wteams <- data.frame(rbind(Wteams, new_teams))
  }
  
  for (i in which(is.na(Wteams$tid))) {
    Wteams$tid[i] <- UUIDgenerate()
  }
  
  if (length(which(tolower(Wteams$name) == "unattached")) > 0) {
    Wteams <- Wteams[-which(tolower(Wteams$name) == "unattached"),]
  }
  if (length(which(!is.na(Wteams$firstrank))) > 0) {
    Wteams <- Wteams[which(!is.na(Wteams$firstrank)),]
  }
  
  Wteams$newcount <- 5
  for (i in 1:length(Wteams$rank)) {
    if (length(which(is.na(Wteams[i,c(4,6,8,10,12)]))) > 0) {
      if (length(which(is.na(Wteams[i,c(4,6,8,10)]))) > 0) {
        if (length(which(is.na(Wteams[i,c(4,6,8)]))) > 0) {
          if (length(which(is.na(Wteams[i,c(4,6)]))) > 0) {
            Wteams$newcount[i] <- 1
          } else {
            Wteams$newcount[i] <- 2
          }
        } else {
          Wteams$newcount[i] <- 3
        }
      } else {
        Wteams$newcount[i] <- 4
      }
    }
  }
  
  Wteams <- Wteams %>% arrange(desc(teamrank), desc(newcount), 
                               desc(firstrank), desc(secondrank), desc(thirdrank), desc(fourthrank), desc(fifthrank))
  Wrunners$rank <- as.numeric(Wrunners$rank)
  
  Wrunners <- Wrunners %>% 
    arrange(desc(rank))
  
  for (i in 1:length(Wteams$rank)) {
    Wteams$rank[i] <- i
  }
  for (i in 1:length(Wrunners$place)) {
    Wrunners$place[i] <- i
  }
  
  Wteams <- Wteams %>% select(-newcount)
  
  for (i in which(is.na(Wrunners$tid))) {
    Wrunners$tid[i] <- ifelse(length(Wteams[which(tolower(Wrunners[i,]$team) == tolower(Wteams$name)),]$tid)>0,
                              Wteams[which(tolower(Wrunners[i,]$team) == tolower(Wteams$name)),]$tid, NA)
  }
  
  Wrunners <- Wrunners %>% 
    arrange(desc(rank)) %>% 
    arrange(active) %>% 
    arrange(team)
  
  Wrunners$uniqueTableID <- rownames(Wrunners)
  
  Wrunners_table <- Wrunners
  Wteams_table <- Wteams
  
  # no_runners <- data.frame(team = no_runners[-1,])
  # write.csv(no_runners, "errors/Wempty_teams.csv", row.names = FALSE)
  
  list(runners_table, teams_table, Wrunners_table, Wteams_table)
}




