### Database - just teams
library(uuid)
library(dplyr)
library(stringr)

recalculateTeams <- function(runners_table, teams_table, Wrunners_table, Wteams_table) {
  Mrunners <- runners_table[-length(runners_table)]
  # oldMrunners <- read.csv("database files/Mrunners.csv")
  Mteams <- teams_table
  
  Mrunners$place[Mrunners$place == ""] <- NA
  Mrunners$RUNNER.ID <- paste0(Mrunners$last, Mrunners$first, Mrunners$team)
  Mrunners$rid[Mrunners$rid == 0] <- NA
  Mrunners$tid[Mrunners$tid == 0] <- NA
  Mrunners$override_rank[Mrunners$override_rank == ""] <- NA
  Mrunners$rank[Mrunners$rank == ""] <- NA
  Mrunners$previous_rank[Mrunners$previous_rank == ""] <- NA
  Mrunners$previous_place[Mrunners$previous_place == ""] <- NA
  Mrunners$active[Mrunners$active == ""] <- "active"
  Mrunners$rank <- ifelse(is.na(Mrunners$override_rank), Mrunners$rank, Mrunners$override_rank)
  Mrunners$override_rank <- NA
  
  if (length(which(Mrunners$team == "")) > 0) {
    Mrunners <- Mrunners[-which(Mrunners$team == ""),]
  }
  if (length(which(tolower(Mrunners$team) == "unattached")) > 0) {
    Mrunners <- Mrunners[-which(tolower(Mrunners$team) == "unattached"),]
  }
  
  inactives <- Mrunners[which(Mrunners$active == "inactive"),]
  Mrunners <- Mrunners[which(Mrunners$active == "active"),] %>% 
    arrange(desc(rank))
  
  for (i in 1:length(Mrunners$last)) {
    if (is.na(Mrunners$rid[i]) || Mrunners$rid[i] == "") {
      Mrunners$rid[i] <- UUIDgenerate()
    }
  }
  
  Mteams$firstrank <- NA
  Mteams$firstid <- NA
  Mteams$secondrank <- NA
  Mteams$secondid <- NA
  Mteams$thirdrank <- NA
  Mteams$thirdid <- NA
  Mteams$fourthrank <- NA
  Mteams$fourthid <- NA
  Mteams$fifthrank <- NA
  Mteams$fifthid <- NA
  
  no_runners <- data.frame(NA)
  for (i in 1:length(Mteams$name)) {
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
  
  for (i in 1:length(Mteams$name)) {
    if (is.na(Mteams$tid[i])) {
      Mteams$tid[i] <- UUIDgenerate()
    }
  }
  
  if (length(which(tolower(Mteams$name) == "unattached")) > 0) {
    Mteams <- Mteams[-which(tolower(Mteams$name) == "unattached"),]
  }
  if (length(which(!is.na(Mteams$firstrank))) > 0) {
    Mteams <- Mteams[which(!is.na(Mteams$firstrank)),]
  }
  
  Mteams <- Mteams %>% 
    arrange(desc(teamrank))
  Mrunners <- Mrunners %>% 
    arrange(desc(rank))
  
  
  for (i in 1:length(Mteams$rank)) {
    Mteams$rank[i] <- i
  }
  for (i in 1:length(Mrunners$place)) {
    Mrunners$place[i] <- i
  }
  
  Mrunners <- rbind(Mrunners, inactives)
  
  for (i in 1:length(Mrunners$rid)) {
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
  
  no_runners <- data.frame(team = no_runners[-1,])
  # write.csv(no_runners, "errors/Mempty_teams.csv", row.names = FALSE)
  
  
  ###################### WOMEN ##############################
  
  Wrunners <- Wrunners_table[-length(Wrunners_table)]
  # oldWrunners <- read.csv("database files/Wrunners.csv")
  Wteams <- Wteams_table
  
  Wrunners$place[Wrunners$place == ""] <- NA
  Wrunners$RUNNER.ID <- paste0(Wrunners$last, Wrunners$first, Wrunners$team)
  Wrunners$rid[Wrunners$rid == 0] <- NA
  Wrunners$tid[Wrunners$tid == 0] <- NA
  Wrunners$override_rank[Wrunners$override_rank == ""] <- NA
  Wrunners$rank[Wrunners$rank == ""] <- NA
  Wrunners$previous_rank[Wrunners$previous_rank == ""] <- NA
  Wrunners$previous_place[Wrunners$previous_place == ""] <- NA
  Wrunners$active[Wrunners$active == ""] <- "active"
  Wrunners$rank <- ifelse(is.na(Wrunners$override_rank), Wrunners$rank, Wrunners$override_rank)
  Wrunners$override_rank <- NA
  
  
  if (length(which(Wrunners$team == "")) > 0) {
    Wrunners <- Wrunners[-which(Wrunners$team == ""),]
  }
  if (length(which(tolower(Wrunners$team) == "unattached")) > 0) {
    Wrunners <- Wrunners[-which(tolower(Wrunners$team) == "unattached"),]
  }
  
  inactives <- Wrunners[which(Wrunners$active == "inactive"),]
  Wrunners <- Wrunners[which(Wrunners$active == "active"),] %>% 
    arrange(desc(rank))
  
  for (i in 1:length(Wrunners$last)) {
    if (is.na(Wrunners$rid[i]) || Wrunners$rid[i] == "") {
      Wrunners$rid[i] <- UUIDgenerate()
    }
  }
  
  Wteams$firstrank <- NA
  Wteams$firstid <- NA
  Wteams$secondrank <- NA
  Wteams$secondid <- NA
  Wteams$thirdrank <- NA
  Wteams$thirdid <- NA
  Wteams$fourthrank <- NA
  Wteams$fourthid <- NA
  Wteams$fifthrank <- NA
  Wteams$fifthid <- NA
  
  no_runners <- data.frame(NA)
  for (i in 1:length(Wteams$name)) {
    if (tolower(Wteams$name[i]) %in% tolower(Wrunners$team)) {
      Wteams$prev_rate[i] <- Wteams$teamrank[i]
      Wteams$prev_rank[i] <- Wteams$rank[i]
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
  
  for (i in 1:length(Wteams$name)) {
    if (is.na(Wteams$tid[i])) {
      Wteams$tid[i] <- UUIDgenerate()
    }
  }
  
  if (length(which(tolower(Wteams$name) == "unattached")) > 0) {
    Wteams <- Wteams[-which(tolower(Wteams$name) == "unattached"),]
  }
  if (length(which(!is.na(Wteams$firstrank))) > 0) {
    Wteams <- Wteams[which(!is.na(Wteams$firstrank)),]
  }
  
  Wteams <- Wteams %>% 
    arrange(desc(teamrank))
  Wrunners <- Wrunners %>% 
    arrange(desc(rank))
  
  
  for (i in 1:length(Wteams$rank)) {
    Wteams$rank[i] <- i
  }
  for (i in 1:length(Wrunners$place)) {
    Wrunners$place[i] <- i
  }
  
  Wrunners <- rbind(Wrunners, inactives)
  
  for (i in 1:length(Wrunners$rid)) {
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
  
  no_runners <- data.frame(team = no_runners[-1,])
  # write.csv(no_runners, "errors/Wempty_teams.csv", row.names = FALSE)
  list(runners_table, teams_table, Wrunners_table, Wteams_table)
}




