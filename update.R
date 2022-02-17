### update.R
library(jsonlite)

updateFirebase <- function(MensRunners, WomensRunners, MensTeams, WomensTeams) {
  ############ MEN
  testrunners <- MensRunners
  testrunners <- testrunners[which(testrunners$active == "active"),]
  if (length(which(colnames(testrunners) %in% c("uniqueTableID"))) > 0) {
    testrunners <- testrunners[,-which(colnames(testrunners) %in% c("uniqueTableID"))]
  }
  # testrunners$place <- as.integer(testrunners$place)
  # testrunners$rank <- as.numeric(testrunners$rank)
  # testrunners$previous_rank <- as.numeric(testrunners$previous_rank)
  # testrunners$previous_place <- as.integer(testrunners$previous_place)
  testrunners$override_rank[length(testrunners$rid)] <- testrunners$rank[length(testrunners$rid)]
  write_json(testrunners, "www/runners.json")
  test_teams <- MensTeams
  write_json(test_teams, "www/teams.json")
  
  
  ############ WOMEN
  Wtestrunners <- WomensRunners
  Wtestrunners <- Wtestrunners[which(Wtestrunners$active == "active"),]
  if (length(which(colnames(Wtestrunners) %in% c("uniqueTableID"))) > 0) {
    Wtestrunners <- Wtestrunners[,-which(colnames(Wtestrunners) %in% c("uniqueTableID"))]
  }
  # Wtestrunners$place <- as.integer(Wtestrunners$place)
  # Wtestrunners$rank <- as.numeric(Wtestrunners$rank)
  # Wtestrunners$previous_rank <- as.numeric(Wtestrunners$previous_rank)
  # Wtestrunners$previous_place <- as.integer(Wtestrunners$previous_place)
  Wtestrunners$override_rank[length(Wtestrunners$rid)] <- Wtestrunners$rank[length(Wtestrunners$rid)]
  write_json(Wtestrunners, "www/Wrunners.json")
  Wtest_teams <- WomensTeams
  write_json(Wtest_teams, "www/Wteams.json")
  
  updated <- read_json("www/updated.json")
  did <- as.character(updated[[1]]$did)
  write_json(data.frame(did=did, updated=substr(date(), 5, 10)), "www/updated.json")
  
  
  ##### Other Data
  div <- data.frame(name = unique(c(Wtest_teams$div, test_teams$div)))
  conf <- data.frame(name = unique(c(Wtest_teams$conf, test_teams$conf)))
  region <- data.frame(name = unique(c(Wtest_teams$reg, test_teams$reg)))
  
  write_json(div, "www/div.json")
  write_json(conf, "www/conf.json")
  write_json(region, "www/region.json")
  
  system("./gitUpdate.sh")
  system("./firebaseCmd.sh")
  
  return("All data updated")
}