### update.R
library(jsonlite)

updateFirebase <- function(MensRunners, WomensRunners, MensTeams, WomensTeams) {
  ############ MEN
  testrunners <- MensRunners
  testrunners <- testrunners[which(testrunners$active == "active"),]
  if (length(which(colnames(testrunners) %in% c("uniqueTableID"))) > 0) {
    testrunners <- testrunners[,-which(colnames(testrunners) %in% c("uniqueTableID"))]
  }
  testrunners$place <- as.integer(testrunners$place)
  testrunners$rank <- as.numeric(testrunners$rank)
  testrunners$previous_rank <- as.numeric(testrunners$previous_rank)
  testrunners$previous_place <- as.integer(testrunners$previous_place)
  write_json(testrunners, "www/runners.json")
  test_teams <- MensTeams
  test_teams$rank <- as.integer(test_teams$rank)
  test_teams$prev_rank <- as.integer(test_teams$prev_rank)
  test_teams$teamrank <- as.numeric(test_teams$teamrank)
  test_teams$prev_rate <- as.numeric(test_teams$prev_rate)
  test_teams$firstrank <- as.numeric(test_teams$firstrank)
  test_teams$secondrank <- as.numeric(test_teams$secondrank)
  test_teams$thirdrank <- as.numeric(test_teams$thirdrank)
  test_teams$fourthrank <- as.numeric(test_teams$fourthrank)
  test_teams$fifthrank <- as.numeric(test_teams$fifthrank)
  write_json(test_teams, "www/teams.json")
  
  
  ############ WOMEN
  Wtestrunners <- WomensRunners
  Wtestrunners <- Wtestrunners[which(Wtestrunners$active == "active"),]
  if (length(which(colnames(Wtestrunners) %in% c("uniqueTableID"))) > 0) {
    Wtestrunners <- Wtestrunners[,-which(colnames(Wtestrunners) %in% c("uniqueTableID"))]
  }
  Wtestrunners$place <- as.integer(Wtestrunners$place)
  Wtestrunners$rank <- as.numeric(Wtestrunners$rank)
  Wtestrunners$previous_rank <- as.numeric(Wtestrunners$previous_rank)
  Wtestrunners$previous_place <- as.integer(Wtestrunners$previous_place)
  write_json(Wtestrunners, "www/Wrunners.json")
  Wtest_teams <- WomensTeams
  Wtest_teams$rank <- as.integer(Wtest_teams$rank)
  Wtest_teams$prev_rank <- as.integer(Wtest_teams$prev_rank)
  Wtest_teams$teamrank <- as.numeric(Wtest_teams$teamrank)
  Wtest_teams$prev_rate <- as.numeric(Wtest_teams$prev_rate)
  Wtest_teams$firstrank <- as.numeric(Wtest_teams$firstrank)
  Wtest_teams$secondrank <- as.numeric(Wtest_teams$secondrank)
  Wtest_teams$thirdrank <- as.numeric(Wtest_teams$thirdrank)
  Wtest_teams$fourthrank <- as.numeric(Wtest_teams$fourthrank)
  Wtest_teams$fifthrank <- as.numeric(Wtest_teams$fifthrank)
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