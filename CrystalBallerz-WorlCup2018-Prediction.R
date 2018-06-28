#############################################################################################
#################################### FIFA 2018 Challenge ####################################
################## Authors : Vijay Narayanan, Sonali              ###########################
################## Non-upgrad Football Consultant - Vishal Sharma ###########################
#############################################################################################

library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

# Set working directory
setwd("~/Downloads/Upgrad-IIIT/WorldCup2018")

# Load files
# src: https://www.kaggle.com/agostontorok/soccer-world-cup-2018-winner/data
world_cup_dataset <- read.csv("World Cup 2018 Dataset.csv", stringsAsFactors = FALSE, header = TRUE)

# src: https://www.kaggle.com/agostontorok/soccer-world-cup-2018-winner/data
results <- read.csv("results.csv", stringsAsFactors = FALSE, header = TRUE)

# src: https://www.kaggle.com/agostontorok/soccer-world-cup-2018-winner/data
team_rankings <- read.csv("fifa_ranking.csv", stringsAsFactors = FALSE, header = TRUE)

# src : from https://www.eloratings.net/
elo_ratings <- read.csv("elo_ratings.csv", stringsAsFactors = FALSE, header = TRUE)


# Meta data
# 1. World Cup 2018 Data set has playing nations and match fixtures
# 2. Team Rankings dataset has FIFA rankings of teams from 1993 to 2018
# 3. Results dataset has results of football matches of all football nations from 1930 onwards
# 4. Elo ratings of all football nations. Elo rating system, developed by Dr. Arpad Elo is an useful rating
#    for a zero sum game such as football. Link: https://www.eloratings.net/about

# Data Cleaning

# Get all playing nations in World Cup 2018
playing_nations <- as.character(world_cup_dataset$Team)
playing_nations

# Remove missing value
playing_nations <- playing_nations[-33]
# Replace IRAN with Iran
playing_nations[8] <- "Iran"
# Replace Porugal with Portugal
playing_nations[5] <- "Portugal"
# Replace Costarica with Costa Rica
playing_nations[19] <- "Costa Rica"
# Replace Korea with South Korea
playing_nations[24] <- "South Korea"
# Replace Columbia with Colombia
playing_nations[31] <- "Colombia"

# Dataset - team_rankings
# Change country name "IR Iran" to "Iran"
team_rankings[team_rankings$country_full == "IR Iran", "country_full"] <- "Iran"
# Change country name "Korea Republic" to "South Korea"
team_rankings[team_rankings$country_full == "Korea Republic", "country_full"] <- "South Korea"
# Any missing values?
sum(is.na(team_rankings))
# Any duplicate values?
duplicate_rankings <- team_rankings[which(duplicated(team_rankings)), ]
unique(duplicate_rankings$country_full)
# All duplicate entries belong to country Sudan which is not playing in 2018 world cup. So, these records can be ignored.

# Filter out data only for playing nations
team_rankings <- team_rankings[team_rankings$country_full %in% playing_nations, ]

# Format Ranking Date to dd-mm-yyyy
ranking_date <- as.Date(team_rankings$rank_date, "%Y-%m-%d")
#team_rankings$rank_date <- format(as.Date(ranking_date), "%d-%m-%Y")
#team_rankings$rank_date <- as.Date(team_rankings$rank_date, "%d-%m-%Y")

# Dataset - results
# Change country name "Korea Republic" to "South Korea"
results[results$home_team == "Korea Republic", "home_team"] <- "South Korea"
results[results$away_team == "Korea Republic", "away_team"] <- "South Korea"

# Change format of date column
results_date <- as.Date(results$date, "%Y-%m-%d")
#results$date <- format(as.Date(results_date), "%d-%m-%Y")
#results$date <- as.Date(results$date, "%d-%m-%Y")

# Filter friendly matches as these matching have nothing at stake and may not be considered a serious encounter between teams
results <- filter(results, tournament != "Friendly")

# Filter out only tournaments matchin "FIFA World Cup" and "FIFA World Cup qualification"
results <- filter(results, tournament == "FIFA World Cup" | tournament == "FIFA World Cup qualification")

# Filter records of only playing nations
results <- filter(results, home_team %in% playing_nations & away_team %in% playing_nations)

# Are there duplicates?
sum(duplicated(results))
# Are there any missing values?
sum(is.na(results))

# Dataset - world_cup
searchAndReplace <- function(data, match, replace) {
  return(data.frame(sapply(data, function(x) { gsub(match, replace, x) })))
}

# Replace "Costarica" with "Costa Rica"
world_cup_dataset <- searchAndReplace(world_cup_dataset, "Costarica", "Costa Rica")
# Replace "IRAN" with "Iran"
world_cup_dataset <- searchAndReplace(world_cup_dataset, "IRAN", "Iran")
# Replace "Porugal" with "Portugal"
world_cup_dataset <- searchAndReplace(world_cup_dataset, "Porugal", "Portugal")
# Replace "Columbia" with "Colombia"
world_cup_dataset <- searchAndReplace(world_cup_dataset, "Columbia", "Colombia")
# Replace "Korea" with "South Korea"
world_cup_dataset <- searchAndReplace(world_cup_dataset, "Korea", "South Korea")

# Drop Empty Column X
world_cup_dataset <- world_cup_dataset[, -which(names(world_cup_dataset) %in% c("X"))]

# Drop Empty Row
world_cup_dataset <- filter(world_cup_dataset, Team != "")

# Summary of dataset
summary(world_cup_dataset)
# There appears to be some "N/A" values in numeric columns. Replacing these with 0
world_cup_dataset <- searchAndReplace(world_cup_dataset, "N/A", "0")

# Are there any missing values?
sum(is.na(world_cup_dataset))

# Are there any duplicate records
sum(duplicated(world_cup_dataset))

# Collate information from datasets
# Extract year from date in results dataset
results$match_year <- format(as.Date(results$date), "%Y")

# Filter out records for year > 1993 as Rankings information is only available since 1993
results <- results %>% filter(match_year >= "1993")

# Elo Ratings dataset
str(elo_ratings)

# Remove empty columns
elo_ratings <- elo_ratings[, -c(4,5)]

# Common function
getLatestRankingForNationsByYear <- function(data) {
  return(team_rankings %>% group_by(country_full, year = format(as.Date(rank_date), "%Y")) %>% arrange(desc(rank_date)) %>% slice(which.max(as.Date(rank_date))))
}

# Only fetch the latest ranking for a team in that year
latest_team_rankings <- getLatestRankingForNationsByYear(team_rankings)

# Combine Elo Rating for each country
latest_team_rankings <- merge(latest_team_rankings, elo_ratings, by.x = c("country_full", "year"), by.y = c("country", "year"), all.x = TRUE)

# Set N/A to 1300 - default elo rating for a football team
latest_team_rankings[is.na(latest_team_rankings$elo_rating), "elo_rating"] <- 1300

# Combine results with rankings for home team
home_combined <- merge(results, latest_team_rankings, by.x = c("home_team", "match_year"), by.y = c("country_full", "year"), all.x = TRUE)

# Combine results with rankings for away team
away_combined <- merge(results, latest_team_rankings, by.x = c("away_team", "match_year"), by.y = c("country_full", "year"), all.x = TRUE)

# Delete unwanted columns in home and away dataframes
remove_home_cols <- c("match_year", "away_team", "home_score", "away_score", "tournament", "city", "country", "neutral", "country_abrv", "confederation", "rank_date")
home_combined <- home_combined[, -which(colnames(home_combined) %in% remove_home_cols)]
home_combined <- rename(home_combined, home_rank = rank, home_total_points = total_points, home_previous_points = previous_points,
                        home_rank_change = rank_change, home_cur_year_avg = cur_year_avg, home_cur_year_avg_weighted = cur_year_avg_weighted,
                        home_last_year_avg = last_year_avg, home_last_year_avg_weighted = last_year_avg_weighted, home_two_year_ago_avg = two_year_ago_avg,
                        home_two_year_ago_weighted = two_year_ago_weighted, home_three_year_ago_avg = three_year_ago_avg,
                        home_three_year_ago_weighted = three_year_ago_weighted, home_elo_rating = elo_rating)
remove_away_cols <- c("match_year", "home_team", "home_score", "away_score", "tournament", "city", "country", "neutral", "country_abrv", "confederation", "rank_date")
away_combined <- away_combined[, -which(colnames(away_combined) %in% remove_away_cols)]
away_combined <- rename(away_combined, away_rank = rank, away_total_points = total_points, away_previous_points = previous_points,
                        away_rank_change = rank_change, away_cur_year_avg = cur_year_avg, away_cur_year_avg_weighted = cur_year_avg_weighted,
                        away_last_year_avg = last_year_avg, away_last_year_avg_weighted = last_year_avg_weighted, away_two_year_ago_avg = two_year_ago_avg,
                        away_two_year_ago_weighted = two_year_ago_weighted, away_three_year_ago_avg = three_year_ago_avg,
                        away_three_year_ago_weighted = three_year_ago_weighted, away_elo_rating = elo_rating)

# Merge home and away datasets
combined_results <- merge(results, home_combined, by.x = c("home_team", "date"), by.y = c("home_team", "date"), all = TRUE)
combined_results <- merge(combined_results, away_combined, by.x = c("away_team", "date"), by.y = c("away_team", "date"), all = TRUE)

# Reorder columns
combined_results <- combined_results[, c(2,3,1,4:36)]

# Populate outcome with 0 (home team loses), 0.5 (draw), 1 (home team wins)
combined_results$outcome <- ifelse(combined_results$home_score > combined_results$away_score, 1, ifelse(combined_results$home_score < combined_results$away_score, 0, 0.5))

# Omit rows with NA 
combined_results <- na.omit(combined_results)

# Copy of combined_results dataset
copy_combined_results <- combined_results

# Remove variables that are insignificant for this analysis
cols_remove <- c("date", "home_score", "away_score", "city", "country", "match_year") 
combined_results <- combined_results[, -which(colnames(combined_results) %in% cols_remove)]                 

# Create dummy vars for categorical variables
dummy_1 <- data.frame(model.matrix(~home_team-1, combined_results))
combined_results <- cbind(combined_results, dummy_1)

dummy_2 <- data.frame(model.matrix(~away_team-1, combined_results))
combined_results <- cbind(combined_results, dummy_2)

dummy_3 <- data.frame(model.matrix(~tournament-1, combined_results))
combined_results <- cbind(combined_results, dummy_3)

dummy_4 <- data.frame(model.matrix(~neutral-1, combined_results))
combined_results <- cbind(combined_results, dummy_4)

# Convert target variable to factor
combined_results$outcome <- as.factor(combined_results$outcome)

# Remove columns for which dummy has been added 
combined_results <- combined_results[, -c(1:4)]

# EDA
# Which teams have won the world cup?
previous_winners <- filter(world_cup_dataset, as.character(Previous..titles) > 0)
ggplot(previous_winners, aes(x=Team, y=Previous..titles)) + geom_bar(stat = "identity", fill = "darkgreen")
# Brazil has won the world cup on 5 previous occasions and Germany 4 times

# Which teams have reached the finals before?
previous_finalists <- filter(world_cup_dataset, as.character(Previous..finals) > 0)
ggplot(previous_finalists, aes(x=Team, y=Previous..finals)) + geom_bar(stat = "identity", fill = "lightblue")
# Germany have been in the finals 8 times, Brazil 7 times and Argentina 5 times

# Which teams have reached the semi finals before?
previous_sfinalists <- filter(world_cup_dataset, as.character(Previous..semifinals) > 0)
ggplot(previous_sfinalists, aes(x=Team, y=Previous..semifinals)) + geom_bar(stat = "identity", fill = "brown")
# Argentina, France and Uruguay have been in semi finals 5 times
# Sweden 4 times 

# Top 5 FIFA ranked playing nations
top5 <- as.data.frame(world_cup_dataset[world_cup_dataset$Current..FIFA.rank %in% c(1:5), c("Current..FIFA.rank", "Team")])
ggplot(top5, aes(x=Team, y=Current..FIFA.rank)) + geom_bar(stat = "identity", fill = "orange")

# Top 10 Elo Rated best playing nations
elo_10 <- elo_ratings[elo_ratings$year == "2018", ] %>% arrange(desc(elo_rating)) %>% head(10)
ggplot(elo_10, aes(x=country, y=elo_rating)) + geom_bar(stat = "identity", fill = "blue")

# Set seed
set.seed(123)

# Create Train and Test Datasets (70/30 split)
indices <- sample(1:nrow(combined_results), floor(0.7*nrow(combined_results)))
train = combined_results[indices,]
test = combined_results[-indices,]

##########################
# Random Forest Model
##########################

# Random Forest model with all features
randomForestModel <- randomForest(outcome ~ ., train, ntree = 30000, mtry = 5, nodesize = 0.01 * nrow(train))
predict_match <- predict(randomForestModel, test)


confusionMatrix(test$outcome, predict_match, positive = "1")
# Accuracy : 0.6115
# Class: 0 Class: 0.5 Class: 1
# Sensitivity            0.5357    0.40909   0.6854

# Which are the important variable to predict match outcome?
varImpPlot(randomForestModel)

# Important features
selected_features <- "outcome ~ home_rank + away_rank + home_previous_points + away_previous_points + home_rank_change + 
away_rank_change + home_cur_year_avg + home_cur_year_avg_weighted + home_two_year_ago_avg + home_two_year_ago_weighted +
away_cur_year_avg + away_cur_year_avg_weighted + home_elo_rating + away_elo_rating"
formula_1 <- as.formula(selected_features)

# Random Forest with selected features
randomForestModel_1 <- randomForest(formula_1, train, ntree = 30000, mtry = 5, nodesize = 0.01 * nrow(train))
predict_match_outcome <- predict(randomForestModel_1, test)
confusionMatrix(test$outcome, predict_match_outcome, positive = "1")
# Accuracy : 0.6259
#                       Class: 0 Class: 0.5 Class: 1
# Sensitivity           0.5625     0.4667   0.7143

# Common functions to generate fixtures dataset
getMatchFixtureData <- function(rank_data, orig_data, home_team, away_team) {
  home_team_stats <- rank_data[rank_data$country_full == home_team, ]
  home_except_country_name <- home_team_stats[, -1]
  colnames(home_except_country_name) <- paste("home", colnames(home_except_country_name), sep = "_")
  home_team_data <- cbind(home_team=home_team_stats$country_full, home_except_country_name)
  
  away_team_stats <- rank_data[rank_data$country_full == away_team, ]
  away_except_country_name <- away_team_stats[, -1]
  colnames(away_except_country_name) <- paste("away", colnames(away_except_country_name), sep = "_")
  away_team_data <- cbind(away_team=away_team_stats$country_full, away_except_country_name)

  match_data <- cbind(home_team_data, away_team_data)

  # Append original result
  acutal_result = orig_data[orig_data$home_team == home_team & orig_data$away_team == away_team & orig_data$match_year == "2018", "outcome"]
  if (length(acutal_result) == 0) {
    match_data <- cbind(match_data, acutal_result="N/A")
  } else {
    match_data <- cbind(match_data, acutal_result)
  }
  return(match_data)  
}

# Test predictions using Random Forest Model
# Create Test Data for Group Matches
cols_from_rankings <- c("country_full","rank", "previous_points", "rank_change", "cur_year_avg", "cur_year_avg_weighted",
                        "two_year_ago_avg", "two_year_ago_weighted", "total_points", "elo_rating")
rankings_all <- as.data.frame(latest_team_rankings[latest_team_rankings$country_full %in% playing_nations & latest_team_rankings$year == "2018", which(colnames(latest_team_rankings) %in% cols_from_rankings)])

# Group Phase -
# Win for a team is denoted by pred_outcome dependent variable having a value of 1
# Loss for a team is denoted by pred_outcome dependent variable having a value of 0
# Draw is denoted by pred_outcome dependent variable having a value of 0.5

# Group A
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Russia", "Saudi Arabia")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Egypt", "Uruguay")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Russia", "Egypt")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Uruguay", "Saudi Arabia")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Russia", "Uruguay")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Saudi Arabia", "Egypt")

groupA_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group A
rand_predict_outcomes <- predict(randomForestModel_1, groupA_match_data)
groupA_match_data$pred_outcome <- rand_predict_outcomes

# Group B
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Morocco", "Iran")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Portugal", "Spain")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Portugal", "Morocco")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Iran", "Spain")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Iran", "Portugal")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Spain", "Morocco")

groupB_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group B
rand_predict_outcomes <- predict(randomForestModel_1, groupB_match_data)
groupB_match_data$pred_outcome <- rand_predict_outcomes

# Group C
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "France", "Australia")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Peru", "Denmark")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Denmark", "Australia")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "France", "Peru")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Denmark", "France")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Australia", "Peru")

groupC_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group C
rand_predict_outcomes <- predict(randomForestModel_1, groupC_match_data)
groupC_match_data$pred_outcome <- rand_predict_outcomes

# Group D
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Argentina", "Iceland")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Croatia", "Nigeria")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Argentina", "Croatia")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Nigeria", "Iceland")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Nigeria", "Argentina")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Iceland", "Croatia")

groupD_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group D
rand_predict_outcomes <- predict(randomForestModel_1, groupD_match_data)
groupD_match_data$pred_outcome <- rand_predict_outcomes

# Group E
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Costa Rica", "Serbia")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Brazil", "Switzerland")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Brazil", "Costa Rica")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Serbia", "Switzerland")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Serbia", "Brazil")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Switzerland", "Costa Rica")

groupE_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group E
rand_predict_outcomes <- predict(randomForestModel_1, groupE_match_data)
groupE_match_data$pred_outcome <- rand_predict_outcomes

# Group F
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Germany", "Mexico")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Sweden", "South Korea")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "South Korea", "Mexico")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Germany", "Sweden")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Mexico", "Sweden")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "South Korea", "Germany")

groupF_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group F
rand_predict_outcomes <- predict(randomForestModel_1, groupF_match_data)
groupF_match_data$pred_outcome <- rand_predict_outcomes

# Group G
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Belgium", "Panama")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Tunisia", "England")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Belgium", "Tunisia")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "England", "Panama")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Panama", "Tunisia")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "England", "Belgium")

groupG_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group G
rand_predict_outcomes <- predict(randomForestModel_1, groupG_match_data)
groupG_match_data$pred_outcome <- rand_predict_outcomes

# Group H
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Colombia", "Japan")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Poland", "Senegal")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Japan", "Senegal")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Poland", "Colombia")
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Japan", "Poland")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Senegal", "Colombia")

groupH_match_data <- rbind(match1, match2, match3, match4, match5, match6)

# Using Random Forest to select winners of Group H
rand_predict_outcomes <- predict(randomForestModel_1, groupH_match_data)
groupH_match_data$pred_outcome <- rand_predict_outcomes

# Combine all group matches
group_matches_all <- rbind(groupA_match_data, groupB_match_data, groupC_match_data, groupD_match_data,
                           groupE_match_data, groupF_match_data, groupG_match_data, groupH_match_data)

# Write to a file
write.csv(group_matches_all, "RF_group_matches_results.csv", row.names = FALSE)

print("--- Predictions of Group Matches ---") 
print(group_matches_all[, c(1,11,21,22)])

# Group winners:
# Group A       Group B     Group C     Group D     Group E       Group F     Group G       Group H
# 1 Uruguay     Spain       France      Croatia     Brazil        Sweden      England ??    Japan??
# 2 Russia      Portugal    Denmark     Argentina   Switzerland   Mexico      Belgium ??    Senegal??

# Knock out phase - round 16
# Matches in knock out phase cannot have a draw. It will be based on a penalties. So, prediction can only have 
# a value of 0 or 1
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "Uruguay", "Portugal")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "France", "Argentina")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Brazil", "Mexico")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "England", "Senegal")  #This is likely to change?
match5 <- getMatchFixtureData(rankings_all, copy_combined_results, "Spain", "Russia")
match6 <- getMatchFixtureData(rankings_all, copy_combined_results, "Croatia", "Denmark")
match7 <- getMatchFixtureData(rankings_all, copy_combined_results, "Sweden", "Switzerland")
match8 <- getMatchFixtureData(rankings_all, copy_combined_results, "Japan", "Belgium") #This is likely to change?

knockout_match_data <- rbind(match1, match2, match3, match4, match5, match6, match7, match8)

# Using Random Forest to select winners of knockout matches. Use type = prob to get probabilities of
# a team winning or losing
knockout_predict_outcomes <- as.data.frame(predict(randomForestModel_1, knockout_match_data, type = "prob"))
# As draw is not an outcome, decide winner by checking probability of a win or lose
knockout_match_data$pred_outcome <- ifelse(knockout_predict_outcomes[3] > knockout_predict_outcomes[1], 1, 0)

print("--- Predictions of Knockout phase - Round 16 winners ---") 
print(knockout_match_data[, c(1,11,21,22)])
# Winners from knockout phase - round 16
# Portugal France Brazil England Spain Denmark Switzerland Belgium

# Quarter-finals
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "France", "Portugal")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Brazil", "England")
match3 <- getMatchFixtureData(rankings_all, copy_combined_results, "Spain", "Denmark")
match4 <- getMatchFixtureData(rankings_all, copy_combined_results, "Switzerland", "Belgium")

qfinal_match_data <- rbind(match1, match2, match3, match4)

# Select winners of quarter-final matches
qfinal_predict_outcomes <- as.data.frame(predict(randomForestModel_1, qfinal_match_data, type = "prob"))
qfinal_match_data$pred_outcome <- ifelse(qfinal_predict_outcomes[3] > qfinal_predict_outcomes[1], 1, 0)

print("--- Predictions of Quarter Finals winners ---") 
print(qfinal_match_data[, c(1,11,21,22)])
# France Brazil Denmark Switzerland

# Semi-finals
match1 <- getMatchFixtureData(rankings_all, copy_combined_results, "France", "Brazil")
match2 <- getMatchFixtureData(rankings_all, copy_combined_results, "Denmark", "Switzerland")

sfinal_match_data <- rbind(match1, match2)

# Select winners of semi-final matches
sfinal_predict_outcomes <- as.data.frame(predict(randomForestModel_1, sfinal_match_data, type = "prob"))
sfinal_match_data$pred_outcome <- ifelse(sfinal_predict_outcomes[3] > sfinal_predict_outcomes[1], 1, 0)

print("--- Predictions of Semi Finals winners ---") 
print(sfinal_match_data[, c(1,11,21,22)])
# Brazil Switzerland

# Finals
final_match <- getMatchFixtureData(rankings_all, copy_combined_results, "Brazil", "Switzerland")
final_predict_outcome <- predict(randomForestModel_1, final_match, type = "prob")
final_match$pred_outcome <- ifelse(final_predict_outcome[3] > final_predict_outcome[1], 1, 0)
print("--- Prediction of Final winner ---") 
print(final_match[, c(1,11,22)])
# Runner-up Switzerland
# Brazil to lift the cup
