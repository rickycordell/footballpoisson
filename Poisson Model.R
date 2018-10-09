#Importing and formatting data
champ_data = read.csv("home_away_stats.csv", header = TRUE,
                        col.names = c("Team", "home_scored", "home_conceded",
                                      "away_scored", "away_conceded"), skip = 1,
                      row.names = teams)
teams<- c("aston_villa", "birmingham", "bolton", "blackburn", "brentford", "bristol", "derby", "hull", "ipswich", "leeds", "middlesbrough",
          "millwall", "norwich", "nottingham", "preston", "qpr", "reading", "rotherham", "sheffield_utd", "sheffield_weds",
          "stoke", "swansea", "west_brom", "wigan", "league average")
champ_data[,1]<- NULL


#Adding attack and defence ratings to data frame
champ_data$home_attack_rating<- champ_data[,"home_scored"]/champ_data["league average","home_scored"]
champ_data$home_defence_rating<- champ_data[,"home_conceded"]/champ_data["league average", "home_conceded"]
champ_data$away_attack_rating<- champ_data[,"away_scored"]/champ_data["league average", "away_scored"]
champ_data$away_defence_rating<- champ_data[,"away_conceded"]/champ_data["league average", "away_conceded"]

#Define poisson model
poisson_model <- function(home_team, away_team)
  
{
  home_scored <- champ_data[home_team,"home_scored"]
  home_conceded <- champ_data[home_team, "home_conceded"]
  away_scored <- champ_data[away_team, "away_scored"]
  away_conceded <- champ_data[away_team, "away_conceded"]
  league_home_goals <- champ_data["league average", "home_scored"]
  league_away_goals <- champ_data["league average", "away_scored"]
  home_attack_rating <- champ_data[home_team, "home_attack_rating"]
  away_attack_rating <- champ_data[away_team, "away_attack_rating"]
  home_defence_rating <- champ_data[home_team, "home_defence_rating"]
  away_defence_rating <- champ_data[away_team, "away_defence_rating"]
  
  
  num_trials = 1000000
  
  home_expected_goals <- home_attack_rating * away_defence_rating * league_home_goals
  away_expected_goals <- away_attack_rating * home_defence_rating * league_away_goals
  home_goal_pred <- rpois(num_trials, home_expected_goals)
  away_goal_pred <- rpois(num_trials, away_expected_goals)
  
  total_goals <- home_goal_pred + away_goal_pred
  mean_goals <- mean(total_goals)
  home_win_percentage <- (sum(home_goal_pred > away_goal_pred))/num_trials
  away_win_percentage <- (sum(home_goal_pred < away_goal_pred))/num_trials
  draw_percentage <- (sum(home_goal_pred == away_goal_pred))/num_trials
  over2.5_percentage <- (sum(total_goals > 2))/num_trials
  btts_percentage <- (sum(home_goal_pred > 0 & away_goal_pred > 0))/num_trials
  
  cat("\nFor", home_team, "vs",away_team, "The expected outcomes are:", "\n")
  
  cat("\nHome Win Percentage:", round(home_win_percentage*100,2), "%")
  cat("\nAway Win Percentage:", round(away_win_percentage*100,2),"%")
  cat("\nDraw Percentage:", round(draw_percentage*100,2),"%")
  cat("\nOver 2.5 goals Percentage:", round(over2.5_percentage*100,2),"%")
  cat("\nBoth teams to score Percentage:", round(btts_percentage*100,2),"%", "\n")
  
  cat("\nThe implied odds of these stats (factoring in bookies edge) are:")
  
  #calculating the implied odds with the bookies edge included
  imp_home_odds<- round((1/home_win_percentage)*1.05,2)
  imp_away_odds<- round((1/away_win_percentage)*1.05,2)
  imp_draw_odds<- round((1/draw_percentage)*1.05,2)
  imp_2.5_odds<- round((1/over2.5_percentage)*1.05,2)
  imp_btts_odds<- round((1/btts_percentage)*1.05,2)
  #printing these odds
  cat("\nHome Win:", imp_home_odds)
  cat("\nAway Win:", imp_away_odds)
  cat("\nDraw:", imp_draw_odds)
  cat("\nOver 2.5 goals:", imp_2.5_odds)
  cat("\nBoth teams to score:", imp_btts_odds)
  
  
} 


  