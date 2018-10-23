#Define the function model
prem_model <- function(home_team, away_team, book_home,
                           book_away,book_draw, book_btts, book_over2.5)
  
{
  #Retrieves required information from the premiership stats data frame
  home_scored <- prem_data[home_team,"home_scored"]
  home_conceded <- prem_data[home_team, "home_conceded"]
  away_scored <- prem_data[away_team, "away_scored"]
  away_conceded <- prem_data[away_team, "away_conceded"]
  league_home_goals <- prem_data["league average", "home_scored"]
  league_away_goals <- prem_data["league average", "away_scored"]
  home_attack_rating <- prem_data[home_team, "home_attack_rating"]
  away_attack_rating <- prem_data[away_team, "away_attack_rating"]
  home_defence_rating <- prem_data[home_team, "home_defence_rating"]
  away_defence_rating <- prem_data[away_team, "away_defence_rating"]
  
  #Set the number of times the model will simulate each game
  num_trials = 1000000
  
  #create expected goals from data
  home_expected_goals <- home_attack_rating * away_defence_rating * league_home_goals
  away_expected_goals <- away_attack_rating * home_defence_rating * league_away_goals
  
  #Use poisson distribution to simulate how many goals each team will score
  home_goal_pred <- rpois(num_trials, home_expected_goals)
  away_goal_pred <- rpois(num_trials, away_expected_goals)
  
  #Calculate percentages of results
  total_goals <- home_goal_pred + away_goal_pred
  mean_goals <- mean(total_goals)
  home_win_percentage <<- (sum(home_goal_pred > away_goal_pred))/num_trials
  away_win_percentage <<- (sum(home_goal_pred < away_goal_pred))/num_trials
  draw_percentage <<- (sum(home_goal_pred == away_goal_pred))/num_trials
  over2.5_percentage <<- (sum(total_goals > 2))/num_trials
  btts_percentage <<- (sum(home_goal_pred > 0 & away_goal_pred > 0))/num_trials
  
  #Printing the expected outcomes
  cat("\nFor", home_team, "vs",away_team, "The expected outcomes are:", "\n","\n")
  
  cat(home_team, "Win Percentage:", round(home_win_percentage*100,2), "%","\n")
  cat(away_team, "Win Percentage:", round(away_win_percentage*100,2),"%")
  cat("\nDraw Percentage:", round(draw_percentage*100,2),"%")
  cat("\nOver 2.5 goals Percentage:", round(over2.5_percentage*100,2),"%")
  cat("\nBoth teams to score Percentage:", round(btts_percentage*100,2),"%", "\n")
  
  
  cat("\nThe implied odds are:","\n", "\n")
  
  #calculating the implied odds with the bookies edge included
  imp_home_odds<- round((1/home_win_percentage),2)
  imp_away_odds<- round((1/away_win_percentage),2)
  imp_draw_odds<- round((1/draw_percentage),2)
  imp_2.5_odds<- round((1/over2.5_percentage),2)
  imp_btts_odds<- round((1/btts_percentage),2)
  
  #printing these odds
  cat(home_team,":", imp_home_odds, "\n")
  cat(away_team,":", imp_away_odds)
  cat("\nDraw :", imp_draw_odds)
  cat("\nOver 2.5 goals :", imp_2.5_odds)
  cat("\nBoth teams to score :", imp_btts_odds, "\n")
  
  
  #Using Kelly Criterion to find the value of the bets
  home_wager_amount= round((((book_home - 1) * home_win_percentage) - (1 - home_win_percentage))/ (book_home-1),3)
  away_wager_amount= round((((book_away - 1) * away_win_percentage) - (1 - away_win_percentage))/ (book_away-1),3)
  draw_wager_amount= round((((book_draw - 1) * draw_percentage) - (1 - draw_percentage))/ (book_draw-1),3)
  over2.5_wager_amount= round((((book_over2.5 - 1) * over2.5_percentage) - (1 - over2.5_percentage))/ (book_over2.5-1),3)
  btts_wager_amount= round((((book_btts - 1) * btts_percentage) - (1 - btts_percentage))/ (book_btts-1),3)
  
  cat("\nThe Value of the bets are:")
  
  cat("\nThe Value in betting on",home_team,"is:", home_wager_amount*100, "%")
  cat("\nThe Value in betting on",away_team,"is:", away_wager_amount*100, "%")
  cat("\nThe Value in betting on the draw is:", draw_wager_amount*100, "%")
  cat("\nThe Value in betting on over 2.5 goals is:", over2.5_wager_amount*100, "%")
  cat("\nThe Value in betting on both teams to score is", btts_wager_amount*100, "%")
  
  
  
}





