---
title: "Using the poisson distribution to predict Premier League Matches"
output:
  html_document:
    keep_md: yes
---

The aim of this program is to predict premier league matches using the poisson distribution. The Kelly Criterion will then be used in order to compare the predicted results with bet365's odds in order to see if there is any value in the bets. 

First a vector of all 20 EPL teams will be created, along with a value for the league average. 


```r
prem_teams<- c( "Arsenal", "Bournemouth", "Brighton", "Burnley", "Cardiff", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Huddersfield", "Leicester", "Liverpool", "Manchester City", "Manchester United", "Newcastle","Southampton", "Tottenham", "Watford", "West Ham", "Wolves", "league average")
```

The read.csv function will then be used to import a csv file of each teams Average goals scored and conceded both home and away


```r
#Import data
prem_data = read.csv("prem_stats.csv", header = TRUE,
                     col.names = c("Team", "home_scored", "home_conceded",
                                   "away_scored", "away_conceded"), row.names = prem_teams)
#Remove first column
prem_data[,1]<- NULL
```

First lets check it's working by trying to retrieve some data

```r
prem_data
```

```
##                   home_scored home_conceded away_scored away_conceded
## Arsenal                  1.75          0.75        3.00          1.75
## Bournemouth              2.50          1.25        1.50          1.75
## Brighton                 1.75          1.50        0.50          1.75
## Burnley                  1.50          1.50        1.00          1.50
## Cardiff                  0.75          2.50        0.25          1.75
## Chelsea                  2.50          1.00        2.00          0.25
## Crystal Palace           0.00          1.25        1.25          1.00
## Everton                  1.75          1.25        1.50          1.75
## Fulham                   1.50          2.50        0.75          2.75
## Huddersfield             0.00          1.50        1.00          2.75
## Leicester                1.75          1.25        1.75          1.75
## Liverpool                2.00          0.00        1.75          0.75
## Manchester City          3.25          0.50        2.00          0.25
## Manchester United        1.50          1.75        1.75          1.75
## Newcastle                0.75          2.00        0.75          1.25
## Southampton              0.75          1.75        0.75          1.75
## Tottenham                1.67          1.00        2.00          0.80
## Watford                  1.40          1.60        1.33          1.33
## West Ham                 1.00          1.00        1.00          2.25
## Wolves                   1.50          0.75        0.75          0.75
## league average           1.48          1.33        1.33          1.48
```

For each team ratings will be created for both their home and away attack ratings, as well as their home and away defence ratings. This will allow us to compare each team to the league average. The attack ratings will be created by dividing their average goals scored by the league average, similarly the defence ratings will be created by dividing their average goals conceded by the league average.

```r
#Adding attack and defence ratings to data frame
prem_data$home_attack_rating<- prem_data[,"home_scored"]/prem_data["league average","home_scored"]
prem_data$home_defence_rating<- prem_data[,"home_conceded"]/prem_data["league average", "home_conceded"]
prem_data$away_attack_rating<- prem_data[,"away_scored"]/prem_data["league average", "away_scored"]
prem_data$away_defence_rating<- prem_data[,"away_conceded"]/prem_data["league average", "away_conceded"]
```

Now we have our complete data frame let's have a look at some data

```r
#To view Arsenal's stats
prem_data["Arsenal",]
```

```
##         home_scored home_conceded away_scored away_conceded
## Arsenal        1.75          0.75           3          1.75
##         home_attack_rating home_defence_rating away_attack_rating
## Arsenal           1.182432           0.5639098           2.255639
##         away_defence_rating
## Arsenal            1.182432
```

```r
#To view all team's home attack ratings
prem_data[5]
```

```
##                   home_attack_rating
## Arsenal                    1.1824324
## Bournemouth                1.6891892
## Brighton                   1.1824324
## Burnley                    1.0135135
## Cardiff                    0.5067568
## Chelsea                    1.6891892
## Crystal Palace             0.0000000
## Everton                    1.1824324
## Fulham                     1.0135135
## Huddersfield               0.0000000
## Leicester                  1.1824324
## Liverpool                  1.3513514
## Manchester City            2.1959459
## Manchester United          1.0135135
## Newcastle                  0.5067568
## Southampton                0.5067568
## Tottenham                  1.1283784
## Watford                    0.9459459
## West Ham                   0.6756757
## Wolves                     1.0135135
## league average             1.0000000
```

Now we have our data frame working it is time to create our model

```r
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
```

Now the function has been imported it can be used on the premier league fixtures to find expected results and the implied oddss from this, as well as the value of the bet. For example for Chelsea vs Man United:

```r
#Chelsea vs Manchester United
prem_model("Chelsea", "Manchester United",
          book_home = 1.75, book_away = 5.25, book_draw = 3.85,
          book_btts = 1.75, book_over2.5 = 1.80)
```

```
## 
## For Chelsea vs Manchester United The expected outcomes are: 
##  
## Chelsea Win Percentage: 70.75 % 
## Manchester United Win Percentage: 14.21 %
## Draw Percentage: 15.04 %
## Over 2.5 goals Percentage: 79.97 %
## Both teams to score Percentage: 69.4 % 
## 
## The implied odds are: 
##  
## Chelsea : 1.41 
## Manchester United : 7.04
## Draw : 6.65
## Over 2.5 goals : 1.25
## Both teams to score : 1.44 
## 
## The Value of the bets are:
## The Value in betting on Chelsea is: 31.7 %
## The Value in betting on Manchester United is: -6 %
## The Value in betting on the draw is: -14.8 %
## The Value in betting on over 2.5 goals is: 54.9 %
## The Value in betting on both teams to score is 28.6 %
```

Here we can see our model gives the odds for over 2.5 goals to be 1.25 (as 79% of our one million simulations ended with over 2.5 goals) whilst bet365 are offering odds of 1.90. This is then converted using the Kelly Criterion to suggest we stake 54.7% of our bankroll on over 2.5 goals.


The model can be used to run all fixtures in a weekend.

```r
#Arsenal vs Leicester
prem_model("Arsenal", "Leicester",
          book_home = 1.53, book_away = 6.50, book_draw=3.60,
          book_btts = 1.57, book_over2.5 = 1.44)
```

```
## 
## For Arsenal vs Leicester The expected outcomes are: 
##  
## Arsenal Win Percentage: 62.35 % 
## Leicester Win Percentage: 17.14 %
## Draw Percentage: 20.51 %
## Over 2.5 goals Percentage: 58.86 %
## Both teams to score Percentage: 54.77 % 
## 
## The implied odds are: 
##  
## Arsenal : 1.6 
## Leicester : 5.84
## Draw : 4.88
## Over 2.5 goals : 1.7
## Both teams to score : 1.83 
## 
## The Value of the bets are:
## The Value in betting on Arsenal is: -8.7 %
## The Value in betting on Leicester is: 2.1 %
## The Value in betting on the draw is: -10.1 %
## The Value in betting on over 2.5 goals is: -34.6 %
## The Value in betting on both teams to score is -24.6 %
```

```r
#Cardiff vs Fulham
prem_model("Cardiff", "Fulham",
           book_home = 2.55, book_away = 2.90, book_draw = 3.50,
           book_btts = 1.66, book_over2.5 = 1.80)
```

```
## 
## For Cardiff vs Fulham The expected outcomes are: 
##  
## Cardiff Win Percentage: 37.01 % 
## Fulham Win Percentage: 37.7 %
## Draw Percentage: 25.29 %
## Over 2.5 goals Percentage: 53.13 %
## Both teams to score Percentage: 56.79 % 
## 
## The implied odds are: 
##  
## Cardiff : 2.7 
## Fulham : 2.65
## Draw : 3.95
## Over 2.5 goals : 1.88
## Both teams to score : 1.76 
## 
## The Value of the bets are:
## The Value in betting on Cardiff is: -3.6 %
## The Value in betting on Fulham is: 4.9 %
## The Value in betting on the draw is: -4.6 %
## The Value in betting on over 2.5 goals is: -5.5 %
## The Value in betting on both teams to score is -8.7 %
```

```r
#Manchester City vs Burnley
prem_model("Manchester City", "Burnley",
           book_home = 1.083, book_away = 34.00, book_draw = 13.00,
           book_btts = 2.50, book_over2.5 = 1.28)
```

```
## 
## For Manchester City vs Burnley The expected outcomes are: 
##  
## Manchester City Win Percentage: 91.17 % 
## Burnley Win Percentage: 2 %
## Draw Percentage: 6.84 %
## Over 2.5 goals Percentage: 70.89 %
## Both teams to score Percentage: 30.21 % 
## 
## The implied odds are: 
##  
## Manchester City : 1.1 
## Burnley : 50.1
## Draw : 14.63
## Over 2.5 goals : 1.41
## Both teams to score : 3.31 
## 
## The Value of the bets are:
## The Value in betting on Manchester City is: -15.2 %
## The Value in betting on Burnley is: -1 %
## The Value in betting on the draw is: -0.9 %
## The Value in betting on over 2.5 goals is: -33.1 %
## The Value in betting on both teams to score is -16.3 %
```

```r
#Newcastle vs Brighton
prem_model("Newcastle", "Brighton",
           book_home = 2.20, book_away = 3.80, book_draw = 3.25,
           book_btts = 2.05, book_over2.5 = 2.50)
```

```
## 
## For Newcastle vs Brighton The expected outcomes are: 
##  
## Newcastle Win Percentage: 36.58 % 
## Brighton Win Percentage: 28.72 %
## Draw Percentage: 34.7 %
## Over 2.5 goals Percentage: 22.66 %
## Both teams to score Percentage: 31.03 % 
## 
## The implied odds are: 
##  
## Newcastle : 2.73 
## Brighton : 3.48
## Draw : 2.88
## Over 2.5 goals : 4.41
## Both teams to score : 3.22 
## 
## The Value of the bets are:
## The Value in betting on Newcastle is: -16.3 %
## The Value in betting on Brighton is: 3.3 %
## The Value in betting on the draw is: 5.7 %
## The Value in betting on over 2.5 goals is: -28.9 %
## The Value in betting on both teams to score is -34.7 %
```

```r
#West Ham vs Tottenham
prem_model("West Ham", "Tottenham",
           book_home = 4.10, book_away = 1.90, book_draw = 3.90,
           book_btts = 1.57, book_over2.5 = 1.66)
```

```
## 
## For West Ham vs Tottenham The expected outcomes are: 
##  
## West Ham Win Percentage: 13.2 % 
## Tottenham Win Percentage: 61.01 %
## Draw Percentage: 25.79 %
## Over 2.5 goals Percentage: 33.54 %
## Both teams to score Percentage: 32.46 % 
## 
## The implied odds are: 
##  
## West Ham : 7.58 
## Tottenham : 1.64
## Draw : 3.88
## Over 2.5 goals : 2.98
## Both teams to score : 3.08 
## 
## The Value of the bets are:
## The Value in betting on West Ham is: -14.8 %
## The Value in betting on Tottenham is: 17.7 %
## The Value in betting on the draw is: 0.2 %
## The Value in betting on over 2.5 goals is: -67.2 %
## The Value in betting on both teams to score is -86 %
```

```r
#Wolves vs Watford
prem_model("Wolves", "Watford",
           book_home = 1.75, book_away = 5.25, book_draw = 3.75,
           book_btts = 1.95, book_over2.5 = 2.00)
```

```
## 
## For Wolves vs Watford The expected outcomes are: 
##  
## Wolves Win Percentage: 50.98 % 
## Watford Win Percentage: 20.77 %
## Draw Percentage: 28.25 %
## Over 2.5 goals Percentage: 34.94 %
## Both teams to score Percentage: 39.05 % 
## 
## The implied odds are: 
##  
## Wolves : 1.96 
## Watford : 4.81
## Draw : 3.54
## Over 2.5 goals : 2.86
## Both teams to score : 2.56 
## 
## The Value of the bets are:
## The Value in betting on Wolves is: -14.4 %
## The Value in betting on Watford is: 2.1 %
## The Value in betting on the draw is: 2.2 %
## The Value in betting on over 2.5 goals is: -30.1 %
## The Value in betting on both teams to score is -25.1 %
```

```r
#Huddersfield vs Liverpool
prem_model("Huddersfield", "Liverpool",
           book_home = 11.00, book_away = 1.33, book_draw = 5.50,
           book_btts = 2.25, book_over2.5 = 1.80)
```

```
## 
## For Huddersfield vs Liverpool The expected outcomes are: 
##  
## Huddersfield Win Percentage: 0 % 
## Liverpool Win Percentage: 86.08 %
## Draw Percentage: 13.92 %
## Over 2.5 goals Percentage: 31.66 %
## Both teams to score Percentage: 0 % 
## 
## The implied odds are: 
##  
## Huddersfield : Inf 
## Liverpool : 1.16
## Draw : 7.18
## Over 2.5 goals : 3.16
## Both teams to score : Inf 
## 
## The Value of the bets are:
## The Value in betting on Huddersfield is: -10 %
## The Value in betting on Liverpool is: 43.9 %
## The Value in betting on the draw is: -5.2 %
## The Value in betting on over 2.5 goals is: -53.8 %
## The Value in betting on both teams to score is -80 %
```

```r
#Everton vs Crystal Palace
prem_model("Everton", "Crystal Palace",
           book_home = 1.85, book_away = 4.75, book_draw = 3.60,
           book_btts = 1.80, book_over2.5 = 2.00)
```

```
## 
## For Everton vs Crystal Palace The expected outcomes are: 
##  
## Everton Win Percentage: 36.21 % 
## Crystal Palace Win Percentage: 35.79 %
## Draw Percentage: 28 %
## Over 2.5 goals Percentage: 41.89 %
## Both teams to score Percentage: 47.89 % 
## 
## The implied odds are: 
##  
## Everton : 2.76 
## Crystal Palace : 2.79
## Draw : 3.57
## Over 2.5 goals : 2.39
## Both teams to score : 2.09 
## 
## The Value of the bets are:
## The Value in betting on Everton is: -38.8 %
## The Value in betting on Crystal Palace is: 18.7 %
## The Value in betting on the draw is: 0.3 %
## The Value in betting on over 2.5 goals is: -16.2 %
## The Value in betting on both teams to score is -17.3 %
```

```r
#Bournemouth vs Southampton
prem_model("Bournemouth", "Southampton",
           book_home = 2.05, book_away = 3.75, book_draw = 3.70,
           book_btts = 1.66, book_over2.5 = 1.80)
```

```
## 
## For Bournemouth vs Southampton The expected outcomes are: 
##  
## Bournemouth Win Percentage: 82.54 % 
## Southampton Win Percentage: 6.02 %
## Draw Percentage: 11.43 %
## Over 2.5 goals Percentage: 70.71 %
## Both teams to score Percentage: 47.87 % 
## 
## The implied odds are: 
##  
## Bournemouth : 1.21 
## Southampton : 16.6
## Draw : 8.75
## Over 2.5 goals : 1.41
## Both teams to score : 2.09 
## 
## The Value of the bets are:
## The Value in betting on Bournemouth is: 65.9 %
## The Value in betting on Southampton is: -28.2 %
## The Value in betting on the draw is: -21.4 %
## The Value in betting on over 2.5 goals is: 34.1 %
## The Value in betting on both teams to score is -31.1 %
```

We can see that the best value bets are: Chelsea to beat Man United and over 2.5 goals, Crystal Palace to beat Everton, Liverpool to beat Huddersfield and Tottenham to beat West Ham. 





