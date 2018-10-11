#Importing and formatting data

prem_teams<- c( "Arsenal", "Bournemouth", "Brighton", "Burnley", "Cardiff", "Chelsea", "Crystal Palace", "Everton", "Fulham", "Huddersfield", "Leicester",
                "Liverpool", "Manchester City", "Manchester United", "Newcastle", "Southampton", "Tottenham", "Watford", "West Ham", "Wolves", "league average")

prem_data = read.csv("prem_stats.csv", header = TRUE,
                     col.names = c("Team", "home_scored", "home_conceded",
                                   "away_scored", "away_conceded"), row.names = prem_teams)

prem_data[,1]<- NULL


#Adding attack and defence ratings to data frame
prem_data$home_attack_rating<- prem_data[,"home_scored"]/prem_data["league average","home_scored"]
prem_data$home_defence_rating<- prem_data[,"home_conceded"]/prem_data["league average", "home_conceded"]
prem_data$away_attack_rating<- prem_data[,"away_scored"]/prem_data["league average", "away_scored"]
prem_data$away_defence_rating<- prem_data[,"away_conceded"]/prem_data["league average", "away_conceded"]


#Arsenal vs Leicester
prem_model("Arsenal", "Leicester",
          book_home = 1.44, book_away = 6.50, book_draw=4.50,
          book_btts = 1.61, book_over2.5 = 1.5)


prem_model("Chelsea", "Manchester United",
          book_home = 1.66, book_away = 5.00, book_draw = 3.75,
          book_btts = 1.89, book_over2.5 = 1.90)








