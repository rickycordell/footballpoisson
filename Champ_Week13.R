#Importing and formatting data

champ_teams<- c("Aston Villa", "Birmingham", "Bolton", "Blackburn", "Brentford", "Bristol City", "Derby", "Hull", "Ipswich", "Leeds", "Middlesbrough",
          "Millwall", "Norwich", "Nottingham Forest", "Preston", "QPR", "Reading", "Rotherham", "Sheffield United", "Sheffield Wednesday",
          "Stoke", "Swansea", "West Brom", "Wigan", "league average")

champ_data = read.csv("Champ_Stats.csv", header = TRUE,
                      col.names = c("Team", "home_scored", "home_conceded",
                                    "away_scored", "away_conceded"), skip = 1,
                      row.names = champ_teams)

champ_data[,1]<- NULL


#Adding attack and defence ratings to data frame
champ_data$home_attack_rating<- champ_data[,"home_scored"]/champ_data["league average","home_scored"]
champ_data$home_defence_rating<- champ_data[,"home_conceded"]/champ_data["league average", "home_conceded"]
champ_data$away_attack_rating<- champ_data[,"away_scored"]/champ_data["league average", "away_scored"]
champ_data$away_defence_rating<- champ_data[,"away_conceded"]/champ_data["league average", "away_conceded"]


#Sheffield Wednesday vs Middlesbrough
champ_model("Sheffield Wednesday", "Middlesbrough",
               book_home = 3.40, book_away = 2.15, book_draw=3.30,
               book_btts = 1.83, book_over2.5 = 2.10)
                         
#Blackburn vs leeds
champ_model("Blackburn", "Leeds",
              book_home = 3.25, book_away = 2.20, book_draw=3.25,
              book_btts = 1.72, book_over2.5 = 2.0)

#Aston Villa vs Swansea
champ_model("Aston Villa", "Swansea", 
              book_home = 2.00, book_away = 4.00, book_draw=3.20,
              book_btts = 1.83, book_over2.5 = 2.00)

#Brenford vs Bristol City
champ_model("Brentford", "Bristol City",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Hull vs Preston
champ_model("Hull", "Preston",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Ipswich vs QPR
champ_model("Ipswich", "QPR",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Nottingham Forest vs Norwich
champ_model("Nottingham Forest", "Norwich",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Reading vs Millwall
champ_model("Reading", "Millwall",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Rotherham vs Bolton
champ_model("Rotherham", "Bolton",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Stoke vs Birmingham
champ_model("Stoke", "Birmingham",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Wigan vs West Brom
champ_model("Wigan", "West Brom",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)

#Derby vs Sheffield United
champ_model("Derby", "Sheffield United",
              book_home = 3.40, book_away = 2.15, book_draw=3.30,
              book_btts = 1.83, book_over2.5 = 2.10)
