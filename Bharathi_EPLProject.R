library(tidyverse)
library(lubridate)

EPL_Standings <- function (date,season){
  #Create a variable to pass automatically in the URL for data download
  season <- paste0(substr(season,3,4),substr(season,6,7),"/")
  season <- paste0(season,'/')
  url_season <- paste0('https://www.football-data.co.uk/mmz4281/',season,'E0.csv')
  print(url_season)
  data <- read_csv(url_season)
  print(nrow(data))
  
  # selecting the required columns as per requirement
  df <- data %>%
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>% 
    #Filter for date in a season
    mutate(Date = dmy(Date)) %>% 
    filter(Date <= as.Date(date, "%m/%d/%Y")) %>% 
    #Create scores based on FTR column
    mutate(point_hometeam = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 3, ifelse(FTR == 'A', 0,NA))), # point earned at home team ground
           point_awayteam = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, ifelse(FTR == 'A', 3,NA))), # point earned at away team ground
           win_hometeam = ifelse(FTR == 'H',1,0), # if win at home team then score 1
           win_awayteam = ifelse(FTR == 'A', 1,0), # if win at away team then score 1
           draw_hometeam = ifelse(FTR == 'D',1,0), # if draw at home team then score 1
           draw_awayteam = ifelse(FTR == 'D',1,0), # if draw away team then score 1
           losses_hometeam = ifelse(FTR == 'A',1,0), # if lost at home team then score 1
           losses_awayteam = ifelse(FTR == 'H',1,0)) # if lost away team then score 1
  #Home team metrics
  home_team <- df %>%
    select(Date, HomeTeam, FTHG,FTAG, FTR,point_hometeam, win_hometeam,draw_hometeam,losses_hometeam) %>%
    group_by(TeamName = HomeTeam) %>% # Group by Team name and derive points wins, losses etc
    summarise(count_hometeam = n(), # total games
              point_hometeam = sum(point_hometeam), # total points earned
              wins_hometeam = sum(win_hometeam), # total wins
              draws_hometeam = sum(draw_hometeam), # total draws
              losses_hometeam = sum(losses_hometeam), # total losses
              goals_for_hometeam = sum(FTHG), # total goals scored
              goals_against_hometeam = sum(FTAG)) # total goals opponents scored against
  #Away team metrics
  away_team <- df %>%
    select(Date, AwayTeam, FTHG,FTAG, FTR,point_awayteam, win_awayteam,draw_awayteam,losses_awayteam) %>%
    group_by(TeamName = AwayTeam) %>% 
    summarise(count_awayteam = n(), 
              point_awayteam = sum(point_awayteam), 
              wins_awayteam = sum(win_awayteam), 
              draws_awayteam = sum(draw_awayteam),
              losses_awayteam = sum(losses_awayteam),
              goals_for_awayteam = sum(FTAG), 
              goals_against_awayteam = sum(FTHG)) 
  # joining home and away summaries data 
  mergeDF <- home_team %>%
    full_join(away_team, by = c('TeamName'))
  
  # Convert NA to 0
  mergeDF[is.na(mergeDF)] <- 0
  
  # Creating the metrics as per function need
  mergeDF <- mergeDF %>%
    mutate(MatchesPlayed = count_hometeam + count_awayteam, 
           Points = point_hometeam + point_awayteam, 
           PPM = round(Points/MatchesPlayed,3), 
           PtPct = round(Points/(3*MatchesPlayed),2), 
           Wins = wins_hometeam + wins_awayteam, 
           Draws = draws_hometeam + draws_awayteam, 
           Losses = losses_hometeam + losses_awayteam, 
           Record = paste0(Wins,'-',Losses,'-',Draws), 
           HomeRec = paste0(wins_hometeam,'-',losses_hometeam,'-',draws_hometeam), 
           AwayRec = paste0(wins_awayteam,'-',losses_awayteam,'-',draws_awayteam), 
           GS = goals_for_hometeam + goals_for_awayteam, 
           GSM = round(GS/MatchesPlayed,2), 
           GA = goals_against_hometeam + goals_against_awayteam, 
           GAM = round(GA/MatchesPlayed,2)) 
  
  output <- mergeDF %>% 
    arrange(desc(PPM), desc(Wins),desc(GSM),GAM) %>% #Sort according to required metrics 
    select(TeamName, Record, HomeRec,AwayRec,MatchesPlayed,Points,PPM,PtPct,GS,GSM,GA,GAM)
  
  return(as.data.frame(output))
}

#Test it out
EPL_Standings("12/31/2019","2019/20")



