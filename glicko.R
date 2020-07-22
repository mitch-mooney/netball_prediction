library(PlayerRatings)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(stringr)
library(gghighlight)
library(directlabels)
library(data.table)
library(plotly)
library(rjson)
library(ggplot2)
library(flipTime)

# Read in all json files
filenames <- list.files("~/Documents/R/Champion_data/data_lake/National/", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
myJSON <- lapply(filenames, function(x) fromJSON(file=x)) # a list in which each element is one of your original JSON files

#create dataframe with same columns as output
netball<- data.frame(matrix (ncol = 15, nrow =0))
names<- c("squadId","period","squadCode","squadNickname","squadName","stat","value","round", "home_team", "away_team", "game", "year", "matchType", "team", "opponent")
colnames(netball)<-names
#run for loop to gather all the scoreFlow data into a dataframe
for (i in myJSON){
    team_stats <- i$teamPeriodStats$team
    team_stats <- dplyr::bind_rows(team_stats)
    team_info <- i$teamInfo$team
    team_info <- dplyr::bind_rows(team_info)
    home_info <- i$teamInfo$team[[1]]$squadName
    away_info <- i$teamInfo$team[[2]]$squadName
    match_time <- i$matchInfo
    match_time<- dplyr::bind_rows(match_time)
    match_time <- match_time%>%separate(localStartTime, c("year", "month", "day"), "-")
    team_stats <- dplyr::left_join(team_stats, team_info, by = "squadId")
    home_team <- dplyr::data_frame(squadId = c(i$matchInfo$homeSquadId, 
                                               i$matchInfo$awaySquadId), homeTeam = c(1L, 0L))
    team_stats <- dplyr::left_join(team_stats, home_team, by = "squadId")
    final_period <- i$matchInfo$periodCompleted
    team_stats <- team_stats %>% dplyr::filter(period <= final_period) %>% 
      tidyr::gather(stat, value, -squadId, -squadName, -squadNickname, 
                    -squadCode, -period) %>% dplyr::mutate(round = i$matchInfo$roundNumber,
                                                           home_team = i$matchInfo$homeSquadId,
                                                           away_team = i$matchInfo$awaySquadId,
                                                           game = i$matchInfo$matchNumber,
                                                           year = match_time$year,
                                                           date = match_time$utcStartTime,
                                                           matchType = match_time$matchType,
                                                           team = ifelse(squadId == home_team, home_info, away_info),
                                                           opponent = ifelse(squadId == away_team, home_info, away_info))
    team_stats <- team_stats %>%
      filter(stat == "goals")
    netball <- rbind(netball, team_stats)
}


#clean data
netball$team<-str_replace(netball$team, "Ascot Park Hotel Southern Steel", "Southern Steel")
netball$opponent<-str_replace(netball$opponent,"Ascot Park Hotel Southern Steel", "Southern Steel")
netball$team<-str_replace(netball$team, "Cold Power Magic", "Waikato BOP Magic")
netball$opponent<-str_replace(netball$opponent,"Cold Power Magic", "Waikato BOP Magic")
netball$team<-str_replace(netball$team, "WBOP Magic", "Waikato BOP Magic")
netball$opponent<-str_replace(netball$opponent,"WBOP Magic", "Waikato BOP Magic")
netball$team<-str_replace(netball$team, "The Coffee Club Queensland Firebirds", "Queensland Firebirds")
netball$opponent<-str_replace(netball$opponent,"The Coffee Club Queensland Firebirds", "Queensland Firebirds")
netball$team<-str_replace(netball$team, "Splice Construction Magic", "Waikato BOP Magic")
netball$opponent<-str_replace(netball$opponent,"Splice Construction Magic", "Waikato BOP Magic")
netball$team<-str_replace(netball$team, "SKYCITY Mystics", "Northern Mystics")
netball$opponent<-str_replace(netball$opponent,"SKYCITY Mystics", "Northern Mystics")
netball$team<-str_replace(netball$team, "The Good Oil Tactix", "Tactix")
netball$opponent<-str_replace(netball$opponent,"The Good Oil Tactix", "Tactix")
netball$team<-str_replace(netball$team, "Silvermoon Tactix", "Tactix")
netball$opponent<-str_replace(netball$opponent,"Silvermoon Tactix", "Tactix")
netball$team<-str_replace(netball$team, "Mainland Tactix", "Tactix")
netball$opponent<-str_replace(netball$opponent,"Mainland Tactix", "Tactix")
netball$team<-str_replace(netball$team, "Te Wānanga o Raukawa Pulse", "Central Pulse")
netball$opponent<-str_replace(netball$opponent,"Te Wānanga o Raukawa Pulse", "Central Pulse")
netball$team<-str_replace(netball$team, "Te Wananga o Raukawa Pulse", "Central Pulse")
netball$opponent<-str_replace(netball$opponent,"Te Wananga o Raukawa Pulse", "Central Pulse")

#create round ID string for later
SSN <- netball %>%
  mutate(matchCode = ifelse(matchType == "F" & round > 10, round+(game/10), ifelse(matchType == "F" & round <10, 15+(game/10), round)))
SSN$seas_rnd<-paste(SSN$year, formatC((SSN$matchCode*10+1), width=3, flag="0"), sep=".") #get unique number in a sequence
#create unique match ID
SSN$match_ID <- paste(SSN$year, SSN$home_team, SSN$away_team, SSN$seas_rnd, width=3 ,sep=".")
# aggregate score to match score
teams<-  aggregate(SSN$value,by=list(Category = SSN$team, SSN$opponent, SSN$seas_rnd, SSN$match_ID, SSN$date), FUN=sum)
# rename columns
names(teams)[1] <- "team"
names(teams)[2] <- "opponent"
names(teams)[3] <- "round"
names(teams)[4] <- "match_ID"
names(teams)[5] <- "date"
names(teams)[6] <- "score"

teams$date<-AsDateTime(teams$date) #make string a datetime
teams$date <- as.POSIXct(teams$date)
teams$match <- trunc(rank(teams$date))

sapply(teams, class) # check type of variables
teams$round <- as.numeric(teams$round) #turn round into numerical value
teams$week <- match(teams$round, sort(unique(teams$round))) #return order of for round unique value

#score difference from long format
teams<-teams %>% 
  group_by(match_ID) %>% 
  mutate(score_diff = (score*2) - sum(score))
#turn score difference into an integer W = 1, D = 0.5, L = 0
teams$results <- ifelse(teams$score_diff < 0, 0, ifelse(teams$score_diff > 0, 1, 0.5))
teams$week <- match(teams$match, sort(unique(teams$match))) #return order of for round unique value

df1 <- teams[!duplicated(teams$match_ID),]
#glicko2 dataframe
df<- df1%>%
  ungroup() %>% 
  select(week, team, opponent, results) 

#glicko2
#glicko2(df, history = T)
rating<-glicko2(df, history = T)

#plot(rating, players = rating$ratings$player)

#glicko(df, history = T)

#hist(rating)

plot_df <- as.data.frame(rating$history)
setDT(plot_df, keep.rownames = TRUE)[]
plot_df <- melt(plot_df)
plot_df$variable <- as.character(plot_df$variable)
plot_df[,"week"] <- NA
plot_df[,"var"] <- NA
plot_df[,4:5] <- data.frame(do.call('rbind', strsplit(as.character(plot_df$variable),'.',fixed=TRUE)))
plot_df$var <- as.character(plot_df$var) # make var a charecter
plot_df$week <- as.character(plot_df$week) # make week a charecter
plot_df$week<- as.numeric(plot_df$week) # make week a numerical value
names(plot_df)[1] <- "team" # change column name to team for join


