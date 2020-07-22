library(ggforce)
library(ggpmisc)

# Read in all json files
filenames <- list.files("~/Documents/R/Champion_data/data_lake/National/", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
myJSON <- lapply(filenames, function(x) fromJSON(file=x)) # a list in which each element is one of your original JSON files
match_stats<- data.frame(matrix (ncol = 0, nrow =0))
#run netball_match_function.R before running this line
for (j in myJSON) {
data<-netball_match(j)
match_stats <-plyr::rbind.fill(match_stats, data) 
}

##  Dealing with identification for ratings data
#create round ID string for later
match_stats <- match_stats %>%
  mutate(matchCode = ifelse(matchType == "F" & round > 10, round+(round/10), ifelse(matchType == "F" & round <10, 15+(round/10), round)))
match_stats$seas_rnd<-paste(match_stats$year, formatC((match_stats$matchCode*10+1), width=3, flag="0"), sep=".") #get unique number in a sequence

#create unique match ID
match_stats$seas_rnd <- as.numeric(match_stats$seas_rnd) #turn round into numerical value
#clean data
match_stats$team<-str_replace(match_stats$team, "Ascot Park Hotel Southern Steel", "Southern Steel")
match_stats$opponent<-str_replace(match_stats$opponent,"Ascot Park Hotel Southern Steel", "Southern Steel")
match_stats$team<-str_replace(match_stats$team, "Cold Power Magic", "Waikato BOP Magic")
match_stats$opponent<-str_replace(match_stats$opponent,"Cold Power Magic", "Waikato BOP Magic")
match_stats$team<-str_replace(match_stats$team, "WBOP Magic", "Waikato BOP Magic")
match_stats$opponent<-str_replace(match_stats$opponent,"WBOP Magic", "Waikato BOP Magic")
match_stats$team<-str_replace(match_stats$team, "The Coffee Club Queensland Firebirds", "Queensland Firebirds")
match_stats$opponent<-str_replace(match_stats$opponent,"The Coffee Club Queensland Firebirds", "Queensland Firebirds")
match_stats$team<-str_replace(match_stats$team, "Splice Construction Magic", "Waikato BOP Magic")
match_stats$opponent<-str_replace(match_stats$opponent,"Splice Construction Magic", "Waikato BOP Magic")
match_stats$team<-str_replace(match_stats$team, "SKYCITY Mystics", "Northern Mystics")
match_stats$opponent<-str_replace(match_stats$opponent,"SKYCITY Mystics", "Northern Mystics")
match_stats$team<-str_replace(match_stats$team, "The Good Oil Tactix", "Tactix")
match_stats$opponent<-str_replace(match_stats$opponent,"The Good Oil Tactix", "Tactix")
match_stats$team<-str_replace(match_stats$team, "Silvermoon Tactix", "Tactix")
match_stats$opponent<-str_replace(match_stats$opponent,"Silvermoon Tactix", "Tactix")
match_stats$team<-str_replace(match_stats$team, "Mainland Tactix", "Tactix")
match_stats$opponent<-str_replace(match_stats$opponent,"Mainland Tactix", "Tactix")
match_stats$team<-str_replace(match_stats$team, "Te Wānanga o Raukawa Pulse", "Central Pulse")
match_stats$opponent<-str_replace(match_stats$opponent,"Te Wānanga o Raukawa Pulse", "Central Pulse")
match_stats$team<-str_replace(match_stats$team, "Te Wananga o Raukawa Pulse", "Central Pulse")
match_stats$opponent<-str_replace(match_stats$opponent,"Te Wananga o Raukawa Pulse", "Central Pulse")

#use same datetime format as Twitter
match_stats$date<-AsDateTime(match_stats$date) 
match_stats$date <- as.POSIXct(match_stats$date)
match_stats$match <- trunc(rank(match_stats$date))
match_stats$week <- match(match_stats$match, sort(unique(match_stats$match))) #return order of for round unique value
# merge Glicko ratings and score dataframes based on week && team
# Filter only ratings
ratings_df <- plot_df %>%
  filter(var == "Rating")
#join the datasets from week and team
match_data <- left_join(match_stats, ratings_df,
                   by = c("week" = "week", "team" = "team"))
match_data$year <- as.integer(match_data$year)
#identify opposition rating
match_data<-match_data %>% 
  group_by(year, round, game, venue) %>% 
  mutate(Margin = (goals*2) - (sum(goals))) %>% 
  mutate(opp_goals = goals - Margin) %>% 
  mutate(shoot = goals/goalAttempts) %>% 
  mutate(opp_pen = sum(unique(penalties)) - (penalties)) %>% 
  mutate(pen_diff = penalties - opp_pen) %>% 
  ungroup()
match_data<-match_data[!is.na(match_data$shoot), ]
match_data<-match_data[!duplicated(match_data[,c("matchType", "year", "round", "game", "venue", "value", "shoot")]),]
#turn score difference into an integer D = 2, W = 1, L = 0
match_data <- match_data %>% 
  mutate(results = ifelse(Margin < 0, 0, ifelse(Margin > 0, 1, 2)))
         
#bind fixture
fixture <- read.csv('fixture.csv')
fixture$date<-AsDateTime(fixture$date) 
#use same datetime format as Twitter
fixture$date <- as.POSIXct(fixture$date)
fixture$home_team<-unlist(squadID[match(fixture$team, squadID$team),2]) #add home_team squad ID returns a list so unlist is required 
fixture$away_team<-unlist(squadID[match(fixture$opponent, squadID$team),2]) #add away_team squad ID returns a list so unlist is required
#create round ID string for later
fixture <- fixture %>%
  mutate(matchCode = ifelse(matchType == "F" & round > 10, round+(round/10), ifelse(matchType == "F" & round <10, 15+(round/10), round)))
fixture$seas_rnd<-paste(fixture$year, formatC((fixture$matchCode*10+1), width=3, flag="0"), sep=".") #get unique number in a sequence
#create unique match ID
fixture$matchID <- paste(fixture$year, fixture$home_team, fixture$away_team, fixture$seas_rnd, width=3 ,sep=".")
fixture$seas_rnd <- as.numeric(fixture$seas_rnd)

match_data <- dplyr::bind_rows(match_data, fixture)

#change team names & home and away status to integer values
match_data$Team <- as.numeric(ordered(match_data$team, levels = c("Southern Steel", "Central Pulse","West Coast Fever","Melbourne Vixens",
                                                                  "Queensland Firebirds","Adelaide Thunderbirds","Waikato BOP Magic",
                                                                  "Northern Mystics","Tactix","NSW Swifts","Northern Stars","GIANTS Netball",
                                                                  "Magpies Netball", "Sunshine Coast Lightning")))

match_data$opposition <- as.numeric(ordered(match_data$opponent, levels = c("Southern Steel", "Central Pulse","West Coast Fever","Melbourne Vixens",
                                                                            "Queensland Firebirds","Adelaide Thunderbirds","Waikato BOP Magic",
                                                                            "Northern Mystics","Tactix","NSW Swifts","Northern Stars","GIANTS Netball",
                                                                            "Magpies Netball", "Sunshine Coast Lightning")))

match_data$matchID<-paste(formatC((match_data$Team * match_data$opposition), width=1, flag="0"), sep=".")#get unique number in a sequence


# previous encounter data
match_data<-match_data %>% 
  group_by(team, opponent) %>% 
  mutate(last_encounter_margin = lag(Margin, order_by = date)) %>% 
  mutate(last_encounter_acc = lag(shoot, order_by = date)) %>% 
  mutate(last_encounter_penDiff = lag(pen_diff, order_by=date)) %>% 
  ungroup()

match_data<-match_data %>%
  group_by(team) %>%
  mutate(last_scoreDiff = lag(Margin, order_by=date)) %>%
  mutate(last_result = lag(results, order_by=date)) %>%
  mutate(team_rating = lag(value, order_by=date))%>%
  ungroup()

match_data<-match_data %>% 
  group_by(matchType,year, round, game, venue) %>% 
  mutate(opp_rating = (sum(team_rating)-team_rating)) %>% 
  mutate(status = as.factor(seq(1,2)))%>%
  mutate(Status = ifelse(status == 1, "Home", "Away")) %>% 
  ungroup()


Margin <- match_data %>%
  select(results, Team, opposition, status, last_scoreDiff, 
         last_encounter_margin,last_encounter_acc, last_encounter_penDiff, 
         last_result, team_rating, opp_rating, Margin) %>% 
  mutate(Margin = ifelse(is.na(Margin), 999, Margin))

Margin<-Margin[complete.cases(Margin), ] #remove NAs from data frame
Margin<-Margin%>%
  filter(results != 2) #remove draws
#Create data frame for margin predictions used in DeepLearning_Margin.R
model_data <- match_data %>%
  select(results, Team, opposition, status, last_scoreDiff, 
         last_encounter_margin,last_encounter_acc, last_encounter_penDiff, 
         last_result, team_rating, opp_rating
  )
model_data<-model_data[complete.cases(model_data), ] #remove NAs from data frame
model_data<-model_data%>%
  filter(results != 2) #remove draws ensure that the loss function is "binary_crossentropy", if you want to keep Draws change to "categorical_crossentropy"
