netball_match <- function (match) 
{
  player_stats <- match$playerPeriodStats$player
  player_stats <- dplyr::bind_rows(player_stats)
  home_info <- match$teamInfo$team[[1]]$squadName
  away_info <- match$teamInfo$team[[2]]$squadName
  match_time <- match$matchInfo
  match_time<- dplyr::bind_rows(match_time)
  match_time <- match_time%>%tidyr::separate(localStartTime, c("year", "month", "day"), "-")
  period_info <- match$periodInfo$qtr
  period_info <- dplyr::bind_rows(period_info)
 
  # ***************** REORGANISE DATA TO LONG FORM *************************** #
  #then transpose into long form
  final_period <- match$matchInfo$periodCompleted
  player_stats <- player_stats %>% 
    dplyr::filter(period <=final_period) %>% 
    tidyr::gather(stat,value, -period, -squadId) %>% 
    dplyr::mutate(round = match$matchInfo$roundNumber, 
                  game = match$matchInfo$matchNumber,
                  year = match_time$year,
                  date = match$matchInfo$localStartTime,
                  venue = match$matchInfo$venueCode,
                  matchType = match_time$matchType,
                  home_team = match$matchInfo$homeSquadId,
                  away_team = match$matchInfo$awaySquadId,
                  team = ifelse(squadId == home_team, home_info, away_info),
                  opponent = ifelse(squadId == away_team, home_info, away_info)
    )
  #aggregate values into full match
  player_stats$value <- as.numeric(player_stats$value)
  player_stats <- aggregate(player_stats$value,by=list(Category = player_stats$stat,player_stats$opponent, player_stats$team, player_stats$round, player_stats$game, player_stats$year,player_stats$matchType, player_stats$date, player_stats$venue), FUN=sum)
  player_stats<-player_stats %>% 
    dplyr::rename(
      stat = Category,
      opponent = Group.2,
      team = Group.3,
      round = Group.4,
      game = Group.5,
      year = Group.6,
      matchType = Group.7,
      date = Group.8,
      venue = Group.9,
      value = x 
    )
  data_wide <- tidyr::spread(player_stats, stat, value)
  
data_wide
}