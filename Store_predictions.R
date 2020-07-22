library(formattable)
#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         #Draw_Prob = V3,
         Tips = V3)

season_predictions <-read.csv('fixture_res.csv')

new_predictions<-score_data_lean %>% 
  filter(Margin == 999) %>% 
  mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
  select(Tips,	pred_loss_prob,	pred_win_prob,margin_est_linear)

table<-cbind(fixture, new_predictions)
table <- table %>% 
  select(date, team, opponent, round, year, Tips, pred_loss_prob, pred_win_prob, margin_est_linear) %>% 
  mutate(pred_loss_prob = round(pred_loss_prob, digits = 2), 
         pred_win_prob = round(pred_win_prob, digits = 2),
         margin_est_linear = round(margin_est_linear, digits = 0)) %>%
  mutate(Team_predicted = ifelse(Tips == 1, team, opponent)) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate = margin_est_linear,
  )

formattable(table, align = c("l", rep("c", NCOL(table) - 1)))

#bind new with previous predictions
new_season_pred<-plyr::rbind.fill(season_predictions, table)
#rewrite csv with up to date predictions to keep tally
write.csv(new_season_pred,'fixture_res.csv')

table_final <- table[1:4,]
#display tips in table with probabilities
formattable(table_final, align = c("l", rep("c", NCOL(table_final) - 1)))

#bits and brier scores following https://rpubs.com/DamienG/613310
season_predictions$predicted_prob = pmax(season_predictions$Loss_prob, season_predictions$Win_Prob)
season_predictions$brier = (season_predictions$predicted_prob - season_predictions$Tip_Outcome)^2
season_predictions$bits = ifelse(season_predictions$Tip_Outcome == 1, 1 + log(season_predictions$predicted_prob, base = 2), 
                                 ifelse(season_predictions$Tip_Outcome == 0, 1 + log(1 - season_predictions$predicted_prob, base = 2),
                                        1 + 0.5*log(season_predictions$predicted_prob*(1-season_predictions$predicted_prob), base = 2)))

#create dataframe comparing home and away prediction scores
accuracy = season_predictions %>% 
  group_by(Status, Round) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), Tips = sum(correct_tips))

formattable(accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))

model_accuracy = season_predictions %>% 
  group_by(Status) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), Tips = sum(correct_tips), Correct = round(Tips/n(), 2))

formattable(model_accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))
# see if how well predicting teams
team_accuracy = season_predictions %>% 
  group_by(Status, Team) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>% 
  mutate(margin_error = margin_estimate_1 - Actual.Margin) %>% 
  summarise(Brier = round(mean(brier), 3), Bits = round(mean(bits), 3), MAE = round(mean(margin_error)),
            Accuracy = round(100*mean(Tip_Outcome), 1))
formattable(team_accuracy, align = c("l", rep("c", NCOL(team_accuracy) - 1)))

# Betting return
ROI<-season_predictions %>%
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0)) %>%
  mutate(paid = ifelse(Actual == 1, Odds, Opp_Odds)) %>% 
  mutate(return = Tip_Outcome * paid) %>% 
  mutate(ROI = return *10)

ROI %>% 
  filter(predicted_prob > 0.70) %>% 
  filter(Round != "Round 2") %>% 
  group_by(Status, Round) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)

ROI %>% 
  filter(predicted_prob > 0.70) %>% 
  filter(Round != "Round 2") %>% 
  group_by(Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)


