###############################
# Made By Alfredo Rodriguez ##
# Made 2/25/2021 ############

# GOAL: Try to see which players are overpaid vs underpaid. Using 2016 data with Lahman Data Source ####


library(Lahman)
library(dplyr)
library(ggplot2)
library(caret)
LahmanData



Salaries = Salaries %>%
  filter(yearID > 2015)
Salaries

teamSalary <- "PIT"

Salaries = Salaries %>%
  filter(teamID == teamSalary)

LahmanData

Batting
batting_Pit <- Batting %>%
  filter(yearID > 2015, teamID == "PIT")

tail(batting_Pit)

str(batting_Pit)
str(Salaries)
batting_PitWSalary <- inner_join(batting_Pit, Salaries, by = "playerID")
batting_PitWSalary <- batting_PitWSalary %>%
  bind_rows() %>%
  as.data.frame() %>%
  select(playerID,
         G,
         AB,
         R,
         H,
         X2B,
         X3B,
         HR,
         RBI,
         SO,
         salary) %>%
  mutate(runs_game = R^2/G^2, hpg = H^2/G^2, XBHpg = (X2B + X3B)^2/G^2, HRpg = HR^2/G^2, SOpg = SO^2/G^2)

summary(batting_PitWSalary)
lm1 <- lm(salary ~ runs_game + hpg + hpg + XBHpg + HRpg + SOpg,
          data = batting_PitWSalary)
summary(lm1)

lm2 <- lm(salary ~ R + G + H + HR + X2B + X3B + SO, data = batting_PitWSalary)
summary(lm2)


preds <- predict(lm1, batting_PitWSalary)
preds

batting_PitWSalary$pred <- preds
batting_PitWSalary$SalaryDiff <- batting_PitWSalary$salary - round(preds)

RMSE(batting_PitWSalary$salary, batting_PitWSalary$pred)

batting_PitWSalary %>%
  ggplot(aes(pred, salary)) +
  geom_point() +
  geom_smooth() +
  ggtitle('PredictedSalary vs Actual Salary')


batting_PitWSalary %>%
  bind_rows() %>%
  as.data.frame() %>%
  select(playerID,
         salary,
         pred,
         SalaryDiff)
         
  
