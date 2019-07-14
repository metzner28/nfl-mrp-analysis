library(tidyverse)
library(readxl)
library(stargazer)
library(lfe)
library(plm)
library(sandwich)

rm(list=ls())
setwd('/Users/Eli 1/Dropbox/Documents/yale/junior/ECON 438/paper 3')

getWinsAdded = function(df, model) {
  wins = 0
  for (i in 1:length(model$coefficients[1:7,])) {
    stats = df %>%
      pull(rownames(model$coefficients)[i])
    pred = model$coefficients[i] * stats
    wins = wins + pred
  }
  return(wins)
}

getRevAdded = function(wins) {
  avgRevGR = 0.08388273
  Rev18Total = 12078*(1+avgRevGR)
  RevShare = 0.008*0.01*Rev18Total*wins
  return(RevShare)
}

df0 = read_excel('438p3passing.xlsx', sheet = 1)
for (i in 2:10)
  df0 = rbind(df0, read_excel('438p3passing.xlsx', sheet = i))
dfQB = df0 %>%
  rename(player = X__1) %>%
  separate(player, sep = "\\\\", into = c('name', 'code')) %>%
  select(-c(Rk, name)) %>%
  separate(QBrec, into = c('W', 'L', 'T')) %>%
  select(c(code, Tm, Age, W, `NY/A`, `Cmp%` , `TD%` , `Int%`, YR, Pos)) %>%
  filter(Pos == 'QB') %>%
  select(-Pos) %>%
  rename(Cmp = `Cmp%`, Yd = `NY/A`, TD = `TD%`, Int = `Int%`) %>%
  mutate(Tm = str_replace_all(Tm, 'LAC', 'SDG')) %>%
  mutate(Tm = str_replace_all(Tm, 'LAR', 'STL'))
  
dfRush0 = read_excel('438p3rushing.xlsx', sheet = 1)
for (j in 2:10) 
  dfRush0 = rbind(dfRush0, read_excel('438p3rushing.xlsx', sheet = j))
dfRush1 = dfRush0 %>%
  separate(Player, sep = "\\\\", into = c('name', 'code')) %>%
  select(-name, -Rk) %>%
  rename_if(c(rep(F,4),rep(T,9), F), function(x) return(str_c('Rush', x)))

dfQBRush = dfRush1 %>%
  filter(Pos == 'QB') %>%
  transmute(code = code,
            Tm = str_replace_all(Tm, 'LAC', 'SDG'),
            Tm = str_replace_all(Tm, 'LAR', 'STL'),
            YR = YR,
            RuYd = RushYds/RushAtt,
            RuTD = RushTD/RushAtt * 100,
            RuFmb = RushFmb/RushAtt * 100)

dfTmRush = dfRush1 %>%
  group_by(Tm, YR) %>%
  summarise(TmRuYd = sum(RushYds)) %>%
  filter(Tm != '2TM' & Tm != '3TM')

dfDef0 = read_excel('438p3defense.xlsx', sheet = 1)
for (k in 2:10)
  dfDef0 = rbind(dfDef0, read_excel('438p3defense.xlsx', sheet = k))
dfDef1 = dfDef0 %>%
  select(Tm, PF, TO, YR) %>%
  rename(DefTO = TO)

stats = inner_join(dfQB, dfQBRush, by = c('code', 'YR', 'Tm')) %>%
  inner_join(dfDef1, by = c('Tm', 'YR')) %>%
  inner_join(dfTmRush, by = c('Tm', 'YR'))

modelWins0 = felm(W ~ Yd + Cmp + TD + Int + RuTD + RuFmb + Age**2 | factor(Tm) + factor(YR), 
                  data = stats)
modelWins1 = felm(W ~ Yd + Cmp + TD + Int + RuTD + RuFmb + Age**2 + TmRuYd | factor(Tm) + factor(YR),
                  data = stats)
modelWins2 = felm(W ~ Yd + Cmp + TD + Int + RuTD + RuFmb + Age**2 + DefTO | factor(Tm) + factor(YR),
                  data = stats)
modelWinsTotal = felm(W ~ Yd + Cmp + TD + Int + Age**2 + RuTD + RuFmb + TmRuYd + DefTO | factor(Tm) + factor(YR),
                      data = stats)

WinsModel = stargazer(modelWins0, modelWins1, modelWins2, modelWinsTotal, header=FALSE, type='text', 
                   title = "Fixed-Effects Wins Model Estimation Results",
                   out = 'WinsModelp3.txt')

##### 2018 prediction scripts ###

dfPass18 = read_excel('438p3data2018.xlsx', sheet = 'pass') %>%
  separate(Player, sep = "\\\\", into = c('name', 'code')) %>%
  select(-name) %>%
  separate(QBrec, into = c('W', 'L', 'T')) %>%
  select(c(code, Tm, Age, W, `NY/A`, `Cmp%` , `TD%` , `Int%`, YR, Pos)) %>%
  filter(Pos == 'QB') %>%
  select(-Pos) %>%
  rename(Cmp = `Cmp%`, Yd = `NY/A`, TD = `TD%`, Int = `Int%`)

dfRush18 = read_excel('438p3data2018.xlsx', sheet = 'rush') %>%
  separate(Player, sep = "\\\\", into = c('name', 'code')) %>%
  select(-name) %>%
  select(c(code, Tm, Age, Att, TD, Fmb, Pos)) %>%
  filter(Pos == 'QB') %>%
  select(-Pos) %>%
  transmute(code = code,
            Tm = Tm,
            RuTD = 100 * TD/Att,
            RuFmb = 100 * Fmb/Att)

stats18 = inner_join(dfPass18, dfRush18, by = c('code', 'Tm'))
salary18 = read_excel('438p3data2018.xlsx', sheet = 'salary') %>%
  separate(Player, sep = "\\\\", into = c('name', 'code')) %>%
  select(-Pos)
statsRev18 = inner_join(stats18, salary18, by = c('code', 'Tm'))

######## bad revenue models
# modelRev0 = plm(Rev ~ lag_wins + Pop + Ypc, model = 'pooling', data = dfRev)
# vcov0 = vcovNW(modelRev0, type = "HC3", cluster = 'group')
# se0 = sqrt(diag(vcov0))
# 
# modelRevT = plm(Rev ~ lag_wins + Pop + Ypc + time, model = 'pooling', data = dfRev)
# vcovT = vcovNW(modelRevT, type = "HC3", cluster = 'group')
# seT = sqrt(diag(vcovT))
# 
# modelRevAuto = plm(Rev ~ lag_wins + RevLag, model = 'pooling', data = dfRev)
# vcovAuto = vcovNW(modelRevAuto, type = "HC3", cluster = "group")
# seAuto = sqrt(diag(vcovAuto))
########

dfRev = read_excel('438p3rev.xlsx') %>%
  mutate(Rev = log(rev17),
         Pop = log(pop),
         Ypc = log(ypc17m),
         RevLag = log(lag_rev17))

modelRevPct = plm(revPct ~ wins_t1 + Pop + Ypc, model = 'pooling', data = dfRev)
vcovPct = vcovNW(modelRevPct, type = "HC3", cluster = 'group')
sePct = sqrt(diag(vcovPct))

modelRevAutoPct = plm(revPct ~ wins_t1 + RevPct_t1, model = 'pooling', data = dfRev)
vcovAutoPct = vcovNW(modelRevAutoPct, type = "HC3", cluster = 'group')
seAutoPct = sqrt(diag(vcovAutoPct))

modelRev_doubleLag = plm(revPct ~ wins_t1 + wins_t2 + RevPct_t1, model = 'pooling', data = dfRev)
vcovRev_doubleLag = vcovNW(modelRev_doubleLag, type = "HC3", cluster = 'group')
sedoubleLag = sqrt(diag(vcovRev_doubleLag))

RevModelUpdated = stargazer(modelRevPct, modelRevAutoPct, modelRev_doubleLag,
                            header = FALSE, type = 'text', out = 'RevModelsUpdated_2_newdf.txt',
                            se = list(sePct, seAutoPct, sedoubleLag), 
                            title = "Revenue Autoregressive Model Estimation Results",
                            dep.var.labels = "RevPct")

statsRev18$WinsAdded = getWinsAdded(statsRev18, modelWinsTotal)
statsRev18$RevAdded = sapply(statsRev18$WinsAdded, getRevAdded)

isOutlier = function(sal, rev) {
  sdRev = sd(dfPred$MRP)
  if (rev - sal > 2*sdRev) return('underpaid')
  else if (abs(rev - sal) < 2*sdRev) return('fairly paid')
  else if (rev - sal < -2*sdRev) return('overpaid')
}

dfPred = statsRev18 %>%
  select(c(name, Tm, Age, Sal, WinsAdded, RevAdded)) %>%
  arrange(-RevAdded) %>%
  rename(MRP = RevAdded) %>%
  mutate(Salary = Sal / 10^6) %>%
  select(-Sal)

# dfPred$Wins15 = c(11, 11, 11, 8, 5, 4, 9, 5, 7, 11, 7, 9, 6, 6, 7, 6, 9, 8,
#                   5, 5, 7, 7, 6, 7, 3, 5, 5, 4, 6, 3, 4, 3)

dfPred$outlier = mapply(isOutlier, dfPred$Salary, dfPred$MRP)

plot = ggplot(dfPred) +
  geom_point(aes(x = Salary, y = MRP, color = outlier)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey50') +
  ylim(0,15) + 
  theme_minimal()

dfPredFinal = dfPred %>%
  select(-c(outlier))

dfRevTotals = dfRev %>%
  group_by(year) %>%
  summarise(sum(revenue))
  

  