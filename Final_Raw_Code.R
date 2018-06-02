library(readxl)
library(car)
library(pscl)
library(FactoMineR)
library(MASS)
library(survey)
nba <- read_excel("UPDATEDTEAMDATA.xlsx")
scatterplotMatrix(nba)

#testing zero inflated model

mod <- zeroinfl(Playoff_Wins ~ DRB, data = nba, dist = "negbin")
summary(mod)

#testing some pca stuff

pca <- PCA(nba[5:14])
pca$var$coord
pca$ind$coord

#start fitting hurdle models

mod.hur <- hurdle(Playoff_Wins ~ FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "poisson")
summary(mod.hur)
hur.basic <- hurdle(Playoff_Wins ~ 1, data = nba, dist = "poisson")

hur.step <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(hur.step)

hur.stepNoComp <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf), direction = "both", k = log(nrow(prin_data)))
summary(hur.stepNoComp)

hur.stepNoCompAIC <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf), direction = "both")
summary(hur.stepNoCompAIC)

hur.stepBk <- stepAIC(mod.hur, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(hur.stepBk)

#No off or def rating
hur.full <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf, data = nba, dist = "poisson")
summary(hur.full)
hur.BK <-stepAIC(hur.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "backward", k = log(nrow(prin_data)))
summary(hur.BK)

hur.fwd <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "forward", k = log(nrow(prin_data)))
summary(hur.fwd)

hur.AIC <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both")
summary(hur.AIC)

#Try negbin dist instead of poisson

mod.negbin <- hurdle(Playoff_Wins ~ FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "negbin")
summary(mod.negbin)
negbin.basic <- hurdle(Playoff_Wins ~ 1, data = nba, dist = "negbin")

negbin.step <- stepAIC(negbin.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(negbin.step)

#No off or def rating included 
negbin.full <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf, data = nba, dist = "negbin")
summary(negbin.full)
negbin.BK <-stepAIC(negbin.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "backward", k = log(nrow(prin_data)))
summary(negbin.BK)

#Looks like negbin is not going to be useful in this case

#Check zeroinfl model

mod.zero <- zeroinfl(Playoff_Wins ~ FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "poisson")
summary(mod.zero)
zero.basic <- zeroinfl(Playoff_Wins ~ 1, data = nba, dist = "poisson")

zero.step <- stepAIC(zero.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(zero.step)

#No off or def rating included 
zero.full <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf, data = nba, dist = "negbin")
summary(zero.full)
zero.BK <-stepAIC(zero.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "backward", k = log(nrow(prin_data)))
summary(zero.BK)


#More PCA stuff (begin using PCA to create a predictive model)

prin_comp <- princomp(nba[5:14], scale = TRUE)
names(prin_comp)
prin_comp$scores

prin_data = data.frame(PO_Wins = nba$Playoff_Wins, prin_comp$scores)



prin.mod <- zeroinfl(PO_Wins ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.10, data = prin_data, dist = "poisson")

summary(prin.mod)

prin.step <- stepAIC(prin.mod, scale = list(lower = ~ 1, upper = ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.9 + Comp.10),
                    direction = "both", k = log(nrow(prin_data)))

summary(prin.step)

