library(readxl)
library(car)
library(pscl)
library(FactoMineR)
library(MASS)
library(survey)
library(ggformula)
library(sandwich)

nba <- read_excel("updatedteamdata3.xlsx")
scatterplotMatrix(nba)

#Some EDA

plot(sort(nba$Playoff_Wins))
gf_histogram(~Playoff_Wins, data = nba)

#Regular Poisson

reg.pois <- glm(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, family = "poisson", data = nba)
summary(reg.pois)
#The regular model doesn't look rediculous, but it does seem a little bit funky (notice the ConfWest signficance, and that nearly
#half of the features appear to be ar least somewhat important)

#Quasi Poisson

mod.quasi <- glm(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, family = quasipoisson, data = nba)
summary(mod.quasi)

#Negative Binomial 

mod.nb <- glm.nb(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba)
summary(mod.nb)

#Still similar to the previous regular models

#Now we will take a look at some models that can hopefully handle the large number of zeros in our data
#The model types that we will investigate are the hurdle and zero inflated model

#testing zero inflated model

mod <- zeroinfl(Playoff_Wins ~ DRB, data = nba, dist = "negbin")
summary(mod)

#testing some pca stuff

pca <- PCA(nba[5:14])
pca$var$coord
pca$ind$coord

#start fitting hurdle models using BIC

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
hur.full <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "poisson")
summary(hur.full)
hur.BK <-stepAIC(hur.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(hur.BK)

hur.fwd <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "forward", k = log(nrow(prin_data)))
summary(hur.fwd)

hur.bw <- stepAIC(hur.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "backward", k = log(nrow(prin_data)))
summary(hur.fwd)

#Re-Fit the model using AIC

hur.AIC <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both")
summary(hur.AIC)

hur.FAIC <- stepAIC(hur.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both")
summary(hur.FAIC)

#Try negbin dist instead of poisson (BIC)

mod.negbin <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "negbin")
summary(mod.negbin)
negbin.basic <- hurdle(Playoff_Wins ~ 1, data = nba, dist = "negbin")

negbin.step <- stepAIC(negbin.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(negbin.step)

#No off or def rating included 
negbin.full <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "negbin")
summary(negbin.full)
negbin.BK <-stepAIC(negbin.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(negbin.BK)

#Looks like negbin is not going to be useful in this case

#Check zeroinfl model (BIC)

mod.zero <- zeroinfl(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "poisson")
summary(mod.zero)
zero.basic <- zeroinfl(Playoff_Wins ~ 1, data = nba, dist = "poisson")

#stepwise selection
zero.step <- stepAIC(zero.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(zero.step)

#No off or def rating included 
#the four lines below are error prone 
zero.full <- zeroinfl(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff, data = nba, dist = "poisson")
summary(zero.full)
zero.BK <-stepAIC(zero.full, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff), direction = "both", k = log(nrow(prin_data)))
summary(zero.BK)

#quick manual fit (for some function testing)
zero.test <- zeroinfl(Playoff_Wins ~ FT + ThreeP + STL + TOV, data = nba, dist ="poisson")
summary(zero.test)

#fit testing for: zero.BK zero.step hur.BK hur.step

BIC <- c(AIC(zero.BK), AIC(zero.step), AIC(negbin.step), AIC(negbin.BK), AIC(hur.BK), AIC(hur.step))
plot(BIC)

Loglik <- c(zero.BK$loglik, zero.step$loglik, negbin.step$loglik, negbin.BK$loglik, hur.BK$loglik, hur.step$loglik)
plot(Loglik)

#Check for interactions

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + Def_Rtg*Conf + PF*Conf + TOV*Conf + BLK*Conf + STL*Conf + AST*Conf + FT*Conf + DRB*Conf + ORB*Conf + ThreeP*Conf + TwoP*Conf), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + Off_Eff*Def_Rtg + Off_Eff*PF + Off_Eff*TOV + Off_Eff*BLK + Off_Eff*STL + Off_Eff*AST + Off_Eff*FT + Off_Eff*DRB + Off_Eff*ORB + Off_Eff*ThreeP + Off_Eff*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + Def_Rtg*PF + Def_Rtg*TOV + Def_Rtg*BLK + Def_Rtg*STL + Def_Rtg*AST + Def_Rtg*FT + Def_Rtg*DRB + Def_Rtg*ORB + Def_Rtg*ThreeP + Def_Rtg*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + PF*TOV + PF*BLK + PF*AST + PF*FT + PF*DRB + PF*ORB + PF*ThreeP + PF*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + TOV*BLK + TOV*STL + TOV*AST + TOV*FT + TOV*ORB + TOV*DRB + TOV*ThreeP + TOV*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + BLK*STL + BLK*AST + BLK*FT + BLK*DRB + BLK*ORB + BLK*ThreeP + BLK*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + STL*AST + STL*FT + STL*DRB + STL*ORB + STL*ThreeP + STL*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + AST*FT + AST*DRB + AST*ORB + AST*ThreeP + AST*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + FT*DRB + FT*ORB + FT*ThreeP + FT*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + DRB*ORB + DRB*ThreeP + DRB*TwoP), direction = "both", k = log(nrow(prin_data)))

hur.inter <- stepAIC(hur.basic, scope = list(lower = ~ 1, upper = ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff
                                             + Off_Eff*Conf + ORB*ThreeP + ORB*TwoP + ThreeP*TwoP), direction = "both", k = log(nrow(prin_data)))

#summary of interaction model (I removed all of summaries I had inbetween each model re-fitting) 
summary(hur.inter)
summary(hur.full)
#It looks like Conf*Off_Eff might be significant

#Inspect the models more closely
hur.Finter <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff + Off_Eff*Conf, data = nba, dist = "poisson")

#Permutation test significance

numSim <- 1000
result <- numeric(numSim)
for (i in 1:numSim) {
  tempData <- nba[sample(nrow(nba), size = 85),]
  mod.red <- hurdle(Playoff_Wins ~ Off_Eff + Def_Rtg +Conf + STL + TOV + Off_Eff*Conf, data = tempData, dist = "poisson")
  mod.f <- hurdle(Playoff_Wins ~ TwoP + ThreeP + ORB + DRB + FT + AST + STL + BLK + TOV + PF + Conf + Def_Rtg + Off_Eff + Off_Eff*Conf, data = tempData, dist = "poisson") 
  pVal <- pchisq(2*(mod.f$loglik - mod.red$loglik), df = 16, lower.tail = FALSE)
  result[i] <- pVal
  rm(tempData)
}
summary(result)
gf_histogram(~result)


#Check BIC and LogLik graphs once again


BIC <- c(AIC(zero.BK), AIC(zero.step), AIC(negbin.step), AIC(negbin.BK), AIC(hur.BK), AIC(hur.step), AIC(hur.inter))
plot(BIC)

Loglik <- c(zero.BK$loglik, zero.step$loglik, negbin.step$loglik, negbin.BK$loglik, hur.BK$loglik, hur.step$loglik, hur.inter$loglik)
plot(Loglik)

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

