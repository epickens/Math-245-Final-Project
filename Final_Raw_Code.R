library(readxl)
library(car)
library(pscl)
library(FactoMineR)
library(MASS)
library(survey)
nba <- read_excel("UPDATEDTEAMDATA.xlsx")
scatterplotMatrix(nba)

mod <- zeroinfl(Playoff_Wins ~ DRB, data = nba, dist = "negbin")
summary(mod)

pca <- prcomp(nba, scale = TRUE)
pca <- PCA(nba[5:14])
pca$var$coord
pca$ind$coord
mod.hur <- hurdle(Playoff_Wins ~ DRB + ThreeP + TwoP + FT + TRB + AST + STL + BLK + TOV + PF, data = nba, dist = "negbin")
summary(mod.hur)


prin_comp <- princomp(nba[5:14], scale = TRUE)
names(prin_comp)
prin_comp$scores

prin_data = data.frame(PO_Wins = nba$Playoff_Wins, prin_comp$scores)



prin.mod <- zeroinfl(PO_Wins ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.10, data = prin_data, dist = "poisson")

summary(prin.mod)

prin.step <- stepAIC(prin.mod, scale = list(lower = ~ 1, upper = ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6 + Comp.7 + Comp.8 + Comp.9 + Comp.10),
                    direction = "both", k = log(nrow(prin_data)))

summary(prin.step)
