library(readxl)
library(car)
library(pscl)
library(FactoMineR)
nba <- read_excel("UPDATEDTEAMDATA.xlsx")
scatterplotMatrix(nba)

mod <- zeroinfl(Playoff_Wins ~ DRB, data = nba, dist = "negbin")
summary(mod)

pca <- prcomp(nba, scale = TRUE)
