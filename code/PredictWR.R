library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
library(bit64)
library(dplyr)
library(stringr)
library(anytime)
library(knitr)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(openxlsx)
library(expss)
library(cluster)
library(factoextra)
library(skimr)
WR <- read_csv("WR Scouting.csv")

WR.1 <- WR %>%
  rename(
    round = `Round #`, 
    pick = `Pick #`, 
    DR = `Dominator (30% = 50th)`,
    TS = `Target Share`,
    BA = `Breakout Age`,
    DT = `>D/T?`, 
    underBA = `<=20 BA (20 = 65th)`,
    highEnd = `High End WR1?`, 
    extraDR = `Extra Dominator`, 
    rTargets = `Rookie Targets`, 
    rYards = `Rookie Yards`, 
    top12 = `Top 12 Seasons`, 
    top24 = `Top 24 Seasons`,
    seasons = `Seasons`
  )

WR.1$underBA[WR.1$underBA == "No"] <- 0 
WR.1$underBA[WR.1$underBA == "Yes"] <- 1

WR.1$highEnd[WR.1$highEnd == "No"] <- 0 
WR.1$highEnd[WR.1$highEnd == "Yes"] <- 1

WR.1$extraDR[WR.1$extraDR == "No"] <- 0 
WR.1$extraDR[WR.1$extraDR == "Yes"] <- 1

WR.1$top12pt <- WR.1$top12 / WR.1$seasons
WR.1$top24pt <- WR.1$top24 / WR.1$seasons
WR.1$predicted_rTargets <- linear_rTargets(WR.1$DR, WR.1$TS, WR.1$BA, WR.1$RAS, WR.1$cluster, WR.1$pick)
WR.1$targetBinary <- WR.1$rTargets > WR.1$predicted_rTargets
WR.1$targetComparison <- WR.1$rTargets - WR.1$predicted_rTargets

#subset for analysis of predicted vs actual targets and future production

test_rTargets <- subset(WR.1, select = c('Player',
                                'rTargets',
                                'predicted_rTargets',
                                'targetBinary',
                                'targetComparison',
                                'top12pt',
                                'top24pt'))

WR.2 <- subset(WR.1, seasons != 0)

WR.1num <- subset(WR.1, select = c('DR',
                                   'TS', 
                                   'BA', 
                                   'RAS'))
c('DR') %in% colnames(WR.1num)

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

names(WR.1num)
seg.summ(WR.1num, WR.1$DR)

WR.1df <- as.data.frame(WR.1num)

class(WR.1df)

WR.seg.8<- kmeans(WR.1df, centers = 8, nstart = 25, iter.max = 100)
WR.seg.8

pca.8 <-fviz_cluster(WR.seg.8, WR.1df)
pca.8

WR.1$cluster <- WR.seg.8$cluster

WR.2 <- subset(WR.1, seasons != 0)

# high DR, low TS, low BA
# 
# high DR, high TS, low BA
# 
# high DR, low TS, high BA
# 
# high DR, high TS, high BA 
# 
# low DR, low TS, low BA
#  
# low DR, high TS, low BA
#  
# low DR, low TS, high BA
# 
# low DR, high TS, high BA 
# 

# function for averaging chosen stats by cluster 

cluster_mean <- function(measure) {
  aggregate(x = measure,
            by = list(WR.2$cluster),
            FUN = mean)
}

cluster_sd <- function(measure) {
aggregate(x = measure, 
          by = list(WR.2$cluster),
          FUN = sd)
}

# linear modeling

targetsModel <- lm(rTargets ~ 0 + DR + TS + BA + RAS + cluster + pick, data = WR.2)
summary(targetsModel)

linear_rTargets <- function(DR, TS, BA, RAS, cluster, pick) {
  -0.09447*DR + 0.16630*TS + 0.30112*BA + 6.03988*`RAS` + 1.80943*cluster + -0.16763*pick
}
