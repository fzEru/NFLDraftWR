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
WR.1$PredictedrTargets <- linear_rTargets(WR.1$DR, WR.1$TS, WR.1$BA)

WR.2 <- subset(WR.1, seasons != 0)

WR.1num <- subset(WR.1, select = c('DR',
                                   'TS', 
                                   'BA'))
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
View(WR.1)

# high DR, low TS, low BA
# 8 
# high DR, high TS, low BA
# 2
# high DR, low TS, high BA
# 3
# high DR, high TS, high BA 
# 5
# low DR, low TS, low BA
# 7 
# low DR, high TS, low BA
# 6 
# low DR, low TS, high BA
# 4
# low DR, high TS, high BA 
# 1

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

metricsModel <- lm(rTargets ~ 0 + DR + TS + BA + highEnd + cluster, data = subset(WR.2, round == 3))
summary(metricsModel)

metricsModelv2 <- lm(rTargets ~ 0 + DR + TS + BA + cluster, data = WR.2, subset = round)
summary(metricsModelv2)
# 99.9 R-squared??


linear_rTargets <- function(DR, TS, BA, hEN, hEY, cluster) {
  -2.8040*DR + 2.1592*TS + 0.4397*BA + 14.3876*hEN + 47.7590*hEY + 15.9283*cluster
}


