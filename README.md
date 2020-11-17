#### Exercice Sondage stratifié #####

# Datat ----------------------------
N <- data("sond_strat")

pacman::p_load(tidyverse)
set.seed(1234)
N <- 1e6
sero1 <- data.frame(risk = rbinom(N, 1, .3)) %>%
  mutate(sero = rbinom(N, 1, ifelse(risk == 1, .45, .01)))

## Exercice 1 ----------------------
#1.1
prop.table(table(sero$risk))*100 # 30% à risque - 70% non à risque
#1.2
prop.table(table(sero$sero))*100 # 14% séroprévalence dans la population totale
#1.3
with(sero, tapply(risk, sero, mean))

## Exercie 2 ----------------------
#2.1
sero500 <- sero[sample(1:nrow(sero), 500, replace=FALSE), ]
prop.table(table(sero500$risk))*100 # 28% à risque - 72% non à risque

#2.2
seroP <- numeric(length=50)
for (i in 1:50)
{
  seroP[i]<-mean(sero[sample(1:nrow(sero), 500, replace=FALSE),"sero"])
}
mean(seroP)
quantile(seroP, probs = c(0.025,0.975))

## Exercice 3
#