.libPaths("F:/LocalProjects/R/3.5")
.libPaths()


require(tidyverse)

##############
###  DATA ####
getwd()
dat1 <- read_csv("DemoData_June2021.csv")
dat <- dat1 %>% mutate_all(na_if,"")
names(dat)[25] = "density"
names(dat)[26] = "lambda"
names(dat)[27] = "CalfCow"
names(dat)[15] = "Range"

#############
## "Density (Ind./100SqKm)"
dens <- dat%>%
  filter(!is.na(density))%>%
  select(density,Province)%>%
  mutate(density = as.numeric(gsub("[^0-9.-]", "", density)))

dens %>%
  group_by(Province)%>%
  summarize(recMean = mean(density))

#############
## Recruitment
rec <- dat%>%
  filter(!is.na(CalfCow))
rec %>%
  group_by(Range)%>%  # Province
  summarize(
    N = length(CalfCow),
    recMean = mean(CalfCow),
    recSD = sd(CalfCow),
    recSE = recSD/sqrt(N))%>%
  filter(N > 1)%>%
  ggplot(aes(x=Range, y=recMean)) + 
  geom_errorbar(aes(ymin=recMean-recSE, ymax=recMean+recSE), width=.1) +
  geom_point()

rec %>%
  ggplot(aes(x=CalfCow, y=lambda)) + 
  # geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_point()

#############
## lamb

lamb <- dat%>%
  filter(!is.na(lambda),lambda > 0)

hist(lamb$lambda)
lamb %>%
  ggplot(aes(x=Years_Monitored, y=lambda)) + 
  # geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_point()

lamb %>%
  filter(lambda > 0)%>%
  group_by(Range)%>%
  summarize(
    N = length(lambda),
    recMean = mean(lambda),
    recSD = sd(lambda),
    recSE = recSD/sqrt(N))%>%
  filter(N > 1)%>%
  ggplot(aes(x=Range, y=recMean)) + 
  geom_errorbar(aes(ymin=recMean-recSE, ymax=recMean+recSE), width=.1) +
  geom_point()


