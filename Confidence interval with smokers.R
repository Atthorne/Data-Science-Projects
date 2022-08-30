
## Andrew Thorne ##


library(tidyverse)
library(mdsr)
library(mosaicData)
library(boot)
#BEGIN NO. 1 
# 2.58 standard deviations for a 99% Confidence Interval 

#END NO. 1 

#BEGIN NO. 2 

Avg_age <- lm(age ~ 1, Gestation) 
confint(Avg_age, level=0.95)

t.test(Gestation$age,conf.level = 0.95)

#END NO. 2

#BEGIN NO. 3 

data(Gestation)
X=Gestation$age
X=na.omit(X)
est=function(data,indices){median(data[indices, ])}
set.seed(53)
boot=boot(data.frame(X),est,R=2000)
boot.ci(boot,type="norm",conf=0.95)

#END NO. 3 

#BEGIN NO. 5 

Reg.model <- lm(wt + age ~ 1, Gestation)
confint(Reg.model, level = 0.95)
t.test(Gestation$wt+Gestation$age,conf.level = 0.95)

#END NO. 5

#BEGIN NO. 7 

Whickham %>% 
  count(smoker) %>% 
  mutate(smoker_percent = n / sum(n))

Whickham %>% 
  count(outcome) %>% 
  mutate(outcome_percent = n / sum(n))

Whickham %>% 
  count(smoker,outcome) %>% 
  group_by(smoker) %>% 
  mutate(outcome_percent = n/sum(n)) %>% 
  filter(outcome=="Dead")

Whickham <- Whickham %>% 
  mutate(cat_age = ifelse(age <= 44, "18-44",ifelse(age <= 64, "45-64", "65+")))
Whickham %>% 
  count(cat_age,smoker,outcome) %>% 
  group_by(cat_age,smoker) %>% 
  mutate(outcome_percent = n/sum(n)) %>% 
  filter(outcome=="Dead")
  

ggplot(Whickham, aes(x=smoker, fill = outcome)) + geom_bar(position = "fill") + 
  labs(y="Proportion") + facet_grid(. ~ cat_age) + theme_bw()

#AS YOU CAN SEE WITH AGE BEING A FACTOR, THE OLDER THAT THESE SMOKERS WERE,
#THE HIGHER CHANCE OF DEAD BEING AN OUTCOE WAS FOR THEM. A DIRECT RELATIONSHIP 
#IS ESTABLISHED BETWEEN MORTALITY RATE OF SMOKERS AND AGE. 

#END NO. 7 

### END ### 
