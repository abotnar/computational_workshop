library(tidyverse)
library(magrittr)
library(GGally)

setwd('~/lyme')
pop.ld.prism <- read_csv('pop.ld.prism.csv')

#create a 4x4 summary plot using ggpairs
ggpairs(pop.ld.prism, columns=c('prcp', 'avtemp', 'size', 'cases'))

#add new columns for log10(size) and log10(cases+1)
pop.ld.prism %<>% mutate(log10size=log10(size))
pop.ld.prism %<>% mutate(log10cases=log10(cases+1))

#create new ggpairs plot with above columns
ggpairs(pop.ld.prism, columns=c('prcp', 'avtemp', 'log10size', 'log10cases')) #add 1 to log cases because can't take the log of 0, it is undefined

#set seed random sample
set.seed(222)
small<-pop.ld.prism %>% sample_n(100) 

ggplot(data=small)+ geom_point(aes(prcp,avtemp))+ geom_smooth(aes(prcp, avtemp), method = 'lm')

#create a linear model object and view
myModel <- lm(avtemp~prcp, data = small)

summary(myModel)

summary(myModel)$coefficients[2,1] #slope is 0.0067
summary(myModel)$coefficients[2,4] #is statistically significantly different because p<0.05

#single line of code to generate single ggplot of year by total
pop.ld.prism %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year, y=total))

#create a data frame grouped by state and nest it
by_state <- pop.ld.prism %>% group_by(state)
by_state %<>% nest()
by_state

#test the nest using GA
by_state$data[[10]]


#task12
linGrowth_model <- function(df) {
  lm(size~year, data = df)
}

models <- purrr::map(by_state$data, linGrowth_model)
#rm(list=ls())
 #task13
by_state %<>% mutate (model = map(data, linGrowth_model))

library(modelr)
by_state %<>% mutate(resids = map2(data,model,add_residuals))

by_state #structure of resids is in tibble

#task15
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = map(resids,sum_resids))

#task16
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))

#unnest
slopes <- unnest(by_state,slope)
totalResids <- unnest(by_state,totalResid)

#task17 plot growth rate for all states
slopes %<>% ggplot(aes(state,slope))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#task18 plot total residuals for all states
totalResids %<>% ggplot(aes(state,totalResid))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#task19 repeat task9+10 by using new name (by_state2)
by_state2 <- pop.ld.prism %>% group_by(state)
by_state2 %<>% nest()
by_state2

#task20 write a function that accepts an element of by_state2 and returns spearman correlation
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method='spearman')$estimate)
}

by_state2 %<>% mutate(spCor = purrr::map(data, runCor))

spCors <- unnest(by_state2,spCor)
spCors %<>% arrange(desc(spCor))
spCors$state <- factor(spCors$state, levels=unique(spCors$state))

ggplot(spCors,aes(state,spCor))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
