# Author: Alona Botnar #
# Date: May 14, 2018 #
# Purpose: To Learn Programming using R #
# Data: West Nile Virus by CDC and US Geological Survey #

#load data
wnv <- read.csv('wnv.csv')

#perform analysis
wnv$Year <- as.factor(wnv$Year)
ggplot(data = wnv) + geom_histogram(aes(x=State, y=Total, fill= Year), stat='identity')+ labs(x='year', y= 'case count', title= 'Total # of cases in each state in each year') +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create a new column of log of Total

wnv$logTotal <- log(wnv$Total)
ggplot(data = wnv) + geom_histogram(aes(x=State, y=logTotal, fill= Year), stat='identity')+ labs(x='year', y= 'case count', title= 'Log Total # of cases in each state in each year') +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create new graph with log of total
ggplot(data = wnv) + geom_histogram(aes(x=State, y=log(Total), fill= Year), stat='identity')+ labs(x='year', y= 'case count', title= 'Total # of cases in each state in each year') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = wnv) + geom_histogram(aes(x=State, y=Total, fill= Year), stat='identity')+ 
  labs(x='year', y= 'case count', title= 'Total # of cases in each state in each year') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()

#calculate raw case fatality rate (CFR)
wnv$CFR <- wnv$Fatal / wnv$Total 
ggplot(data = wnv) + geom_histogram(aes(x=State, y=CFR, fill= Year), stat='identity')+ 
  labs(x='year', y= 'CFR', title= 'Raw Case Fatality Rate Per Person Per Year') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#verify the variable Total
wnv$EncephMen + wnv$Fever + wnv$Other
wnv$Verify_Total <- wnv$EncephMen + wnv$Fever + wnv$Other
wnv$Total == wnv$Verify_Total
wnv$Total == wnv$EncephMen + wnv$Fever + wnv$Other

#annual case count for each state


#calculate neuroinvasive disease rate (NDR)
wnv$NDR <- wnv$EncephMen/wnv$Total

mean <- function(x) {  
  s <- sum(x)
  n <- length(x)
  m <- s/n
  return(m)}

x <- wnv$NDR
mean(x)

standard.error <- function(x) {
  s <- sd(x)
  n <- sqrt(length(x))
  m <- s/n
  return(m)
}

x <- wnv$NDR
standard.error(x)


mean(subset(wnv, State == "California")$NDR)
mean(subset(wnv, State == "Colorado")$NDR)
mean(subset(wnv, State == "New York")$NDR)

standard.error(subset(wnv, State == "California")$NDR)
standard.error(subset(wnv, State == "Colorado")$NDR)
standard.error(subset(wnv, State == "New York")$NDR)

#plot NDR as a bar graph

ggplot(data=subset(wnv, State %in% c('California', 'Colorado', 'New York') + 
                     geom_bar(mapping = aes(x=DNR))))