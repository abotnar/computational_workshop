library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

setwd('~/lyme')
ld <- read_csv('lyme.csv')
prism <- read_csv('climate.csv')
pop <- read_csv('pop.csv')


#convert pop data to tidy data format
#reorganize data by fips and bring 2000s to top of list

pop %<>% select(fips,starts_with("pop2"))

#strings the data by year and orders the Fips- transpose from row to column

pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit

#add column that says just year instead of string year

pop %<>% mutate(year=str_replace_all(str_year,"pop",""))

#define column year as an integer (before was a variable)

pop %<>% mutate(year=as.integer(year))

#remove the 0 in front of fips...so starts with a 1 now and goes in order

pop %<>% mutate(fips=str_replace_all(fips,"^0",""))

#defines fips column as integer

pop %<>% mutate(fips=as.integer(fips))

# delete str_year column

pop$str_year <- NULL

#convert ld data to tidy data format

#transposes cases and years (row to column)
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")

#add column taking str year to year
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))

#define year column as integer
ld %<>% mutate(year=as.integer(year))

#rename titles to state and county
ld %<>% rename(state=STNAME,county=CTYNAME)

#building the fips using a function based on state and city codes if there are 3 characters then use those as integer, 2 characters add a 0 in front, 1 character add 00 in front
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    3
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

#uses the above function to create fips
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) # takes about 10 seconds

#remove unwanted columns and keep state, county, cases, year and fips
ld %<>% select(-c(STCODE,CTYCODE,str_year))

#combine ld and prism data
ld.prism<- inner_join(ld,prism)

#combine pop with ld and prism data
pop.ld.prism<- inner_join(pop,ld.prism)

#create new data frame to determine cases of lyme disease reported each year
cases_by_year <- ld %>% ungroup %>% group_by(year) %>% summarize(total=sum(cases)) %>% arrange(desc(total))

#create new data frame to determine average number of cases in each state- averaged acrouss county and year
#average by state 
avg_cases_state <- pop.ld.prism %>% group_by(state)  %>% summarize(avCase=mean(cases)) %>% arrange(desc(avCase))

#average by state and county and year
avg_cases_state_county <- pop.ld.prism %>% group_by(state, county, year)  %>% summarize(avCase=mean(cases)) %>% arrange(desc(avCase))

#average by year
avg_cases_year <- pop.ld.prism %>% group_by(year)  %>% summarize(avCase=mean(cases)) %>% arrange(desc(avCase))

#worst year was 2009
#3 states most impacted on average are Connecticut, Massachusetts, & Delaware

#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")


#create new dataset copying the grouped data file of pop,ld, and prism with fips
ag.fips <- group_by(pop.ld.prism,fips)

#create new dataset that summarizes previous dataset into sum of all cases by fip
ld.16y<-summarize(ag.fips,all.cases=sum(cases))

#add state and county data - join by fips
ld.16y<-left_join(select(pop.ld.prism,c(state,county,fips)),ld.16y)

#retain only unique rows (remove multiple)
ld.16y<-distinct(ld.16y)

#rename state and county to region and subregion
ld.16y %<>% rename(region=state,subregion=county)

#double checks previous run
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")

#convert region from upper to lowercase
ld.16y$region<-tolower(ld.16y$region)

#convert subregion from upper to lowercase
ld.16y$subregion<-tolower(ld.16y$subregion)

#create new column of log10 of cases from all.cases
ld.16y %<>% mutate(log10cases=log10(1+all.cases))

#create new data set joining previous set with county_map data set (so now have coordinates in dataset)
map.ld.16y<-left_join(county_map,ld.16y)

#create a point plot of above data set, set aes to long and lat with color being log10 cases, use color gradient- rainbow
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))

#save data frames (combined pop.ld.prism)
save(pop.ld.prism, file = 'pop.ld.prism.RData')
save(pop.ld.prism, file = 'pop.ld.prism.RDa')

write_csv(pop.ld.prism, file('pop.ld.prism.csv'))



