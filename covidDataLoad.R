# R script for loading covid-related data from:
#   1) Czech UZIS/MZ data through apify
#   2) Johns-Hopkins github repo
#   3) Population size in countries (from wikipedia)
#
# Started Eda Bakstein, eduard.bakstein@nudz.cz 2020-03-16


# required libraries
library(tidyverse)
library(lubridate)

# web scraping
library(rvest)
library(jsonlite)



## 1) LOAD CZECH DATA FROM APIFY
covidLoadCZ<-function(){
  
  apifyCZurl<-'https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true'
  czParsed <- fromJSON(txt=apifyCZurl)
  
  # time series
  testedCases<-czParsed$numberOfTestedGraph %>% mutate(value=as.numeric(str_remove(value,',')), date=as.Date(substr(date,1,10))) %>% rename(testedCases='value')
  totalPositiveTests<-czParsed$totalPositiveTests %>% mutate(value=as.numeric(str_remove(value,',')), date=as.Date(substr(date,1,10))) %>% rename(totalCases='value')
  
  # merge + compute incident cases
  covidCZ<-merge(testedCases,totalPositiveTests,by='date',all.x=T,all.y=T) %>% arrange(date) %>% mutate(newCases=c(0,diff(totalCases)))
  covidCZ$propIncrease<-c(0,covidCZ$newCases[2:nrow(covidCZ)]/covidCZ$totalCases[1:(nrow(covidCZ)-1)])
  covidCZ[!is.na(covidCZ$propIncrease) & covidCZ$propIncrease==Inf,'propIncrease']<-1
  
  # cases by region
  covidCZregions <- czParsed$infectedByRegion %>% mutate(value=as.numeric(value)) %>% rename(totalCases='value')
  
  # quarantine by infection country
  countryOfInfection <- czParsed$countryOfInfection %>% mutate(value=as.numeric(value)) %>% rename(country='countryName',count='value')
  
  # age sex distribution
  sexes <- str_replace(czParsed$infectedByAgeSex$sex,c('muž','žena'),c('m','f')) 
  infectedByAgeSex <- rbind(mutate(czParsed$infectedByAgeSex$infectedByAge[[1]],sex=sexes[1]),mutate(czParsed$infectedByAgeSex$infectedByAge[[2]],sex=sexes[2])) %>% 
        rename(ageGrp='age',infected='value') %>% 
        mutate(sex=as.factor(sex),ageLb=as.numeric(str_extract(ageGrp,'^(\\d{1,2})')))
  
  # return both as a list
  return(list(covidCZ=covidCZ,covidCZregions=covidCZregions,countryOfInfection=countryOfInfection,infectedByAgeSex=infectedByAgeSex))
  
}


## 2) LOAD JONS-HOPKINS DATA FROM GITHUB
covidLoadJHU<-function(){
  
  jhu_base_url<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  
  # jhu_conf_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", "time_series_19-covid-Confirmed.csv", sep = "")
  
  # jhu_conf_url <- paste0(jhu_base_url,"time_series_19-covid-Confirmed.csv")
  # jhu_deaths_url <- paste0(jhu_base_url,"time_series_19-covid-Deaths.csv")
  # jhu_recovered_url <- paste0(jhu_base_url,"time_series_19-covid-Recovered.csv")
  
  jhu_conf_url <- paste0(jhu_base_url,"time_series_covid19_confirmed_global.csv")
  jhu_deaths_url <- paste0(jhu_base_url,"time_series_covid19_deaths_global.csv")
  # jhu_recovered_url <- paste0(jhu_base_url,"time_series_covid19_recovered_global.csv")
  
    
  covid_conf<-read_csv(jhu_conf_url) %>% 
        rename(province = "Province/State", country = "Country/Region") %>% 
        pivot_longer(-c(province, country, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
        arrange(province, Date) %>% 
        group_by(country, Date) %>% 
        summarise(cases=sum(cumulative_cases)) %>% 
        mutate(Date=mdy(Date)) %>% arrange(country, Date) %>% 
        group_by(country) %>% 
        mutate(incident = c(0, diff(cases))) 
  
  covid_deaths<-read_csv(jhu_deaths_url) %>% 
        rename(province = "Province/State",country = "Country/Region") %>% 
        pivot_longer(-c(province, country, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
        arrange(province, Date) %>% group_by(country, Date) %>% 
        summarise(deaths=sum(cumulative_cases)) %>% 
        mutate(Date=mdy(Date)) %>% arrange(country, Date)
  
  # covid_recovered<-read_csv(jhu_recovered_url) %>% rename(province = "Province/State", 
  #                                                         country = "Country/Region") %>% pivot_longer(-c(province, 
  #                                                                                                         country, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% arrange(province, Date) %>% group_by(country, Date) %>% summarise(recovered=sum(cumulative_cases)) %>% mutate(Date=mdy(Date)) %>% arrange(country, Date)
  
  # combine the 2 tables
  covid<-left_join(covid_conf,covid_deaths,by=c('country','Date'))
  # covid<-left_join(covid,covid_recovered,by=c('country','Date')) 
  
  # modify country names - to be consistent with wiki 
  covid$country <- str_remove(covid$country,' \\(.+\\)') # remove country specifier (...republic of)
  covid$country <- str_remove(covid$country,'Mainland ')
  covid$country <- str_replace(covid$country,'Republic of Korea','South Korea')
  covid$country <- str_replace(covid$country,'Korea, South','South Korea')
  covid$country <- str_replace(covid$country,'Czechia','Czech Republic')
  covid$country <- str_replace(covid$country,'US','United States')
  
  # return combined data
  return(covid)
}

## 3) Ppopulation from wikipedia
loadPopWiki<-function(){
  ## WIKI number of inhabitants
  
  # get number of inhabitants: scrape from a wiki page 
  
  # the URL of the wikipedia page to use is in wp_page_url
  wp_page_url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)'
  page_html <- read_html(wp_page_url)
  population <- page_html %>% html_nodes("table") %>% .[[4]] %>% 
    html_table(fill = TRUE)
  
  # clean up and convert to numbers
  population <- population %>% rename(country='Country or area',population2018='Population(1 July 2018)',population='Population(1 July 2019)') %>% mutate(country=str_replace(country,'\\[.+\\]','')) %>% mutate(population=as.numeric(str_replace_all(population,'\\,','')))
  
  return(population)
}

attachPopCounts<-function(covid){
  
  # load data from wikipedia
  population<-loadPopWiki()
  
  # attach the population data to the covid dataset
  covid<-left_join(covid,population[,c('country','population')], by='country')
  covid$casesPer1M <- 10^6*covid$cases/covid$population
  
  return(covid)
}

# 4) Public Policy CZ, Wiki
loadPolicyCzWiki <- function(){
  
  # the URL of the wikipedia page to use is in wp_page_url
  wp_page_url <- 'https://cs.wikipedia.org/wiki/Pandemie_COVID-19_v_%C4%8Cesku'
  page_html <- read_html(wp_page_url)
  
  tbl <- page_html %>% html_nodes("table") %>% .[[3]]
  
  # parse the text
  policy <- tbl %>% html_table(fill = TRUE)
  
  # # parse links
  # policy$URL <- tbl %>% html_nodes('tr td a') %>% html_attr('href')
  
  names(policy)<-c('date','policyNo','description','valid','link')
  policy$date <- parse_date(policy$date,format='%d. %B %Y',locale = locale('cs'))
  
  return(policy)
}

# 5) Public policy CZ, zakonyprolidi
loadPolicyCZzakonyprolidi <- function(){
  
  wp_page_url <- 'https://www.zakonyprolidi.cz/koronavirus'
  page_html <- read_html(wp_page_url)
  policy <- page_html %>% html_node('.ResultList')%>% html_nodes(".Item") %>% map_df(~{
      ident = .x %>% html_nodes( 'a') %>%  html_text()
      link = .x %>% html_nodes('a') %>%  html_attr('href')
      validSince = .x %>% html_nodes('.NodeNote') %>% html_text() %>% str_extract('\\d{1,2}\\.\\d{1,2}\\.\\d{4}') %>% parse_date(format='%d.%m.%Y')
      desc =  .x %>% html_nodes('.NodeName') %>% html_text()
      data.frame(validSince, ident,link,desc)
    })
  return(policy)
}

# 6) population counts from UN https://population.un.org/wpp/Download/Standard/CSV/
# TBD

