# environment
library(httr)
library(xml2)
library(rvest)
library(stringr)
library(magrittr)
library(lubridate)

# get URL
weather_url <- 'https://darksky.net/details/42.3523,-71.1214/2018-12-1/us12/en' 

# Raw Data
weather_raw <- weather_url %>% GET() %>% read_html(encoding = 'UTF-8')
weather_info_raw <- weather_raw %>% html_nodes("script") %>% html_text()

# Draw Data
weather_info_vec <- weather_info_raw %>% strsplit(',\\{') %>% unlist
result_df <- NULL
for(i in seq(length(weather_info_vec))){
  weather_info_vec2 <- unlist(strsplit(weather_info_vec[i], ","))[1:17]
  weather_info_vec3 <- gsub('\"', "", weather_info_vec2)
  name_vec <- str_match(weather_info_vec2, '\\\"(.*?)\\\":')[,2]
  val_vec  <- gsub('\"|}', "",str_match(weather_info_vec2, ':(.*)')[,2])
  result_df <- rbind(result_df, val_vec)
  colnames(result_df) <- name_vec
}
print(result_df)


# weather clawer
location <- data.frame(place = c('Boston University', 
                                 'Back Bay', 
                                 'Becon Hill', 
                                 'Fenway', 
                                 'Financial District',
                                 'Haymarket Square', 
                                 'North End', 
                                 'North Station', 
                                 'Northeastern University', 
                                 'South Station', 
                                 'Theatre District', 
                                 'West End'), 
                       coordinate = c('42.3523,-71.1214', 
                                      '42.3518,-71.0805', 
                                      '42.3593,-71.0682',
                                      '42.3495,-71.0992',
                                      '42.3524,-71.0562',
                                      '42.364,-71.0576',
                                      '42.3644,-71.0547',
                                      '42.3671,-71.0633', 
                                      '42.3355,-71.0889',
                                      '42.3529,-71.0555',
                                      '42.3504,-71.0649',
                                      '42.3646,-71.0659'))
result_total <- NULL

for(area in location$place) {
  month <- 10
  day <- 1
  result <- NULL
  while(month < 13) {
    weather_url <- paste0('https://darksky.net/details/', location$coordinate[location$place == area], '/2018-', month, '-', day,'/us12/en')
    if(day < 32) {
      weather_raw <- weather_url %>% GET() %>% read_html(encoding = 'UTF-8')
      weather_info_raw <- weather_raw %>% html_nodes("script") %>% html_text()
      weather_info_vec <- weather_info_raw %>% strsplit(',\\{') %>% unlist
      for(i in seq(length(weather_info_vec))){
        weather_info_vec2 <- unlist(strsplit(weather_info_vec[i], ","))[1:17]
        weather_info_vec3 <- gsub('\"', "", weather_info_vec2)
        name_vec <- str_match(weather_info_vec2, '\\\"(.*?)\\\":')[,2]
        val_vec  <- gsub('\"|}', "",str_match(weather_info_vec2, ':(.*)')[,2])
        result <- rbind(result, val_vec)
        colnames(result) <- name_vec
      }
      day <- day + 1
    } else {
      month <- month + 1
      day <- 1
    }
  }
  result_df <- as.data.frame(result)
  result_df$location <- area
  result_total <- rbind(result_total, result_df)
}

# Save result
write.csv(result_total, 'web_weather.csv')

