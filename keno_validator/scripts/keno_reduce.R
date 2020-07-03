library('tidyverse')
library('rvest')
library('httr')
library('lubridate')
options(stringsAsFactors = F, scipen = 999)


##### ESTABLISH TIMES FOR TODAYS RESULTS
start_time <- as.numeric(seconds(floor_date(Sys.time(), 'day'))) * 1000

##### ESTABLISH CONNECTION TO SITE
keno_url <- str_c('https://play.kylottery.com/api/v1/draw-games/draws?game-names=Keno',
                  '&date-from=', start_time)

##### PARSE RESULTS FROM SESSION
keno_data <- jsonlite::fromJSON(keno_url)
keno_results <- do.call('bind_rows', lapply(1:nrow(keno_data$draws), function(x){
  if(keno_data$draws$status[[x]] == 'CLOSED'){
    data.frame(draw_id = as.numeric(keno_data$draws$id[[x]]),
               results = as.numeric(keno_data$draws$results[[x]]$primary[[1]]))
  } else {
    data.frame()
  }
}))

keno_results <- keno_results %>%
  group_by(results) %>%
  dplyr::summarize(freq = dplyr::n())

# keno_results %>%
#   mutate(results = results %% 10) %>%
#   group_by(results) %>%
#   summarize(freq = sum(freq)) %>%
#   arrange(freq)

##### LEAST COLUMN SEVEN PICKER
keno_results %>%
  mutate(column = results %% 10) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  ungroup() %>%
  filter((results %% 10) == column[which.min(col_freq)]) %>%
  arrange(freq)

##### TEN PICKER
# bind_rows(sample_n(head(arrange(keno_results, -freq), 10), size = 5),
#           sample_n(tail(arrange(keno_results, -freq), 10), size = 5))
