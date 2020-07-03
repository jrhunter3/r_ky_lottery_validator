library('tidyverse')
library('rvest')
library('httr')
library('lubridate')
library('arrangements')
options(stringsAsFactors = F, scipen = 999)

##### ESTABLISH TIMES FOR TODAYS RESULTS
start_time <- as.numeric(seconds(floor_date(Sys.time(), 'day'))) * 1000

##### ESTABLISH CONNECTION TO SITE
keno_url <- str_c('https://play.kylottery.com/api/v1/draw-games/draws?game-names=Keno',
                  '&date-from=', start_time)

##### PARSE RESULTS FROM SESSION
keno_data <- jsonlite::fromJSON(keno_url)
keno_results <- do.call('bind_rows', lapply(1:nrow(filter(keno_data$draws, status == 'CLOSED')), function(i){
  data.frame(
    draw_time = as.POSIXct(keno_data$draws$drawTime[[i]] / 1000, origin = '1970-01-01', timezone='America/Chicago'),
    draw_id = as.numeric(keno_data$draws$id[[i]]),
    primary = as.numeric(keno_data$draws$results[[i]]$primary[[1]]),
    bullseye = as.numeric(keno_data$draws$results[[i]]$secondary[[1]]),
    multiplier = keno_data$draws$results[[i]]$multiplier
  ) %>%
    arrange(draw_id, primary)
}))

##### OBTAIN ALL COMBINATIONS OF SEVEN NUMBERS
pick_sevens <- do.call('bind_rows', lapply(unique(keno_results$draw_id), function(x) {
  filter(keno_results, draw_id == x) %>% 
    pull(primary) %>%
    combinations(k = 7) %>%
    as.data.frame() %>%
    mutate(draw_group = str_c(V1, '-', V2, '-', V3, '-',
                              V4, '-', V5, '-', V6, '-', V7),
           draw_id = x) %>%
    select(draw_id, draw_group)}))

pick_fours <- do.call('bind_rows', lapply(unique(keno_results$draw_id), function(x) {
  filter(keno_results, draw_id == x) %>% 
    pull(primary) %>%
    combinations(k = 4) %>%
    as.data.frame() %>%
    mutate(draw_group = str_c(V1, '-', V2, '-', V3, '-', V4),
           draw_id = x) %>%
    select(draw_id, draw_group)}))

##### REQUIRES TOO MUCH MEMORY
# pick_tens <- do.call('bind_rows', lapply(unique(keno_results$draw_id), function(x) {
#   filter(keno_results, draw_id == x) %>% 
#     pull(primary) %>%
#     combinations(k = 10) %>%
#     as.data.frame() %>%
#     mutate(draw_group = str_c(V1, '-', V2, '-', V3, '-',
#                               V4, '-', V5, '-', V6, '-',
#                               V7, '-', V8, '-', V9, '-', V10),
#            draw_id = x) %>%
#     left_join(keno_results, by = 'draw_id') %>%
#     select(draw_time, draw_id, draw_group, bullseye, multiplier)}))

write_rds(pick_sevens,
          str_c('results/',as.Date(floor_date(Sys.time(), 'day')), '.RDS'))

pick_sevens %>%
  select(draw_group) %>%
  group_by(draw_group) %>%
  count() %>%
  arrange(-n)
head()

pick_fours %>%
  select(draw_group) %>%
  group_by(draw_group) %>%
  count() %>%
  arrange(-n) %>%
  head(20)
