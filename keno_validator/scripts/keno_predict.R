library('tidyverse')
# library('parallel')
# cl <- makeCluster(detectCores()-1)
# clusterEvalQ(cl, library('dplyr'))
# clusterExport(cl, varlist = list("keno_results"))

glimpse(keno_results)

##### ESTABLISH TIMES FOR YESTERDAYS RESULTS
start_time <- as.numeric(seconds(floor_date(Sys.time(), 'day'))) * 1000

##### ESTABLISH CONNECTION TO SITE
keno_url <- str_c('https://play.kylottery.com/api/v1/draw-games/draws?game-names=Keno',
                  '&date-from=', start_time)

##### PARSE RESULTS FROM SESSION
keno_data <- jsonlite::fromJSON(keno_url)

keno_results <- do.call('bind_rows', lapply(1:nrow(keno_data$draws), function(i){
  if(keno_data$draws$status[[i]] == 'CLOSED'){
    data.frame(
      draw_time = as.POSIXct(keno_data$draws$drawTime[[i]] / 1000, origin = '1970-01-01', timezone='America/Chicago'),
      draw_id = as.numeric(keno_data$draws$id[[i]]),
      primary = as.numeric(keno_data$draws$results[[i]]$primary[[1]]),
      bullseye = as.numeric(keno_data$draws$results[[i]]$secondary[[1]]),
      multiplier = keno_data$draws$results[[i]]$multiplier
    ) %>%
      arrange(draw_id, primary)
  } else {
    data.frame()
  }
}))

keno_results %>%
  group_by(multiplier) %>%
  count()

ggplot(keno_results) +
  geom_col(aes(x = draw_time, y = multiplier))

keno_results %>%
  group_by(draw_id, primary) %>%
  summarise(freq = n()) %>%
  group_by(primary) %>%
  arrange(draw_id) %>%
  mutate(freq = cumsum(freq)) %>% {
    lm(primary ~ freq, data = .)
  } %>%
  summary()

keno_results %>%
  select(draw_id, draw = primary) %>%
  mutate(drawn = T) %>%
  complete(draw, nesting(draw_id)) %>%
  mutate(drawn = ifelse(draw_id == 618015 & is.na(drawn), F, drawn)) %>%
  spread(draw, drawn) %>%
  fill(-draw_id) %>%
  gather(draw, drawn, -draw_id) %>%
  mutate(draw = parse_number(draw)) %>%
  group_by(draw, drawn) %>%
  count() %>%
  filter(!drawn) %>% View()

# MOSTLY AROUND 12 DRAWS BEFORE A NUMBER WILL COME UP AGAIN

##### CHOOSE LEAST COLUMN
keno_results %>%
  #filter(draw_id < 618495) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(column = results %% 10) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(col_freq, freq) %>%
  head(8)

##### CHOOSE LEAST ROW
keno_results %>%
  filter(draw_id < 618495) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
         row = ifelse((results %% 10 == 0), row - 1, row)) %>%
  group_by(row) %>%
  mutate(row_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(row_freq, freq) %>%
  head(10)

##### CHOOSE LEAST FREQ
keno_results %>%
  filter(draw_id < 618495) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(column = results %% 10) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  ungroup() %>% arrange(freq) %>%
  head(30) %>%
  select(results) %>%
  sample_n(7)

keno_results %>%
  filter(draw_id < 618490) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
         row = ifelse((results %% 10 == 0), row - 1, row)) %>%
  group_by(row) %>%
  mutate(row_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(row_freq, freq) %>%
  head(10) %>%
  select(results) %>%
  sample_n(7) %>%
  arrange(results)

keno_results %>%
  filter(draw_id == 618490) %>%
  pull(primary)

keno_results %>%
  filter(draw_id < 618482) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(column = results %% 10) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(col_freq, freq) %>%
  head(8) %>%
  select(results) %>%
  sample_n(7)

keno_results %>%
  filter(draw_id == 618482) %>%
  pull(primary)

##### CHOOSE LEAST COLUMN AND ROW
keno_results %>%
  filter(draw_id < 618486) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(column = results %% 10,
         row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
         row = ifelse((results %% 10 == 0), row - 1, row)) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  group_by(row) %>%
  mutate(row_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(col_freq + row_freq, freq) %>%
  select(results) %>%
  head(25) %>%
  sample_n(10) %>%
  arrange(results)

keno_results %>%
  filter(draw_id == 618486) %>%
  pull(primary)

###### THERE IS ALWAYS A COUPLE FROM THE TOP NUMBERS TO ENSURE NO 10 SPOT $5 PRIZE
keno_results %>%
  filter(draw_id < 618494) %>%
  group_by(results = primary) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(column = results %% 10,
         row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
         row = ifelse((results %% 10 == 0), row - 1, row)) %>%
  group_by(column) %>%
  mutate(col_freq = sum(freq)) %>%
  group_by(row) %>%
  mutate(row_freq = sum(freq)) %>%
  ungroup() %>%
  arrange(col_freq + row_freq, freq) %>%
  select(results) %>%
  head(25) %>%
  tail(10) %>%
  sample_n(10) %>%
  arrange(results)

keno_results %>%
  filter(draw_id == 618491) %>%
  pull(primary)

sum(sample(1:80, size = 10) %in% (keno_results%>%filter(draw_id == 618444)%>%pull(primary)))
test <- sample(1:80, size = 10)
sum(test %in% (keno_results%>%filter(draw_id == 618464)%>%pull(primary)))
       
# do.call('bind_rows', parLapply(cl, min(keno_results$draw_id):max(keno_results$draw_id), function(x){
#   sum(sample(1:80, size = 10) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
# }))

##### TESTING PURE RANDOMNESS
do.call('rbind', lapply(min(keno_results$draw_id):max(keno_results$draw_id), function(x){
  sum(sample(1:80, size = 7) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

##### TESTING THE LEAST COLUMN AND ROW METHOD
do.call('rbind', lapply((min(keno_results$draw_id)+1):max(keno_results$draw_id), function(x){
  sum((keno_results %>%
         filter(draw_id < x) %>%
         group_by(results = primary) %>%
         dplyr::summarize(freq = dplyr::n()) %>%
         mutate(column = results %% 10,
                row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
                row = ifelse((results %% 10 == 0), row - 1, row)) %>%
         group_by(column) %>%
         mutate(col_freq = sum(freq)) %>%
         group_by(row) %>%
         mutate(row_freq = sum(freq)) %>%
         ungroup() %>%
         arrange(col_freq + row_freq, freq) %>%
         select(results) %>%
         head(25) %>%
         sample_n(7) %>%
         pull(results)) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

##### TESTING THE LEAST COLUMN METHOD
do.call('rbind', lapply((min(keno_results$draw_id)+1):max(keno_results$draw_id), function(x){
  sum((keno_results %>%
         filter(draw_id < x) %>%
         group_by(results = primary) %>%
         dplyr::summarize(freq = dplyr::n()) %>%
         mutate(column = results %% 10) %>%
         group_by(column) %>%
         mutate(col_freq = sum(freq)) %>%
         ungroup() %>%
         arrange(col_freq, freq) %>%
         head(8) %>%
         select(results) %>%
         sample_n(7) %>%
         pull(results)) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

##### TESTING THE LEAST ROW METHOD
do.call('rbind', lapply((min(keno_results$draw_id)+1):max(keno_results$draw_id), function(x){
  sum((keno_results %>%
         filter(draw_id < x) %>%
         group_by(results = primary) %>%
         dplyr::summarize(freq = dplyr::n()) %>%
         mutate(row = ifelse(results < 10, 0, parse_number(str_sub(results, start = 1L, end = 1L))),
                row = ifelse((results %% 10 == 0), row - 1, row)) %>%
         group_by(row) %>%
         mutate(row_freq = sum(freq)) %>%
         ungroup() %>%
         arrange(row_freq, freq) %>%
         head(10) %>%
         select(results) %>%
         sample_n(7) %>%
         pull(results)) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

##### TESTING THE RANDOM COLUMN METHOD
do.call('rbind', lapply((min(keno_results$draw_id)+10):max(keno_results$draw_id), function(x){
  sum((keno_results %>%
         filter(draw_id < x) %>%
         group_by(results = primary) %>%
         dplyr::summarize(freq = dplyr::n()) %>%
         mutate(column = results %% 10) %>%
         group_by(column) %>%
         mutate(col_freq = sum(freq)) %>%
         ungroup() %>%
         filter(column == sample(0:9, size = 1)) %>%
         arrange(col_freq, freq) %>%
         head(8) %>%
         select(results) %>%
         sample_n(7) %>%
         pull(results)) %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

##### TESTING SAME RANDOM NUMBERS ALL DAY METHOD
test <- sample(1:80, size = 7)
do.call('rbind', lapply(min(keno_results$draw_id):max(keno_results$draw_id), function(x){
  sum(test %in% (keno_results%>%filter(draw_id == x)%>%pull(primary)))
})) %>% as.data.frame() %>%
  group_by(V1) %>%
  count() %>%
  arrange(V1)

sample(1:80, size = 10)
sample(0:1, size = 1) #MULTI
sample(0:1, size = 1) #BULLSEYE