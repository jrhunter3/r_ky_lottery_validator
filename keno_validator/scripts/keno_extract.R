library('tidyverse')
library('rvest')
library('httr')
library('lubridate')
library('arules')
library('arulesViz')
options(stringsAsFactors = F, scipen = 999)

##### USED TO MASK BOT FROM WEBSITE (NOT NEEDED; I FOUND API)
# user_agent_url <- 'https://developers.whatismybrowser.com/useragents/explore/software_type_specific/web-browser/2'
# 
# s <- html_session(user_agent_url)
# user_agent_df <- html_table(s)[[1]]
# user_agent_list <- user_agent_df %>%
#   filter(OS %in% c('Windows', 'Mac OS X')) %>%
#   pull(`User agent`)

##### ESTABLISH TIMES FOR YESTERDAYS RESULTS
start_time <- as.numeric(seconds(floor_date(Sys.time() - ddays(1), 'day'))) * 1000
end_time <- as.numeric(seconds(floor_date(Sys.time(), 'day'))) * 1000

##### ESTABLISH CONNECTION TO SITE
keno_url <- str_c('https://play.kylottery.com/api/v1/draw-games/draws?game-names=Keno',
                  '&date-from=', start_time, '&date-to=', end_time)

##### PARSE RESULTS FROM SESSION
keno_data <- jsonlite::fromJSON(keno_url)
keno_results <- do.call('rbind', lapply(1:nrow(keno_data$draws), function(x){
  data.frame(draw_id = as.numeric(keno_data$draws$id[[x]]),
             results = as.numeric(keno_data$draws$results[[x]]$primary[[1]]))
}))

# CONVERT TO TRANSACTIONAL DATA
keno_results %>%
  plyr::ddply('draw_id', function(df){paste(df$results, collapse = ",")}) %>%
  select(numbers = V1) %>%
  write.csv('data/draw_basket.csv', append = T)

keno_transactions <- read.transactions('data/draw_basket.csv', format = 'basket', sep = ",")

# CREATE APRIORI RULES
association.rules <- apriori(keno_transactions,
                             parameter = list(supp=0.001, conf=0.8, maxlen=10, minlen=1))
association.rules <- association.rules[!is.redundant(association.rules)]

summary(association.rules)
inspect(association.rules[1:10])
itemFrequencyPlot(keno_transactions, topN=10)

#### ENRICH DATA
keno_results <- keno_results %>%
  group_by(results) %>%
  dplyr::summarize(freq = dplyr::n()) %>%
  mutate(date = as.Date(floor_date(Sys.time() - ddays(1), 'day'))) %>%
  arrange(results)

#### COLLECT DAILY HISTORICAL COPY
write_rds(keno_results,
          str_c('results/',as.Date(floor_date(Sys.time() - ddays(1), 'day')), '.RDS'))

#### APPEND NEW DATA TO OLD DATA
read_rds('results/historical.RDS') %>%
  bind_rows(keno_results) %>%
  write_rds('results/historical.RDS')

read_rds('results/historical.RDS') %>%
  ggplot() +
  geom_col(aes(x = as.factor(results), y = freq), fill = 'blue') +
  theme_bw() +
  labs(title = 'Keno Draws',
       x = 'Number',
       y = 'Count')
