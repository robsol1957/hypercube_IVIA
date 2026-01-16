source("fns.R")
get_lead_time <- function(df) {

  df$cumprob <- cumsum(df$prob)
  s <- runif(1,min=0,max=,max(df$cumprob))
  r <- df %>%
    mutate(
      min = lag(cumprob),
      min = ifelse(is.na(min), 0, min)
    ) %>%
    filter(s < cumprob, s >= min)
 as.numeric( r$days)
}


all_forecast_200_10_0_9 <- read_csv("all_forecast_200_10_0.9.csv")
avg_daily_sales <- 5.255030
lead_times <- data.frame(days=c(1:3),prob=c(.7,.2,.1))
target_stocks=5
# avg_lead_time <- sum(lead_times$days*lead_times$prob)/sum(lead_times$prob)

results <- data.frame(day=c(1:365))
results$sales=0
results$opening_stock=0
results$on_order=0
results$received=0

results$orders=0
results$lt=0
results$dd=0
results$closing_stock = 0
day=1
for(day in 2:(nrow(results)-5)){
  results$opening_stock[day] <- results$closing_stock[day-1]
  results$sales[day] <- rpois(1,avg_daily_sales)
  
  #results$on_order[day] <- results$on_order[day-1]
  
  results$closing_stock[day] <- results$closing_stock[day-1] + 
    results$received[day] -
    results$sales[day] 
    
  results$orders[day]=max(0,target_stocks - results$closing_stock[day])
  results$lt[day] <- get_lead_time(lead_times)
  results$dd[day] = results$lt[day] +day
  results$received[results$dd[day]] <- results$received[results$dd[day]]+
    results$orders[day]
  results$on_order[day+1]= results$on_order[day]+results$orders[day]-
    results$received[day]
  
}
results %>% ggplot(aes(x=day,y=closing_stock))+geom_point()

get_lead_time(lead_times)

rpois(100,avg_lead_time)
mean(rpois(1000,avg_daily_sales))

df <- lead_times

