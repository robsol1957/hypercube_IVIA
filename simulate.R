source("fns.R")



#all_forecast_200_10_0_9 <- read_csv("all_forecast_200_10_0.9.csv")


# avg_lead_time <- sum(lead_times$days*lead_times$prob)/sum(lead_times$prob)

get_lead_time <- function(df) {
  df$cumprob <- cumsum(df$prob)
  s <- runif(1, min = 0, max = , max(df$cumprob))
  r <- df %>%
    mutate(min = lag(cumprob), min = ifelse(is.na(min), 0, min)) %>%
    filter(s < cumprob, s >= min)
  as.numeric(r$days)
}
simulate <- function(target_stocks,
                     avg_daily_sales,
                     lead_times,
                     min_batch
                     ) {
  results <- data.frame(day = c(1:365))
  
  results$opening_stock = 0
  results$received = 0
  results$soh = 0
  results$theoretic_sales =0
  results$demand = 0
  results$sales = 0
  results$lost_sales = 0
  results$on_order = 0
  results$closing_stock = 0
  results$orders = 0
  results$lt = 0
  results$dd = 0
  results$closing_stock[1] <- target_stocks
  for (day in 2:(nrow(results) - 1)) {
    results$theoretic_sales[day] <- avg_daily_sales
    results$opening_stock[day] <- results$closing_stock[day - 1]
    results$soh[day] <- results$opening_stock[day] + results$received[day]
    results$demand[day] <- rpois(1, avg_daily_sales)
    if (results$demand[day] > results$soh[day]) {
      results$sales[day] <- results$soh[day]
      results$lost_sales[day] <- results$demand[day] - results$soh[day]
    } else {
      results$sales[day] <-  results$demand[day]
      results$lost_sales[day]   <- 0
    }
    
    
    results$closing_stock[day] <- results$soh[day] -
      results$sales[day]
    outstanding_orders = results$on_order[day]-results$received[day]
    pot_orders <- max(0, target_stocks - results$closing_stock[day]-outstanding_orders)
    if (pot_orders >= min_batch) {
      results$orders[day] = pot_orders
    } else {
      results$orders[day] = 0
    }
    results$lt[day] <- get_lead_time(lead_times)
    
    results$dd[day] = results$lt[day] + day
    if (results$dd[day] <= nrow(results)) {
      results$received[results$dd[day]] <- results$received[results$dd[day]] +
        results$orders[day]
    }
    results$on_order[day + 1] = outstanding_orders + results$orders[day] 
    
  }
  list(
    results_df = results,
    avg_stocks = mean(results$closing_stock),
    sales = sum(results$sales),
    lost_sales = sum(results$lost_sales),
    theoretic_sales= sum(results$theoretic_sales)
  )
}


run_simulation_stock_seq <- function(stock_seq,
                               avg_daily_sales,
                               lead_times,
                               min_batch) {
  been_b4=FALSE
  for (stocks in stock_seq) {
    print(stocks)
    results <- simulate(
      target_stocks = stocks,
      avg_daily_sales = avg_daily_sales,
      lead_times = lead_times,
      min_batch = min_batch
    )
    if (!been_b4) {
      all_results <- data.frame(
        target_stock = stocks,
        avg_inv = results$avg_stocks,
        sales = results$sales,
        theoretic_sales = results$theoretic_sales,
        lost_sales = results$lost_sales
      )
      been_b4=TRUE
    } else {
      all_results <- rbind(
        all_results,
        data.frame(
          target_stock = stocks,
          avg_inv = results$avg_stocks,
          sales = results$sales,
          theoretic_sales = results$theoretic_sales,
          lost_sales = results$lost_sales
        )
      )
    }
  }
  all_results
}
avg_daily_sales <- 5.255030
lead_times <- data.frame(days = c(1:3), prob = c(.7, .2, .1))
results <- simulate(target_stocks=10,
                     avg_daily_sales=avg_daily_sales,
                     lead_times=lead_times ,
                     min_batch=1)
df <- results$results_df
 df %>% ggplot(aes(x=day,y=closing_stock))+geom_point()

all_results <- run_simulation_stock_seq(stock_seq=rep.int(seq(2:20),times=10),
                   avg_daily_sales=avg_daily_sales,
                   lead_times=lead_times,
                   min_batch=1)
all_results %>% ggplot(aes(x = target_stock, y = sales)) + geom_point(colour="blue")+
  geom_smooth(colour="blue",fill="blue",alpha=0.1)+
  geom_point(aes(y=lost_sales),colour="red")+
  geom_smooth(aes(y=lost_sales),colour="red",fill="red",alpha=0.1)+
  geom_line(aes(y=theoretic_sales),colour="black")+
  labs(title="Impact of Target Stock on Sales performance",
       subtitle = "Blue - Sales : Red - Lost Sales : Black -Expected Sales",
       x="Target Stock Level (units)",
       y= "Sales (units)")+
  annotate("label",x=max(all_results$target_stock/2),y=max(all_results$sales/2),
           label = paste0("avg_sales :", round(avg_daily_sales,2)),
           fill="pink",colour="black"
           )

