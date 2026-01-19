source("fns.R")



#all_forecast_200_10_0_9 <- read_csv("all_forecast_200_10_0.9.csv")


# avg_lead_time <- sum(lead_times$days*lead_times$prob)/sum(lead_times$prob)


avg_daily_sales <- 5.255030
lead_times <- data.frame(days = c(1:3), prob = c(.7, .2, .1))
min_batch=1
results <- simulate(target_stocks=10,
                     avg_daily_sales=avg_daily_sales,
                     lead_times=lead_times ,
                     min_batch=min_batch)

df <- results$results_df
 df %>% ggplot(aes(x=day,y=closing_stock))+geom_point()

all_results <- run_simulation_stock_seq(stock_seq=rep.int(seq(2:20),times=10),
                   avg_daily_sales=avg_daily_sales,
                   lead_times=lead_times,
                   min_batch=1)
write.csv(all_results,paste0("all_results_",avg_daily_sales,"_min_batch_",min_batch,".csv"),row.names = F)
all_results <- read_csv("all_results_5.25503_min_batch_1.csv") %>% 
  mutate(pct = sales/theoretic_sales)

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






df <- all_results %>% 
  mutate(pct = sales/max(sales),
         pct2=pct*pct) 
summ <- all_results %>% 
  group_by(avg_daily_sales, min_batch, avg_lt,target_stock) %>%
  summarise(avg_pct = mean(pct))
df <- left_join(df,summ)
use <- df  %>% 
  filter(pct>0.2 & pct<0.98)
#nls_model <- nls(target_ ~ (s+pct2) / (a + b * pct), data = use, start = list(a = 0.1, b = 0.1,s=1))
nls_model <- nls(avg_inv ~ (s) / (2*(1 - pct)), data = use, start = list(s=-.2))
summary(nls_model)
df$fit <- predict(nls_model,newdata = df$pct)

df %>% ggplot(aes(x=pct,y=avg_inv))+ geom_point()+
  geom_smooth()+
  geom_line(aes(y=fit),colour="red")

loess_m <- loess(df$target_stock~df$pct)
df$fit <- predict(loess_m,newdata = df$pct)
goal=0.95
yg <- predict(loess_m,goal)
path <- data.frame(x=c(min(df$pct),goal,goal),y=c(yg,yg,0))
df %>% ggplot(aes(x=pct,y=target_stock))+ geom_point()+
  geom_line(aes(y=fit),colour="red")+
  annotate("label",x=min(df$pct+.2),y=yg+1,
           label = paste0("target stock level :", ceiling(yg)),
           fill="pink",colour="black")+
  geom_line(data=path,aes(x=x,y=y))

  



