library(readr)
library(tidyverse)
library(stringr)
library(forecast)
"https://drive.google.com/drive/u/1/folders/1r9ZoiurpGuBy8OxkeuF3EptEgSXcC6Nz"
ewma.filter <- function (x, ratio, forecasts) {
  init <- round(0.5 / ratio)
  init <- mean(x[1:init])
  c(stats::filter(x * ratio, 1 - ratio, "recursive", init = init))
}

fit_forecast <- function(df, rows, forecasts) {
  comment = ""
  if (nrow(df) > (rows + forecasts)) {
    test_end = rows
    train <- df[1:test_end, ]
    test <- df[(test_end + 1):(test_end + forecasts), ]
    fit <- ets(train$sales, model = "ANN")
    lambda <- fit$par[1]
    if (lambda < 0.05) {
      lambda <- 0.05
      comment = paste0(comment,
                       "Warning lambda :",
                       round(lambda, digits = 3),
                       " default to 0.05 ")
    } else {
      comment = paste0(comment, "lambda :", round(lambda, digits = 3))
    }
    train$ewma <- ewma.filter(x = train$sales, ratio = lambda)
    train$resid <- train$ewma - train$sales
    train$last_Std_est <- roll::roll_sd(train$resid, 30)
    fcast <- forecast(fit, h = forecasts)
    test$forecast <- fcast$mean
    test$resid <- as.numeric(test$forecast - test$sales)
    test <- test %>%
      mutate(cum_resid = cumsum(test$resid))
    shapero_sales <- shapiro.test(train$sales)
    shapero_resid <- shapiro.test(train$resid)
    pacf_sales <- get_pacf(train$sales,plot=F)
    pacf_resid <- get_pacf(train$resid,plot=F)
    ret <- list(
      succeed = T,
      test = test,
      train = train,
      model = fit,
      rmse = sqrt(sum(test$resid^2)),
      shapero_sales_p_value = shapero_sales$p.value,
      max_pacf_sales= pacf_sales$max_acf,
      max_Lag_sales = pacf_sales$max_lag,
      shapero_resid_p_value = shapero_resid$p.value,
      max_pacf_resid = pacf_resid$max_acf,
      max_Lag_resid = pacf_resid$max_lag,
      comment = comment
    )
  } else {
    ret <- list(succeed = F, comment = "Not enough data to forecast")
  }
  ret
}
get_pacf <- function(vec,plot = T) {
  pacf <- pacf(vec,plot=plot)
  max_acf <- max(abs(pacf$acf))
  lag <- match(max_acf,pacf$acf)
  if(is.na(lag)){
    lag <- match(-max_acf,pacf$acf)
  }
  max_acf <- max(abs(pacf$acf)) / (2 / sqrt(length(vec)))
  list(pacf = pacf, max_acf = max_acf,max_lag = lag)
}

file_result <- function(df, rows, forecasts, prob) {
  fcast <- fit_forecast(df = df,
                        rows = rows,
                        forecasts = forecasts)
  z <- qnorm(prob)
  if (fcast$succeed == T) {
    test <- fcast$test
    train <- fcast$train
    ret <- data.frame(
      product = df$GTIN[1],
      prod_name = df$pack_long_name[1],
      store = df$name[1],
      rows = rows,
      forecasts =  forecasts,
      prob = prob,
      z = z,
      forecast = fcast$test$forecast[1],
      sd_est = train$last_Std_est[nrow(train)],
      act_avg = mean(test$sales),
      proposed_stock = ceiling(fcast$test$forecast[1] + z * train$last_Std_est[nrow(train)]),
      max_Cum_resid = max(fcast$test$cum_resid),
      shapero_sales_p_value = fcast$shapero_sales_p_value,
      max_pacf_sales= fcast$max_pacf_sales,
      max_lag_sales=fcast$max_Lag_sales,
      shapero_resid_p_value = fcast$shapero_resid_p_value,
      max_pacf_resid = fcast$max_pacf_resid,
      max_lag_resid=fcast$max_Lag_resid,
      comment = fcast$comment
    )
  } else {
    ret <- data.frame(
      product = df$GTIN[1],
      prod_name = df$pack_long_name[1],
      store = df$name[1],
      rows = rows,
      forecasts =  forecasts,
      prob = prob,
      z = z,
      forecast = NA,
      sd_est = NA,
      act_avg = NA,
      proposed_stock = NA,
      max_Cum_resid = NA,
      shapero_sales_p_value = NA,
      max_pacf_sales= NA,
      max_lag_sales=NA,
      shapero_resid_p_value = NA,
      max_pacf_resid = NA,
      max_lag_resid =NA,
      comment = fcast$comment
    )
  }
}

gen_all_forecasts <- function(df, rows, forecasts,prob,quiet=T) {
  task <- df %>%  select(name, GTIN) %>% arrange(name, GTIN)
  for (i in 1:nrow(task)) {
    if (!quiet) {
      print(i)
    }
    df <- left_join(task[i, ],
                    total_sales_by_product_by_store_day,
                    join_by(name == name, GTIN == GTIN))
    date_sequence_daily <- data.frame(sale_date = seq(
      from = min(df$sale_date),
      to = max(df$sale_date),
      by = "day"
    ))
    df <- left_join(date_sequence_daily, df, join_by(sale_date == sale_date))
    df$sales[is.na(df$sales)] <- 0
    
    fcast <- fit_forecast(df = df,
                          rows = rows,
                          forecasts = forecasts)
    test <- fcast$test
    train <- fcast$train
    row <- file_result(
      df = df,
      forecasts = forecasts,
      rows = rows,
      prob = prob
    )
    if (i == 1) {
      all_fcast <- row
    } else {
      all_fcast = rbind(all_fcast, row)
    }
  }
  all_fcast
}