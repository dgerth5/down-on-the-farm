library(riingo) 

RIINGO_TOKEN = "b90d53d93827cce6877317ddcb68cf8cbeeb2399"
riingo_set_token(RIINGO_TOKEN)

df = riingo_prices("AAPL", start_date = "01-01-2022", end_date = "01-01-2023")

prices = df$adjClose

library(forecast)

prices_ts <- ts(prices)

model <- ets(prices_ts)
print(model)
