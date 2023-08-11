library("basedosdados")
library("dplyr")
library("astsa")
library("ggplot2")
# Defina o seu projeto no Google Cloud
set_billing_id("teste-prax")

# Para carregar o dado direto no R
query <- bdplyr("br_mme_consumo_energia_eletrica.uf")
df <- bd_collect(query)

head(df)

# Checking for NULL values on the 'numero_consumidores' column
nullscons <- sum(is.na(df$numero_consumidores))

# Checking for total rows of the df
rowsdf <- nrow(df)

numrowsok <- rowsdf - nullscons

numrowsok

# Cleaning df
cleandf <- df %>% tidyr::drop_na()

# Convert year and month into a date format
cleandf$date <- as.Date(paste(cleandf$ano, cleandf$mes, 1, sep = "-"), "%Y-%m-%d")

# Arrange the data by date
cleandf <- cleandf[order(cleandf$date),]

# Check for missing values
sum(is.na(cleandf))

# Remove rows with missing values (if any)
cleandf <- na.omit(cleandf)

cleandf$sigla_uf <- as.factor(cleandf$sigla_uf)
cleandf$tipo_consumo <- as.factor(cleandf$tipo_consumo)

cleandf

cleandf_agg <- cleandf %>%
              group_by(date) %>%
              summarise(total_consumo = sum(consumo))


plot(cleandf_agg)

new <- diff(log(cleandf_agg$total_consumo))

plot(new)

sarima(new, 1, 0, 1, 1, 0, 1, 12)

sarima.for(new, 36, 1, 0, 1, 1, 0, 1, 12)

train <- head(new, length(new) - 12)
test <- tail(new, 12)
model <- sarima(train, 1, 0, 1, 1, 0, 1, 12)

library(forecast)
forecasts <- sarima.for(train, 12, 1, 0, 1, 1, 0, 1, 12)

mae <- mean(abs(forecasts$pred - test))
mse <- mean((forecasts$pred - test)^2)
rmse <- sqrt(mse)
mape <- mean(abs((forecasts$pred - test)/test)) * 100

cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\nMAPE:")




#################################
#            EDA               #
################################

# Create a time series plot
ggplot(cleandf_agg, aes(x = date, y = total_consumo)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Total Consumption Over Time", x = "Year", y = "Total Consumption")

