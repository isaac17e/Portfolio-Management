# Capital Market Line & Security Market Line
# By Isaac Echeverri Florez
rm(list = ls())

#CML

library(quantmod)
library(tidyverse)
library(PortfolioAnalytics)
library(frenchdata)

# Función para obtener datos de activos
get_asset_data <- function(ticker, start_date, end_date) {
  getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
}

# Parámetros editables
risk_free_rate <- 0.09  
tickers <- c("AJG", "ABBV", "EW", "KDP", "ENTG", "FDX", "JKHY", "TJX", "NEE", 
             "ZTS", "TLT", "GLD", "COR", "PEP", "DUK", "FFIV")
portfolio_std <- 0.08  # Desviación estándar objetivo
start_date <- as.Date("2020-01-01")  
end_date <- Sys.Date()
benchmark <- "SPY"  

# Obtener datos de activos y benchmark
data_list <- lapply(tickers, get_asset_data, start_date = start_date, end_date = end_date)
benchmark_data <- get_asset_data(benchmark, start_date, end_date)

# Calcular retornos diarios
returns_list <- lapply(data_list, function(x) dailyReturn(Ad(x)))
returns_df <- do.call(cbind, returns_list) %>% na.omit()
colnames(returns_df) <- tickers

# Calcular retornos mensuales para Fama-French
returns_monthly <- apply.monthly(returns_df, function(x) colMeans(x, na.rm = TRUE) * 21) %>% na.omit()

# Calcular retornos del benchmark
benchmark_returns <- dailyReturn(Ad(benchmark_data)) %>% na.omit()

# Obtener datos de Fama-French 3 factores
ff_data <- download_french_data("Fama/French 3 Factors")
ff_data <- ff_data$subsets$data[[1]] %>%
  mutate(date = as.Date(ym(date))) %>%
  filter(date >= start_date & date <= end_date, month(date) == 8) %>%
  mutate(across(c(`Mkt-RF`, SMB, HML), ~ as.numeric(. / 100))) %>%
  xts(.[, c("Mkt-RF", "SMB", "HML")], order.by = .$date)

# Verificar que ff_data no esté vacío
if (nrow(ff_data) == 0 || any(!is.numeric(as.matrix(ff_data)))) {
  warning("Datos de Fama-French vacíos o no numéricos. Usando retornos históricos como respaldo.")
  expected_returns <- colMeans(returns_df) * 252
  names(expected_returns) <- tickers
  betas <- matrix(NA, nrow = length(tickers), ncol = 3)
  rownames(betas) <- tickers
  colnames(betas) <- c("beta_mkt", "beta_smb", "beta_hml")
  for (sym in tickers) {
    betas[sym, ] <- c(1, 0, 0)  
  }
} else {
  # Alinear fechas de retornos mensuales con ff_data
  returns_monthly <- returns_monthly[index(returns_monthly) %in% index(ff_data)]
  
  # Calcular betas de Fama-French
  rf <- risk_free_rate / 12  # Tasa libre de riesgo mensual
  betas <- matrix(NA, nrow = length(tickers), ncol = 3)
  rownames(betas) <- tickers
  colnames(betas) <- c("beta_mkt", "beta_smb", "beta_hml")
  
  for (sym in tickers) {
    asset_rets <- returns_monthly[, sym]
    if (sum(!is.na(asset_rets)) >= 4) {
      data <- merge(xts(asset_rets, order.by = index(asset_rets)), ff_data)
      data$excess_return <- data[, 1] - rf
      model <- lm(excess_return ~ `Mkt-RF` + SMB + HML, data = na.omit(data))
      betas[sym, ] <- coef(model)[-1]
    }
  }
  
  # Manejo de NA en betas
  if (any(is.na(betas))) {
    warning("Algunos betas son NA. Usando retornos históricos como respaldo.")
    for (sym in tickers[is.na(betas[, "beta_mkt"])]) {
      betas[sym, ] <- c(1, 0, 0)  
    }
  }
  
  # Calcular primas de los factores
  market_premium <- mean(ff_data$`Mkt-RF`, na.rm = TRUE) * 12  
  smb_premium <- mean(ff_data$SMB, na.rm = TRUE) * 12
  hml_premium <- mean(ff_data$HML, na.rm = TRUE) * 12
  
  # Calcular retornos esperados con Fama-French
  expected_returns <- rf * 12 + betas[, "beta_mkt"] * market_premium + 
    betas[, "beta_smb"] * smb_premium + betas[, "beta_hml"] * hml_premium
  names(expected_returns) <- tickers
}

# Optimizar portafolio
portf <- portfolio.spec(assets = tickers) %>%
  add.constraint(type = "weight_sum", min_sum = 1, max_sum = 1) %>%
  add.constraint(type = "box", min = 0, max = 0.3) %>%
  add.constraint(type = "risk", name = "StdDev", target = portfolio_std) %>%
  add.objective(type = "return", name = "mean") %>%
  add.objective(type = "risk", name = "StdDev")

opt <- optimize.portfolio(returns_df, portf, optimize_method = "ROI")
weights <- extractWeights(opt)

# Calcular métricas
asset_std <- apply(returns_df, 2, sd) * sqrt(252)
portfolio_beta <- sum(weights * betas[, "beta_mkt"])
cov_matrix <- cov(returns_df) * 252
portfolio_return <- sum(weights * expected_returns)
portfolio_std_calc <- sqrt(t(weights) %*% cov_matrix %*% weights)
market_return <- mean(benchmark_returns) * 252
market_std <- sd(benchmark_returns) * sqrt(252)
cml_return <- risk_free_rate + ((market_return - risk_free_rate) / market_std) * portfolio_std

# Imprimir resultados
cat("Resultados del análisis:\n")
cat("------------------------\n")
cat("Rendimientos esperados de los activos (Fama-French):\n")
print(round(expected_returns * 100, 2))
cat("\nDesviación estándar de los activos:\n")
print(round(asset_std * 100, 2))
cat("\nBetas de mercado de los activos:\n")
print(round(betas[, "beta_mkt"], 2))
cat("\nRendimiento esperado del portafolio:", round(portfolio_return * 100, 2), "%\n")
cat("Desviación estándar del portafolio (calculada):", round(portfolio_std_calc * 100, 2), "%\n")
cat("Beta del portafolio:", round(portfolio_beta, 2), "\n")
cat("Rendimiento esperado del mercado (SPY):", round(market_return * 100, 2), "%\n")
cat("Rendimiento esperado según CML:", round(cml_return * 100, 2), "%\n")

# Graficar CML
plot_data <- data.frame(
  Risk = seq(0, 0.3, length.out = 100),
  Return = risk_free_rate + ((market_return - risk_free_rate) / market_std) * seq(0, 0.3, length.out = 100)
)

library(ggplot2)
ggplot(plot_data, aes(x = Risk * 100, y = Return * 100)) +
  geom_line(color = "blue", size = 1) +
  annotate("point", x = portfolio_std * 100, y = portfolio_return * 100, color = "red", size = 3) +
  labs(title = "Capital Market Line (CML)", x = "Riesgo (Desviación Estándar %)", y = "Rendimiento Esperado %") +
  theme_minimal()





# SML

# Cargar paquetes necesarios
library(quantmod)
library(tidyverse)
library(PortfolioAnalytics)

# Líneas editables para los inputs
risk_free_rate <- 0.09  
tickers <- c("AJG", "ABBV", "EW", "KDP", "ENTG", "FDX", "JKHY", "TJX", "NEE", 
             "ZTS", "TLT", "GLD", "COR", "PEP", "DUK", "FFIV")
start_date <- as.Date("2019-01-01")  
end_date <- Sys.Date()
benchmark <- "SPY"  

# Función para obtener datos de precios desde Yahoo Finance
get_asset_data <- function(ticker, start_date, end_date) {
  Sys.sleep(1)  
  tryCatch(
    {
      data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      if (nrow(data) == 0) {
        message("No se encontraron datos para ", ticker)
        return(NULL)
      }
      return(data)
    },
    error = function(e) {
      message("Error al descargar datos para ", ticker, ": ", e$message)
      return(NULL)
    }
  )
}

# Obtener datos de los activos y del benchmark
data_list <- lapply(tickers, get_asset_data, start_date = start_date, end_date = end_date)
benchmark_data <- get_asset_data(benchmark, start_date, end_date)

# Verificar si hubo errores en la descarga de datos
valid_tickers <- tickers[!sapply(data_list, is.null)]
data_list <- data_list[!sapply(data_list, is.null)]
if (length(data_list) == 0 || is.null(benchmark_data)) {
  cat("Tickers con problemas:\n")
  for (i in seq_along(tickers)) {
    if (is.null(data_list[[i]])) {
      cat(tickers[i], "\n")
    }
  }
  stop("No se pudieron descargar datos válidos para ningún ticker o el benchmark.")
}

# Imprimir fechas disponibles para cada ticker
cat("\nFechas disponibles para cada ticker:\n")
for (i in seq_along(valid_tickers)) {
  cat(valid_tickers[i], ": ", nrow(data_list[[i]]), " filas, desde ", 
      as.character(as.Date(start(index(data_list[[i]])))), " hasta ", 
      as.character(as.Date(end(index(data_list[[i]])))), "\n")
}
cat("SPY: ", nrow(benchmark_data), " filas, desde ", 
    as.character(as.Date(start(index(benchmark_data)))), " hasta ", 
    as.character(as.Date(end(index(benchmark_data)))), "\n")

# Calcular retornos diarios
returns_list <- lapply(data_list, function(x) {
  if (!is.null(x) && nrow(x) > 0) {
    dailyReturn(Ad(x), type = "log")  
  } else {
    NULL
  }
})
returns_list <- returns_list[!sapply(returns_list, is.null)]  
if (length(returns_list) == 0) {
  stop("No se pudieron calcular retornos para ningún activo.")
}
returns_df <- do.call(cbind, returns_list) %>% na.omit()
colnames(returns_df) <- valid_tickers

# Calcular retornos del benchmark
benchmark_returns <- dailyReturn(Ad(benchmark_data), type = "log") %>% na.omit()

# Alinear datos de activos y benchmark
common_dates <- intersect(index(returns_df), index(benchmark_returns))
cat("\nNúmero de fechas comunes:", length(common_dates), "\n")
cat("Primeras 5 fechas comunes:", head(as.character(common_dates), 5), "\n")
cat("Índices de returns_df:", head(as.character(index(returns_df)), 5), "\n")
cat("Índices de benchmark_returns:", head(as.character(index(benchmark_returns)), 5), "\n")

if (length(common_dates) == 0) {
  cat("Fechas en returns_df:", as.character(range(index(returns_df))), "\n")
  cat("Fechas en benchmark_returns:", as.character(range(index(benchmark_returns))), "\n")
  stop("No hay fechas comunes entre los activos y el benchmark. Verifica los datos.")
}

# Convertir common_dates a Date para evitar problemas de formato
common_dates <- as.Date(common_dates)
returns_df <- returns_df[common_dates, , drop = FALSE]
benchmark_returns <- benchmark_returns[common_dates, , drop = FALSE]

# Verificar que returns_df y benchmark_returns no estén vacíos después de la alineación
if (nrow(returns_df) == 0 || nrow(benchmark_returns) == 0) {
  stop("No hay datos después de alinear fechas. Verifica los datos descargados.")
}

# Optimizar portafolio
portf <- portfolio.spec(assets = valid_tickers) %>%
  add.constraint(type = "weight_sum", min_sum = 1, max_sum = 1) %>%
  add.constraint(type = "box", min = 0, max = 0.3) %>%
  add.constraint(type = "risk", name = "StdDev", target = 0.08) %>%
  add.objective(type = "return", name = "mean") %>%
  add.objective(type = "risk", name = "StdDev")

opt <- optimize.portfolio(returns_df, portf, optimize_method = "ROI")
weights <- extractWeights(opt)

# Calcular métricas
expected_returns <- colMeans(returns_df, na.rm = TRUE) * 252
asset_std <- apply(returns_df, 2, sd, na.rm = TRUE) * sqrt(252)
betas <- sapply(1:ncol(returns_df), function(i) {
  cov(returns_df[, i], benchmark_returns, use = "complete.obs") / 
    var(benchmark_returns, use = "complete.obs")
})
portfolio_return <- sum(weights * expected_returns)
cov_matrix <- cov(returns_df, use = "complete.obs") * 252
portfolio_std_calc <- sqrt(t(weights) %*% cov_matrix %*% weights)[1, 1]
portfolio_beta <- sum(weights * betas)
market_return <- mean(benchmark_returns, na.rm = TRUE) * 252
market_std <- sd(benchmark_returns, na.rm = TRUE) * sqrt(252)
sml_returns <- risk_free_rate + betas * (market_return - risk_free_rate)
portfolio_sml_return <- risk_free_rate + portfolio_beta * (market_return - risk_free_rate)

# Imprimir resultados
cat("Resultados del análisis para la SML:\n")
cat("------------------------\n")
cat("Tasa libre de riesgo (Rf):", round(risk_free_rate * 100, 2), "%\n")
cat("Rendimiento esperado del mercado (E(Rm)):", round(market_return * 100, 2), "%\n")
cat("Volatilidad del mercado (σm):", round(market_std * 100, 2), "%\n")
cat("Rendimiento esperado del portafolio (E(Rp)):", round(portfolio_return * 100, 2), "%\n")
cat("Desviación estándar del portafolio (σp):", round(portfolio_std_calc * 100, 2), "%\n")
cat("Beta del portafolio:", round(portfolio_beta, 2), "\n")
cat("\nRendimientos esperados de los activos (E(Ri)):\n")
print(round(expected_returns * 100, 2))
cat("\nBetas de los activos (βi):\n")
print(round(betas, 2))
cat("\nRendimientos esperados según SML para los activos:\n")
print(round(sml_returns * 100, 2))
cat("\nRendimiento esperado del portafolio según SML:", round(portfolio_sml_return * 100, 2), "%\n")

# Gráfico de la SML
plot_data <- data.frame(
  Beta = seq(0, max(betas, portfolio_beta, na.rm = TRUE) + 0.5, length.out = 100),
  Return = risk_free_rate + seq(0, max(betas, portfolio_beta, na.rm = TRUE) + 0.5, length.out = 100) * (market_return - risk_free_rate)
)

asset_plot_data <- data.frame(
  Beta = c(betas, portfolio_beta),
  Return = c(expected_returns, portfolio_return),
  Ticker = c(valid_tickers, "Portfolio")
)

ggplot() +
  geom_line(data = plot_data, aes(x = Beta, y = Return * 100), color = "blue", size = 1) +
  geom_point(data = asset_plot_data, aes(x = Beta, y = Return * 100, color = Ticker), size = 3) +
  geom_text(data = asset_plot_data, aes(x = Beta, y = Return * 100, label = Ticker), vjust = -1, size = 3) +
  labs(title = "Security Market Line (SML)", x = "Beta (β)", y = "Rendimiento Esperado (%)") +
  theme_minimal() +
  theme(legend.position = "none")