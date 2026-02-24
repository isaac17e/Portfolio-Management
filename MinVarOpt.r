# ==============================================================================
# OPTIMIZACIÓN DE PORTAFOLIOS CON MINIMA VARIANZA
# BY: Isaac Echeverri Florez
# ==============================================================================
rm(list = ls())

library(tidyverse)
library(tidyquant)         
library(PortfolioAnalytics) 
library(ROI)           
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)    
library(PerformanceAnalytics)
library(ggcorrplot)         
library(scales)
library(knitr)
library(gridExtra)
library(rvest)
library(httr)
# ==============================================================================
# SECCIÓN 1: PARÁMETROS CONFIGURABLES
# ==============================================================================

# SELECCIÓN DE UNIVERSO DE INVERSIÓN
n_top_sp500  <- 220
n_top_nasdaq <- 120
benchmark    <- "SPY"

# HORIZONTE DE DATOS HISTÓRICOS
start_date <- "2020-01-01"   # Inicio amplio para cálculos sólidos
end_date   <- Sys.Date()     # Siempre hasta hoy

#   Tres meses: target_month <- c(1, 2, 3)
target_month <- c(2, 3, 4)

# PARÁMETROS FINANCIEROS
risk_free_rate <- 0.075

# PERFIL DE RIESGO DEL INVERSOR
weight_sharpe <- 0.25          # Rendimiento empieza a importar
weight_low_vol <- 0.50         # Estabilidad sigue siendo prioritaria
weight_decorr <- 0.25          # Diversificación más valorada

n_divers_candidates <- 20
volatility_percentile <- 0.40  # Moderado-estricto: 40% menos volátil
correlation_percentile <- 0.60 # Moderado: acepta más universo
max_assets_in_portfolio <- 12      # Máximo de activos en el portafolio final

# RESTRICCIONES DE PONDERACIÓN
max_weight_per_asset <- 0.20
min_weight_per_asset <- 0.02

# ESTRATEGIA DE PONDERACIÓN
require_full_investment <- FALSE
min_total_weight        <- 0.95
max_total_weight        <- 1.00

# ==============================================================================
# SECCIÓN 2: FUNCIONES DE WEB SCRAPING
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║     OPTIMIZACIÓN DE PORTAFOLIO - S&P 500 & NASDAQ SCRAPING   ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

safe_scrape_table <- function(url, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      cat(paste("   Intento", attempt, "de", max_retries, "...\n"))
      
      page <- read_html(httr::GET(url, httr::user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
      )))
      
      tables <- page %>% html_table(fill = TRUE)
      
      if (length(tables) > 0) {
        cat("   ✓ Tabla obtenida exitosamente\n")
        return(tables[[1]])
      }
      
    }, error = function(e) {
      cat(paste("   ✗ Error:", e$message, "\n"))
      if (attempt < max_retries) {
        Sys.sleep(2)
      }
    })
  }
  
  cat("   ✗ No se pudo obtener la tabla después de", max_retries, "intentos\n")
  return(NULL)
}

get_sp500_tickers <- function(n_top = 50) {
  cat("\n📊 Obteniendo tickers del S&P 500...\n")
  
  sp500_tbl <- safe_scrape_table("https://www.slickcharts.com/sp500")
  
  if (is.null(sp500_tbl)) {
    stop("Error: No se pudo obtener la tabla del S&P 500 desde slickcharts.com")
  }
  
  ticker_col <- NULL
  possible_names <- c("Symbol", "Ticker", "Company", "symbol", "ticker")
  
  for (col_name in possible_names) {
    if (col_name %in% names(sp500_tbl)) {
      ticker_col <- col_name
      break
    }
  }
  
  if (is.null(ticker_col)) {
    cat("   ⚠ No se identificó columna de tickers, usando primera columna\n")
    tickers <- sp500_tbl[[1]]
  } else {
    tickers <- sp500_tbl[[ticker_col]]
  }
  
  tickers <- gsub("\\s+", "", tickers)
  tickers <- tickers[nchar(tickers) > 0 & nchar(tickers) <= 5]
  tickers <- head(tickers, n_top)
  
  cat(paste("   ✓ Obtenidos", length(tickers), "tickers del S&P 500\n"))
  return(tickers)
}

get_nasdaq_tickers <- function(n_top = 30) {
  cat("\n📊 Obteniendo tickers del NASDAQ...\n")
  
  nasdaq_tbl <- safe_scrape_table("https://stockanalysis.com/list/nasdaq-stocks/")
  
  if (is.null(nasdaq_tbl)) {
    stop("Error: No se pudo obtener la tabla del NASDAQ desde stockanalysis.com")
  }
  
  ticker_col <- NULL
  possible_names <- c("Symbol", "Ticker", "ticker", "symbol", "Stock")
  
  for (col_name in possible_names) {
    if (col_name %in% names(nasdaq_tbl)) {
      ticker_col <- col_name
      break
    }
  }
  
  if (is.null(ticker_col)) {
    cat("   ⚠ No se identificó columna de tickers, usando primera columna\n")
    tickers <- nasdaq_tbl[[1]]
  } else {
    tickers <- nasdaq_tbl[[ticker_col]]
  }
  
  tickers <- gsub("\\s+", "", tickers)
  tickers <- tickers[nchar(tickers) > 0 & nchar(tickers) <= 5]
  tickers <- head(tickers, n_top)
  
  cat(paste("   ✓ Obtenidos", length(tickers), "tickers del NASDAQ\n"))
  return(tickers)
}

# ==============================================================================
# SECCIÓN 3: OBTENCIÓN DE TICKERS
# ==============================================================================

sp500_tickers  <- get_sp500_tickers(n_top_sp500)
nasdaq_tickers <- get_nasdaq_tickers(n_top_nasdaq)
all_tickers    <- unique(c(sp500_tickers, nasdaq_tickers))

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("UNIVERSO DE INVERSIÓN\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(paste("• Tickers del S&P 500:", length(sp500_tickers), "\n"))
cat(paste("• Tickers del NASDAQ:", length(nasdaq_tickers), "\n"))
cat(paste("• Total únicos:", length(all_tickers), "\n\n"))

# ==============================================================================
# SECCIÓN 4: DESCARGA Y PREPARACIÓN DE DATOS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("DESCARGA DE DATOS HISTÓRICOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("[INFO] Descargando datos desde Yahoo Finance...\n")
cat(paste("[INFO] Periodo histórico:", start_date, "hasta", end_date, "\n"))
cat(paste("[INFO] Meses objetivo:", paste(target_month, collapse = ", "), "\n"))
cat("[INFO] Esto puede tomar varios minutos...\n\n")

download_ticker_data <- function(ticker, start, end, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      data <- tq_get(ticker, 
                     get = "stock.prices", 
                     from = start, 
                     to = end)
      if (nrow(data) > 0) return(data)
    }, error = function(e) {
      if (attempt == max_retries) return(NULL)
      Sys.sleep(1)
    })
  }
  return(NULL)
}

stock_data_list    <- list()
successful_tickers <- c()
failed_tickers     <- c()

pb <- txtProgressBar(min = 0, max = length(all_tickers), style = 3)

for (i in seq_along(all_tickers)) {
  ticker <- all_tickers[i]
  data   <- download_ticker_data(ticker, start_date, end_date)
  
  if (!is.null(data) && nrow(data) > 0) {
    stock_data_list[[ticker]] <- data
    successful_tickers <- c(successful_tickers, ticker)
  } else {
    failed_tickers <- c(failed_tickers, ticker)
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\n\n")
cat(paste("✓ Descargados exitosamente:", length(successful_tickers), "tickers\n"))

if (length(failed_tickers) > 0) {
  cat(paste("✗ Fallaron:", length(failed_tickers), "tickers\n"))
  cat("   Tickers fallidos:", paste(head(failed_tickers, 10), collapse = ", "),
      ifelse(length(failed_tickers) > 10, "...", ""), "\n")
}

stock_data <- bind_rows(stock_data_list)

cat("\n[INFO] Descargando benchmark (SPY)...\n")
benchmark_data <- tq_get(benchmark,
                         get = "stock.prices",
                         from = start_date,
                         to = end_date)

if (nrow(stock_data) == 0 || nrow(benchmark_data) == 0) {
  stop("Error: No se descargaron datos suficientes.")
}

cat("[INFO] Datos descargados correctamente.\n\n")

# Preparar precios
prices_wide <- stock_data %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted)

numeric_cols <- names(prices_wide)[-1]
for (col in numeric_cols) {
  prices_wide[[col]] <- as.numeric(prices_wide[[col]])
}

# Filtrar tickers con datos completos (>80%)
n_expected       <- nrow(prices_wide)
complete_tickers <- colnames(prices_wide)[-1]
na_counts        <- colSums(is.na(prices_wide[, complete_tickers]))
threshold        <- 0.2 * n_expected
valid_tickers    <- names(na_counts[na_counts < threshold])

cat(paste("[INFO] Tickers con datos completos (>80%):", length(valid_tickers), "\n"))

if (length(valid_tickers) == 0) {
  cat("[ADVERTENCIA] Ningún ticker cumple el umbral del 80%. Usando todos los disponibles.\n")
  valid_tickers <- complete_tickers
}

prices_wide_clean <- prices_wide %>%
  select(date, all_of(valid_tickers)) %>%
  drop_na()

if (nrow(prices_wide_clean) == 0) {
  stop("Error: No hay datos después de eliminar NAs. Revise el rango de fechas.")
}

cat(paste("[INFO] Filas después de limpieza:", nrow(prices_wide_clean), "\n"))

# Benchmark
benchmark_prices <- benchmark_data %>%
  select(date, adjusted) %>%
  rename(SPY = adjusted) %>%
  mutate(SPY = as.numeric(SPY))

# Calcular retornos logarítmicos sobre el histórico completo
cat("[INFO] Calculando retornos logarítmicos...\n")

prices_xts <- prices_wide_clean %>%
  select(-date) %>%
  as.matrix() %>%
  xts(order.by = prices_wide_clean$date)

log_returns_full <- Return.calculate(prices_xts, method = "log") %>%
  na.omit()

benchmark_xts <- benchmark_prices %>%
  select(-date) %>%
  as.matrix() %>%
  xts(order.by = benchmark_prices$date)

benchmark_returns_full <- Return.calculate(benchmark_xts, method = "log") %>%
  na.omit()

# Alinear fechas
common_dates <- index(log_returns_full)[index(log_returns_full) %in% index(benchmark_returns_full)]

if (length(common_dates) == 0) {
  stop("Error: No hay fechas comunes entre los activos y el benchmark.")
}

log_returns_full   <- log_returns_full[common_dates]
benchmark_returns_full <- benchmark_returns_full[common_dates]

# ---------------------------------------------------------------------------
# FILTRO POR MESES OBJETIVO
# ---------------------------------------------------------------------------

cat(paste("\n[INFO] Filtrando observaciones para meses:", 
          paste(target_month, collapse = ", "), "...\n"))

month_filter <- as.integer(format(index(log_returns_full), "%m")) %in% target_month

log_returns        <- log_returns_full[month_filter]
benchmark_returns  <- benchmark_returns_full[month_filter]

n_obs      <- nrow(log_returns)
n_years    <- length(unique(format(index(log_returns_full), "%Y")))
month_names <- month.name[target_month]

cat(paste("[INFO] Observaciones en meses objetivo:", n_obs, "\n"))
cat(paste("[INFO] Años cubiertos:", n_years, "\n"))

if (n_obs < 30) {
  warning(paste(
    "ADVERTENCIA: Solo hay", n_obs, "observaciones para los meses seleccionados.",
    "Los cálculos pueden ser inestables. Considere ampliar target_month o start_date."
  ))
}

cat(paste("[INFO] Tickers para análisis:", ncol(log_returns), "\n"))

# Verificar integridad
if (any(is.na(log_returns)) || any(is.infinite(log_returns))) {
  cat("[ADVERTENCIA] Se encontraron NAs o valores infinitos. Limpiando...\n")
  
  problematic_cols <- colnames(log_returns)[colSums(is.na(log_returns) | is.infinite(log_returns)) > 0]
  
  if (length(problematic_cols) > 0) {
    cat(paste("   Tickers con problemas:", paste(problematic_cols, collapse = ", "), "\n"))
    good_cols   <- setdiff(colnames(log_returns), problematic_cols)
    log_returns <- log_returns[, good_cols]
    cat(paste("   Tickers restantes:", ncol(log_returns), "\n"))
  }
}

if (ncol(log_returns) < 5) {
  stop("Error: Quedan menos de 5 tickers válidos.")
}

cat("\n")

# ==============================================================================
# SECCIÓN 5: ANÁLISIS DESCRIPTIVO Y SELECCIÓN POR PERFIL DE RIESGO
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("ANÁLISIS DE SELECCIÓN DE ACTIVOS POR PERFIL DE RIESGO\n")
cat(paste("Meses analizados:", paste(month_names, collapse = ", "), "\n"))
cat("═══════════════════════════════════════════════════════════════\n\n")

safe_sd   <- function(x) tryCatch(sd(x,   na.rm = TRUE), error = function(e) NA)
safe_mean <- function(x) tryCatch(mean(x, na.rm = TRUE), error = function(e) NA)

asset_stats <- data.frame(
  Symbol      = colnames(log_returns),
  Mean_Return = sapply(1:ncol(log_returns), function(i) safe_mean(log_returns[, i]) * 252),
  Volatility  = sapply(1:ncol(log_returns), function(i) safe_sd(log_returns[, i])   * sqrt(252)),
  stringsAsFactors = FALSE
)

asset_stats$Sharpe <- (asset_stats$Mean_Return - risk_free_rate) / asset_stats$Volatility

correlations <- sapply(colnames(log_returns), function(ticker) {
  tryCatch(
    cor(log_returns[, ticker], benchmark_returns[, 1], use = "complete.obs"),
    error = function(e) NA
  )
})

asset_stats$Correlation_SPY <- correlations

mdd_values <- sapply(colnames(log_returns), function(ticker) {
  tryCatch(maxDrawdown(log_returns[, ticker]), error = function(e) NA)
})

asset_stats$Max_Drawdown <- mdd_values

asset_stats <- asset_stats %>%
  filter(!is.na(Sharpe) & !is.na(Volatility) & !is.na(Correlation_SPY) &
           !is.infinite(Sharpe) & !is.infinite(Volatility)) %>%
  arrange(Correlation_SPY)

cat(paste("[INFO] Activos con métricas válidas:", nrow(asset_stats), "\n\n"))

cat("Top 15 activos con MENOR correlación con SPY:\n")
print(head(asset_stats %>%
             select(Symbol, Correlation_SPY, Volatility, Sharpe, Max_Drawdown) %>%
             mutate(
               Correlation_SPY = round(Correlation_SPY, 3),
               Volatility      = percent(Volatility, accuracy = 0.1),
               Sharpe          = round(Sharpe, 2),
               Max_Drawdown    = percent(Max_Drawdown, accuracy = 0.1)
             ), 15))

# PUNTUACIÓN Y RANKING
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

asset_stats <- asset_stats %>%
  mutate(
    Score_Sharpe  = normalize(Sharpe),
    Score_LowVol  = 1 - normalize(Volatility),
    Score_Decorr  = 1 - normalize(abs(Correlation_SPY)),
    Risk_Score    = weight_sharpe  * Score_Sharpe +
      weight_low_vol * Score_LowVol +
      weight_decorr  * Score_Decorr
  ) %>%
  arrange(desc(Risk_Score))

vol_threshold  <- quantile(asset_stats$Volatility,            volatility_percentile,  na.rm = TRUE)
corr_threshold <- quantile(abs(asset_stats$Correlation_SPY),  correlation_percentile, na.rm = TRUE)

asset_stats <- asset_stats %>%
  mutate(
    Passes_Vol  = Volatility <= vol_threshold,
    Passes_Corr = abs(Correlation_SPY) <= corr_threshold,
    Passes_All  = Passes_Vol & Passes_Corr
  )

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("RANKING DE ACTIVOS SEGÚN PERFIL DE RIESGO\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(paste("Criterios: Sharpe (", weight_sharpe*100, "%), ",
          "Baja Volatilidad (", weight_low_vol*100, "%), ",
          "Decorrelación (", weight_decorr*100, "%)\n", sep = ""))
cat(paste("Umbral Volatilidad: <=", round(vol_threshold*100, 2), "%\n", sep = ""))
cat(paste("Umbral Correlación SPY: <=", round(corr_threshold, 3), "\n\n", sep = ""))

cat("Top 20 activos por Risk Score:\n")
print(head(asset_stats %>%
             select(Symbol, Risk_Score, Sharpe, Volatility, Correlation_SPY,
                    Passes_Vol, Passes_Corr) %>%
             mutate(
               Volatility      = percent(Volatility, accuracy = 0.01),
               Sharpe          = round(Sharpe, 2),
               Correlation_SPY = round(Correlation_SPY, 3),
               Risk_Score      = round(Risk_Score, 3)
             ), 20))

selected_tickers <- asset_stats %>%
  filter(Passes_All) %>%
  arrange(desc(Risk_Score)) %>%
  head(n_divers_candidates) %>%
  pull(Symbol)

if (length(selected_tickers) < 5) {
  cat("\n[ADVERTENCIA] Muy pocos activos pasan todos los filtros.\n")
  cat("[INFO] Seleccionando top activos por Risk_Score sin filtros estrictos...\n\n")
  
  selected_tickers <- asset_stats %>%
    arrange(desc(Risk_Score)) %>%
    head(n_divers_candidates) %>%
    pull(Symbol)
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("ACTIVOS SELECCIONADOS PARA OPTIMIZACIÓN\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(paste("Total seleccionado:", length(selected_tickers), "\n\n"))
print(selected_tickers)
cat("\n")

log_returns_selected <- log_returns[, selected_tickers]

# ==============================================================================
# SECCIÓN 6: ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("ESTADÍSTICAS DESCRIPTIVAS DE ACTIVOS SELECCIONADOS\n")
cat(paste("Meses analizados:", paste(month_names, collapse = ", "), "\n"))
cat("═══════════════════════════════════════════════════════════════\n\n")

descriptive_stats <- data.frame(
  Symbol           = colnames(log_returns_selected),
  Media_Diaria     = colMeans(log_returns_selected),
  Mediana_Diaria   = apply(log_returns_selected, 2, median),
  SD_Diaria        = apply(log_returns_selected, 2, sd),
  Retorno_Anual    = colMeans(log_returns_selected) * 252,
  Volatilidad_Anual = apply(log_returns_selected, 2, sd) * sqrt(252),
  Asimetria        = apply(log_returns_selected, 2, skewness),
  Curtosis         = apply(log_returns_selected, 2, kurtosis),
  Max_Drawdown     = sapply(colnames(log_returns_selected), function(x) {
    maxDrawdown(log_returns_selected[, x])
  })
)

print(descriptive_stats %>%
        mutate(
          Retorno_Anual     = percent(Retorno_Anual,     accuracy = 0.1),
          Volatilidad_Anual = percent(Volatilidad_Anual, accuracy = 0.1),
          Max_Drawdown      = percent(Max_Drawdown,      accuracy = 0.1),
          Asimetria         = round(Asimetria, 2),
          Curtosis          = round(Curtosis,  2)
        ))

# ==============================================================================
# SECCIÓN 7: OPTIMIZACIÓN DE PORTAFOLIOS
# ==============================================================================

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("OPTIMIZACIÓN DE PORTAFOLIOS\n")
cat(paste("Meses analizados:", paste(month_names, collapse = ", "), "\n"))
cat("═══════════════════════════════════════════════════════════════\n\n")

mean_ret <- colMeans(log_returns_selected)
cov_mat  <- cov(log_returns_selected)
sd_ret   <- apply(log_returns_selected, 2, sd)

port_spec <- portfolio.spec(assets = colnames(log_returns_selected))

if (require_full_investment) {
  cat("[INFO] Aplicando restricción: Suma de pesos = 1 (inversión total)\n")
  port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
} else {
  cat("[INFO] Optimización SIN restricción de inversión total\n")
  cat(paste("      Rango permitido:", percent(min_total_weight), "a",
            percent(max_total_weight), "del capital\n"))
  
  port_spec <- add.constraint(portfolio = port_spec,
                              type    = "weight_sum",
                              min_sum = min_total_weight,
                              max_sum = max_total_weight)
}

port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Permitir peso 0 para que el solver descarte activos naturalmente
# Solo se aplica max_weight_per_asset como límite superior
port_spec <- add.constraint(portfolio = port_spec,
                            type = "box",
                            min  = 0,
                            max  = max_weight_per_asset)

cat("[INFO] Optimizando portafolio de Mínima Varianza...\n")
cat(paste("[INFO] Pool de", length(selected_tickers), "candidatos\n\n"))

port_minvar <- port_spec
port_minvar <- add.objective(portfolio = port_minvar, type = "risk", name = "StdDev")

opt_min_var <- optimize.portfolio(R               = log_returns_selected,
                                  portfolio       = port_minvar,
                                  optimize_method = "ROI",
                                  trace           = TRUE)

# Identificar activos con peso significativo (>0.1%)
w                <- extractWeights(opt_min_var)
selected_tickers <- names(w[w > 0.001])

cat(paste("\n✓ Portafolio optimizado con", length(selected_tickers), "activos\n"))
cat(paste("  Activos:", paste(selected_tickers, collapse = ", "), "\n\n"))

# Actualizar retornos al subconjunto con pesos significativos
log_returns_selected <- log_returns_selected[, selected_tickers]
mean_ret <- colMeans(log_returns_selected)
cov_mat  <- cov(log_returns_selected)
sd_ret   <- apply(log_returns_selected, 2, sd)

# ==============================================================================
# SECCIÓN 8: FRONTERA EFICIENTE
# ==============================================================================

cat("\n[INFO] Calculando Frontera Eficiente...\n")

min_ret        <- min(mean_ret) * 252
max_ret        <- max(mean_ret) * 252
target_returns <- seq(min_ret, max_ret, length.out = 50)

efficient_frontier <- data.frame(Return = numeric(0), Risk = numeric(0))

n   <- length(selected_tickers)
Dmat <- 2 * cov_mat
dvec <- rep(0, n)

for (target_ret in target_returns) {
  tryCatch({
    # Restricciones: long-only, suma de pesos en [min_total, max_total],
    # retorno objetivo >= target_ret/252 (diario)
    target_daily <- target_ret / 252
    
    Amat <- cbind(
      diag(n),                          # long-only
      rep(1, n),                        # sum >= min
      rep(-1, n),                       # sum <= max
      mean_ret,                         # retorno >= target
      -diag(n)                          # w_i <= max_weight
    )
    
    bvec <- c(
      rep(0, n),                        # long-only
      min_total_weight,                 # sum >= min
      -max_total_weight,                # sum <= max
      target_daily,                     # retorno
      rep(-max_weight_per_asset, n)     # w_i <= max_weight
    )
    
    sol <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec,
                              Amat = Amat, bvec = bvec,
                              meq  = 0)
    
    w    <- sol$solution
    w[w < 1e-6] <- 0
    
    ret  <- sum(w * mean_ret) * 252
    risk <- sqrt(as.numeric(t(w) %*% cov_mat %*% w)) * sqrt(252)
    
    efficient_frontier <- rbind(efficient_frontier,
                                data.frame(Return = ret, Risk = risk))
  }, error = function(e) {})
}

cat(paste("[INFO] Frontera eficiente calculada con", nrow(efficient_frontier), "puntos.\n"))

# ==============================================================================
# SECCIÓN 9: EXTRACCIÓN DE MÉTRICAS
# ==============================================================================

extract_metrics <- function(opt_obj, label) {
  w_raw <- extractWeights(opt_obj)
  
  common <- intersect(names(w_raw), colnames(log_returns_selected))
  w      <- w_raw[common]
  
  mr  <- mean_ret[common]
  cm  <- cov_mat[common, common]
  ret_mat <- log_returns_selected[, common]
  
  total_weight  <- sum(w)
  cash_position <- 1 - total_weight
  
  ret    <- sum(w * mr) * 252
  risk   <- sqrt(as.numeric(t(w) %*% cm %*% w)) * sqrt(252)
  sharpe <- (ret - risk_free_rate) / risk
  
  port_returns <- xts(as.numeric(ret_mat %*% w),
                      order.by = index(ret_mat))
  mdd <- maxDrawdown(port_returns)
  
  # --- SORTINO ---
  rf_daily       <- risk_free_rate / 252
  excess_returns <- as.numeric(port_returns) - rf_daily
  downside_ret   <- excess_returns[excess_returns < 0]
  downside_dev   <- sqrt(mean(downside_ret^2)) * sqrt(252)  # Downside deviation anualizada
  sortino        <- (ret - risk_free_rate) / downside_dev
  # ----------------
  
  list(Label         = label,
       Return        = ret,
       Risk          = risk,
       Sharpe        = sharpe,
       Sortino       = sortino,
       MaxDrawdown   = mdd,
       Weights       = w,
       Total_Weight  = total_weight,
       Cash_Position = cash_position)
}

metrics_minvar <- extract_metrics(opt_min_var, "Mínima Varianza")

# ==============================================================================
# SECCIÓN 10: VISUALIZACIONES
# ==============================================================================

cat("\n\n[INFO] Generando visualizaciones...\n")

df_points <- data.frame(
  Label  = "Mínima Varianza",
  Risk   = metrics_minvar$Risk,
  Return = metrics_minvar$Return
)

asset_metrics <- data.frame(
  Symbol = colnames(log_returns_selected),
  Return = mean_ret * 252,
  Risk   = sd_ret   * sqrt(252)
)

# ---------------------------------------------------------------------------
# GRÁFICO 1: Frontera Eficiente
# ---------------------------------------------------------------------------

p1 <- ggplot() +
  geom_line(data = efficient_frontier, aes(x = Risk, y = Return),
            color = "#08519c", size = 1.5, alpha = 0.9) +
  geom_point(data = efficient_frontier, aes(x = Risk, y = Return),
             color = "#08519c", size = 1.8, alpha = 0.5) +
  
  geom_point(data = df_points, aes(x = Risk, y = Return),
             shape = 21, size = 6, stroke = 2, fill = "#2ca25f") +
  geom_text(data = df_points, aes(x = Risk, y = Return, label = Label),
            vjust = -1.8, fontface = "bold", size = 3.5) +
  
  geom_point(data = asset_metrics, aes(x = Risk, y = Return),
             color = "#ff7f00", size = 3.5, alpha = 0.7) +
  geom_text(data = asset_metrics, aes(x = Risk, y = Return, label = Symbol),
            vjust = 1.8, size = 2.8, fontface = "bold") +
  
  labs(title = "Frontera Eficiente & Optimización de Markowitz",
       subtitle = paste("Meses:", paste(month_names, collapse = ", "),
                        "| Universo: S&P 500 (", n_top_sp500, ") + NASDAQ (",
                        n_top_nasdaq, ") | Seleccionados:", length(selected_tickers)),
       x       = "Riesgo Anualizado (Volatilidad)",
       y       = "Retorno Esperado Anualizado",
       caption = paste("Data: Yahoo Finance | Histórico:", start_date, "-", end_date,
                       "| Método: Mean-Variance Optimization (ROI Solver)")) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "gray70", fill = NA, size = 1)
  )

# ---------------------------------------------------------------------------
# GRÁFICO 2: Asignación de Activos
# ---------------------------------------------------------------------------

prepare_weights_with_cash <- function(weights, total_weight, label) {
  df <- data.frame(
    Symbol    = names(weights),
    Weight    = as.numeric(weights),
    Portfolio = label
  )
  
  if (!require_full_investment && abs(1 - total_weight) > 0.001) {
    df <- rbind(df, data.frame(
      Symbol    = "EFECTIVO",
      Weight    = 1 - total_weight,
      Portfolio = label
    ))
  }
  
  return(df)
}

df_weights <- prepare_weights_with_cash(metrics_minvar$Weights,
                                        metrics_minvar$Total_Weight,
                                        "Mínima Varianza")

p2 <- ggplot(df_weights, aes(x = Portfolio, y = Weight, fill = Symbol)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4) +
  geom_text(aes(label = ifelse(abs(Weight) > 0.03, percent(Weight, accuracy = 0.1), "")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white", fontface = "bold") +
  labs(title    = "Asignación Óptima de Activos — Mínima Varianza",
       subtitle = ifelse(require_full_investment,
                         paste("Restricción: Inversión Total 100% | Peso máximo",
                               percent(max_weight_per_asset)),
                         paste("Optimización flexible:", percent(min_total_weight), "a",
                               percent(max_total_weight), "| Peso máximo por activo",
                               percent(max_weight_per_asset))),
       y = "Peso del Portafolio (%)",
       x = "") +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title          = element_text(size = 14, face = "bold"),
    legend.position     = "right",
    panel.grid.major.x  = element_blank(),
    axis.text.x         = element_text(size = 11, face = "bold")
  )

# ---------------------------------------------------------------------------
# GRÁFICO 3: Matriz de Correlación
# ---------------------------------------------------------------------------

corr_matrix <- cor(log_returns_selected)

p3 <- ggcorrplot(corr_matrix,
                 hc.order  = TRUE,
                 type      = "lower",
                 lab       = TRUE,
                 lab_size  = 3,
                 colors    = c("#E46726", "white", "#6D9EC1"),
                 title     = paste("Matriz de Correlación —",
                                   paste(month_names, collapse = ", ")),
                 ggtheme   = theme_minimal())

print(p1)
print(p2)
print(p3)

# ==============================================================================
# SECCIÓN 11: RESUMEN FINAL
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN DE OPTIMIZACIÓN DE PORTAFOLIO           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat(paste("Meses analizados:", paste(month_names, collapse = ", "), "\n"))
cat(paste("Observaciones utilizadas:", n_obs, "días de trading\n"))
cat(paste("Años cubiertos:", n_years, "\n\n"))

summary_table <- data.frame(
  Métrica = c("Retorno Esperado (Anual)",
              "Volatilidad (Anual)",
              "Ratio de Sharpe",
              "Ratio de Sortino",       
              "Maximum Drawdown",
              "Peso Total Invertido",
              "Posición en Efectivo"),
  Mínima_Varianza = c(
    percent(metrics_minvar$Return,       accuracy = 0.01),
    percent(metrics_minvar$Risk,         accuracy = 0.01),
    round(metrics_minvar$Sharpe,         3),
    round(metrics_minvar$Sortino,         3),           
    percent(metrics_minvar$MaxDrawdown,  accuracy = 0.01),
    percent(metrics_minvar$Total_Weight, accuracy = 0.01),
    percent(metrics_minvar$Cash_Position,accuracy = 0.01)
  )
)

print(summary_table)

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("ASIGNACIÓN DEL PORTAFOLIO DE MÍNIMA VARIANZA\n")
cat("═══════════════════════════════════════════════════════════════\n")

minvar_allocation <- data.frame(
  Activo = names(metrics_minvar$Weights),
  Peso   = percent(metrics_minvar$Weights, accuracy = 0.01)
) %>%
  filter(metrics_minvar$Weights > 0.001) %>%
  arrange(desc(metrics_minvar$Weights))

print(minvar_allocation)

cat(paste("\nPeso Total Invertido:", percent(metrics_minvar$Total_Weight, accuracy = 0.01), "\n"))
if (abs(metrics_minvar$Cash_Position) > 0.001) {
  if (metrics_minvar$Cash_Position > 0) {
    cat(paste("Posición en Efectivo:", percent(metrics_minvar$Cash_Position, accuracy = 0.01), "\n"))
  } else {
    cat(paste("Apalancamiento:", percent(abs(metrics_minvar$Cash_Position), accuracy = 0.01), "\n"))
  }
}
