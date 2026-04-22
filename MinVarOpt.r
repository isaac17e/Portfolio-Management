# ==============================================================================
# OPTIMIZACIÓN DE PORTAFOLIOS CON MÍNIMA VARIANZA
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
library(nlshrink)
library(lubridate)

# ==============================================================================
# SECCIÓN 1: PARÁMETROS CONFIGURABLES
# ==============================================================================

n_top_sp500  <- 20
n_top_nasdaq <- 20
benchmark    <- "SPY"

# Mes(es) objetivo: sobre estos se evaluarán las métricas del portafolio.
target_month <- c(4)

# Ventana de estimación fija: 5 años de retornos mensuales completos
estimation_years <- 5
estimation_start <- as.character(Sys.Date() - round(estimation_years * 365))
estimation_end   <- as.character(Sys.Date())

# Factor de anualización mensual
annualization_factor <- 12

risk_free_rate <- 0.03685

weight_sharpe  <- 0.20
weight_low_vol <- 0.35
weight_decorr  <- 0.55

n_divers_candidates     <- 45
volatility_percentile   <- 0.85
correlation_percentile  <- 0.75
max_assets_in_portfolio <- 15

max_weight_per_asset <- 0.20
min_weight_per_asset <- 0.02

require_full_investment <- FALSE
min_total_weight        <- 0.95
max_total_weight        <- 1.00

horizon_months <- length(target_month)
horizon_label  <- paste(month.name[target_month], collapse = " + ")

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("CONFIGURACIÓN DEL MODELO\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(paste("[CONFIG] Mes(es) objetivo (evaluación):", horizon_label, "\n"))
cat(paste("[CONFIG] Horizonte de inversión:        ", horizon_months, "mes(es)\n"))
cat(paste("[CONFIG] Ventana estimación:            ", estimation_years, "años\n"))
cat(paste("[CONFIG] Desde:", estimation_start, "hasta:", estimation_end, "\n"))
cat(paste("[CONFIG] Factor anualización:           ", annualization_factor, "(mensual)\n\n"))

# ==============================================================================
# SECCIÓN 2: FUNCIONES DE WEB SCRAPING
# ==============================================================================

safe_scrape_table <- function(url, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      cat(paste("   Intento", attempt, "de", max_retries, "...\n"))
      
      response <- httr::GET(
        url,
        httr::add_headers(
          `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
          `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "en-US,en;q=0.5",
          `Referer`         = "https://www.google.com/"
        )
      )
      
      page   <- read_html(httr::content(response, as = "text", encoding = "UTF-8"))
      tables <- page %>% html_table(fill = TRUE)
      
      if (length(tables) > 0) {
        cat("   ✓ Tabla obtenida exitosamente\n")
        return(tables[[1]])
      }
      
    }, error = function(e) {
      cat(paste("   ✗ Error:", e$message, "\n"))
      if (attempt < max_retries) Sys.sleep(3)
    })
  }
  
  cat("   ✗ No se pudo obtener la tabla después de", max_retries, "intentos\n")
  return(NULL)
}

get_sp500_tickers <- function(n_top = 50) {
  cat("\n📊 Obteniendo tickers del S&P 500...\n")
  
  sp500_tbl <- safe_scrape_table("https://www.slickcharts.com/sp500")
  
  if (is.null(sp500_tbl)) {
    cat("   ↩ Intentando con Wikipedia como alternativa...\n")
    sp500_tbl <- safe_scrape_table(
      "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    )
  }
  
  if (is.null(sp500_tbl)) {
    stop("Error: No se pudo obtener la tabla del S&P 500 desde ninguna fuente")
  }
  
  ticker_col     <- NULL
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
  
  ticker_col     <- NULL
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

cat("[INFO] Descargando retornos mensuales desde Yahoo Finance...\n")
cat(paste("[INFO] Periodo estimación:", estimation_start, "hasta", estimation_end, "\n"))
cat("[INFO] Esto puede tomar varios minutos...\n\n")

# ---------------------------------------------------------------------------
# FLUJO 1: Retornos mensuales completos — usados para ESTIMAR parámetros
# Se descarga el histórico de 5 años completo sin filtro de mes.
# tq_transmute con periodReturn calcula el retorno de cada mes usando
# cierres ajustados, sin necesidad de resamplear manualmente.
# ---------------------------------------------------------------------------

cat("[INFO] Calculando retornos mensuales (histórico completo 5 años)...\n")

raw_monthly <- tq_get(
  all_tickers,
  get  = "stock.prices",
  from = estimation_start,
  to   = estimation_end
) %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "monthly_return"
  ) %>%
  filter(!is.na(monthly_return)) %>%
  ungroup()

cat(paste("[INFO] Observaciones descargadas:", nrow(raw_monthly), "\n"))
cat(paste("[INFO] Tickers con datos:", length(unique(raw_monthly$symbol)), "\n"))

# Filtrar tickers con cobertura suficiente (>80% de los meses esperados)
expected_months <- estimation_years * 12

coverage <- raw_monthly %>%
  group_by(symbol) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= round(expected_months * 0.80))

cat(paste("[INFO] Tickers con cobertura >80% (",
          round(expected_months * 0.80), "meses):", nrow(coverage), "\n"))

monthly_full <- raw_monthly %>%
  filter(symbol %in% coverage$symbol)

# ---------------------------------------------------------------------------
# FLUJO 2: Retornos del mes objetivo — usados para EVALUAR métricas
# Se filtra el histórico por target_month. Para horizontes de 2-3 meses,
# los retornos se acumulan por año con prod(1 + r) - 1.
# ---------------------------------------------------------------------------

cat(paste("\n[INFO] Filtrando retornos para mes(es) objetivo:", horizon_label, "...\n"))

monthly_target <- raw_monthly %>%
  filter(
    symbol %in% coverage$symbol,
    month(date) %in% target_month
  ) %>%
  mutate(year = year(date))

if (horizon_months > 1) {
  monthly_target <- monthly_target %>%
    group_by(symbol, year) %>%
    summarise(
      monthly_return = prod(1 + monthly_return) - 1,
      date           = max(date),
      n_months_found = n(),
      .groups        = "drop"
    ) %>%
    filter(n_months_found == horizon_months)
  
  cat(paste("[INFO] Retornos acumulados (", horizon_months,
            "meses) calculados correctamente\n"))
}

n_target_obs <- monthly_target %>%
  group_by(symbol) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(min = min(n), median = median(n), max = max(n))

cat(paste("[INFO] Observaciones en mes(es) objetivo por ticker —",
          "Min:", n_target_obs$min,
          "| Mediana:", round(n_target_obs$median),
          "| Max:", n_target_obs$max, "\n"))

if (n_target_obs$median < 3) {
  warning(paste(
    "ADVERTENCIA: Menos de 3 observaciones históricas en el mes objetivo.",
    "Las métricas de evaluación pueden ser inestables.",
    "Considere ampliar estimation_years."
  ))
}

# ---------------------------------------------------------------------------
# BENCHMARK
# ---------------------------------------------------------------------------

cat("\n[INFO] Descargando benchmark (SPY)...\n")

benchmark_monthly <- tq_get(
  benchmark,
  get  = "stock.prices",
  from = estimation_start,
  to   = estimation_end
) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "benchmark_return"
  ) %>%
  filter(!is.na(benchmark_return))

benchmark_target <- benchmark_monthly %>%
  filter(month(date) %in% target_month) %>%
  mutate(year = year(date))

if (horizon_months > 1) {
  benchmark_target <- benchmark_target %>%
    group_by(year) %>%
    summarise(
      benchmark_return = prod(1 + benchmark_return) - 1,
      date             = max(date),
      .groups          = "drop"
    )
}

cat(paste("[INFO] Benchmark:", nrow(benchmark_monthly),
          "meses completos |", nrow(benchmark_target), "períodos objetivo\n"))

# ---------------------------------------------------------------------------
# CONSTRUCCIÓN DE MATRICES XTS
# ---------------------------------------------------------------------------

cat("\n[INFO] Construyendo matrices XTS para optimización...\n")

prices_wide_full <- monthly_full %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date) %>%
  drop_na()

n_obs_full    <- nrow(prices_wide_full)
valid_tickers <- colnames(prices_wide_full)[-1]

log_returns_full <- xts(
  as.matrix(prices_wide_full[, -1]),
  order.by = prices_wide_full$date
)

prices_wide_target <- monthly_target %>%
  select(symbol, date, monthly_return) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date)

common_tickers_target <- intersect(valid_tickers, colnames(prices_wide_target)[-1])
prices_wide_target    <- prices_wide_target %>%
  select(date, all_of(common_tickers_target)) %>%
  drop_na()

log_returns_target <- xts(
  as.matrix(prices_wide_target[, -1]),
  order.by = prices_wide_target$date
)

benchmark_xts_full <- xts(
  matrix(benchmark_monthly$benchmark_return, ncol = 1,
         dimnames = list(NULL, "SPY")),
  order.by = benchmark_monthly$date
)

benchmark_xts_target <- xts(
  matrix(benchmark_target$benchmark_return, ncol = 1,
         dimnames = list(NULL, "SPY")),
  order.by = benchmark_target$date
)

cat(paste("[INFO] Matriz estimación:  ", nrow(log_returns_full),
          "meses ×", ncol(log_returns_full), "activos\n"))
cat(paste("[INFO] Matriz evaluación:  ", nrow(log_returns_target),
          "períodos objetivo ×", ncol(log_returns_target), "activos\n"))

ratio_obs <- nrow(log_returns_full) / ncol(log_returns_full)
cat(paste("[INFO] Ratio obs/activos (estimación):", round(ratio_obs, 2),
          ifelse(ratio_obs >= 3, "✓ Aceptable", "⚠ Bajo"), "\n"))

if (nrow(log_returns_full) < 24) {
  stop("Error: Menos de 24 meses de datos para estimación. Revise estimation_years.")
}

if (ncol(log_returns_full) < 5) {
  stop("Error: Menos de 5 tickers válidos.")
}

# ---------------------------------------------------------------------------
# COVARIANZA CON LEDOIT-WOLF SHRINKAGE (sobre histórico completo)
# ---------------------------------------------------------------------------

cat("\n[INFO] Estimando matriz de covarianza con Ledoit-Wolf shrinkage...\n")

returns_matrix_full <- as.matrix(log_returns_full)

cov_mat_shrink <- tryCatch(
  nlshrink::linshrink_cov(returns_matrix_full),
  error = function(e) {
    cat("[ADVERTENCIA] Ledoit-Wolf falló, usando covarianza muestral.\n")
    cov(returns_matrix_full)
  }
)

cov_mat <- cov_mat_shrink

cat(paste("[INFO] Dimensión matriz de covarianza:",
          nrow(cov_mat), "x", ncol(cov_mat), "\n"))
cat("[INFO] Datos listos para optimización.\n\n")

# ==============================================================================
# SECCIÓN 5: ANÁLISIS DESCRIPTIVO Y SELECCIÓN POR PERFIL DE RIESGO
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("ANÁLISIS DE SELECCIÓN DE ACTIVOS POR PERFIL DE RIESGO\n")
cat(paste("Estimación: histórico completo", estimation_years, "años\n"))
cat(paste("Evaluación: mes(es) objetivo —", horizon_label, "\n"))
cat("═══════════════════════════════════════════════════════════════\n\n")

safe_sd   <- function(x) tryCatch(sd(x,   na.rm = TRUE), error = function(e) NA)
safe_mean <- function(x) tryCatch(mean(x, na.rm = TRUE), error = function(e) NA)

asset_stats <- data.frame(
  Symbol      = colnames(log_returns_full),
  Mean_Return = sapply(1:ncol(log_returns_full),
                       function(i) safe_mean(log_returns_full[, i]) * annualization_factor),
  Volatility  = sapply(1:ncol(log_returns_full),
                       function(i) safe_sd(log_returns_full[, i]) * sqrt(annualization_factor)),
  stringsAsFactors = FALSE
)

asset_stats$Sharpe <- (asset_stats$Mean_Return - risk_free_rate) / asset_stats$Volatility

common_full       <- index(log_returns_full)[index(log_returns_full) %in% index(benchmark_xts_full)]
log_ret_aligned   <- log_returns_full[common_full]
bench_ret_aligned <- benchmark_xts_full[common_full]

correlations <- sapply(colnames(log_ret_aligned), function(ticker) {
  tryCatch(
    cor(log_ret_aligned[, ticker], bench_ret_aligned[, 1], use = "complete.obs"),
    error = function(e) NA
  )
})

asset_stats$Correlation_SPY <- correlations

mdd_values <- sapply(colnames(log_returns_full), function(ticker) {
  tryCatch(maxDrawdown(log_returns_full[, ticker]), error = function(e) NA)
})

asset_stats$Max_Drawdown <- mdd_values

asset_stats <- asset_stats %>%
  filter(!is.na(Sharpe) & !is.na(Volatility) & !is.na(Correlation_SPY) &
           !is.infinite(Sharpe) & !is.infinite(Volatility)) %>%
  arrange(Correlation_SPY)

cat(paste("[INFO] Activos con métricas válidas:", nrow(asset_stats), "\n\n"))

cat("Top 15 activos con MENOR correlación con SPY (histórico completo):\n")
print(head(asset_stats %>%
             select(Symbol, Correlation_SPY, Volatility, Sharpe, Max_Drawdown) %>%
             mutate(
               Correlation_SPY = round(Correlation_SPY, 3),
               Volatility      = percent(Volatility, accuracy = 0.1),
               Sharpe          = round(Sharpe, 2),
               Max_Drawdown    = percent(Max_Drawdown, accuracy = 0.1)
             ), 15))

normalize <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
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

vol_threshold  <- quantile(asset_stats$Volatility,           volatility_percentile,  na.rm = TRUE)
corr_threshold <- quantile(abs(asset_stats$Correlation_SPY), correlation_percentile, na.rm = TRUE)

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

log_returns_selected        <- log_returns_full[, selected_tickers]
log_returns_selected_target <- log_returns_target[,
                                                  intersect(selected_tickers, colnames(log_returns_target))]

# ==============================================================================
# SECCIÓN 6: ESTADÍSTICAS DESCRIPTIVAS
# ==============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("ESTADÍSTICAS DESCRIPTIVAS — HISTÓRICO COMPLETO (ESTIMACIÓN)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

descriptive_stats <- data.frame(
  Symbol            = colnames(log_returns_selected),
  Media_Mensual     = colMeans(log_returns_selected),
  Mediana_Mensual   = apply(log_returns_selected, 2, median),
  SD_Mensual        = apply(log_returns_selected, 2, sd),
  Retorno_Anual     = colMeans(log_returns_selected) * annualization_factor,
  Volatilidad_Anual = apply(log_returns_selected, 2, sd) * sqrt(annualization_factor),
  Asimetria         = apply(log_returns_selected, 2, skewness),
  Curtosis          = apply(log_returns_selected, 2, kurtosis),
  Max_Drawdown      = sapply(colnames(log_returns_selected), function(x) {
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
cat("Datos usados: histórico completo 5 años (estimación)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

mean_ret <- colMeans(log_returns_selected)
cov_mat  <- cov_mat_shrink[selected_tickers, selected_tickers]
sd_ret   <- apply(log_returns_selected, 2, sd)

port_spec <- portfolio.spec(assets = colnames(log_returns_selected))

if (require_full_investment) {
  port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
} else {
  port_spec <- add.constraint(portfolio = port_spec,
                              type    = "weight_sum",
                              min_sum = min_total_weight,
                              max_sum = max_total_weight)
}

port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.constraint(portfolio = port_spec,
                            type = "box",
                            min  = 0,
                            max  = max_weight_per_asset)

cat("[INFO] Optimizando portafolio de Mínima Varianza...\n")

port_minvar <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

opt_min_var <- optimize.portfolio(R               = log_returns_selected,
                                  portfolio       = port_minvar,
                                  optimize_method = "ROI",
                                  trace           = TRUE)

w_raw <- extractWeights(opt_min_var)
w_raw <- w_raw[w_raw > 0.001]

if (length(w_raw) > max_assets_in_portfolio) {
  cat(paste("\n[INFO] Aplicando límite de cardinalidad:",
            length(w_raw), "→", max_assets_in_portfolio, "activos\n"))
  w_raw <- sort(w_raw, decreasing = TRUE)[1:max_assets_in_portfolio]
  w_raw <- w_raw / sum(w_raw) * min(sum(w_raw), max_total_weight)
}

selected_tickers <- names(w_raw)

cat(paste("\n✓ Portafolio optimizado con", length(selected_tickers), "activos\n"))
cat(paste("  Activos:", paste(selected_tickers, collapse = ", "), "\n\n"))

log_returns_selected        <- log_returns_full[, selected_tickers]
log_returns_selected_target <- log_returns_target[,
                                                  intersect(selected_tickers, colnames(log_returns_target))]

mean_ret <- colMeans(log_returns_selected)
cov_mat  <- cov_mat_shrink[selected_tickers, selected_tickers]
sd_ret   <- apply(log_returns_selected, 2, sd)

# ==============================================================================
# SECCIÓN 8: FRONTERA EFICIENTE
# ==============================================================================

cat("\n[INFO] Calculando Frontera Eficiente...\n")

min_ret        <- min(mean_ret) * annualization_factor
max_ret        <- max(mean_ret) * annualization_factor
target_returns <- seq(min_ret, max_ret, length.out = 50)

efficient_frontier <- data.frame(Return = numeric(0), Risk = numeric(0))

n    <- length(selected_tickers)
Dmat <- 2 * cov_mat
dvec <- rep(0, n)

for (target_ret in target_returns) {
  tryCatch({
    target_periodic <- target_ret / annualization_factor
    
    Amat <- cbind(diag(n), rep(1, n), rep(-1, n), mean_ret, -diag(n))
    bvec <- c(rep(0, n), min_total_weight, -max_total_weight,
              target_periodic, rep(-max_weight_per_asset, n))
    
    sol <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec,
                              Amat = Amat, bvec = bvec, meq = 0)
    
    w    <- sol$solution
    w[w < 1e-6] <- 0
    
    ret  <- sum(w * mean_ret) * annualization_factor
    risk <- sqrt(as.numeric(t(w) %*% cov_mat %*% w)) * sqrt(annualization_factor)
    
    efficient_frontier <- rbind(efficient_frontier,
                                data.frame(Return = ret, Risk = risk))
  }, error = function(e) {})
}

cat(paste("[INFO] Frontera eficiente calculada con",
          nrow(efficient_frontier), "puntos.\n"))

# ==============================================================================
# SECCIÓN 9: EXTRACCIÓN DE MÉTRICAS
# Retorno/riesgo: estimados sobre histórico completo (5 años)
# Sharpe, Sortino, MDD: evaluados sobre retornos del mes(es) objetivo
# ==============================================================================

extract_metrics <- function(opt_obj, label) {
  w_raw <- extractWeights(opt_obj)
  w_raw <- w_raw[w_raw > 0.001]
  
  if (length(w_raw) > max_assets_in_portfolio) {
    w_raw <- sort(w_raw, decreasing = TRUE)[1:max_assets_in_portfolio]
    w_raw <- w_raw / sum(w_raw) * min(sum(w_raw), max_total_weight)
  }
  
  common_full   <- intersect(names(w_raw), colnames(log_returns_selected))
  common_target <- intersect(names(w_raw), colnames(log_returns_selected_target))
  
  w_full   <- w_raw[common_full]
  w_target <- w_raw[common_target]
  w_target <- w_target / sum(w_target)
  
  mr <- mean_ret[common_full]
  cm <- cov_mat[common_full, common_full]
  
  total_weight  <- sum(w_full)
  cash_position <- 1 - total_weight
  
  # Retorno y riesgo anualizados — estimados sobre histórico completo
  ret  <- sum(w_full * mr) * annualization_factor
  risk <- sqrt(as.numeric(t(w_full) %*% cm %*% w_full)) * sqrt(annualization_factor)
  
  # Métricas de evaluación — sobre retornos del mes(es) objetivo
  ret_mat_target  <- log_returns_selected_target[, common_target]
  port_ret_target <- xts(as.numeric(ret_mat_target %*% w_target),
                         order.by = index(ret_mat_target))
  
  rf_periodic   <- risk_free_rate / annualization_factor * horizon_months
  excess_target <- as.numeric(port_ret_target) - rf_periodic
  downside_ret  <- excess_target[excess_target < 0]
  
  ret_target_mean <- mean(as.numeric(port_ret_target)) * (annualization_factor / horizon_months)
  risk_target_sd  <- sd(as.numeric(port_ret_target))   * sqrt(annualization_factor / horizon_months)
  sharpe          <- (ret_target_mean - risk_free_rate) / risk_target_sd
  
  downside_dev <- if (length(downside_ret) > 1)
    sqrt(mean(downside_ret^2)) * sqrt(annualization_factor / horizon_months)
  else NA_real_
  
  sortino <- if (!is.na(downside_dev) && downside_dev > 0)
    (ret_target_mean - risk_free_rate) / downside_dev
  else NA_real_
  
  mdd <- maxDrawdown(port_ret_target)
  
  cat(paste("\n[INFO] Métricas de evaluación basadas en",
            nrow(port_ret_target), "períodos objetivo (", horizon_label, ")\n"))
  
  list(Label            = label,
       Return           = ret,
       Risk             = risk,
       Return_Target    = ret_target_mean,
       Risk_Target      = risk_target_sd,
       Sharpe           = sharpe,
       Sortino          = sortino,
       MaxDrawdown      = mdd,
       Weights          = w_full,
       Total_Weight     = total_weight,
       Cash_Position    = cash_position,
       N_Target_Periods = nrow(port_ret_target))
}

metrics_minvar <- extract_metrics(opt_min_var, "Mínima Varianza")

cat("\n[INFO] Métricas calculadas correctamente\n")
cat(paste("  Activos en portafolio:", length(metrics_minvar$Weights), "\n"))
cat(paste("  Retorno esperado (estimación, anual):",
          percent(metrics_minvar$Return, accuracy = 0.01), "\n"))
cat(paste("  Volatilidad (estimación, anual):     ",
          percent(metrics_minvar$Risk, accuracy = 0.01), "\n"))
cat(paste("  Sharpe (evaluación mes objetivo):    ",
          round(metrics_minvar$Sharpe, 3), "\n"))
cat(paste("  Períodos objetivo usados:            ",
          metrics_minvar$N_Target_Periods, "\n"))

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
  Return = mean_ret * annualization_factor,
  Risk   = sd_ret   * sqrt(annualization_factor)
)

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
  labs(title    = "Frontera Eficiente & Optimización de Markowitz",
       subtitle = paste("Estimación:", estimation_years, "años | Evaluación:", horizon_label,
                        "| Activos:", length(selected_tickers),
                        "| Covarianza: Ledoit-Wolf"),
       x        = "Riesgo Anualizado (Volatilidad)",
       y        = "Retorno Esperado Anualizado",
       caption  = paste("Data: Yahoo Finance | Periodo:",
                        estimation_start, "-", estimation_end)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 16, face = "bold"),
    plot.subtitle    = element_text(size = 10),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "gray70", fill = NA, size = 1)
  )

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
  geom_text(aes(label = ifelse(abs(Weight) > 0.03,
                               percent(Weight, accuracy = 0.1), "")),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  labs(title    = "Asignación Óptima de Activos — Mínima Varianza",
       subtitle = paste("Mes(es) objetivo:", horizon_label,
                        "| Peso máximo por activo:",
                        percent(max_weight_per_asset)),
       y = "Peso del Portafolio (%)",
       x = "") +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    plot.title         = element_text(size = 14, face = "bold"),
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 11, face = "bold")
  )

corr_matrix <- cor(log_returns_selected)

p3 <- ggcorrplot(corr_matrix,
                 hc.order = TRUE,
                 type     = "lower",
                 lab      = TRUE,
                 lab_size = 3,
                 colors   = c("#E46726", "white", "#6D9EC1"),
                 title    = paste("Matriz de Correlación — Estimación",
                                  estimation_years, "años"),
                 ggtheme  = theme_minimal())

print(p1)
print(p2)
print(p3)

# ==============================================================================
# SECCIÓN 11: RESUMEN FINAL
# ==============================================================================

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN DE OPTIMIZACIÓN DE PORTAFOLIO           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat(paste("Mes(es) objetivo (evaluación):", horizon_label, "\n"))
cat(paste("Horizonte de inversión:       ", horizon_months, "mes(es)\n"))
cat(paste("Ventana de estimación:        ", estimation_years, "años\n"))
cat(paste("Observaciones estimación:     ", n_obs_full, "meses\n"))
cat(paste("Períodos objetivo evaluados:  ", metrics_minvar$N_Target_Periods, "\n\n"))

summary_table <- data.frame(
  Métrica = c(
    "Retorno Esperado Anual (estimación histórica)",
    "Volatilidad Anual (estimación histórica)",
    paste("Retorno Anualizado (evaluación —", horizon_label, ")"),
    paste("Volatilidad Anualizada (evaluación —", horizon_label, ")"),
    paste("Ratio de Sharpe (evaluación —", horizon_label, ")"),
    paste("Ratio de Sortino (evaluación —", horizon_label, ")"),
    paste("Maximum Drawdown (evaluación —", horizon_label, ")"),
    "Peso Total Invertido",
    "Posición en Efectivo"
  ),
  Valor = c(
    percent(metrics_minvar$Return,        accuracy = 0.01),
    percent(metrics_minvar$Risk,          accuracy = 0.01),
    percent(metrics_minvar$Return_Target, accuracy = 0.01),
    percent(metrics_minvar$Risk_Target,   accuracy = 0.01),
    round(metrics_minvar$Sharpe,          3),
    round(metrics_minvar$Sortino,         3),
    percent(metrics_minvar$MaxDrawdown,   accuracy = 0.01),
    percent(metrics_minvar$Total_Weight,  accuracy = 0.01),
    percent(metrics_minvar$Cash_Position, accuracy = 0.01)
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

cat(paste("\nPeso Total Invertido:",
          percent(metrics_minvar$Total_Weight, accuracy = 0.01), "\n"))

if (abs(metrics_minvar$Cash_Position) > 0.001) {
  if (metrics_minvar$Cash_Position > 0) {
    cat(paste("Posición en Efectivo:",
              percent(metrics_minvar$Cash_Position, accuracy = 0.01), "\n"))
  } else {
    cat(paste("Apalancamiento:",
              percent(abs(metrics_minvar$Cash_Position), accuracy = 0.01), "\n"))
  }
}
