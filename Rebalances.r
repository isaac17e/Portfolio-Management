# ============================================================================
# SISTEMA DE OPTIMIZACIÓN DE PORTAFOLIO CON REEMPLAZO DE TICKERS
# By Isaac Echeverri - Versión Utilidad Cuadrática + Selección Avanzada
# ============================================================================
rm(list = ls())

library(tidyquant)
library(dplyr)
library(tidyr)
library(rvest)
library(quadprog)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(lubridate)
library(ggplot2)
library(xts)
library(knitr)
library(kableExtra)
library(FFdownload)

if ("plyr" %in% (.packages())) {
  detach("package:plyr", unload = TRUE, force = TRUE)
}

# ============================================================================
# PARÁMETROS DE ENTRADA (EDITABLES)
# ============================================================================

# 1. PORTAFOLIO ACTUAL
tickers <- c("AXSM", "KRYS", "AXON", "MDGL", "SMCI", "SMMT", "ENPH", "ARGX", "GLD",
             "CELC")

pesos <- c(0.15, 0.146, 0.136, 0.122, 0.109, 0.089, 0.0838, 0.0765, 0.05, 0.0383)

# 2. TICKERS A ELIMINAR
tickers_eliminar <- c("AXON")

# 3. ETIQUETA DE CONTEXTO (solo para el reporte, NO filtra datos)
target_months  <- c(4)
target_years   <- 2015:2025
horizon_months <- length(target_months)
horizon_label  <- paste(month.abb[target_months], collapse = "-")

# 4. HORIZONTE DE DATOS HISTÓRICOS
start_date <- "2020-01-01"
end_date   <- Sys.Date()

# 5. TASA LIBRE DE RIESGO (anual, T-bills)
risk_free_rate <- 0.03685
rf_monthly     <- risk_free_rate / 12   # Para excesos mensuales

# 6. LAMBDA (aversión al riesgo)
lambda <- 0.5

# 7. PARÁMETROS DE SELECCIÓN DE CANDIDATOS
n_candidates_nasdaq  <- 20
n_top_sp500          <- 20
min_observations     <- 24     # Mínimo 2 años de retornos mensuales
ideal_observations   <- 48     # Ideal 4 años (penalización de calidad)

volatility_percentile  <- 0.25
correlation_percentile <- 0.50
correlation_order      <- 0

# 8. PESOS DE SCORING
weight_sharpe  <- 0.15
weight_low_vol <- 0.65
weight_decorr  <- 0.20

# 9. RESTRICCIONES DE PORTAFOLIO
max_weight <- 0.12
n_sim      <- 5000
seed       <- 123

# 10. CLASIFICACIÓN DE ACTIVOS
etf_tickers       <- c("SPY", "GLD", "SLV", "TLT", "IEF", "VTI", "QQQ")
commodity_tickers <- c("GLD", "SLV", "GDX", "GDXJ")

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

safe_scrape_table <- function(url) {
  tryCatch({
    page   <- read_html(url)
    tables <- page %>% html_table(fill = TRUE)
    if (length(tables) > 0) return(tables[[1]])
    return(NULL)
  }, error = function(e) {
    cat(sprintf("Error al obtener tabla de %s\n", url))
    return(NULL)
  })
}

# ============================================================================
# INICIO DEL PROCESO
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("    SISTEMA DE OPTIMIZACIÓN DE PORTAFOLIO CON REEMPLAZO INTELIGENTE    \n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("📅 Datos históricos: %s → %s\n", start_date, end_date))
cat(sprintf("📅 Etiqueta de reporte: %s %d-%d\n",
            horizon_label, min(target_years), max(target_years)))
cat(sprintf("💰 RF Anual (T-bills): %.4f%%\n",  risk_free_rate * 100))
cat(sprintf("💰 RF Mensual: %.6f%%\n",           rf_monthly * 100))
cat(sprintf("🎯 Lambda (aversión al riesgo): %.2f\n\n", lambda))

tickers_mantener <- setdiff(tickers, tickers_eliminar)
pesos_mantener   <- pesos[which(tickers %in% tickers_mantener)]

cat(sprintf("🗑️  Tickers a eliminar (%d): %s\n",
            length(tickers_eliminar), paste(tickers_eliminar, collapse = ", ")))
cat(sprintf("✅ Tickers a mantener (%d): %s\n\n",
            length(tickers_mantener), paste(tickers_mantener, collapse = ", ")))

# ============================================================================
# PASO 1: OBTENCIÓN DE CANDIDATOS DEL NASDAQ Y S&P 500
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 1: OBTENCIÓN DE CANDIDATOS\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("📊 Obteniendo tickers del NASDAQ...\n")
nasdaq_tbl <- safe_scrape_table("https://stockanalysis.com/list/nasdaq-stocks/")

if (is.null(nasdaq_tbl)) {
  cat("❌ No se pudo obtener la lista del NASDAQ. Usando lista alternativa...\n")
  candidatos_nasdaq <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA", "META", "TSLA",
                         "AVGO", "ASML", "COST", "ADBE", "NFLX", "PEP", "CSCO",
                         "AMD", "INTC", "QCOM", "INTU", "TXN", "AMAT", "HON",
                         "BKNG", "SBUX", "GILD", "MDLZ", "ADI", "REGN", "VRTX",
                         "ISRG", "LRCX", "PANW", "MU", "ADP", "KLAC", "SNPS",
                         "CDNS", "MELI", "CTAS", "PYPL", "MRNA", "NXPI", "ABNB",
                         "MAR", "ORLY", "WDAY", "FTNT", "CHTR", "PAYX", "DXCM",
                         "ZS", "CRWD", "DDOG", "NET", "SNOW")
} else {
  if      ("Symbol" %in% names(nasdaq_tbl)) candidatos_nasdaq <- nasdaq_tbl$Symbol
  else if ("Ticker" %in% names(nasdaq_tbl)) candidatos_nasdaq <- nasdaq_tbl$Ticker
  else                                       candidatos_nasdaq <- nasdaq_tbl[[1]]
}

candidatos_nasdaq <- setdiff(candidatos_nasdaq, tickers)
candidatos_nasdaq <- head(candidatos_nasdaq, n_candidates_nasdaq)
cat(sprintf("  ✓ Candidatos del NASDAQ: %d\n", length(candidatos_nasdaq)))

cat("📊 Obteniendo tickers del S&P 500...\n")
sp500_tbl <- safe_scrape_table("https://www.slickcharts.com/sp500")

if (is.null(sp500_tbl)) {
  cat("⚠️  No se pudo obtener la lista del S&P 500. Continuando solo con NASDAQ...\n")
  candidatos_sp500 <- character(0)
} else {
  if      ("Symbol" %in% names(sp500_tbl)) candidatos_sp500 <- sp500_tbl$Symbol
  else if ("Ticker" %in% names(sp500_tbl)) candidatos_sp500 <- sp500_tbl$Ticker
  else                                      candidatos_sp500 <- sp500_tbl[[2]]
  
  candidatos_sp500 <- setdiff(candidatos_sp500, tickers)
  candidatos_sp500 <- head(candidatos_sp500, n_top_sp500)
  cat(sprintf("  ✓ Candidatos del S&P 500: %d\n", length(candidatos_sp500)))
}

candidatos_combinados <- unique(c(candidatos_nasdaq, candidatos_sp500))
all_tickers           <- unique(c(tickers_mantener, candidatos_combinados))

cat(sprintf("\n📊 Total candidatos únicos: %d\n",    length(candidatos_combinados)))
cat(sprintf("   • Del NASDAQ: %d\n",                 length(candidatos_nasdaq)))
cat(sprintf("   • Del S&P 500: %d\n",                length(candidatos_sp500)))
cat(sprintf("   • Duplicados eliminados: %d\n\n",
            length(candidatos_nasdaq) + length(candidatos_sp500) - length(candidatos_combinados)))

# ============================================================================
# PASO 2: DESCARGA DE DATOS Y RETORNOS MENSUALES
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 2: DESCARGA DE DATOS Y RETORNOS MENSUALES\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("📥 Descargando retornos mensuales desde %s...\n", start_date))
cat("   Esto puede tomar varios minutos...\n\n")

stock_data_raw <- tq_get(all_tickers,
                         get  = "stock.prices",
                         from = start_date,
                         to   = end_date)

# Benchmark
cat("📥 Descargando benchmark (SPY)...\n")
benchmark_raw <- tq_get("SPY",
                        get  = "stock.prices",
                        from = start_date,
                        to   = end_date)

# ---------------------------------------------------------------------------
# Retornos mensuales con periodReturn (igual que Código 1)
# ---------------------------------------------------------------------------
raw_monthly <- stock_data_raw %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "monthly_return") %>%
  filter(!is.na(monthly_return)) %>%
  ungroup()

# Filtrar cobertura >80% de meses esperados
expected_months <- as.numeric(difftime(as.Date(end_date), as.Date(start_date),
                                       units = "days")) / 30.44

coverage <- raw_monthly %>%
  group_by(symbol) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(n_obs >= round(expected_months * 0.80))

raw_monthly <- raw_monthly %>% filter(symbol %in% coverage$symbol)

cat(sprintf("  ✓ Tickers con >80%% de cobertura mensual: %d\n", nrow(coverage)))

# Matriz wide → xts
prices_wide_clean <- raw_monthly %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date) %>%
  drop_na()

log_returns_full <- xts(as.matrix(prices_wide_clean[, -1]),
                        order.by = prices_wide_clean$date)

# Benchmark mensual
benchmark_returns_full <- benchmark_raw %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "benchmark_return") %>%
  filter(!is.na(benchmark_return)) %>%
  { xts(.$benchmark_return, order.by = .$date) }

# Alinear fechas comunes
common_dates           <- index(log_returns_full)[index(log_returns_full) %in%
                                                    index(benchmark_returns_full)]
log_returns_full       <- log_returns_full[common_dates]
benchmark_returns_full <- benchmark_returns_full[common_dates]

n_obs   <- nrow(log_returns_full)
n_years <- n_obs / 12

cat(sprintf("  ✓ Observaciones mensuales: %d\n", n_obs))
cat(sprintf("  ✓ Años cubiertos: %.1f\n\n", n_years))

# Separar retornos de tickers_mantener y candidatos
valid_mantener   <- intersect(tickers_mantener,      colnames(log_returns_full))
valid_candidatos <- intersect(candidatos_combinados, colnames(log_returns_full))
log_returns_all  <- log_returns_full[, c(valid_mantener, valid_candidatos)]

# ============================================================================
# PASO 3: ESTADÍSTICAS DESCRIPTIVAS (base mensual, anualización ×12)
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 3: ESTADÍSTICAS DESCRIPTIVAS\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

mean_ret_monthly <- colMeans(log_returns_all)
sd_ret_monthly   <- apply(log_returns_all, 2, sd)

summary_stats <- data.frame(
  symbol      = colnames(log_returns_all),
  mean_return = mean_ret_monthly * 12,          # Retorno anual
  sd_return   = sd_ret_monthly   * sqrt(12),    # Volatilidad anual
  n_obs       = nrow(log_returns_all),
  stringsAsFactors = FALSE
) %>%
  filter(sd_return > 0) %>%
  mutate(
    sharpe_ratio         = (mean_return - risk_free_rate) / sd_return,
    data_quality_penalty = pmin(n_obs / ideal_observations, 1.0)
  )

cat(sprintf("  ✓ Activos con métricas válidas: %d\n\n", nrow(summary_stats)))

# Correlación promedio con el resto
returns_matrix <- as.matrix(log_returns_all)
cor_matrix_all <- cor(returns_matrix, use = "pairwise.complete.obs")

avg_cor_by_ticker <- data.frame(
  symbol  = colnames(cor_matrix_all),
  avg_cor = colMeans(abs(cor_matrix_all), na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Correlación con benchmark
cor_benchmark <- sapply(colnames(log_returns_all), function(tk) {
  tryCatch(
    cor(log_returns_all[, tk], benchmark_returns_full[, 1], use = "complete.obs"),
    error = function(e) NA
  )
})

summary_stats <- summary_stats %>%
  left_join(avg_cor_by_ticker, by = "symbol") %>%
  mutate(
    avg_cor          = coalesce(avg_cor, median(avg_cor, na.rm = TRUE)),
    cor_benchmark    = cor_benchmark[symbol],
    is_etf_commodity = symbol %in% c(etf_tickers, commodity_tickers)
  )

# ============================================================================
# FAMA-FRENCH (retornos mensuales del histórico)
# ============================================================================

cat("📥 Descargando datos Fama-French...\n")

ff_data <- tryCatch({
  ff_raw <- download_french_data("Fama/French 3 Factors")$subsets$data[[1]] %>%
    mutate(date = as.Date(ym(date))) %>%
    filter(date >= as.Date(start_date)) %>%
    mutate(across(c(`Mkt-RF`, SMB, HML), ~ . / 100))
  
  xts(ff_raw[, c("Mkt-RF", "SMB", "HML")], order.by = ff_raw$date)
  
}, error = function(e) {
  cat("⚠️ Error con Fama-French. Continuando sin ellos.\n")
  return(NULL)
})

# Betas FF3 usando retornos mensuales directos
if (!is.null(ff_data)) {
  cat("📊 Calculando betas Fama-French (retornos mensuales)...\n")
  
  ff_monthly <- ff_data[index(ff_data) %in% index(log_returns_all)]
  
  if (nrow(ff_monthly) >= 6) {
    common_m <- index(log_returns_all)[index(log_returns_all) %in% index(ff_monthly)]
    lr_m     <- log_returns_all[common_m]
    ff_m     <- ff_monthly[common_m]
    
    ff_stats_list <- lapply(colnames(lr_m), function(tk) {
      tryCatch({
        excess <- as.numeric(lr_m[, tk]) - rf_monthly
        model  <- lm(excess ~ as.numeric(ff_m$`Mkt-RF`) +
                       as.numeric(ff_m$SMB) +
                       as.numeric(ff_m$HML))
        data.frame(symbol   = tk,
                   beta_mkt = coef(model)[2],
                   beta_smb = coef(model)[3],
                   beta_hml = coef(model)[4])
      }, error = function(e) {
        data.frame(symbol = tk, beta_mkt = NA_real_,
                   beta_smb = NA_real_, beta_hml = NA_real_)
      })
    })
    
    ff_stats <- bind_rows(ff_stats_list)
    
    mkt_prem <- mean(as.numeric(ff_m$`Mkt-RF`), na.rm = TRUE)
    smb_prem <- mean(as.numeric(ff_m$SMB),       na.rm = TRUE)
    hml_prem <- mean(as.numeric(ff_m$HML),       na.rm = TRUE)
    
    ff_stats <- ff_stats %>%
      mutate(ff_expected_return_monthly = rf_monthly +
               beta_mkt * mkt_prem + beta_smb * smb_prem + beta_hml * hml_prem,
             ff_expected_return = ff_expected_return_monthly * 12)
    
    cat(sprintf("  ✓ Betas calculados para %d activos\n", nrow(ff_stats)))
  } else {
    ff_stats <- data.frame(symbol = character(), beta_mkt = numeric(),
                           beta_smb = numeric(), beta_hml = numeric(),
                           ff_expected_return = numeric())
  }
} else {
  ff_stats <- data.frame(symbol = character(), beta_mkt = numeric(),
                         beta_smb = numeric(), beta_hml = numeric(),
                         ff_expected_return = numeric())
}

combined_stats <- summary_stats %>%
  left_join(ff_stats %>% select(symbol, beta_mkt, beta_smb, beta_hml, ff_expected_return),
            by = "symbol") %>%
  mutate(
    ff_expected_return    = coalesce(ff_expected_return, mean_return),
    adjusted_return       = (mean_return + ff_expected_return) / 2,
    sharpe_ratio_adjusted = (adjusted_return - risk_free_rate) / sd_return
  )

cat(sprintf("✅ combined_stats creado: %d activos\n\n", nrow(combined_stats)))

# ============================================================================
# PASO 4: SELECCIÓN DE CANDIDATOS PARA REEMPLAZO
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 4: SELECCIÓN DE CANDIDATOS PARA REEMPLAZO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("🎯 Prioridad: Menor volatilidad anual | Datos desde: %s\n", start_date))
cat(sprintf("  🔧 Correlación: %s\n\n",
            ifelse(correlation_order == 1,
                   "Alta correlación preferida",
                   "Baja correlación preferida")))

combined_stats_candidatos <- combined_stats %>%
  filter(!symbol %in% tickers_mantener)

cat(sprintf("  ℹ️  Universo de candidatos: %d tickers\n\n",
            nrow(combined_stats_candidatos)))

select_optimal_candidates <- function(df, n_candidates) {
  df_filtered <- df %>%
    dplyr::filter(
      n_obs >= min_observations,
      sd_return > 0,
      !is.na(sharpe_ratio_adjusted),
      is.finite(sharpe_ratio_adjusted)
    )
  
  cat(sprintf("  ✓ Activos con datos válidos (≥%d meses): %d\n",
              min_observations, nrow(df_filtered)))
  
  sd_threshold <- quantile(df_filtered$sd_return, volatility_percentile, na.rm = TRUE)
  cat(sprintf("  ✓ Umbral volatilidad (P%.0f): %.2f%%\n",
              volatility_percentile * 100, sd_threshold * 100))
  
  cor_threshold <- quantile(df_filtered$avg_cor, correlation_percentile, na.rm = TRUE)
  
  if (correlation_order == 1) {
    df_candidates <- df_filtered %>%
      dplyr::filter(sd_return <= sd_threshold, avg_cor >= cor_threshold)
    cat(sprintf("  ✓ Umbral correlación (P%.0f, alta): %.3f\n",
                correlation_percentile * 100, cor_threshold))
  } else {
    df_candidates <- df_filtered %>%
      dplyr::filter(sd_return <= sd_threshold, avg_cor <= cor_threshold)
    cat(sprintf("  ✓ Umbral correlación (P%.0f, baja): %.3f\n",
                correlation_percentile * 100, cor_threshold))
  }
  
  cat(sprintf("  ✓ Activos que cumplen ambos criterios: %d\n", nrow(df_candidates)))
  
  if (nrow(df_candidates) < n_candidates * 0.5) {
    cat("  ⚠️ Pocos candidatos. Relajando filtros...\n")
    sd_threshold  <- quantile(df_filtered$sd_return, 0.75, na.rm = TRUE)
    cor_threshold <- quantile(df_filtered$avg_cor,   0.80, na.rm = TRUE)
    
    if (correlation_order == 1) {
      df_candidates <- df_filtered %>%
        dplyr::filter(sd_return <= sd_threshold, avg_cor >= cor_threshold)
    } else {
      df_candidates <- df_filtered %>%
        dplyr::filter(sd_return <= sd_threshold, avg_cor <= cor_threshold)
    }
    cat(sprintf("  ✓ Con filtros relajados: %d candidatos\n", nrow(df_candidates)))
  }
  
  df_candidates <- df_candidates %>%
    dplyr::mutate(
      sharpe_norm = (sharpe_ratio_adjusted - min(sharpe_ratio_adjusted, na.rm = TRUE)) /
        (max(sharpe_ratio_adjusted, na.rm = TRUE) - min(sharpe_ratio_adjusted, na.rm = TRUE)),
      vol_norm    = 1 - (sd_return - min(sd_return, na.rm = TRUE)) /
        (max(sd_return, na.rm = TRUE) - min(sd_return, na.rm = TRUE)),
      corr_norm   = if (correlation_order == 1) {
        (avg_cor - min(avg_cor, na.rm = TRUE)) /
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      } else {
        1 - (avg_cor - min(avg_cor, na.rm = TRUE)) /
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      },
      sharpe_norm = ifelse(is.nan(sharpe_norm), 0.5, sharpe_norm),
      vol_norm    = ifelse(is.nan(vol_norm),    0.5, vol_norm),
      corr_norm   = ifelse(is.nan(corr_norm),   0.5, corr_norm),
      composite_score = (weight_sharpe  * sharpe_norm +
                           weight_low_vol * vol_norm    +
                           weight_decorr  * corr_norm) * data_quality_penalty
    ) %>%
    dplyr::arrange(desc(composite_score))
  
  n_to_select <- min(n_candidates, nrow(df_candidates))
  selected    <- df_candidates$symbol[1:n_to_select]
  
  cat("\n  📋 Top 10 candidatos seleccionados:\n")
  print(df_candidates[1:min(10, nrow(df_candidates)),
                      c("symbol", "sharpe_ratio_adjusted", "sd_return",
                        "avg_cor", "n_obs", "composite_score")], row.names = FALSE)
  
  return(list(selected = selected, stats = df_candidates))
}

n_reemplazos                    <- length(tickers_eliminar)
resultado_seleccion             <- select_optimal_candidates(combined_stats_candidatos, n_reemplazos)
ticker_candidatos_seleccionados <- resultado_seleccion$selected

cat(sprintf("\n✅ Candidatos seleccionados: %s\n\n",
            paste(ticker_candidatos_seleccionados, collapse = ", ")))

# ============================================================================
# PASO 5: CONSTRUCCIÓN DEL NUEVO PORTAFOLIO (PESOS ORIGINALES + REEMPLAZO)
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 5: CONSTRUCCIÓN DEL NUEVO PORTAFOLIO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

nuevo_portafolio <- c(tickers_mantener, ticker_candidatos_seleccionados)
valid_port       <- intersect(nuevo_portafolio, colnames(log_returns_full))

cat(sprintf("📊 Activos para el portafolio final: %d\n", length(valid_port)))
cat(sprintf("   • Mantenidos: %d\n", length(intersect(tickers_mantener, valid_port))))
cat(sprintf("   • Nuevos:     %d\n\n", length(intersect(ticker_candidatos_seleccionados, valid_port))))

# ---------------------------------------------------------------------------
# Pesos: originales para los mantenidos + peso del eliminado al nuevo
# ---------------------------------------------------------------------------
peso_eliminado <- sum(pesos[tickers %in% tickers_eliminar])  # soporta >1 eliminado

pesos_finales <- setNames(pesos_mantener, tickers_mantener)

for (i in seq_along(ticker_candidatos_seleccionados)) {
  tk <- ticker_candidatos_seleccionados[i]
  if (tk %in% valid_port) {
    # Reparte el peso eliminado equitativamente entre los nuevos entrantes
    pesos_finales[tk] <- peso_eliminado / length(ticker_candidatos_seleccionados)
  }
}

# Normalizar (corrige cualquier diferencia de redondeo)
pesos_finales <- pesos_finales / sum(pesos_finales)
weights_opt   <- pesos_finales[valid_port]

cat("📋 Pesos asignados:\n")
cat(sprintf("   • Peso liberado por eliminados (%s): %.2f%%\n",
            paste(tickers_eliminar, collapse = ", "), peso_eliminado * 100))
for (tk in ticker_candidatos_seleccionados) {
  cat(sprintf("   • %s recibe: %.2f%%\n", tk,
              weights_opt[tk] * 100))
}
cat(sprintf("   • Suma total de pesos: %.4f%%\n\n", sum(weights_opt) * 100))

# ---------------------------------------------------------------------------
# Retornos mensuales del portafolio con los pesos fijos
# ---------------------------------------------------------------------------
lr_port <- log_returns_full[, valid_port]

mu_monthly      <- colMeans(lr_port)
cov_mat_monthly <- cov(lr_port)
mu_annual       <- mu_monthly      * 12
cov_mat_annual  <- cov_mat_monthly * 12

if (any(!is.finite(mu_annual)))      mu_annual[!is.finite(mu_annual)]           <- 0
if (any(!is.finite(cov_mat_annual))) cov_mat_annual[!is.finite(cov_mat_annual)] <- 0

# ---------------------------------------------------------------------------
# MÉTRICAS (anualizadas ×12)
# ---------------------------------------------------------------------------
ret_opt    <- sum(weights_opt * mu_annual)
sd_opt     <- sqrt(as.numeric(t(weights_opt) %*% cov_mat_annual %*% weights_opt))
sharpe_opt <- (ret_opt - risk_free_rate) / sd_opt

utility_opt <- ret_opt - (lambda / 2) * sd_opt^2

# Retornos mensuales históricos del portafolio
port_returns_monthly <- xts(
  as.numeric(as.matrix(lr_port) %*% weights_opt),
  order.by = index(lr_port)
)

# VaR paramétrico anual
var_parametric <- ret_opt + qnorm(0.05) * sd_opt

# CVaR: escalar por raíz de 12 (consistente con la volatilidad)
cvar_95_monthly <- mean(
  as.numeric(port_returns_monthly[
    as.numeric(port_returns_monthly) <=
      quantile(as.numeric(port_returns_monthly), 0.05, na.rm = TRUE)
  ]),
  na.rm = TRUE
)
cvar_95 <- cvar_95_monthly * sqrt(12)

# Tracking error anualizado
bm_aligned       <- benchmark_returns_full[index(lr_port)]
relative_monthly <- port_returns_monthly - as.numeric(bm_aligned)
tracking_error   <- sd(relative_monthly, na.rm = TRUE) * sqrt(12)
relative_var     <- mean(relative_monthly, na.rm = TRUE) * 12 -
  qnorm(0.95) * tracking_error

# Sortino anualizado
excess_monthly <- as.numeric(port_returns_monthly) - rf_monthly
downside       <- excess_monthly[excess_monthly < 0]
downside_dev   <- sqrt(mean(downside^2)) * sqrt(12)
sortino_opt    <- (ret_opt - risk_free_rate) / downside_dev

# ---------------------------------------------------------------------------
# PORTAFOLIO DE REFERENCIA: optimizador cuadrático (solo comparativo)
# ---------------------------------------------------------------------------
cat("⚙️  Calculando portafolio de referencia (optimizador) para comparación...\n\n")

assets <- valid_port
etf_commodity_idx <- which(assets %in% c(etf_tickers, commodity_tickers))
stock_idx         <- which(!assets %in% c(etf_tickers, commodity_tickers))

portf_ref <- portfolio.spec(assets = assets) %>%
  add.constraint(type = "box", min = 0, max = max_weight)

if (length(etf_commodity_idx) > 0 && length(stock_idx) > 0) {
  portf_ref <- portf_ref %>%
    add.constraint(type      = "group",
                   groups    = list(etf_commodity_idx, stock_idx),
                   group_min = c(0.05, 0.40),
                   group_max = c(0.50, 1.00))
}

portf_ref <- portf_ref %>%
  add.objective(type = "return", name = "mean") %>%
  add.objective(type = "risk",   name = "var", risk_aversion = lambda)

set.seed(seed)
opt_ref        <- optimize.portfolio(R = lr_port, portfolio = portf_ref,
                                     optimize_method = "random",
                                     search_size = n_sim, trace = FALSE)
weights_ref    <- extractWeights(opt_ref)
ret_ref        <- sum(weights_ref * mu_annual)
sd_ref         <- sqrt(as.numeric(t(weights_ref) %*% cov_mat_annual %*% weights_ref))
sharpe_ref     <- (ret_ref - risk_free_rate) / sd_ref
utility_ref    <- ret_ref - (lambda / 2) * sd_ref^2

cat(sprintf("  ✓ Portafolio propio   → Retorno: %.2f%% | Vol: %.2f%% | Sharpe: %.4f | Utilidad: %.6f\n",
            ret_opt * 100, sd_opt * 100, sharpe_opt, utility_opt))
cat(sprintf("  ✓ Portafolio óptimo   → Retorno: %.2f%% | Vol: %.2f%% | Sharpe: %.4f | Utilidad: %.6f\n\n",
            ret_ref * 100, sd_ref * 100, sharpe_ref, utility_ref))

# ============================================================================
# PASO 6: ANÁLISIS DE DRAWDOWN (retornos mensuales históricos)
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 6: ANÁLISIS DE DRAWDOWN HISTÓRICO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("📥 Usando retornos mensuales desde %s (%d obs)...\n\n",
            start_date, length(port_returns_monthly)))

port_cumret <- cumprod(1 + port_returns_monthly)
port_cummax <- cummax(port_cumret)
port_dd     <- (port_cumret - port_cummax) / port_cummax

mdd_hist <- min(port_dd, na.rm = TRUE)

dd_neg             <- as.numeric(port_dd[port_dd < 0])
escenario_p90      <- quantile(dd_neg, 0.10, na.rm = TRUE)
escenario_mediana  <- median(dd_neg,          na.rm = TRUE)
escenario_promedio <- mean(dd_neg,             na.rm = TRUE)
mdd_reciente       <- min(tail(port_dd, 12),   na.rm = TRUE)   # último año ≈ 12 meses

cat(sprintf("  ✓ Max Drawdown: %.2f%%\n",          mdd_hist        * 100))
cat(sprintf("  ✓ P90 Drawdown: %.2f%%\n",          escenario_p90   * 100))
cat(sprintf("  ✓ MDD reciente (1 año): %.2f%%\n\n", mdd_reciente   * 100))

# ============================================================================
# REPORTE FINAL
# ============================================================================
cat(sprintf("\n\n📈 MÉTRICAS (base anual | histórico: %s → %s)\n\n",
            start_date, end_date))

cat(sprintf("  ➤ Horizonte de datos: %.1f años (%d meses)\n",    n_years, n_obs))
cat(sprintf("  ➤ Lambda (aversión al riesgo): %.2f\n",            lambda))
cat(sprintf("  ➤ RF Anual (T-bills): %.4f%%\n",                   risk_free_rate * 100))

cat(sprintf("  ➤ Retorno Esperado Anual: %.4f%%\n",               ret_opt * 100))
cat("     ⚠️  Basado en retornos históricos 2020-2026 (bull market post-COVID).\n")
cat("         No es un estimado forward-looking. Puede estar sobreestimado.\n")

cat(sprintf("  ➤ Volatilidad Anual: %.4f%%\n",                    sd_opt * 100))
if (sd_opt > 0.30) {
  cat(sprintf("     ⚠️  Volatilidad alta (%.1fx la del SPY ~18%%).\n",
              sd_opt / 0.18))
  cat("         Portafolio concentrado en biotech/growth de alta dispersión.\n")
}

cat(sprintf("  ➤ Sharpe Ratio (anual): %.4f\n",                   sharpe_opt))
cat(sprintf("  ➤ Sortino Ratio (anual): %.4f\n",                  sortino_opt))
if (sortino_opt > sharpe_opt) {
  cat("     ✅ Sortino > Sharpe: la volatilidad es predominantemente al alza.\n")
}

cat(sprintf("  ➤ Utilidad Cuadrática (convicción): %.6f\n",       utility_opt))
cat(sprintf("  ➤ Utilidad Cuadrática (óptimo ref): %.6f\n",       utility_ref))
if (utility_opt >= utility_ref) {
  cat("     ✅ Utilidad propia >= óptimo: los pesos de convicción son eficientes.\n")
} else {
  cat(sprintf("     ℹ️  Diferencia vs óptimo: %.6f (%.2f%% menor utilidad).\n",
              utility_ref - utility_opt,
              (utility_ref - utility_opt) / abs(utility_ref) * 100))
}

cat(sprintf("  ➤ VaR (95%%, anual): %.4f%%\n",                    var_parametric * 100))
cat("     ℹ️  Pérdida máxima esperada en el 5%% peor de los años (dist. normal).\n")

cat(sprintf("  ➤ CVaR (95%%, anual): %.4f%%\n",                   cvar_95 * 100))
cat("     ℹ️  Promedio del 5%% peor de meses, escalado x√12.\n")
cat(sprintf("         Referencia: MDD histórico observado fue %.2f%%.\n",
            mdd_hist * 100))

cat(sprintf("  ➤ Tracking Error (anual): %.4f%%\n",               tracking_error * 100))
if (tracking_error > 0.30) {
  cat("     ⚠️  Comportamiento muy independiente al SPY.\n")
  cat("         Alta dispersión relativa esperada en cualquier dirección.\n")
}

cat(sprintf("  ➤ Relative VaR (95%%, anual): %.4f%%\n",           relative_var * 100))
cat(sprintf("     ℹ️  En el 5%% peor de escenarios relativos, el portafolio puede\n"))
cat(sprintf("         quedar %.2f pp por debajo del SPY en un año.\n",
            abs(relative_var) * 100))
