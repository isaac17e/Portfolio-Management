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

# === SOLUCIÓN DEFINITIVA: Eliminar plyr que causa conflictos ===
if ("plyr" %in% (.packages())) {
  detach("package:plyr", unload = TRUE, force = TRUE)
}
# ============================================================================
# PARÁMETROS DE ENTRADA (EDITABLES)
# ============================================================================

# 1. PORTAFOLIO ACTUAL
tickers <- c("ADI", "ELV", "TLT", "MSCI", "GD", "LHX", "LMT", "FAN", "ENTG", 
             "GOLD", "EW", "DGE.L", "NFLX", "ATO", "TJX", "PSA", "AEE", "RMD", "ISRG", 
             "STE", "FAST")

pesos <- c(0.101, 0.086, 0.081, 0.076, 0.072, 0.072, 0.072, 0.062, 0.05, 0.05, 
           0.042, 0.039, 0.03, 0.025, 0.023, 0.022, 0.02, 0.02, 0.017, 0.017, 0.015)

# 2. TICKERS A ELIMINAR (Editable)
tickers_eliminar <- c("NFLX", "ISRG")

# 3. HORIZONTE TEMPORAL
target_months <- c(2)  # Enero, Febrero (editable)
target_years <- 2015:2025     # Rango de años (editable)
horizon_months <- length(target_months)
horizon_label <- paste(month.abb[target_months], collapse = "-")

# 4. LAMBDA (Coeficiente de aversión al riesgo)
lambda <- 1

# 5. PARÁMETROS DE SELECCIÓN DE CANDIDATOS
n_candidates_nasdaq <- 250     # Número de tickers a tomar del NASDAQ
n_top_sp500 <- 200              # **NUEVO: Número de tickers a tomar del S&P 500**
min_observations <- 5
ideal_observations <- 8

volatility_percentile <- 0.40  # Moderado-estricto: 40% menos volátil
correlation_percentile <- 0.60 # Moderado: acepta más universo
correlation_order <- 0

# 6. PESOS DE SCORING
weight_sharpe <- 0.25          # Rendimiento empieza a importar
weight_low_vol <- 0.50         # Estabilidad sigue siendo prioritaria
weight_decorr <- 0.25          # Diversificación más valorada

# 7. RESTRICCIONES DE PORTAFOLIO
max_weight <- 0.12
rf_rate_period <- 0.0075
n_sim <- 5000
seed <- 123

# 8. CLASIFICACIÓN DE ACTIVOS (Opcional)
etf_tickers <- c("SPY", "GLD", "SLV", "TLT", "IEF", "VTI", "QQQ")
commodity_tickers <- c("GLD", "SLV", "GDX", "GDXJ")

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

safe_scrape_table <- function(url) {
  tryCatch({
    page <- read_html(url)
    tables <- page %>% html_table(fill = TRUE)
    if (length(tables) > 0) {
      return(tables[[1]])
    }
    return(NULL)
  }, error = function(e) {
    cat(sprintf("Error al obtener tabla de %s\n", url))
    return(NULL)
  })
}

descargar_datos_ticker <- function(ticker, fecha_inicio, fecha_fin) {
  tryCatch({
    datos <- tq_get(ticker, from = fecha_inicio, to = fecha_fin)
    if (!is.null(datos) && nrow(datos) > 30) {
      return(datos)
    }
    return(NULL)
  }, error = function(e) {
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

fecha_fin <- Sys.Date()
fecha_inicio_data <- as.Date(sprintf("%d-01-01", min(target_years)))
fecha_inicio_historico <- as.Date("2014-01-01")

cat(sprintf("📅 Horizonte: %d meses (%s)\n", horizon_months, horizon_label))
cat(sprintf("📅 Período: %d-%d\n", min(target_years), max(target_years)))
cat(sprintf("🎯 Lambda (aversión al riesgo): %.2f\n\n", lambda))

tickers_mantener <- setdiff(tickers, tickers_eliminar)
indices_mantener <- which(tickers %in% tickers_mantener)
pesos_mantener <- pesos[indices_mantener]

cat(sprintf("🗑️  Tickers a eliminar (%d): %s\n", 
            length(tickers_eliminar), paste(tickers_eliminar, collapse = ", ")))
cat(sprintf("✅ Tickers a mantener (%d): %s\n\n", 
            length(tickers_mantener), paste(tickers_mantener, collapse = ", ")))

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 1: OBTENCIÓN DE CANDIDATOS DEL NASDAQ Y S&P 500\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# === NASDAQ ===
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
  if ("Symbol" %in% names(nasdaq_tbl)) {
    candidatos_nasdaq <- nasdaq_tbl$Symbol
  } else if ("Ticker" %in% names(nasdaq_tbl)) {
    candidatos_nasdaq <- nasdaq_tbl$Ticker
  } else {
    candidatos_nasdaq <- nasdaq_tbl[[1]]
  }
}

candidatos_nasdaq <- setdiff(candidatos_nasdaq, tickers)
candidatos_nasdaq <- head(candidatos_nasdaq, n_candidates_nasdaq)

cat(sprintf("  ✓ Candidatos del NASDAQ: %d\n", length(candidatos_nasdaq)))

# === S&P 500 (NUEVO) ===
cat("📊 Obteniendo tickers del S&P 500...\n")
sp500_tbl <- safe_scrape_table("https://www.slickcharts.com/sp500")

if (is.null(sp500_tbl)) {
  cat("⚠️  No se pudo obtener la lista del S&P 500. Continuando solo con NASDAQ...\n")
  candidatos_sp500 <- character(0)
} else {
  # La tabla de SlickCharts tiene una columna 'Symbol' o similar
  if ("Symbol" %in% names(sp500_tbl)) {
    candidatos_sp500 <- sp500_tbl$Symbol
  } else if ("Ticker" %in% names(sp500_tbl)) {
    candidatos_sp500 <- sp500_tbl$Ticker
  } else {
    # Si no encuentra la columna esperada, intenta con la segunda columna
    # (la primera suele ser el ranking)
    candidatos_sp500 <- sp500_tbl[[2]]
  }
  
  # Remover tickers que ya están en el portafolio actual
  candidatos_sp500 <- setdiff(candidatos_sp500, tickers)
  # Tomar solo los primeros n_top_sp500
  candidatos_sp500 <- head(candidatos_sp500, n_top_sp500)
  
  cat(sprintf("  ✓ Candidatos del S&P 500: %d\n", length(candidatos_sp500)))
}

# === COMBINAR CANDIDATOS ===
# Combinar ambas listas eliminando duplicados
candidatos_combinados <- unique(c(candidatos_nasdaq, candidatos_sp500))

cat(sprintf("\n📊 Total de candidatos únicos: %d\n", length(candidatos_combinados)))
cat(sprintf("   • Del NASDAQ: %d\n", length(candidatos_nasdaq)))
cat(sprintf("   • Del S&P 500: %d\n", length(candidatos_sp500)))
cat(sprintf("   • Duplicados eliminados: %d\n\n", 
            length(candidatos_nasdaq) + length(candidatos_sp500) - length(candidatos_combinados)))

# Actualizar la variable para el resto del código
all_tickers <- unique(c(tickers_mantener, candidatos_combinados))

# ============================================================================
# PASO 2: DESCARGAR PRECIOS MENSUALES Y ACUMULAR POR HORIZONTE
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 2: DESCARGA Y PROCESAMIENTO DE DATOS\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("📥 Descargando datos de precios MENSUALES...\n")
df_prices_monthly <- tq_get(
  all_tickers,
  from = sprintf("%d-01-01", min(target_years)),
  to   = sprintf("%d-12-31", max(target_years) + 1)
) %>%
  group_by(symbol) %>%
  tq_transmute(
    adjusted,
    periodReturn,
    period = "monthly",
    col_rename = "monthly_return"
  ) %>%
  filter(!is.na(monthly_return))

cat(sprintf("\n🔄 Acumulando retornos para horizonte de %d mes(es) (%s)...\n", 
            horizon_months, horizon_label))

df_prices_accumulated <- df_prices_monthly %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  filter(month %in% target_months, year %in% target_years) %>%
  group_by(symbol, year) %>%
  dplyr::summarise(
    accumulated_return = prod(1 + monthly_return) - 1,
    n_months = n(),
    .groups = 'drop'
  ) %>%
  filter(n_months == horizon_months) %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", year, max(target_months)))
  )

cat(sprintf("  ✓ Períodos completos encontrados: %d años\n", 
            length(unique(df_prices_accumulated$year))))
cat(sprintf("  ✓ Total observaciones (ticker-año): %d\n", nrow(df_prices_accumulated)))

df_prices <- df_prices_accumulated %>%
  select(symbol, date, monthly_return = accumulated_return)

# === BENCHMARK (SPY) ===
cat("\n📥 Procesando benchmark (SPY) con horizonte multi-período...\n")
benchmark_prices_monthly <- tq_get(
  "SPY",
  from = sprintf("%d-01-01", min(target_years)),
  to   = sprintf("%d-12-31", max(target_years) + 1)
) %>%
  tq_transmute(
    adjusted,
    periodReturn,
    period = "monthly",
    col_rename = "benchmark_return"
  ) %>%
  filter(!is.na(benchmark_return))

benchmark_prices_accumulated <- benchmark_prices_monthly %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  filter(month %in% target_months) %>%
  group_by(year) %>%
  dplyr::summarise(
    accumulated_return = prod(1 + benchmark_return) - 1,
    n_months = n(),
    .groups = 'drop'
  ) %>%
  filter(n_months == horizon_months) %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", year, max(target_months)))
  )

benchmark_prices <- benchmark_prices_accumulated %>%
  select(date, benchmark_return = accumulated_return)

# ============================================================================
# PASO 3: ESTADÍSTICAS DESCRIPTIVAS
# ============================================================================

cat(sprintf("\n📊 Calculando estadísticas para horizonte de %d mes(es)...\n", horizon_months))

summary_stats <- df_prices %>%
  group_by(symbol) %>%
  dplyr::summarise(
    mean_return = mean(monthly_return, na.rm = TRUE),
    sd_return   = sd(monthly_return, na.rm = TRUE),
    n_obs       = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= min_observations, sd_return > 0) %>%
  mutate(
    sharpe_ratio = (mean_return - rf_rate_period) / sd_return,
    data_quality_penalty = pmin(n_obs / ideal_observations, 1.0)
  )

cat(sprintf("\n=== 📊 DIAGNÓSTICO DE COBERTURA DE DATOS (%d meses: %s) ===\n", 
            horizon_months, horizon_label))

if (nrow(summary_stats) > 0) {
  coverage_summary <- summary_stats %>%
    summarise(
      tickers_total = n(),
      obs_min = min(n_obs),
      obs_q25 = quantile(n_obs, 0.25),
      obs_median = median(n_obs),
      obs_q75 = quantile(n_obs, 0.75),
      obs_max = max(n_obs),
      pct_ideal = mean(n_obs >= ideal_observations) * 100,
      pct_acceptable = mean(n_obs >= min_observations) * 100
    )
  
  cat(sprintf("  Tickers totales: %d\n", coverage_summary$tickers_total))
  cat(sprintf("  Observaciones - Min: %d | Q25: %.0f | Mediana: %.0f | Q75: %.0f | Max: %d\n",
              coverage_summary$obs_min, coverage_summary$obs_q25, 
              coverage_summary$obs_median, coverage_summary$obs_q75, 
              coverage_summary$obs_max))
  cat(sprintf("  Tickers con ≥%d obs (ideal): %.1f%%\n", 
              ideal_observations, coverage_summary$pct_ideal))
  cat(sprintf("  Tickers con ≥%d obs (mínimo): %.1f%%\n\n", 
              min_observations, coverage_summary$pct_acceptable))
} else {
  cat("  ⚠️ No hay tickers con datos suficientes\n")
  stop("❌ No se puede continuar sin datos válidos")
}

# === CORRELACIONES ===
cat("📊 Calculando matriz de correlaciones...\n")
corr_pairs <- df_prices %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  select(-date) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.table() %>%
  as.data.frame() %>%
  filter(as.character(Var1) < as.character(Var2))

avg_cor_by_ticker <- corr_pairs %>%
  group_by(Var1) %>%
  summarise(avg_cor = mean(abs(Freq), na.rm = TRUE)) %>%
  rename(symbol = Var1)

# === FAMA-FRENCH ===
cat("📥 Descargando datos Fama-French...\n")
ff_data <- tryCatch({
  ff_raw <- download_french_data("Fama/French 3 Factors")$subsets$data[[1]] %>%
    mutate(date = as.Date(ym(date))) %>%
    filter(year(date) %in% target_years, month(date) %in% target_months) %>%
    mutate(across(c(`Mkt-RF`, SMB, HML), ~ . / 100))
  
  ff_accumulated <- ff_raw %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    dplyr::summarise(
      `Mkt-RF` = prod(1 + `Mkt-RF`) - 1,
      SMB = prod(1 + SMB) - 1,
      HML = prod(1 + HML) - 1,
      n_months = n(),
      .groups = 'drop'
    ) %>%
    filter(n_months == horizon_months) %>%
    mutate(date = as.Date(sprintf("%d-%02d-01", year, max(target_months))))
  
  xts(ff_accumulated[, c("Mkt-RF", "SMB", "HML")], order.by = ff_accumulated$date)
}, error = function(e) {
  cat("⚠️ Error con Fama-French. Continuando sin ellos.\n")
  return(NULL)
})

# === BETAS FF3 ===
if (!is.null(ff_data)) {
  cat("📊 Calculando betas Fama-French...\n")
  
  returns_merged <- df_prices %>%
    left_join(benchmark_prices, by = "date") %>%
    mutate(excess_return = monthly_return - rf_rate_period) %>%
    filter(date %in% index(ff_data))
  
  if (nrow(returns_merged) > 0) {
    ff_stats <- returns_merged %>%
      group_by(symbol) %>%
      filter(n() >= 3) %>%
      do({
        tryCatch({
          model <- lm(excess_return ~ ff_data$`Mkt-RF` + ff_data$SMB + ff_data$HML, data = .)
          data.frame(
            symbol = unique(.$symbol),
            beta_mkt = coef(model)[2],
            beta_smb = coef(model)[3],
            beta_hml = coef(model)[4]
          )
        }, error = function(e) {
          data.frame(
            symbol = unique(.$symbol),
            beta_mkt = NA_real_,
            beta_smb = NA_real_,
            beta_hml = NA_real_
          )
        })
      }) %>%
      ungroup()
    
    market_premium <- mean(ff_data$`Mkt-RF`, na.rm = TRUE)
    smb_premium <- mean(ff_data$SMB, na.rm = TRUE)
    hml_premium <- mean(ff_data$HML, na.rm = TRUE)
    
    ff_stats <- ff_stats %>%
      mutate(ff_expected_return = rf_rate_period +
               beta_mkt * market_premium +
               beta_smb * smb_premium +
               beta_hml * hml_premium)
    
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

# === COMBINAR MÉTRICAS ===
combined_stats <- summary_stats %>%
  left_join(avg_cor_by_ticker, by = "symbol") %>%
  left_join(ff_stats %>% select(symbol, beta_mkt, beta_smb, beta_hml, ff_expected_return), 
            by = "symbol") %>%
  mutate(
    avg_cor = coalesce(avg_cor, median(avg_cor, na.rm = TRUE)),
    ff_expected_return = coalesce(ff_expected_return, mean_return),
    adjusted_return = ifelse(!is.na(ff_expected_return), 
                             (mean_return + ff_expected_return) / 2, 
                             mean_return),
    sharpe_ratio_adjusted = (adjusted_return - rf_rate_period) / sd_return,
    is_etf_commodity = symbol %in% c(etf_tickers, commodity_tickers)
  )

cat(sprintf("✅ combined_stats creado: %d activos\n\n", nrow(combined_stats)))

# ============================================================================
# PASO 4: SELECCIÓN DE CANDIDATOS (SOLO PARA REEMPLAZOS)
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 3: SELECCIÓN DE CANDIDATOS PARA REEMPLAZO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("🎯 Seleccionando candidatos - PRIORIDAD: Menor SD en %d mes(es) (%s)\n", 
            horizon_months, horizon_label))
cat(sprintf("  🔧 Modo de correlación: %s\n", 
            ifelse(correlation_order == 1, 
                   "Mayor a menor (alta correlación preferida)", 
                   "Menor a mayor (baja correlación preferida)")))

combined_stats_candidatos <- combined_stats %>%
  filter(!symbol %in% tickers_mantener)

cat(sprintf("  ℹ️  Universo de candidatos para evaluación: %d tickers\n\n", 
            nrow(combined_stats_candidatos)))

select_optimal_candidates <- function(df, n_candidates) {
  df_filtered <- df %>%
    dplyr::filter(
      n_obs >= min_observations,
      sd_return > 0,
      !is.na(sharpe_ratio_adjusted),
      is.finite(sharpe_ratio_adjusted)
    )
  
  cat(sprintf("  ✓ Activos con datos válidos (≥%d obs): %d\n", 
              min_observations, nrow(df_filtered)))
  
  sd_threshold <- quantile(df_filtered$sd_return, volatility_percentile, na.rm = TRUE)
  df_low_vol <- df_filtered %>%
    dplyr::filter(sd_return <= sd_threshold)
  
  cat(sprintf("  ✓ Activos con SD <= %.4f (P%.0f): %d\n", 
              sd_threshold, volatility_percentile*100, nrow(df_low_vol)))
  
  cor_threshold <- quantile(df_filtered$avg_cor, correlation_percentile, na.rm = TRUE)
  
  if (correlation_order == 1) {
    df_decorr <- df_filtered %>%
      dplyr::filter(avg_cor >= cor_threshold)
    cat(sprintf("  ✓ Activos con Corr >= %.3f (P%.0f - más correlacionados): %d\n", 
                cor_threshold, correlation_percentile*100, nrow(df_decorr)))
  } else {
    df_decorr <- df_filtered %>%
      dplyr::filter(avg_cor <= cor_threshold)
    cat(sprintf("  ✓ Activos con Corr <= %.3f (P%.0f - menos correlacionados): %d\n", 
                cor_threshold, correlation_percentile*100, nrow(df_decorr)))
  }
  
  if (correlation_order == 1) {
    df_candidates <- df_filtered %>%
      dplyr::filter(
        sd_return <= sd_threshold,
        avg_cor >= cor_threshold
      )
  } else {
    df_candidates <- df_filtered %>%
      dplyr::filter(
        sd_return <= sd_threshold,
        avg_cor <= cor_threshold
      )
  }
  
  cat(sprintf("  ✓ Activos que cumplen ambos criterios: %d\n", nrow(df_candidates)))
  
  if (nrow(df_candidates) < n_candidates * 0.5) {
    cat("  ⚠️ Pocos candidatos. Relajando filtros...\n")
    
    sd_threshold <- quantile(df_filtered$sd_return, 0.75, na.rm = TRUE)
    cor_threshold <- quantile(df_filtered$avg_cor, 0.80, na.rm = TRUE)
    
    if (correlation_order == 1) {
      df_candidates <- df_filtered %>%
        dplyr::filter(
          sd_return <= sd_threshold,
          avg_cor >= cor_threshold
        )
    } else {
      df_candidates <- df_filtered %>%
        dplyr::filter(
          sd_return <= sd_threshold,
          avg_cor <= cor_threshold
        )
    }
    
    cat(sprintf("  ✓ Con filtros relajados: %d candidatos\n", nrow(df_candidates)))
  }
  
  df_candidates <- df_candidates %>%
    dplyr::mutate(
      sharpe_norm = (sharpe_ratio_adjusted - min(sharpe_ratio_adjusted, na.rm = TRUE)) / 
        (max(sharpe_ratio_adjusted, na.rm = TRUE) - min(sharpe_ratio_adjusted, na.rm = TRUE)),
      vol_norm = 1 - (sd_return - min(sd_return, na.rm = TRUE)) / 
        (max(sd_return, na.rm = TRUE) - min(sd_return, na.rm = TRUE))
    )
  
  if (correlation_order == 1) {
    df_candidates <- df_candidates %>%
      dplyr::mutate(
        corr_norm = (avg_cor - min(avg_cor, na.rm = TRUE)) / 
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      )
  } else {
    df_candidates <- df_candidates %>%
      dplyr::mutate(
        corr_norm = 1 - (avg_cor - min(avg_cor, na.rm = TRUE)) / 
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      )
  }
  
  df_candidates <- df_candidates %>%
    dplyr::mutate(
      sharpe_norm = ifelse(is.nan(sharpe_norm), 0.5, sharpe_norm),
      vol_norm = ifelse(is.nan(vol_norm), 0.5, vol_norm),
      corr_norm = ifelse(is.nan(corr_norm), 0.5, corr_norm)
    )
  
  df_candidates <- df_candidates %>%
    dplyr::mutate(
      composite_score_raw = 
        weight_sharpe * sharpe_norm +
        weight_low_vol * vol_norm +
        weight_decorr * corr_norm,
      composite_score = composite_score_raw * data_quality_penalty
    ) %>%
    dplyr::arrange(desc(composite_score))
  
  n_to_select <- min(n_candidates, nrow(df_candidates))
  selected <- df_candidates$symbol[1:n_to_select]
  
  n_top_show <- min(10, nrow(df_candidates))
  top_selected <- df_candidates[1:n_top_show, ] %>%
    dplyr::select(symbol, sharpe_ratio_adjusted, sd_return, avg_cor, 
                  n_obs, data_quality_penalty, composite_score)
  
  cat("\n  📋 Top 10 candidatos seleccionados:\n")
  print(top_selected, n = 10)
  
  return(list(selected = selected, stats = df_candidates))
}

n_reemplazos <- length(tickers_eliminar)
resultado_seleccion <- select_optimal_candidates(combined_stats_candidatos, n_reemplazos)
ticker_candidatos_seleccionados <- resultado_seleccion$selected

cat(sprintf("\n✅ Candidatos seleccionados para reemplazo: %d\n", 
            length(ticker_candidatos_seleccionados)))
cat(sprintf("   %s\n\n", paste(ticker_candidatos_seleccionados, collapse = ", ")))

# ============================================================================
# PASO 5: CONSTRUCCIÓN DEL NUEVO PORTAFOLIO
# ============================================================================

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 4: OPTIMIZACIÓN DEL NUEVO PORTAFOLIO\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

nuevo_portafolio <- c(tickers_mantener, ticker_candidatos_seleccionados)

cat(sprintf("📊 Portafolio completo para optimización: %d activos\n", length(nuevo_portafolio)))
cat(sprintf("   • Mantenidos: %d\n", length(tickers_mantener)))
cat(sprintf("   • Nuevos: %d\n\n", length(ticker_candidatos_seleccionados)))

df_xts <- df_prices %>%
  filter(symbol %in% nuevo_portafolio) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date) %>%
  drop_na()

dates <- df_xts$date
df_xts_matrix <- as.matrix(df_xts[,-1])
assets <- colnames(df_xts_matrix)
df_xts <- xts(df_xts_matrix, order.by = dates)

cat(sprintf("✅ Matriz de retornos: %d observaciones × %d activos\n", nrow(df_xts), ncol(df_xts)))

mu <- colMeans(df_xts[, assets], na.rm = TRUE)
cov_mat <- cov(df_xts[, assets], use = "pairwise.complete.obs")

if (any(!is.finite(mu))) {
  mu[!is.finite(mu)] <- 0
}

if (any(!is.finite(cov_mat))) {
  cov_mat[!is.finite(cov_mat)] <- 0
}

cat("\n⚙️ Configurando portafolio con utilidad cuadrática...\n")

etf_commodity_assets <- which(assets %in% c(etf_tickers, commodity_tickers))
stock_assets <- which(!assets %in% c(etf_tickers, commodity_tickers))

portf <- portfolio.spec(assets = assets) %>%
  add.constraint(type = "box", min = 0, max = max_weight)

if (length(etf_commodity_assets) > 0 && length(stock_assets) > 0) {
  portf <- portf %>%
    add.constraint(type = "group", 
                   groups = list(etf_commodity_assets, stock_assets),
                   group_min = c(0.05, 0.40),
                   group_max = c(0.50, 1.00))
}

portf <- portf %>%
  add.objective(type = "return", name = "mean") %>%
  add.objective(type = "risk", name = "var", risk_aversion = lambda)

cat(sprintf("🚀 Ejecutando optimización con λ=%.2f y %d simulaciones...\n", lambda, n_sim))
set.seed(seed)
opt <- optimize.portfolio(
  R = df_xts[, assets],
  portfolio = portf,
  optimize_method = "random",
  search_size = n_sim,
  trace = FALSE
)

weights_opt <- extractWeights(opt)
ret_opt <- sum(weights_opt * mu)
sd_opt <- sqrt(t(weights_opt) %*% cov_mat %*% weights_opt)
sharpe_opt <- (ret_opt - rf_rate_period) / sd_opt
utility_opt <- ret_opt - (lambda / 2) * (sd_opt^2)

portfolio_returns_full <- Return.portfolio(df_xts[, assets], weights = weights_opt)
var_parametric <- ret_opt - qnorm(0.95) * sd_opt
cvar_95 <- mean(portfolio_returns_full[portfolio_returns_full <= quantile(portfolio_returns_full, 0.05, na.rm = TRUE)], na.rm = TRUE)

benchmark_xts <- xts(benchmark_prices$benchmark_return, order.by = benchmark_prices$date)
df_xts_with_bench <- merge(df_xts[, assets], benchmark = benchmark_xts, all = FALSE) %>% na.omit()

ticker_cols_in_bench <- setdiff(colnames(df_xts_with_bench), "benchmark")
benchmark_aligned <- df_xts_with_bench$benchmark
portfolio_returns_aligned <- Return.portfolio(df_xts_with_bench[, ticker_cols_in_bench], weights = weights_opt)

tracking_error <- StdDev(portfolio_returns_aligned - benchmark_aligned)
relative_returns <- portfolio_returns_aligned - benchmark_aligned
relative_var <- mean(relative_returns, na.rm = TRUE) - qnorm(0.95) * sd(relative_returns, na.rm = TRUE)

# ============================================================================
# PASO 6: ANÁLISIS HISTÓRICO DE DRAWDOWN (2014-presente)
# ============================================================================

cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat("  PASO 5: ANÁLISIS HISTÓRICO DE DRAWDOWN\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("📥 Descargando datos históricos (2014-presente)...\n")

df_hist_monthly <- tq_get(
  assets,
  from = fecha_inicio_historico,
  to = Sys.Date()
) %>%
  group_by(symbol) %>%
  tq_transmute(
    adjusted,
    periodReturn,
    period = "monthly",
    col_rename = "monthly_return"
  ) %>%
  filter(!is.na(monthly_return))

df_hist_xts <- df_hist_monthly %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date) %>%
  drop_na()

dates_hist <- df_hist_xts$date
df_hist_matrix <- as.matrix(df_hist_xts[,-1])
assets_hist <- colnames(df_hist_matrix)
df_hist_xts <- xts(df_hist_matrix, order.by = dates_hist)

portfolio_returns_hist <- Return.portfolio(df_hist_xts[, assets_hist], weights = weights_opt)

portfolio_cumret <- cumprod(1 + portfolio_returns_hist)
portfolio_cummax <- cummax(portfolio_cumret)
portfolio_dd <- (portfolio_cumret - portfolio_cummax) / portfolio_cummax

mdd_hist <- min(portfolio_dd, na.rm = TRUE)

dd_valores <- as.numeric(portfolio_dd[portfolio_dd < 0])
escenario_p90 <- quantile(dd_valores, 0.10, na.rm = TRUE)
escenario_mediana <- median(dd_valores, na.rm = TRUE)
escenario_promedio <- mean(dd_valores, na.rm = TRUE)
mdd_reciente <- min(tail(portfolio_dd, 12), na.rm = TRUE)

cat(sprintf("✅ Drawdown histórico calculado: %d meses de datos\n\n", length(portfolio_dd)))

# ============================================================================
# REPORTE FINAL
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("                    REPORTE DE OPTIMIZACIÓN FINAL                      \n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

pesos_df <- tibble(symbol = names(weights_opt), weight = weights_opt) %>%
  filter(weight > 0.001) %>%
  arrange(desc(weight)) %>%
  mutate(
    status = ifelse(symbol %in% tickers_mantener, "Mantenido", "NUEVO")
  )

cat("📊 PORTAFOLIO OPTIMIZADO\n\n")
print(kable(pesos_df %>% 
              mutate(weight = sprintf("%.2f%%", weight * 100)),
            col.names = c("Ticker", "Peso", "Status"),
            align = c("l", "r", "c")))

cat(sprintf("\n\n📈 MÉTRICAS DEL PERÍODO (%s: %s %d-%d)\n\n", 
            horizon_label, paste(month.name[target_months], collapse = ", "), 
            min(target_years), max(target_years)))
cat(sprintf("  ➤ Horizonte: %d mes(es)\n", horizon_months))
cat(sprintf("  ➤ Lambda (aversión al riesgo): %.2f\n", lambda))
cat(sprintf("  ➤ Retorno Esperado: %.4f%%\n", ret_opt * 100))
cat(sprintf("  ➤ Volatilidad: %.4f%%\n", sd_opt * 100))
cat(sprintf("  ➤ Sharpe Ratio: %.4f\n", sharpe_opt))
cat(sprintf("  ➤ Utilidad Cuadrática: %.6f\n", utility_opt))
cat(sprintf("  ➤ VaR (95%%): %.4f%%\n", var_parametric * 100))
cat(sprintf("  ➤ CVaR (95%%): %.4f%%\n", cvar_95 * 100))
cat(sprintf("  ➤ Tracking Error: %.4f%%\n", tracking_error * 100))
cat(sprintf("  ➤ Relative VaR (95%%): %.4f%%\n", relative_var * 100))

annualization_factor <- 12 / horizon_months
cat(sprintf("\n📅 MÉTRICAS ANUALIZADAS (aproximadas)\n\n"))
cat(sprintf("  ➤ Retorno anual: %.2f%%\n", ret_opt * annualization_factor * 100))
cat(sprintf("  ➤ Volatilidad anual: %.2f%%\n", sd_opt * sqrt(annualization_factor) * 100))

cat(sprintf("\n\n📉 ANÁLISIS DE DRAWDOWN HISTÓRICO (2014-presente)\n\n"))
cat(sprintf("  ➤ Max Drawdown Histórico: %.2f%%\n", mdd_hist * 100))
cat(sprintf("  ➤ Escenario Conservador (P90): %.2f%%\n", escenario_p90 * 100))
cat(sprintf("  ➤ Escenario Típico (Mediana): %.2f%%\n", escenario_mediana * 100))
cat(sprintf("  ➤ Promedio: %.2f%%\n", escenario_promedio * 100))
cat(sprintf("  ➤ Mejor escenario histórico: 0.00%%\n"))
cat(sprintf("  ➤ MDD Más Reciente (1 año): %.2f%%\n", mdd_reciente * 100))

cat(sprintf("\n\n🔄 RESUMEN DE CAMBIOS\n\n"))
cat(sprintf("Tickers Eliminados: %d\n", length(tickers_eliminar)))
for (ticker in tickers_eliminar) {
  cat(sprintf("  ❌ %s\n", ticker))
}

cat(sprintf("\nTickers Agregados: %d\n", length(ticker_candidatos_seleccionados)))
stats_nuevos <- resultado_seleccion$stats %>%
  filter(symbol %in% ticker_candidatos_seleccionados) %>%
  select(symbol, sharpe_ratio_adjusted, sd_return, composite_score)

for (i in 1:nrow(stats_nuevos)) {
  cat(sprintf("  ✅ %s (Sharpe: %.4f, SD: %.4f, Score: %.4f)\n", 
              stats_nuevos$symbol[i],
              stats_nuevos$sharpe_ratio_adjusted[i],
              stats_nuevos$sd_return[i],
              stats_nuevos$composite_score[i]))
}

cat(sprintf("\nTotal Activos en Portafolio: %d\n", sum(pesos_df$weight > 0.001)))
cat(sprintf("Suma de Pesos: %.2f%%\n", sum(pesos_df$weight) * 100))

cat("\n═══════════════════════════════════════════════════════════════════════\n")
cat(sprintf("Reporte generado: %s\n", Sys.time()))
cat("═══════════════════════════════════════════════════════════════════════\n")