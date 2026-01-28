# ===============================================
# Script: Optimización de portafolio - Utilidad Cuadrática
# Autor: Isaac Echeverri
# Modificación: Horizonte temporal flexible + utilidad cuadrática
# ===============================================
rm(list = ls())

library(tidyverse)
library(tidyquant)
library(rvest)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(xts)
library(ROI)
library(ROI.plugin.glpk)
library(frenchdata)
library(janitor)
library(quantmod)
library(lubridate)
library(dplyr)

# === PARÁMETROS CONFIGURABLES ===

n_top_nasdaq <- 350
n_top_sp500 <- 350
n_top_int <- 50  
target_months <- c(02)
target_years <- 2014:2025
mdd_start_year <- 2014  
rf_rate <- 0.095  
seed <- 123
max_weight <- 0.20
n_sim <- 5000
target_total_tickers <- 800

# Risk Profile
lambda <- 3  
# Pesos de selección
weight_sharpe <- 0.50          # Rendimiento es REY
weight_low_vol <- 0.25         # Volatilidad secundaria
weight_decorr <- 0.25 # Diversificación táctica

n_divers_candidates <- 45
volatility_percentile <- 0.75  # Muy permisivo: acepta alta volatilidad
correlation_percentile <- 0.70 # Muy permisivo: universo amplio

correlation_order <- 0

min_observations <- 10
ideal_observations <- 15

# === VALIDACIÓN DE PARÁMETROS ===
if (length(target_months) < 1 || length(target_months) > 3) {
  stop("❌ Error: target_months debe contener 1, 2 o 3 meses")
}

if (any(target_months < 1) || any(target_months > 12)) {
  stop("❌ Error: Los meses deben estar entre 1 y 12")
}

horizon_months <- length(target_months)
horizon_label <- paste(month.abb[target_months], collapse = "-")

cat("\n", rep("=", 60), "\n", sep = "")
cat("🎯 HORIZONTE TEMPORAL: ", horizon_months, " mes(es) - ", horizon_label, "\n", sep = "")
cat(rep("=", 60), "\n\n", sep = "")

rf_rate_period <- (rf_rate / 12) * horizon_months

# === FUNCIÓN AUXILIAR: Web scraping seguro ===
safe_scrape_table <- function(url, fallback = NULL) {
  tryCatch({
    read_html(url) %>%
      html_node("table") %>%
      html_table(fill = TRUE)
  }, error = function(e) {
    cat(sprintf("⚠️ Error al obtener datos de %s\n", url))
    return(fallback)
  })
}

# === OBTENER TICKERS: S&P 500 ===
cat("📊 Obteniendo tickers del S&P 500...\n")
sp500_tbl <- safe_scrape_table("https://www.slickcharts.com/sp500")

if (!is.null(sp500_tbl)) {
  sp500_tickers <- sp500_tbl %>%
    dplyr::slice(1:min(n_top_sp500, nrow(.))) %>%
    dplyr::pull(Symbol) %>%
    str_replace_all("\\.", "-") %>%
    toupper() %>%
    unique()
  cat(sprintf("✅ S&P 500: %d tickers objetivo, %d únicos obtenidos\n", n_top_sp500, length(sp500_tickers)))
} else {
  sp500_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "TSLA", 
                     "BRK-B", "UNH", "JNJ", "V", "XOM", "WMT", "JPM", "PG")
  cat("⚠️ Usando S&P 500 fallback\n")
}

# === OBTENER TICKERS: NASDAQ ===
cat("\n📊 Obteniendo tickers del NASDAQ...\n")
nasdaq_tbl <- safe_scrape_table("https://stockanalysis.com/list/nasdaq-stocks/")

if (!is.null(nasdaq_tbl)) {
  nasdaq_tbl_clean <- nasdaq_tbl %>%
    janitor::clean_names() %>%
    dplyr::filter(
      !is.na(symbol),
      !grepl("\\^|\\$", symbol),
      nchar(symbol) >= 1,
      nchar(symbol) <= 5,
      !grepl("^[0-9]", symbol)
    ) %>%
    dplyr::mutate(symbol = toupper(str_replace_all(symbol, "\\.", "-")))
  
  nasdaq_tickers <- nasdaq_tbl_clean %>%
    dplyr::slice(1:min(n_top_nasdaq, nrow(.))) %>%
    dplyr::pull(symbol) %>%
    unique()
  
  cat(sprintf("✅ NASDAQ: %d tickers objetivo, %d únicos obtenidos\n", n_top_nasdaq, length(nasdaq_tickers)))
  
  shortage_nasdaq <- n_top_nasdaq - length(nasdaq_tickers)
  if (shortage_nasdaq > 0 && nrow(nasdaq_tbl_clean) > n_top_nasdaq) {
    cat(sprintf("  🔄 Compensando %d tickers faltantes del NASDAQ...\n", shortage_nasdaq))
    
    additional_needed <- min(shortage_nasdaq * 2, nrow(nasdaq_tbl_clean) - n_top_nasdaq)
    
    additional_nasdaq <- nasdaq_tbl_clean %>%
      dplyr::slice((n_top_nasdaq + 1):min((n_top_nasdaq + additional_needed), nrow(.))) %>%
      dplyr::pull(symbol) %>%
      unique()
    
    additional_nasdaq_unique <- setdiff(additional_nasdaq, nasdaq_tickers)
    
    if (length(additional_nasdaq_unique) > 0) {
      to_add <- additional_nasdaq_unique[1:min(shortage_nasdaq, length(additional_nasdaq_unique))]
      nasdaq_tickers <- c(nasdaq_tickers, to_add)
      cat(sprintf("  ✅ Añadidos %d tickers compensatorios del NASDAQ\n", length(to_add)))
    }
  }
  
} else {
  nasdaq_tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "TSLA", 
                      "AVGO", "ASML", "COST", "NFLX", "AMD", "PEP", "ADBE", "CSCO")
  cat("⚠️ Usando NASDAQ fallback\n")
}

# === ETFs ===
etf_tickers <- c("SPY", "QQQ", "GLD", "USO", "TLT", "VWO", "XLF", "XLE", "XLK", "XLU", 
                 "IWM", "EEM", "HYG", "VNQ", "SLV", "PDBC", "XLV", "XLI", "XLP", "XLRE", 
                 "XLY", "VOO", "VTI", "VYM", "ARKG", "ARKW", "ARKF", "ICLN", "TAN", "FAN", 
                 "LIT", "SMH", "SOXX", "CIBR", "BUG", "BOTZ", "ROBO", "GDXJ", "GDX")

# === COMMODITIES ===
commodity_tickers <- c("SLV", "UNG")

# === TICKERS INTERNACIONALES ===
international_tickers_full <- c(
  "RY.TO", "SHOP.TO", "TD.TO", "BN.TO", "ENB.TO", "TRI.TO", "BNS.TO", 
  "CP.TO", "CNQ.TO", "AEM.TO", "SU.TO", "TRP.TO", "WCN.TO", "FNV.TO", 
  "SAP.TO", "SIE.DE", "DTE.DE", "ALV.DE", "MBG.DE", "IFX.DE", "BMW.DE", 
  "DB1.DE", "DHL.DE", "DBK.DE", "MUV2.DE", "AZN.L", "HSBC", "ULVR.L", 
  "BP", "GSK.L", "RIO.L", "BATS.L", "GLEN.L", "DGE.L", "NG.L", "MC.PA", 
  "TTE.PA", "SAN.PA", "OR.PA", "SU.PA", "AI.PA", "BNP.PA", "RMS.PA", 
  "CS.PA", "SAF.PA", "CAP.PA", "ITX.MC", "IBE.MC", "BBVA.MC", "SAN.MC", 
  "7203.T", "6758.T", "6861.T", "8306.T", "9984.T", "6367.T", "6098.T", 
  "4063.T", "7974.T", "9432.T", "6501.T", "7267.T", "8316.T", "4568.T", 
  "6902.T", "4502.T", "8031.T"
)

international_tickers <- head(international_tickers_full, n_top_int)

# === COMBINAR Y LIMPIAR ===
cat("\n📊 Combinando y limpiando tickers...\n")

sp500_tickers_clean <- unique(toupper(sp500_tickers))
nasdaq_tickers_clean <- unique(toupper(nasdaq_tickers))
etf_tickers_clean <- unique(toupper(etf_tickers))
commodity_tickers_clean <- unique(toupper(commodity_tickers))
international_tickers_clean <- unique(toupper(international_tickers))

cat("\n📋 Tickers disponibles por fuente:\n")
cat(sprintf("  • S&P 500: %d\n", length(sp500_tickers_clean)))
cat(sprintf("  • NASDAQ: %d\n", length(nasdaq_tickers_clean)))
cat(sprintf("  • ETFs: %d\n", length(etf_tickers_clean)))
cat(sprintf("  • Commodities: %d\n", length(commodity_tickers_clean)))
cat(sprintf("  • Internacional: %d (de %d disponibles)\n", 
            length(international_tickers_clean), 
            length(international_tickers_full)))

all_tickers_raw <- c(
  sp500_tickers_clean,
  nasdaq_tickers_clean,
  etf_tickers_clean,
  commodity_tickers_clean,
  international_tickers_clean
)

cat(sprintf("\n  ✓ Total bruto (con duplicados): %d\n", length(all_tickers_raw)))

all_tickers <- unique(all_tickers_raw)

duplicates_removed <- length(all_tickers_raw) - length(all_tickers)
cat(sprintf("  ✓ Duplicados eliminados: %d\n", duplicates_removed))
cat(sprintf("  ✓ Total único después de combinación: %d\n", length(all_tickers)))

all_tickers_before_clean <- length(all_tickers)
all_tickers <- all_tickers[
  !grepl("\\^|\\$", all_tickers) &
    nchar(all_tickers) >= 1 & 
    nchar(all_tickers) <= 5 &
    !grepl("^[0-9]", all_tickers) &
    all_tickers != ""
]

cleaned_out <- all_tickers_before_clean - length(all_tickers)
if (cleaned_out > 0) {
  cat(sprintf("  ✓ Tickers eliminados por limpieza adicional: %d\n", cleaned_out))
}

if (length(all_tickers) < target_total_tickers) {
  shortage <- target_total_tickers - length(all_tickers)
  cat(sprintf("\n  ⚠️ Faltan %d tickers para alcanzar target de %d\n", shortage, target_total_tickers))
  
  if (length(international_tickers_full) > n_top_int) {
    cat("  🔄 Compensando con más tickers internacionales...\n")
    
    additional_int_available <- international_tickers_full[(n_top_int + 1):length(international_tickers_full)]
    additional_int_available_clean <- unique(toupper(additional_int_available))
    additional_int_unique <- setdiff(additional_int_available_clean, all_tickers)
    
    if (length(additional_int_unique) > 0) {
      to_add_int <- additional_int_unique[1:min(shortage, length(additional_int_unique))]
      all_tickers <- c(all_tickers, to_add_int)
      shortage <- shortage - length(to_add_int)
      cat(sprintf("  ✅ Añadidos %d tickers internacionales adicionales\n", length(to_add_int)))
    }
  }
  
  if (shortage > 0 && !is.null(nasdaq_tbl_clean) && nrow(nasdaq_tbl_clean) > length(nasdaq_tickers)) {
    cat("  🔄 Compensando con más tickers del NASDAQ...\n")
    
    additional_nasdaq_available <- nasdaq_tbl_clean %>%
      dplyr::slice((length(nasdaq_tickers) + 1):nrow(.)) %>%
      dplyr::pull(symbol) %>%
      unique()
    
    additional_nasdaq_unique <- setdiff(additional_nasdaq_available, all_tickers)
    
    if (length(additional_nasdaq_unique) > 0) {
      to_add_nasdaq <- additional_nasdaq_unique[1:min(shortage, length(additional_nasdaq_unique))]
      all_tickers <- c(all_tickers, to_add_nasdaq)
      cat(sprintf("  ✅ Añadidos %d tickers del NASDAQ para completar\n", length(to_add_nasdaq)))
    }
  }
}

all_tickers <- unique(all_tickers)

cat(sprintf("\n✅ Total tickers FINAL (únicos): %d", length(all_tickers)))

if (length(all_tickers) < target_total_tickers) {
  cat(sprintf(" ⚠️ (objetivo era %d, faltan %d)\n", 
              target_total_tickers, 
              target_total_tickers - length(all_tickers)))
} else {
  cat(" ✅\n")
}

cat("\n📋 Composición final del universo:\n")
cat(sprintf("  • S&P 500: %d\n", sum(all_tickers %in% sp500_tickers_clean)))
cat(sprintf("  • NASDAQ: %d\n", sum(all_tickers %in% nasdaq_tickers_clean)))
cat(sprintf("  • ETFs: %d\n", sum(all_tickers %in% etf_tickers_clean)))
cat(sprintf("  • Commodities: %d\n", sum(all_tickers %in% commodity_tickers_clean)))
cat(sprintf("  • Internacional: %d\n", sum(all_tickers %in% c(international_tickers_clean, toupper(international_tickers_full)))))

overlap_sp_nasdaq <- sum(sp500_tickers_clean %in% nasdaq_tickers_clean)
cat(sprintf("  • Overlap S&P-NASDAQ: %d (%.1f%% del S&P)\n", 
            overlap_sp_nasdaq, 
            100 * overlap_sp_nasdaq / length(sp500_tickers_clean)))

overlap_etf_sp <- sum(etf_tickers_clean %in% sp500_tickers_clean)
overlap_etf_nasdaq <- sum(etf_tickers_clean %in% nasdaq_tickers_clean)
cat(sprintf("  • Overlap ETF-S&P: %d | ETF-NASDAQ: %d\n", overlap_etf_sp, overlap_etf_nasdaq))

cat("\n")

# === DESCARGAR PRECIOS MENSUALES ===
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
  summarise(
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
  summarise(
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

# === ESTADÍSTICAS DESCRIPTIVAS ===
cat(sprintf("\n📊 Calculando estadísticas para horizonte de %d mes(es)...\n", horizon_months))

summary_stats <- df_prices %>%
  group_by(symbol) %>%
  summarise(
    mean_return = mean(monthly_return, na.rm = TRUE),
    sd_return   = sd(monthly_return, na.rm = TRUE),
    n_obs       = sum(!is.na(monthly_return))
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
  
  coverage_plot <- ggplot(summary_stats, aes(x = n_obs)) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = ideal_observations, color = "darkgreen", 
               linetype = "dashed", size = 1) +
    geom_vline(xintercept = min_observations, color = "orange", 
               linetype = "dashed", size = 1) +
    annotate("text", x = ideal_observations + 0.5, y = Inf, 
             label = sprintf("Ideal (%d)", ideal_observations), 
             color = "darkgreen", hjust = 0, vjust = 1.5, size = 3.5) +
    annotate("text", x = min_observations + 0.5, y = Inf, 
             label = sprintf("Mínimo (%d)", min_observations), 
             color = "orange", hjust = 0, vjust = 3, size = 3.5) +
    labs(
      title = "Distribución de Observaciones por Ticker",
      subtitle = sprintf("Horizonte: %d mes(es) (%s) | %d-%d", 
                         horizon_months, horizon_label,
                         min(target_years), max(target_years)),
      x = "Número de Observaciones",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(coverage_plot)
  
} else {
  cat("  ⚠️ No hay tickers con datos suficientes para el análisis\n")
  cat(sprintf("     Horizonte: %d mes(es) (%s) | %d-%d\n", 
              horizon_months, horizon_label, min(target_years), max(target_years)))
  cat(sprintf("     Observaciones mínimas requeridas: %d\n", min_observations))
  cat("\n  💡 Sugerencias:\n")
  cat("     • Ampliar el rango de años (target_years)\n")
  cat("     • Reducir min_observations\n")
  cat("     • Verificar conectividad para descarga de datos\n\n")
  
  stop("❌ No se puede continuar sin datos válidos. Ajusta los parámetros y vuelve a intentar.")
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
    summarise(
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
    cat("  ⚠️ No hay datos coincidentes para FF3\n")
    ff_stats <- data.frame(symbol = character(), beta_mkt = numeric(), 
                           beta_smb = numeric(), beta_hml = numeric(), 
                           ff_expected_return = numeric())
  }
} else {
  cat("  ⚠️ Continuando sin datos Fama-French\n")
  ff_stats <- data.frame(symbol = character(), beta_mkt = numeric(), 
                         beta_smb = numeric(), beta_hml = numeric(), 
                         ff_expected_return = numeric())
}

cat(sprintf("✅ ff_stats creado: %d filas\n", nrow(ff_stats)))

# === COMBINAR MÉTRICAS ===
combined_stats <- summary_stats %>%
  left_join(avg_cor_by_ticker, by = "symbol") %>%
  left_join(ff_stats %>% select(symbol, beta_mkt, beta_smb, beta_hml, ff_expected_return), by = "symbol") %>%
  mutate(
    avg_cor = coalesce(avg_cor, median(avg_cor, na.rm = TRUE)),
    ff_expected_return = coalesce(ff_expected_return, mean_return),
    adjusted_return = ifelse(!is.na(ff_expected_return), 
                             (mean_return + ff_expected_return) / 2, 
                             mean_return),
    sharpe_ratio_adjusted = (adjusted_return - rf_rate_period) / sd_return,
    is_etf_commodity = symbol %in% c(etf_tickers, commodity_tickers)
  )

if (!exists("combined_stats") || nrow(combined_stats) == 0) {
  cat("⚠️ Error: combined_stats no se creó correctamente\n")
  stop("❌ No se puede continuar sin combined_stats")
}

cat(sprintf("✅ combined_stats creado exitosamente: %d activos\n", nrow(combined_stats)))

# === SELECCIÓN DE CANDIDATOS ===
cat(sprintf("\n🎯 Seleccionando candidatos - PRIORIDAD: Menor SD en %d mes(es) (%s)\n", 
            horizon_months, horizon_label))
cat(sprintf("  🔧 Modo de correlación: %s\n", 
            ifelse(correlation_order == 1, 
                   "Mayor a menor (alta correlación preferida)", 
                   "Menor a mayor (baja correlación preferida)")))

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
  
  cat("\n  📋 Top 10 activos seleccionados:\n")
  print(top_selected, n = 10)
  
  penalized_tickers <- df_candidates %>%
    dplyr::filter(data_quality_penalty < 1.0) %>%
    dplyr::filter(symbol %in% selected)
  
  if (nrow(penalized_tickers) > 0) {
    cat(sprintf("\n  ⚠️ %d tickers en portafolio con penalización por datos limitados:\n", 
                nrow(penalized_tickers)))
    print(penalized_tickers %>% 
            dplyr::select(symbol, n_obs, data_quality_penalty) %>% 
            dplyr::arrange(data_quality_penalty), 
          n = nrow(penalized_tickers))
  }
  
  return(selected)
}

ticker_candidates <- select_optimal_candidates(combined_stats, n_divers_candidates)
cat(sprintf("\n✅ Total de candidatos seleccionados: %d\n\n", length(ticker_candidates)))

# === CONSTRUCCIÓN DE MATRIZ DE RETORNOS ===
df_xts <- df_prices %>%
  filter(symbol %in% ticker_candidates) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date) %>%
  drop_na()

dates <- df_xts$date
df_xts_matrix <- as.matrix(df_xts[,-1])
ticker_candidates <- colnames(df_xts_matrix)
df_xts <- xts(df_xts_matrix, order.by = dates)

cat(sprintf("✅ Matriz de retornos: %d observaciones × %d activos\n", nrow(df_xts), ncol(df_xts)))

# === MEDIA Y COVARIANZA ===
assets <- ticker_candidates
mu <- colMeans(df_xts[, assets], na.rm = TRUE)
cov_mat <- cov(df_xts[, assets], use = "pairwise.complete.obs")

if (any(!is.finite(mu))) {
  cat("⚠️ Limpiando valores no finitos en mu...\n")
  mu[!is.finite(mu)] <- 0
}

if (any(!is.finite(cov_mat))) {
  cat("⚠️ Limpiando valores no finitos en cov_mat...\n")
  cov_mat[!is.finite(cov_mat)] <- 0
}

# === ESPECIFICACIÓN DE PORTAFOLIO ===
cat("⚙️ Configurando portafolio con utilidad cuadrática...\n")

etf_commodity_assets <- which(assets %in% c(etf_tickers, commodity_tickers))
stock_assets <- which(!assets %in% c(etf_tickers, commodity_tickers))

portf <- portfolio.spec(assets = assets) %>%
  add.constraint(type = "weight_sum", min_sum = 1, max_sum = 1) %>%
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

# === OPTIMIZACIÓN ===
cat(sprintf("🚀 Ejecutando optimización con λ=%.2f y %d simulaciones...\n", lambda, n_sim))
set.seed(seed)
opt <- optimize.portfolio(
  R = df_xts[, assets],
  portfolio = portf,
  optimize_method = "random",
  search_size = n_sim,
  trace = FALSE
)

# === RESULTADOS ===
weights_opt <- extractWeights(opt)
ret_opt <- sum(weights_opt * mu)
sd_opt <- sqrt(t(weights_opt) %*% cov_mat %*% weights_opt)
sharpe_opt <- (ret_opt - rf_rate_period) / sd_opt
utility_opt <- ret_opt - (lambda / 2) * (sd_opt^2)

portfolio_returns_full <- Return.portfolio(df_xts[, ticker_candidates], weights = weights_opt)
var_parametric <- ret_opt - qnorm(0.95) * sd_opt
cvar_95 <- mean(portfolio_returns_full[portfolio_returns_full <= quantile(portfolio_returns_full, 0.05, na.rm = TRUE)], na.rm = TRUE)

benchmark_xts <- xts(benchmark_prices$benchmark_return, order.by = benchmark_prices$date)
df_xts_with_bench <- merge(df_xts[, ticker_candidates], benchmark = benchmark_xts, all = FALSE) %>% na.omit()

ticker_cols_in_bench <- setdiff(colnames(df_xts_with_bench), "benchmark")
benchmark_aligned <- df_xts_with_bench$benchmark
portfolio_returns_aligned <- Return.portfolio(df_xts_with_bench[, ticker_cols_in_bench], weights = weights_opt)

tracking_error <- StdDev(portfolio_returns_aligned - benchmark_aligned)
relative_returns <- portfolio_returns_aligned - benchmark_aligned
relative_var <- mean(relative_returns, na.rm = TRUE) - qnorm(0.95) * sd(relative_returns, na.rm = TRUE)

pesos <- tibble(symbol = names(weights_opt), weight = weights_opt) %>%
  filter(weight > 0.01) %>%
  arrange(desc(weight))

cat("\n=== 📊 PORTAFOLIO ÓPTIMO (UTILIDAD CUADRÁTICA) ===\n")
print(pesos)

cat(sprintf("\n=== 📈 MÉTRICAS DEL PERÍODO (%s: %s %d-%d) ===\n", 
            horizon_label, month.name[max(target_months)], 
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
cat(sprintf("\n=== 📅 MÉTRICAS ANUALIZADAS (aproximadas) ===\n"))
cat(sprintf("  ➤ Retorno anual: %.2f%%\n", ret_opt * annualization_factor * 100))
cat(sprintf("  ➤ Volatilidad anual: %.2f%%\n", sd_opt * sqrt(annualization_factor) * 100))

# === FRONTERA EFICIENTE CON UTILIDAD CUADRÁTICA ===
cat("\n📊 Generando frontera eficiente con utilidad cuadrática...\n")
random_weights <- random_portfolios(portf, permutations = n_sim, rp_method = "sample")

returns_vals <- apply(random_weights, 1, function(w) sum(w * mu))
risk_vals <- apply(random_weights, 1, function(w) sqrt(t(w) %*% cov_mat %*% w))
utility_vals <- apply(random_weights, 1, function(w) {
  ret <- sum(w * mu)
  vol <- sqrt(t(w) %*% cov_mat %*% w)
  ret - (lambda / 2) * (vol^2)
})

frontier_df <- tibble(
  risk = risk_vals,
  ret = returns_vals,
  utility = utility_vals
) %>%
  filter(!is.na(ret) & !is.na(risk) & !is.na(utility))

fv <- frontier_df %>% arrange(risk)
efficient_points <- fv[1, , drop = FALSE]
for (i in 2:nrow(fv)) {
  if (!is.na(fv$ret[i]) && fv$ret[i] > max(efficient_points$ret, na.rm = TRUE)) {
    efficient_points <- bind_rows(efficient_points, fv[i, ])
  }
}

opt_point <- tibble(risk = sd_opt, ret = ret_opt, utility = utility_opt)

g <- ggplot(frontier_df, aes(x = risk, y = ret)) +
  geom_point(aes(color = utility), alpha = 0.3, size = 1.5) +
  scale_color_gradient2(
    low = "#d73027", 
    mid = "#fee08b", 
    high = "#1a9850",
    midpoint = median(frontier_df$utility, na.rm = TRUE),
    name = sprintf("Utilidad\n(λ=%.1f)", lambda)
  ) +
  geom_line(data = efficient_points, aes(x = risk, y = ret),
            color = "darkgreen", size = 1, linetype = "dashed") +
  geom_point(data = opt_point, aes(x = risk, y = ret),
             color = "red", size = 4, shape = 18) +
  annotate("text", x = opt_point$risk, y = opt_point$ret,
           label = sprintf("Óptimo\nU=%.4f", opt_point$utility),
           hjust = -0.1, vjust = 1.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Frontera Eficiente - Utilidad Cuadrática",
    subtitle = sprintf("Horizonte: %d mes(es) (%s) | λ=%.2f | %d-%d", 
                       horizon_months, horizon_label, lambda,
                       min(target_years), max(target_years)),
    x = sprintf("Riesgo (%d meses)", horizon_months),
    y = sprintf("Retorno Esperado (%d meses)", horizon_months)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

print(g)

# === COMPARACIÓN DE LAMBDAS ===
cat("\n", rep("=", 70), "\n", sep = "")
cat("🧪 ANÁLISIS COMPARATIVO: Portafolios por nivel de λ\n")
cat(rep("=", 70), "\n\n", sep = "")

lambda_values <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 7.5, 10.0)
results_comparison <- tibble()

for (lambda_test in lambda_values) {
  cat(sprintf("  Optimizando λ=%.1f... ", lambda_test))
  
  portf_test <- portfolio.spec(assets = assets) %>%
    add.constraint(type = "weight_sum", min_sum = 1, max_sum = 1) %>%
    add.constraint(type = "box", min = 0, max = max_weight)
  
  if (length(etf_commodity_assets) > 0 && length(stock_assets) > 0) {
    portf_test <- portf_test %>%
      add.constraint(type = "group", 
                     groups = list(etf_commodity_assets, stock_assets),
                     group_min = c(0.05, 0.40),
                     group_max = c(0.50, 1.00))
  }
  
  portf_test <- portf_test %>%
    add.objective(type = "return", name = "mean") %>%
    add.objective(type = "risk", name = "var", risk_aversion = lambda_test)
  
  set.seed(seed + as.integer(lambda_test * 100))
  
  opt_test <- tryCatch({
    optimize.portfolio(
      R = df_xts[, assets],
      portfolio = portf_test,
      optimize_method = "random",
      search_size = n_sim,
      trace = FALSE
    )
  }, error = function(e) {
    cat("❌\n")
    return(NULL)
  })
  
  if (!is.null(opt_test)) {
    w <- extractWeights(opt_test)
    
    if (!all(is.na(w)) && sum(w, na.rm = TRUE) > 0.9) {
      ret <- sum(w * mu)
      vol <- sqrt(t(w) %*% cov_mat %*% w)
      sharpe <- (ret - rf_rate_period) / vol
      utility <- ret - (lambda_test / 2) * (vol^2)
      n_activos <- sum(w > 0.01)
      
      results_comparison <- bind_rows(results_comparison, tibble(
        lambda = lambda_test,
        retorno = ret * 100,
        volatilidad = vol * 100,
        sharpe = sharpe,
        utilidad = utility,
        n_activos = n_activos,
        max_peso = max(w) * 100
      ))
      
      cat(sprintf("✓ R=%.2f%% | σ=%.2f%% | U=%.4f\n", 
                  ret * 100, vol * 100, utility))
    } else {
      cat("⚠️\n")
    }
  }
}

if (nrow(results_comparison) > 0) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("📊 RESULTADOS COMPARATIVOS\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  print(results_comparison %>%
          mutate(across(c(retorno, volatilidad, sharpe, utilidad, max_peso), 
                        ~sprintf("%.4f", .))), 
        n = Inf)
  
  cat("\n📈 Visualización comparativa de lambdas...\n")
  
  g_lambda <- ggplot(results_comparison, aes(x = volatilidad, y = retorno)) +
    geom_path(arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
              size = 1, color = "steelblue", alpha = 0.6) +
    geom_point(aes(color = utilidad, size = lambda)) +
    geom_text(aes(label = sprintf("%.1f", lambda)),
              hjust = -0.3, vjust = 0, size = 3) +
    scale_color_gradient2(
      low = "#d73027",
      mid = "#ffffbf",
      high = "#1a9850",
      midpoint = median(results_comparison$utilidad),
      name = "Utilidad"
    ) +
    scale_size_continuous(range = c(3, 6), guide = "none") +
    labs(
      title = "Frontera de Portafolios por Nivel de Aversión al Riesgo",
      subtitle = sprintf("Horizonte: %d mes(es) (%s) | %d-%d",
                         horizon_months, horizon_label,
                         min(target_years), max(target_years)),
      x = "Volatilidad (%)",
      y = "Retorno Esperado (%)",
      caption = "λ bajo = Agresivo | λ alto = Conservador"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(hjust = 1, face = "italic", size = 8)
    )
  
  print(g_lambda)
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("💡 RECOMENDACIÓN BASADA EN UTILIDAD MÁXIMA\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  best_row <- results_comparison %>% filter(utilidad == max(utilidad))
  
  cat(sprintf("  λ óptimo para estos datos: %.1f\n", best_row$lambda))
  cat(sprintf("  • Retorno esperado: %.2f%% (%.1f%% anual)\n",
              best_row$retorno,
              best_row$retorno * (12/horizon_months)))
  cat(sprintf("  • Volatilidad: %.2f%% (%.1f%% anual)\n",
              best_row$volatilidad,
              best_row$volatilidad * sqrt(12/horizon_months)))
  cat(sprintf("  • Sharpe Ratio: %.3f\n", best_row$sharpe))
  cat(sprintf("  • Utilidad: %.4f (máxima)\n", best_row$utilidad))
  cat(sprintf("  • Activos en portafolio: %d\n", best_row$n_activos))
  
  if (abs(best_row$lambda - lambda) > 0.5) {
    cat(sprintf("\n  ⚠️ El λ actual (%.1f) difiere del óptimo (%.1f)\n", 
                lambda, best_row$lambda))
    cat(sprintf("     Considera ajustar lambda <- %.1f en parámetros\n", 
                best_row$lambda))
  } else {
    cat(sprintf("\n  ✅ El λ actual (%.1f) está cerca del óptimo\n", lambda))
  }
}

cat("\n✅ Optimización completada exitosamente\n")

# === 🆕 ANÁLISIS DE MDD DEL PORTAFOLIO ===
cat(sprintf("\n\n=== 📉 ANÁLISIS DE MAXIMUM DRAWDOWN DEL PORTAFOLIO ===\n"))

tickers_portfolio <- pesos$symbol
weights <- setNames(pesos$weight, pesos$symbol)

# Función para calcular MDD
calc_mdd <- function(returns_vector) {
  if (all(is.na(returns_vector)) || length(na.omit(returns_vector)) < 2) return(NA)
  cumulative_values <- cumprod(1 + returns_vector)
  peak <- cummax(cumulative_values)
  drawdown <- (cumulative_values - peak) / peak
  mdd <- min(drawdown, na.rm = TRUE)
  if (is.infinite(mdd) || is.na(mdd)) return(NA)
  return(mdd)
}

# USAR LOS DATOS YA CARGADOS (df_prices_monthly) 
cat(sprintf("📥 Preparando datos históricos del portafolio desde %d...\n", mdd_start_year))

# Para MDD usamos datos mensuales individuales (no acumulados)
portfolio_hist <- df_prices_monthly %>%
  filter(
    symbol %in% tickers_portfolio,
    year(date) >= mdd_start_year,
    year(date) <= max(target_years)
  )

cat(sprintf("  ✓ Datos históricos totales: %d observaciones\n", nrow(portfolio_hist)))
cat(sprintf("  ✓ Período: %s a %s\n", 
            format(min(portfolio_hist$date), "%Y-%m"),
            format(max(portfolio_hist$date), "%Y-%m")))
cat(sprintf("  ✓ Tickers en portafolio: %d\n", length(tickers_portfolio)))

# Verificar datos por ticker
ticker_coverage <- portfolio_hist %>%
  group_by(symbol) %>%
  summarise(n_obs = n(), .groups = 'drop') %>%
  arrange(desc(n_obs))

cat("\n  📊 Cobertura por ticker:\n")
print(as.data.frame(ticker_coverage), row.names = FALSE)

# Crear data frame con retornos ponderados
portfolio_wide <- portfolio_hist %>%
  select(date, symbol, monthly_return) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return)

cat(sprintf("  ✓ Estructura de datos: %d fechas × %d tickers\n", 
            nrow(portfolio_wide), length(tickers_portfolio)))

# Calcular retornos del portafolio con pesos
portfolio_returns_by_year <- portfolio_wide %>%
  arrange(date) %>%
  rowwise() %>%
  mutate(
    available_tickers = sum(!is.na(c_across(all_of(tickers_portfolio)))),
    portfolio_return = if_else(
      available_tickers > 0,
      {
        valid_returns <- c_across(all_of(tickers_portfolio))
        valid_mask <- !is.na(valid_returns)
        if (sum(valid_mask) > 0) {
          valid_weights <- weights[tickers_portfolio][valid_mask]
          weight_sum <- sum(valid_weights)
          normalized_weights <- valid_weights / weight_sum
          sum(valid_returns[valid_mask] * normalized_weights)
        } else {
          NA_real_
        }
      },
      NA_real_
    ),
    year = year(date)
  ) %>%
  ungroup() %>%
  select(date, year, portfolio_return, available_tickers)

cat(sprintf("  ✓ Retornos calculados: %d observaciones totales\n", nrow(portfolio_returns_by_year)))
cat(sprintf("  ✓ Retornos válidos: %d observaciones\n", 
            sum(!is.na(portfolio_returns_by_year$portfolio_return))))

# Filtrar solo observaciones válidas para MDD
valid_returns <- portfolio_returns_by_year %>%
  filter(!is.na(portfolio_return), is.finite(portfolio_return))

# Calcular MDD global
if (nrow(valid_returns) >= 2) {
  cumulative_values <- cumprod(1 + valid_returns$portfolio_return)
  peak <- cummax(cumulative_values)
  drawdown <- (cumulative_values - peak) / peak
  global_mdd <- min(drawdown, na.rm = TRUE)
  
  cat(sprintf("\n  📊 MDD Global del Portafolio (%d-%d): %.2f%%\n", 
              min(valid_returns$year), 
              max(valid_returns$year),
              global_mdd * 100))
  cat(sprintf("      Basado en %d observaciones mensuales\n", nrow(valid_returns)))
}

# Crear tabla con TODOS los años del rango
all_years <- data.frame(year = seq(mdd_start_year, max(target_years)))

# Calcular MDD por año donde haya datos
yearly_mdd_calculated <- valid_returns %>%
  group_by(year) %>%
  summarise(
    mdd = calc_mdd(portfolio_return),
    n_obs = n(),
    avg_tickers = mean(available_tickers, na.rm = TRUE),
    .groups = 'drop'
  )

# Unir con todos los años
yearly_mdd <- all_years %>%
  left_join(yearly_mdd_calculated, by = "year") %>%
  mutate(
    has_data = !is.na(mdd),
    n_obs = replace_na(n_obs, 0)
  )

cat(sprintf("  ✓ Años con datos de MDD: %d de %d años posibles\n", 
            sum(yearly_mdd$has_data), nrow(yearly_mdd)))
cat(sprintf("  ✓ Años sin datos: %d\n", sum(!yearly_mdd$has_data)))

# Filtrar solo años con datos válidos
yearly_mdd_valid <- yearly_mdd %>%
  dplyr::filter(has_data, is.finite(mdd))

if (nrow(yearly_mdd_valid) >= 1) {
  
  cat(sprintf("\n=== 📊 ESTADÍSTICAS DE MDD ===\n"))
  cat(sprintf("Período analizado: %d-%d\n", mdd_start_year, max(target_years)))
  cat(sprintf("Años con datos: %d | Años sin datos: %d\n\n", 
              sum(yearly_mdd$has_data), sum(!yearly_mdd$has_data)))
  
  if (nrow(yearly_mdd_valid) >= 3) {
    # Eliminar outliers
    q1 <- quantile(yearly_mdd_valid$mdd, 0.25, na.rm = TRUE)
    q3 <- quantile(yearly_mdd_valid$mdd, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    yearly_mdd_clean <- yearly_mdd_valid %>%
      dplyr::filter(mdd >= lower_bound & mdd <= upper_bound)
    
    if (nrow(yearly_mdd_clean) < 2) {
      yearly_mdd_clean <- yearly_mdd_valid
      cat("  ℹ️ Usando todos los datos disponibles (sin filtro de outliers)\n\n")
    }
    
    # Estadísticas del MDD
    median_mdd <- median(yearly_mdd_clean$mdd, na.rm = TRUE)
    p90_mdd <- quantile(yearly_mdd_clean$mdd, 0.90, na.rm = TRUE)
    mean_mdd <- mean(yearly_mdd_clean$mdd, na.rm = TRUE)
    min_mdd <- min(yearly_mdd_clean$mdd, na.rm = TRUE)
    max_mdd <- max(yearly_mdd_clean$mdd, na.rm = TRUE)
    
    cat(sprintf("  ➤ Peor escenario histórico: %.2f%%\n", min_mdd * 100))
    cat(sprintf("  ➤ Escenario Conservador (P90): %.2f%%\n", p90_mdd * 100))
    cat(sprintf("  ➤ Escenario Típico (Mediana): %.2f%%\n", median_mdd * 100))
    cat(sprintf("  ➤ Promedio: %.2f%%\n", mean_mdd * 100))
    cat(sprintf("  ➤ Mejor escenario histórico: %.2f%%\n", max_mdd * 100))
    
  } else {
    mean_mdd <- mean(yearly_mdd_valid$mdd, na.rm = TRUE)
    min_mdd <- min(yearly_mdd_valid$mdd, na.rm = TRUE)
    max_mdd <- max(yearly_mdd_valid$mdd, na.rm = TRUE)
    
    cat(sprintf("  ➤ Peor escenario: %.2f%%\n", max_mdd * 100))
    cat(sprintf("  ➤ Promedio: %.2f%%\n", mean_mdd * 100))
    cat(sprintf("  ➤ Mejor escenario: %.2f%%\n", min_mdd * 100))
    
    median_mdd <- mean_mdd
    p90_mdd <- max_mdd
  }
  
  # MDD del último año con datos
  last_year_data <- yearly_mdd_valid %>%
    dplyr::arrange(desc(year))
  
  if (nrow(last_year_data) > 0) {
    last_year_data <- last_year_data[1, ]
    cat(sprintf("  ➤ MDD más reciente (%d): %.2f%%\n", 
                last_year_data$year, 
                last_year_data$mdd * 100))
  }
  
  # Visualización del MDD histórico
  cat("\n📊 Generando gráfico de MDD histórico...\n")
  
  # Preparar datos para el gráfico
  plot_data <- yearly_mdd %>%
    dplyr::mutate(
      mdd_pct = if_else(has_data, mdd * 100, NA_real_),
      year_label = as.character(year)
    )
  
  mdd_plot <- ggplot(plot_data, aes(x = year, y = mdd_pct)) +
    geom_line(color = "darkred", size = 1, na.rm = TRUE) +
    geom_point(aes(color = has_data), size = 2.5, na.rm = TRUE) +
    scale_color_manual(
      values = c("TRUE" = "darkred", "FALSE" = "gray70"),
      labels = c("TRUE" = "Con datos", "FALSE" = "Sin datos"),
      name = "Estado"
    )
  
  # Agregar líneas de referencia
  if (nrow(yearly_mdd_valid) >= 3) {
    mdd_plot <- mdd_plot +
      geom_hline(yintercept = median_mdd * 100, 
                 linetype = "dashed", color = "blue", size = 0.8) +
      geom_hline(yintercept = p90_mdd * 100, 
                 linetype = "dashed", color = "orange", size = 0.8) +
      annotate("text", 
               x = min(yearly_mdd$year) + 2, 
               y = median_mdd * 100 - 0.3, 
               label = sprintf("Mediana: %.2f%%", median_mdd * 100), 
               color = "blue", size = 3.5, hjust = 0) +
      annotate("text", 
               x = min(yearly_mdd$year) + 2, 
               y = p90_mdd * 100 - 0.3, 
               label = sprintf("P90: %.2f%%", p90_mdd * 100), 
               color = "orange", size = 3.5, hjust = 0)
  }
  
  mdd_plot <- mdd_plot +
    labs(
      title = sprintf("Maximum Drawdown Histórico del Portafolio"),
      subtitle = sprintf("Horizonte optimización: %d mes(es) (%s) | Análisis MDD: %d-%d | %d años con datos", 
                         horizon_months, horizon_label,
                         mdd_start_year, max(target_years), 
                         sum(yearly_mdd$has_data)),
      x = "Año",
      y = "MDD (%)"
    ) +
    scale_x_continuous(breaks = seq(mdd_start_year, max(target_years), by = 2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  print(mdd_plot)
  
  # Tabla resumen completa
  cat("\n📋 Resumen detallado por año:\n")
  yearly_summary <- yearly_mdd %>%
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(
      mdd_pct = if_else(has_data, sprintf("%.2f%%", mdd * 100), "Sin datos"),
      obs = if_else(has_data, as.character(n_obs), "-"),
      tickers_promedio = if_else(has_data, sprintf("%.1f", avg_tickers), "-")
    ) %>%
    dplyr::select(year, mdd_pct, obs, tickers_promedio)
  
  colnames(yearly_summary) <- c("Anio", "MDD", "Obs", "Tickers_Prom")
  print(as.data.frame(yearly_summary), row.names = FALSE)
  
} else {
  cat("\n⚠️ No se encontraron suficientes datos para calcular MDD.\n")
  cat(sprintf("   Años analizados: %d-%d\n", mdd_start_year, max(target_years)))
  cat(sprintf("   Años con datos: %d\n", sum(yearly_mdd$has_data)))
  
  if (sum(yearly_mdd$has_data) > 0) {
    cat("\n📋 Años con datos disponibles:\n")
    available_years <- yearly_mdd %>%
      dplyr::filter(has_data) %>%
      dplyr::mutate(mdd_pct = sprintf("%.2f%%", mdd * 100))
    print(as.data.frame(available_years %>% dplyr::select(year, mdd_pct, n_obs)), row.names = FALSE)
  }
}

cat("\n✅ Análisis completado exitosamente!\n")
cat(sprintf("\n📝 RESUMEN EJECUTIVO:\n"))
cat(sprintf("   Horizonte temporal: %d mes(es) (%s)\n", horizon_months, horizon_label))
cat(sprintf("   Número de activos: %d\n", length(tickers_portfolio)))
cat(sprintf("   Retorno esperado (período): %.2f%%\n", ret_opt * 100))
cat(sprintf("   Retorno anualizado (aprox.): %.2f%%\n", ret_opt * annualization_factor * 100))
cat(sprintf("   Volatilidad (período): %.2f%%\n", sd_opt * 100))
cat(sprintf("   Volatilidad anualizada (aprox.): %.2f%%\n", sd_opt * sqrt(annualization_factor) * 100))
cat(sprintf("   Sharpe Ratio: %.4f\n", sharpe_opt))
if (exists("median_mdd")) {
  cat(sprintf("   MDD típico histórico: %.2f%%\n", median_mdd * 100))
}

# 🆕 === RESUMEN DE CALIDAD DE DATOS EN PORTAFOLIO FINAL ===
cat(sprintf("\n=== 📊 CALIDAD DE DATOS EN PORTAFOLIO FINAL ===\n"))

portfolio_data_quality <- combined_stats %>%
  dplyr::filter(symbol %in% tickers_portfolio) %>%
  dplyr::select(symbol, n_obs, data_quality_penalty, sharpe_ratio_adjusted, sd_return) %>%
  dplyr::left_join(pesos, by = "symbol") %>%
  dplyr::arrange(desc(weight))

cat(sprintf("  Activos totales en portafolio: %d\n", nrow(portfolio_data_quality)))
cat(sprintf("  Activos con datos ideales (≥%d obs): %d (%.1f%%)\n", 
            ideal_observations,
            sum(portfolio_data_quality$n_obs >= ideal_observations),
            100 * mean(portfolio_data_quality$n_obs >= ideal_observations)))
cat(sprintf("  Activos con penalización: %d (%.1f%%)\n", 
            sum(portfolio_data_quality$data_quality_penalty < 1.0),
            100 * mean(portfolio_data_quality$data_quality_penalty < 1.0)))

if (any(portfolio_data_quality$data_quality_penalty < 1.0)) {
  cat("\n  📋 Detalle de activos con datos limitados:\n")
  limited_data <- portfolio_data_quality %>%
    dplyr::filter(data_quality_penalty < 1.0) %>%
    dplyr::mutate(
      weight_pct = sprintf("%.2f%%", weight * 100),
      penalty_pct = sprintf("%.1f%%", data_quality_penalty * 100)
    ) %>%
    dplyr::select(Symbol = symbol, Peso = weight_pct, Obs = n_obs, 
                  Penalizacion = penalty_pct, Sharpe = sharpe_ratio_adjusted)
  
  print(as.data.frame(limited_data), row.names = FALSE)
}
cat("\n✅ Script completado con éxito!\n")