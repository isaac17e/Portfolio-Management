# ===============================================
# Script: Optimización de portafolio - Utilidad Cuadrática
# Autor: Isaac Echeverri
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

n_top_nasdaq <- 250
n_top_sp500 <- 250
n_top_int <- 40  
target_months <- c(04)
target_years <- 2015:2025
mdd_start_year <- 2014  

rf_rate <- 0.03685 #T bills 03MY

seed <- 123
max_weight <- 0.15
target_total_tickers <- 540

# Risk Profile
lambda <- 0.5

weight_sharpe <- 0.20
weight_low_vol <- 0.35
weight_decorr <- 0.55

n_divers_candidates <- 50
correlation_order <- 0

min_observations   <- 36
ideal_observations <- 96

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

cat(sprintf("\n🔄 Preparando datos de estimación (todos los meses) y referencia de ejecución (%s)...\n",
            horizon_label))

# --- FLUJO 1: Datos de estimación — todos los meses del año ---
df_prices <- df_prices_monthly %>%
  filter(year(date) %in% target_years) %>%
  select(symbol, date, monthly_return)

cat(sprintf("  ✓ Observaciones para estimación (todos los meses): %d\n", nrow(df_prices)))
cat(sprintf("  ✓ Tickers únicos con datos: %d\n", length(unique(df_prices$symbol))))

# --- FLUJO 2: Retornos de referencia — solo target_months, acumulados por año ---
df_prices_accumulated <- df_prices_monthly %>%
  mutate(
    year  = year(date),
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

cat(sprintf("  ✓ Períodos de referencia (%s) encontrados: %d años\n",
            horizon_label, length(unique(df_prices_accumulated$year))))

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

benchmark_prices <- benchmark_prices_monthly %>%
  filter(year(date) %in% target_years) %>%
  select(date, benchmark_return)

benchmark_prices_accumulated <- benchmark_prices_monthly %>%
  mutate(
    year  = year(date),
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

benchmark_prices_ref <- benchmark_prices_accumulated %>%
  select(date, benchmark_return = accumulated_return)

# === ESTADÍSTICAS DESCRIPTIVAS ===
cat(sprintf("\n📊 Calculando estadísticas sobre retornos mensuales completos (%d-%d)...\n",
            min(target_years), max(target_years)))

summary_stats <- df_prices %>%
  group_by(symbol) %>%
  summarise(
    mean_return = mean(monthly_return, na.rm = TRUE),
    sd_return   = sd(monthly_return, na.rm = TRUE),
    n_obs       = sum(!is.na(monthly_return))
  ) %>%
  filter(n_obs >= ideal_observations, sd_return > 0) %>%
  mutate(
    sharpe_ratio = (mean_return - rf_rate_period) / sd_return
  )

cat(sprintf("\n=== 📊 DIAGNÓSTICO DE COBERTURA DE DATOS (estimación mensual completa) ===\n"))

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
  cat(sprintf("  Todos los activos tienen ≥%d obs (filtro aplicado): 100%%\n\n", 
              ideal_observations))
  
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
cat("📊 Calculando matriz de correlaciones (sobre universo filtrado)...\n")

symbols_validos <- summary_stats$symbol

corr_pairs <- df_prices %>%
  filter(symbol %in% symbols_validos) %>%  
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
cat("📥 Descargando datos Fama-French (todos los meses)...\n")
ff_data <- tryCatch({
  ff_raw <- download_french_data("Fama/French 3 Factors")$subsets$data[[1]] %>%
    # FF3 viene como entero YYYYMM — convertir a fecha anclada al último día del mes
    # para que coincida con tq_get que también ancla al último día
    mutate(
      date_parsed = as.Date(ym(date)),
      date        = ceiling_date(date_parsed, "month") - days(1)
    ) %>%
    select(-date_parsed) %>%
    filter(year(date) %in% target_years) %>%
    mutate(across(c(`Mkt-RF`, SMB, HML), ~ . / 100))
  
  xts(ff_raw[, c("Mkt-RF", "SMB", "HML")], order.by = ff_raw$date)
}, error = function(e) {
  cat("⚠️ Error con Fama-French. Continuando sin ellos.\n")
  return(NULL)
})

# === BETAS FF3 ===
if (!is.null(ff_data)) {
  cat("📊 Calculando betas Fama-French...\n")
  
  ff_df <- data.frame(
    date       = as.Date(index(ff_data)),
    mkt_rf     = as.numeric(ff_data$`Mkt-RF`),
    smb        = as.numeric(ff_data$SMB),
    hml        = as.numeric(ff_data$HML)
  ) %>%
    mutate(year_month = format(date, "%Y-%m"))
  
  # Descargar RF mensual de Fama-French (T-Bill 1M implícito en el dataset)
  ff_rf_raw <- tryCatch({
    download_french_data("Fama/French 3 Factors")$subsets$data[[1]] %>%
      mutate(
        date_parsed = as.Date(ym(date)),
        date        = ceiling_date(date_parsed, "month") - days(1),
        rf_ff       = RF / 100   # RF viene en porcentaje
      ) %>%
      select(date, rf_ff) %>%
      filter(year(date) %in% target_years) %>%
      mutate(year_month = format(date, "%Y-%m"))
  }, error = function(e) {
    cat("  ⚠️ No se pudo extraer RF de FF3. Se usará rf_rate_period como fallback.\n")
    NULL
  })
  
  # Agregar year_month a df_prices y hacer join por año-mes
  returns_merged <- df_prices %>%
    left_join(benchmark_prices, by = "date") %>%
    mutate(year_month = format(date, "%Y-%m")) %>%
    inner_join(ff_df %>% select(year_month, mkt_rf, smb, hml),
               by = "year_month")
  
  # Unir RF de FF si está disponible; si no, usar rf_rate_period como fallback
  if (!is.null(ff_rf_raw)) {
    returns_merged <- returns_merged %>%
      left_join(ff_rf_raw %>% select(year_month, rf_ff), by = "year_month") %>%
      mutate(
        rf_used       = coalesce(rf_ff, rf_rate_period),
        excess_return = monthly_return - rf_used
      )
    cat("  ✓ RF de Fama-French (T-Bill 1M histórico) aplicada correctamente\n")
  } else {
    returns_merged <- returns_merged %>%
      mutate(
        rf_used       = rf_rate_period,
        excess_return = monthly_return - rf_rate_period
      )
    cat("  ⚠️ Usando rf_rate_period como fallback para excess_return\n")
  }
  
  returns_merged <- returns_merged %>%
    filter(!is.na(mkt_rf))
  
  cat(sprintf("  ✓ Observaciones coincidentes FF3 × retornos: %d\n", nrow(returns_merged)))
  
  if (nrow(returns_merged) > 0) {
    returns_with_ff <- returns_merged
    
    ff_stats <- returns_with_ff %>%
      group_by(symbol) %>%
      filter(n() >= 12) %>%
      do({
        tryCatch({
          model <- lm(excess_return ~ mkt_rf + smb + hml, data = .)
          data.frame(
            symbol   = unique(.$symbol),
            beta_mkt = coef(model)[2],
            beta_smb = coef(model)[3],
            beta_hml = coef(model)[4]
          )
        }, error = function(e) {
          data.frame(
            symbol   = unique(.$symbol),
            beta_mkt = NA_real_,
            beta_smb = NA_real_,
            beta_hml = NA_real_
          )
        })
      }) %>%
      ungroup()
    
    market_premium <- mean(ff_data$`Mkt-RF`, na.rm = TRUE)
    smb_premium    <- mean(ff_data$SMB,      na.rm = TRUE)
    hml_premium    <- mean(ff_data$HML,      na.rm = TRUE)
    
    # Para ff_expected_return se usa rf_rate_period (T-Bill 3M actual, horizonte del portafolio)
    # Los premios de mercado ya son excess returns (Mkt-RF de French), consistente.
    ff_stats <- ff_stats %>%
      mutate(ff_expected_return = rf_rate_period +
               beta_mkt * market_premium +
               beta_smb * smb_premium    +
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
cat(sprintf("\n🎯 Seleccionando candidatos - Horizonte: %d mes(es) (%s)\n", 
            horizon_months, horizon_label))
cat(sprintf("  🔧 Pesos: Sharpe=%.0f%% | Vol=%.0f%% | Corr=%.0f%%\n",
            weight_sharpe*100, weight_low_vol*100, weight_decorr*100))

select_optimal_candidates <- function(df, n_candidates) {
  
  df_filtered <- df %>%
    dplyr::filter(
      sd_return > 0,
      !is.na(sharpe_ratio_adjusted),
      is.finite(sharpe_ratio_adjusted)
    )
  
  cat(sprintf("  ✓ Activos que entran al scoring: %d\n", nrow(df_filtered)))
  
  df_scored <- df_filtered %>%
    dplyr::mutate(
      sharpe_norm = (sharpe_ratio_adjusted - min(sharpe_ratio_adjusted, na.rm = TRUE)) /
        (max(sharpe_ratio_adjusted, na.rm = TRUE) - min(sharpe_ratio_adjusted, na.rm = TRUE)),
      
      vol_norm = 1 - (sd_return - min(sd_return, na.rm = TRUE)) /
        (max(sd_return, na.rm = TRUE) - min(sd_return, na.rm = TRUE)),
      
      corr_norm = if (correlation_order == 1) {
        (avg_cor - min(avg_cor, na.rm = TRUE)) /
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      } else {
        1 - (avg_cor - min(avg_cor, na.rm = TRUE)) /
          (max(avg_cor, na.rm = TRUE) - min(avg_cor, na.rm = TRUE))
      }
    ) %>%
    dplyr::mutate(
      sharpe_norm = ifelse(is.nan(sharpe_norm), 0.5, sharpe_norm),
      vol_norm    = ifelse(is.nan(vol_norm),    0.5, vol_norm),
      corr_norm   = ifelse(is.nan(corr_norm),   0.5, corr_norm)
    )
  
  df_scored <- df_scored %>%
    dplyr::mutate(
      composite_score = weight_sharpe  * sharpe_norm +
        weight_low_vol * vol_norm    +
        weight_decorr  * corr_norm
    ) %>%
    dplyr::arrange(desc(composite_score))
  
  n_to_select <- min(n_candidates, nrow(df_scored))
  selected    <- df_scored$symbol[1:n_to_select]
  
  n_top_show  <- min(10, nrow(df_scored))
  top_selected <- df_scored[1:n_top_show, ] %>%
    dplyr::select(symbol, sharpe_ratio_adjusted, sd_return, avg_cor,
                  n_obs, composite_score)
  
  cat("\n  📋 Top 10 activos seleccionados:\n")
  print(top_selected, n = 10)
  
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
cat("⚙️ Configurando portafolio con utilidad cuadrática (ROI)...\n")

etf_commodity_assets <- which(assets %in% c(etf_tickers, commodity_tickers))
stock_assets <- which(!assets %in% c(etf_tickers, commodity_tickers))

n      <- length(assets)
Dmat   <- 2 * lambda * cov_mat
dvec   <- mu

# === CONSTRUCCIÓN DE RESTRICCIONES ===
A_long  <- diag(n)
b_long  <- rep(0, n)

A_sum   <- matrix(rep(1, n), nrow = n)
b_sum   <- 1

A_max   <- -diag(n)
b_max   <- rep(-max_weight, n)

if (length(etf_commodity_assets) > 0 && length(stock_assets) > 0) {
  
  A_etf_min          <- rep(0, n)
  A_etf_min[etf_commodity_assets] <- 1
  
  A_etf_max          <- rep(0, n)
  A_etf_max[etf_commodity_assets] <- -1
  
  A_stk_min          <- rep(0, n)
  A_stk_min[stock_assets] <- 1
  
  A_stk_max          <- rep(0, n)
  A_stk_max[stock_assets] <- -1
  
  Amat <- cbind(A_sum, A_long, A_max, 
                A_etf_min, A_etf_max, 
                A_stk_min, A_stk_max)
  
  bvec <- c(b_sum, b_long, b_max,
            0.05, -0.50,
            0.40, -1.00)
  
  meq <- 1
  
  cat(sprintf("  ✓ Restricciones de grupo activas: ETFs [5%%-50%%] | Acciones [40%%-100%%]\n"))
  
} else {
  
  Amat <- cbind(A_sum, A_long, A_max)
  bvec <- c(b_sum, b_long, b_max)
  meq  <- 1
  
  cat("  ⚠️ Sin restricciones de grupo (todos los activos del mismo tipo)\n")
}

cat(sprintf("  ✓ Activos: %d | Restricciones totales: %d\n", n, ncol(Amat)))

# === OPTIMIZACIÓN QP ===
cat("🚀 Ejecutando optimización QP exacta (quadprog)...\n")

opt_qp <- tryCatch({
  quadprog::solve.QP(
    Dmat = Dmat,
    dvec = dvec,
    Amat = Amat,
    bvec = bvec,
    meq  = meq
  )
}, error = function(e) {
  cat(sprintf("❌ Error en QP: %s\n", e$message))
  cat("   Intentando con matriz regularizada...\n")
  
  for (reg in c(1e-6, 1e-4, 1e-3, 1e-2)) {
    resultado <- tryCatch({
      quadprog::solve.QP(
        Dmat = Dmat + diag(reg, n),
        dvec = dvec,
        Amat = Amat,
        bvec = bvec,
        meq  = meq
      )
    }, error = function(e2) NULL)
    
    if (!is.null(resultado)) {
      cat(sprintf("   ✓ Solución encontrada con regularización: %g\n", reg))
      return(resultado)
    }
  }
  stop("❌ No se pudo resolver el QP ni con regularización máxima.")
})

weights_opt <- opt_qp$solution
weights_opt[weights_opt < 1e-6] <- 0
weights_opt <- weights_opt / sum(weights_opt)
names(weights_opt) <- assets

cat(sprintf("✅ Optimización completada\n"))
cat(sprintf("   Suma de pesos: %.6f\n", sum(weights_opt)))
cat(sprintf("   Activos con peso > 1%%: %d\n", sum(weights_opt > 0.01)))
cat(sprintf("   Peso máximo: %.2f%% (%s)\n", 
            max(weights_opt) * 100, 
            names(weights_opt)[which.max(weights_opt)]))

# === RESULTADOS ===
ret_opt     <- sum(weights_opt * mu)
sd_opt      <- sqrt(as.numeric(t(weights_opt) %*% cov_mat %*% weights_opt))
sharpe_opt  <- (ret_opt - rf_rate_period) / sd_opt
utility_opt <- ret_opt - (lambda / 2) * (sd_opt^2)

portfolio_returns_full <- Return.portfolio(df_xts[, ticker_candidates], weights = weights_opt)

# --- SORTINO ---
excess_returns <- as.numeric(portfolio_returns_full) - rf_rate_period
downside_ret   <- excess_returns[excess_returns < 0]
downside_dev   <- if (length(downside_ret) > 0) sqrt(mean(downside_ret^2)) else NA_real_
sortino_opt    <- if (!is.na(downside_dev) && downside_dev > 0) (ret_opt - rf_rate_period) / downside_dev else NA_real_

var_parametric <- ret_opt - qnorm(0.99) * sd_opt
cvar_99 <- mean(portfolio_returns_full[portfolio_returns_full <= quantile(portfolio_returns_full, 0.01, na.rm = TRUE)], na.rm = TRUE)

benchmark_xts <- xts(benchmark_prices_ref$benchmark_return, order.by = benchmark_prices_ref$date)

tickers_con_peso <- names(weights_opt[weights_opt > 0.01])

df_xts_ref <- df_prices_accumulated %>%
  filter(symbol %in% tickers_con_peso) %>%
  select(symbol, date, monthly_return = accumulated_return) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return) %>%
  arrange(date)

df_xts_ref[is.na(df_xts_ref)] <- 0

if (nrow(df_xts_ref) > 1) {
  dates_ref      <- df_xts_ref$date
  df_xts_ref_mat <- as.matrix(df_xts_ref[, -1])
  df_xts_ref_xts <- xts(df_xts_ref_mat, order.by = dates_ref)
  
  df_xts_with_bench <- merge(df_xts_ref_xts, benchmark = benchmark_xts, all = FALSE) %>% na.omit()
  
  if (nrow(df_xts_with_bench) > 1) {
    ticker_cols_in_bench      <- setdiff(colnames(df_xts_with_bench), "benchmark")
    weights_ref               <- weights_opt[ticker_cols_in_bench]
    weights_ref               <- weights_ref / sum(weights_ref, na.rm = TRUE)
    benchmark_aligned         <- df_xts_with_bench$benchmark
    portfolio_returns_aligned <- Return.portfolio(df_xts_with_bench[, ticker_cols_in_bench],
                                                  weights = weights_ref)
    tracking_error   <- StdDev(portfolio_returns_aligned - benchmark_aligned)
    relative_returns <- portfolio_returns_aligned - benchmark_aligned
    relative_var     <- mean(relative_returns, na.rm = TRUE) - qnorm(0.99) * sd(relative_returns, na.rm = TRUE)
    cat(sprintf("  ✓ Tracking error calculado sobre %d períodos de referencia\n", nrow(df_xts_with_bench)))
  } else {
    tracking_error <- NA_real_
    relative_var   <- NA_real_
    cat("  ⚠️ Insuficientes períodos coincidentes para tracking error\n")
  }
} else {
  tracking_error <- NA_real_
  relative_var   <- NA_real_
  cat("  ⚠️ No hay datos de referencia para tracking error\n")
}

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
cat(sprintf("  ➤ Sortino Ratio: %.4f\n", sortino_opt))
cat(sprintf("  ➤ Utilidad Cuadrática: %.6f\n", utility_opt))
cat(sprintf("  ➤ VaR (99%%): %.4f%%\n", var_parametric * 100))
cat(sprintf("  ➤ CVaR (99%%): %.4f%%\n", cvar_99 * 100))
cat(sprintf("  ➤ Tracking Error: %.4f%%\n", tracking_error * 100))
cat(sprintf("  ➤ Relative VaR (99%%): %.4f%%\n", relative_var * 100))

annualization_factor <- 12 / horizon_months
cat(sprintf("\n=== 📅 MÉTRICAS ANUALIZADAS (aproximadas) ===\n"))
cat(sprintf("  ➤ Retorno anual: %.2f%%\n", ret_opt * annualization_factor * 100))
cat(sprintf("  ➤ Volatilidad anual: %.2f%%\n", sd_opt * sqrt(annualization_factor) * 100))
cat(sprintf("  ➤ Sharpe anualizado: %.4f\n", sharpe_opt * sqrt(annualization_factor)))
cat(sprintf("  ➤ Sortino anualizado: %.4f\n", sortino_opt * sqrt(annualization_factor)))

# === FRONTERA EFICIENTE CON UTILIDAD CUADRÁTICA ===
cat("\n📊 Generando frontera eficiente con utilidad cuadrática (QP exacto)...\n")

min_ret_target <- min(mu)
max_ret_target <- max(mu)
target_returns <- seq(min_ret_target, max_ret_target, length.out = 100)

frontier_list <- list()

for (i in seq_along(target_returns)) {
  target_r <- target_returns[i]
  
  tryCatch({
    A_ret <- mu
    
    if (length(etf_commodity_assets) > 0 && length(stock_assets) > 0) {
      A_etf_min_f <- rep(0, n); A_etf_min_f[etf_commodity_assets] <-  1
      A_etf_max_f <- rep(0, n); A_etf_max_f[etf_commodity_assets] <- -1
      A_stk_min_f <- rep(0, n); A_stk_min_f[stock_assets]         <-  1
      A_stk_max_f <- rep(0, n); A_stk_max_f[stock_assets]         <- -1
      
      Amat_f <- cbind(A_sum, A_long, A_max,
                      A_etf_min_f, A_etf_max_f,
                      A_stk_min_f, A_stk_max_f,
                      A_ret)
      bvec_f <- c(b_sum, b_long, b_max, 0.05, -0.50, 0.40, -1.00, target_r)
    } else {
      Amat_f <- cbind(A_sum, A_long, A_max, A_ret)
      bvec_f <- c(b_sum, b_long, b_max, target_r)
    }
    
    sol_f <- NULL
    for (reg in c(0, 1e-6, 1e-4, 1e-3, 1e-2)) {
      Dmat_f <- 2 * cov_mat + diag(reg, n)
      sol_f <- tryCatch({
        quadprog::solve.QP(
          Dmat = Dmat_f,
          dvec = rep(0, n),
          Amat = Amat_f,
          bvec = bvec_f,
          meq  = 1
        )
      }, error = function(e) NULL)
      if (!is.null(sol_f)) break
    }
    
    if (!is.null(sol_f)) {
      w_f <- sol_f$solution
      w_f[w_f < 1e-6] <- 0
      w_f <- w_f / sum(w_f)
      
      ret_f     <- sum(w_f * mu)
      risk_f    <- sqrt(as.numeric(t(w_f) %*% cov_mat %*% w_f))
      utility_f <- ret_f - (lambda / 2) * (risk_f^2)
      
      frontier_list[[length(frontier_list) + 1]] <- data.frame(
        risk    = risk_f,
        ret     = ret_f,
        utility = utility_f
      )
    }
    
  }, error = function(e) {})
}

if (length(frontier_list) == 0) {
  cat("  ⚠️ No se pudieron calcular puntos para la frontera eficiente.\n")
  frontier_df <- data.frame(risk = numeric(), ret = numeric(), utility = numeric())
} else {
  frontier_df <- bind_rows(frontier_list) %>%
    filter(!is.na(ret), !is.na(risk), !is.na(utility)) %>%
    arrange(risk)
  cat(sprintf("  ✓ Frontera calculada con %d puntos exactos\n", nrow(frontier_df)))
}

opt_point <- tibble(
  risk    = as.numeric(sd_opt),
  ret     = ret_opt,
  utility = utility_opt
)

asset_points <- tibble(
  symbol = assets,
  risk   = sqrt(diag(cov_mat)),
  ret    = mu
)

if (nrow(frontier_df) > 0) {
  g <- ggplot(frontier_df, aes(x = risk, y = ret)) +
    geom_line(aes(color = utility), size = 1.8, alpha = 0.9) +
    scale_color_gradient2(
      low      = "#d73027",
      mid      = "#fee08b",
      high     = "#1a9850",
      midpoint = median(frontier_df$utility, na.rm = TRUE),
      name     = sprintf("Utilidad\n(λ=%.1f)", lambda)
    ) +
    geom_point(data = asset_points, aes(x = risk, y = ret),
               color = "#ff7f00", size = 2.5, alpha = 0.6) +
    geom_text(data = asset_points, aes(x = risk, y = ret, label = symbol),
              vjust = 1.8, size = 2.5, alpha = 0.7) +
    geom_point(data = opt_point, aes(x = risk, y = ret),
               color = "red", size = 5, shape = 18) +
    annotate("text",
             x     = opt_point$risk,
             y     = opt_point$ret,
             label = sprintf("Óptimo\nU=%.4f", opt_point$utility),
             hjust = -0.15, vjust = 1.5, size = 3.5, fontface = "bold") +
    labs(
      title    = "Frontera Eficiente - Utilidad Cuadrática (QP Exacto)",
      subtitle = sprintf("Horizonte: %d mes(es) (%s) | λ=%.2f | %d-%d",
                         horizon_months, horizon_label, lambda,
                         min(target_years), max(target_years)),
      x = sprintf("Riesgo (%d meses)", horizon_months),
      y = sprintf("Retorno Esperado (%d meses)", horizon_months)
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 10),
      legend.position = "right"
    )
  
  print(g)
} else {
  cat("  ⚠️ Gráfico de frontera omitido por falta de puntos válidos.\n")
}

# === COMPARACIÓN DE LAMBDAS ===
cat("\n", rep("=", 70), "\n", sep = "")
cat("🧪 ANÁLISIS COMPARATIVO: Portafolios por nivel de λ (QP Exacto)\n")
cat(rep("=", 70), "\n\n", sep = "")

lambda_values      <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 7.5, 10.0)
results_comparison <- tibble()

for (lambda_test in lambda_values) {
  cat(sprintf("  Optimizando λ=%.1f... ", lambda_test))
  
  Dmat_test <- 2 * lambda_test * cov_mat
  
  opt_test <- tryCatch({
    quadprog::solve.QP(
      Dmat = Dmat_test,
      dvec = dvec,
      Amat = Amat,
      bvec = bvec,
      meq  = meq
    )
  }, error = function(e) {
    tryCatch({
      quadprog::solve.QP(
        Dmat = 2 * lambda_test * cov_mat + diag(1e-8, n),
        dvec = dvec,
        Amat = Amat,
        bvec = bvec,
        meq  = meq
      )
    }, error = function(e2) {
      cat("❌\n")
      return(NULL)
    })
  })
  
  if (!is.null(opt_test)) {
    w <- opt_test$solution
    w[w < 1e-6] <- 0
    w <- w / sum(w)
    names(w) <- assets
    
    if (sum(w, na.rm = TRUE) > 0.9) {
      ret       <- sum(w * mu)
      vol       <- sqrt(as.numeric(t(w) %*% cov_mat %*% w))
      sharpe    <- (ret - rf_rate_period) / vol
      utility   <- ret - (lambda_test / 2) * (vol^2)
      n_activos <- sum(w > 0.01)
      
      port_ret_test  <- as.numeric(Return.portfolio(df_xts[, assets], weights = w))
      excess_test    <- port_ret_test - rf_rate_period
      downside_test  <- excess_test[excess_test < 0]
      downside_dev_t <- if (length(downside_test) > 0) sqrt(mean(downside_test^2)) else NA_real_
      sortino_test   <- if (!is.na(downside_dev_t) && downside_dev_t > 0) (ret - rf_rate_period) / downside_dev_t else NA_real_
      
      results_comparison <- bind_rows(results_comparison, tibble(
        lambda      = lambda_test,
        retorno     = ret * 100,
        volatilidad = vol * 100,
        sharpe      = sharpe,
        sortino     = sortino_test,
        utilidad    = utility,
        n_activos   = n_activos,
        max_peso    = max(w) * 100
      ))
      
      cat(sprintf("✓ R=%.2f%% | σ=%.2f%% | Sharpe=%.3f | Sortino=%.3f | U=%.4f\n",
                  ret * 100, vol * 100, sharpe, sortino_test, utility))
    } else {
      cat("⚠️ Pesos inválidos\n")
    }
  }
}

if (nrow(results_comparison) > 0) {
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("📊 RESULTADOS COMPARATIVOS\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  print(results_comparison %>%
          mutate(across(c(retorno, volatilidad, sharpe, sortino, utilidad, max_peso),
                        ~sprintf("%.4f", .))),
        n = Inf)
  
  cat("\n📈 Visualización comparativa de lambdas...\n")
  
  g_lambda <- ggplot(results_comparison, aes(x = volatilidad, y = retorno)) +
    geom_path(arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
              size = 1, color = "steelblue", alpha = 0.6) +
    geom_point(aes(color = utilidad, size = lambda)) +
    geom_text(aes(label = sprintf("λ=%.1f", lambda)),
              hjust = -0.3, vjust = 0, size = 3) +
    scale_color_gradient2(
      low      = "#d73027",
      mid      = "#ffffbf",
      high     = "#1a9850",
      midpoint = median(results_comparison$utilidad),
      name     = "Utilidad"
    ) +
    scale_size_continuous(range = c(3, 6), guide = "none") +
    labs(
      title    = "Frontera de Portafolios por Nivel de Aversión al Riesgo (QP Exacto)",
      subtitle = sprintf("Horizonte: %d mes(es) (%s) | %d-%d",
                         horizon_months, horizon_label,
                         min(target_years), max(target_years)),
      x       = "Volatilidad (%)",
      y       = "Retorno Esperado (%)",
      caption = "λ bajo = Agresivo | λ alto = Conservador"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9),
      plot.caption  = element_text(hjust = 1, face = "italic", size = 8)
    )
  
  print(g_lambda)
}

# === 🆕 ANÁLISIS DE MDD DEL PORTAFOLIO ===
cat(sprintf("\n\n=== 📉 ANÁLISIS DE MAXIMUM DRAWDOWN DEL PORTAFOLIO ===\n"))

tickers_portfolio <- pesos$symbol
weights <- setNames(pesos$weight, pesos$symbol)

calc_mdd <- function(returns_vector) {
  if (all(is.na(returns_vector)) || length(na.omit(returns_vector)) < 2) return(NA)
  cumulative_values <- cumprod(1 + returns_vector)
  peak <- cummax(cumulative_values)
  drawdown <- (cumulative_values - peak) / peak
  mdd <- min(drawdown, na.rm = TRUE)
  if (is.infinite(mdd) || is.na(mdd)) return(NA)
  return(mdd)
}

cat(sprintf("📥 Preparando datos históricos del portafolio desde %d...\n", mdd_start_year))

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

ticker_coverage <- portfolio_hist %>%
  group_by(symbol) %>%
  summarise(n_obs = n(), .groups = 'drop') %>%
  arrange(desc(n_obs))

cat("\n  📊 Cobertura por ticker:\n")
print(as.data.frame(ticker_coverage), row.names = FALSE)

portfolio_wide <- portfolio_hist %>%
  select(date, symbol, monthly_return) %>%
  pivot_wider(names_from = symbol, values_from = monthly_return)

cat(sprintf("  ✓ Estructura de datos: %d fechas × %d tickers\n", 
            nrow(portfolio_wide), length(tickers_portfolio)))

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

valid_returns <- portfolio_returns_by_year %>%
  filter(!is.na(portfolio_return), is.finite(portfolio_return))

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

all_years <- data.frame(year = seq(mdd_start_year, max(target_years)))

yearly_mdd_calculated <- valid_returns %>%
  group_by(year) %>%
  summarise(
    mdd = calc_mdd(portfolio_return),
    n_obs = n(),
    avg_tickers = mean(available_tickers, na.rm = TRUE),
    .groups = 'drop'
  )

yearly_mdd <- all_years %>%
  left_join(yearly_mdd_calculated, by = "year") %>%
  mutate(
    has_data = !is.na(mdd),
    n_obs = replace_na(n_obs, 0)
  )

cat(sprintf("  ✓ Años con datos de MDD: %d de %d años posibles\n", 
            sum(yearly_mdd$has_data), nrow(yearly_mdd)))
cat(sprintf("  ✓ Años sin datos: %d\n", sum(!yearly_mdd$has_data)))

yearly_mdd_valid <- yearly_mdd %>%
  dplyr::filter(has_data, is.finite(mdd))

if (nrow(yearly_mdd_valid) >= 1) {
  
  cat(sprintf("\n=== 📊 ESTADÍSTICAS DE MDD ===\n"))
  cat(sprintf("Período analizado: %d-%d\n", mdd_start_year, max(target_years)))
  cat(sprintf("Años con datos: %d | Años sin datos: %d\n\n", 
              sum(yearly_mdd$has_data), sum(!yearly_mdd$has_data)))
  
  if (nrow(yearly_mdd_valid) >= 3) {
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
  
  last_year_data <- yearly_mdd_valid %>%
    dplyr::arrange(desc(year))
  
  if (nrow(last_year_data) > 0) {
    last_year_data <- last_year_data[1, ]
    cat(sprintf("  ➤ MDD más reciente (%d): %.2f%%\n", 
                last_year_data$year, 
                last_year_data$mdd * 100))
  }
  
  cat("\n📊 Generando gráfico de MDD histórico...\n")
  
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
cat(sprintf("   Tasa libre de riesgo: %.3f%% anual (T-Bill 3M) → %.4f%% período\n", 
            rf_rate * 100, rf_rate_period * 100))
cat(sprintf("   Número de activos: %d\n", length(tickers_portfolio)))
cat(sprintf("   Retorno esperado (período): %.2f%%\n", ret_opt * 100))
cat(sprintf("   Retorno anualizado (aprox.): %.2f%%\n", ret_opt * annualization_factor * 100))
cat(sprintf("   Volatilidad (período): %.2f%%\n", sd_opt * 100))
cat(sprintf("   Volatilidad anualizada (aprox.): %.2f%%\n", sd_opt * sqrt(annualization_factor) * 100))
cat(sprintf("   Sharpe Ratio: %.4f\n", sharpe_opt))
if (exists("median_mdd")) {
  cat(sprintf("   MDD típico histórico: %.2f%%\n", median_mdd * 100))
}
# === RESUMEN DE CALIDAD DE DATOS EN PORTAFOLIO FINAL ===
cat(sprintf("\n=== 📊 CALIDAD DE DATOS EN PORTAFOLIO FINAL ===\n"))

portfolio_data_quality <- combined_stats %>%
  dplyr::filter(symbol %in% tickers_portfolio) %>%
  dplyr::select(symbol, n_obs, sharpe_ratio_adjusted, sd_return) %>%
  dplyr::left_join(pesos, by = "symbol") %>%
  dplyr::arrange(desc(weight))

cat(sprintf("  Activos totales en portafolio: %d\n", nrow(portfolio_data_quality)))
cat(sprintf("  Activos con datos ideales (≥%d obs): %d (%.1f%%)\n", 
            ideal_observations,
            sum(portfolio_data_quality$n_obs >= ideal_observations),
            100 * mean(portfolio_data_quality$n_obs >= ideal_observations)))

cat("\n  📋 Detalle del portafolio final:\n")
resumen_final <- portfolio_data_quality %>%
  dplyr::mutate(
    weight_pct = sprintf("%.2f%%", weight * 100),
    sd_pct     = sprintf("%.4f%%", sd_return * 100)
  ) %>%
  dplyr::select(Symbol = symbol, Peso = weight_pct, Obs = n_obs,
                SD = sd_pct, Sharpe = sharpe_ratio_adjusted)

print(as.data.frame(resumen_final), row.names = FALSE)
cat("\n✅ Script completado con éxito!\n")