# ================================
# Correlación del Portafolio contra SPY
# By: Isaac Echeverri
# ================================
rm(list = ls())

library(quantmod)

# ===== CONFIGURACIÓN DEL PORTAFOLIO =====
tickers <- c("EVRG", "COR", "BMY", "AMGN", "MCK", "VZ", "LMT", "WM", 
             "NEE", "JNJ", "NOC")
weights <- c(0.196, 0.153, 0.1524, 0.1241, 0.0959, 0.0814, 0.0633, 0.0431, 
             0.0394, 0.03, 0.014)

benchmark <- "SPY"

# Fechas
start_date <- Sys.Date() - 180
end_date <- Sys.Date()

cat("📊 ANÁLISIS DE CORRELACIÓN DEL PORTAFOLIO CON SPY\n")
cat("══════════════════════════════════════════════════\n\n")
cat("Periodo:", format(start_date, "%Y-%m-%d"), "al", format(end_date, "%Y-%m-%d"), "\n")
cat("Activos:", length(tickers), "\n")
cat("Suma de pesos:", round(sum(weights), 3), "\n\n")

# Validar pesos
if (abs(sum(weights) - 1) > 0.001) {
  cat("⚠️  ADVERTENCIA: Los pesos suman", round(sum(weights), 3), "\n")
  cat("    Normalizando a 1.00...\n\n")
  weights <- weights / sum(weights)
}

# Descargar datos
cat("Descargando datos del mercado...\n")
all_symbols <- c(tickers, benchmark)
suppressWarnings(
  getSymbols(all_symbols, src = "yahoo", 
             from = start_date, to = end_date, 
             auto.assign = TRUE, warnings = FALSE)
)

# Función para obtener retornos
get_returns <- function(symbol) {
  tryCatch({
    if (exists(symbol)) {
      prices <- Ad(get(symbol))
      returns <- dailyReturn(na.omit(prices))
      return(na.omit(returns))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Obtener retornos del SPY
spy_returns <- get_returns(benchmark)

if (is.null(spy_returns) || nrow(spy_returns) < 10) {
  stop("❌ Error: No se pudieron obtener datos del SPY")
}

# Arrays para resultados
correlations <- c()
valid_tickers <- c()
valid_weights <- c()

cat("\n📈 CORRELACIONES Y PESOS POR ACTIVO:\n")
cat("─────────────────────────────────────────────────\n")
cat(sprintf("%-10s %8s %12s %10s\n", "Ticker", "Peso", "Correlación", "Contrib."))
cat("─────────────────────────────────────────────────\n")

for (i in 1:length(tickers)) {
  ticker <- tickers[i]
  weight <- weights[i]
  
  asset_returns <- get_returns(ticker)
  
  if (!is.null(asset_returns) && nrow(asset_returns) >= 10) {
    # Alinear fechas con SPY
    combined_returns <- merge(asset_returns, spy_returns, join = "inner")
    
    if (nrow(combined_returns) >= 10) {
      # Calcular correlación
      corr <- cor(combined_returns[, 1], combined_returns[, 2], 
                  use = "complete.obs")
      
      # Contribución ponderada
      contribution <- corr * weight
      
      correlations <- c(correlations, corr)
      valid_tickers <- c(valid_tickers, ticker)
      valid_weights <- c(valid_weights, weight)
      
      # Emoji según correlación
      emoji <- ifelse(abs(corr) > 0.7, "🔴", 
                      ifelse(abs(corr) > 0.4, "🟡", "🟢"))
      
      cat(sprintf("%s %-8s %7.1f%% %+10.3f %+9.3f\n", 
                  emoji, ticker, weight*100, corr, contribution))
    }
  } else {
    cat(sprintf("⚠️  %-8s %7.1f%%   Sin datos\n", ticker, weight*100))
  }
}

# ===== CÁLCULOS FINALES =====
if (length(correlations) > 0) {
  
  # Correlación simple (sin ponderar)
  corr_simple <- mean(correlations)
  
  # Correlación ponderada (LA IMPORTANTE)
  corr_weighted <- sum(correlations * valid_weights) / sum(valid_weights)
  
  # Estadísticas adicionales
  corr_median <- median(correlations)
  corr_max <- max(correlations)
  corr_min <- min(correlations)
  corr_sd <- sd(correlations)
  
  # Clasificación
  high_corr <- sum(abs(correlations) > 0.7)
  medium_corr <- sum(abs(correlations) > 0.4 & abs(correlations) <= 0.7)
  low_corr <- sum(abs(correlations) <= 0.4)
  
  # Peso analizado
  analyzed_weight <- sum(valid_weights)
  
  cat("\n\n🎯 CORRELACIÓN DEL PORTAFOLIO CON SPY:\n")
  cat("══════════════════════════════════════════════════\n")
  cat(sprintf("Correlación Ponderada:  %+.3f ⭐ (LA REAL)\n", corr_weighted))
  cat(sprintf("Correlación Simple:     %+.3f (referencia)\n", corr_simple))
  cat(sprintf("Correlación Mediana:    %+.3f\n", corr_median))
  cat(sprintf("Desviación Estándar:     %.3f\n", corr_sd))
  cat(sprintf("Rango: [%.3f, %.3f]\n", corr_min, corr_max))
  
  cat("\n📊 CARACTERÍSTICAS DEL PORTAFOLIO:\n")
  cat("─────────────────────────────────────────────────\n")
  cat(sprintf("Peso analizado:         %.1f%% del total\n", analyzed_weight*100))
  
  cat("\n📈 DISTRIBUCIÓN DE CORRELACIONES:\n")
  cat("─────────────────────────────────────────────────\n")
  cat(sprintf("🔴 Alta (>0.7):     %2d activos (%.1f%% del peso)\n", 
              high_corr, 
              100*sum(valid_weights[abs(correlations) > 0.7])))
  cat(sprintf("🟡 Media (0.4-0.7): %2d activos (%.1f%% del peso)\n", 
              medium_corr, 
              100*sum(valid_weights[abs(correlations) > 0.4 & abs(correlations) <= 0.7])))
  cat(sprintf("🟢 Baja (<0.4):     %2d activos (%.1f%% del peso)\n", 
              low_corr, 
              100*sum(valid_weights[abs(correlations) <= 0.4])))
  
  cat("\n💡 INTERPRETACIÓN:\n")
  cat("─────────────────────────────────────────────────\n")
  
  if (abs(corr_weighted) > 0.7) {
    cat("⚠️  Portafolio ALTAMENTE correlacionado con SPY\n")
    cat("    → Baja diversificación respecto al mercado\n")
    cat("    → Alto riesgo sistemático\n")
    cat("    → Se mueve muy similar al S&P 500\n")
  } else if (abs(corr_weighted) > 0.4) {
    cat("✓  Portafolio MODERADAMENTE correlacionado con SPY\n")
    cat("    → Diversificación aceptable\n")
    cat("    → Balance entre seguir el mercado y autonomía\n")
    cat("    → Algunas posiciones defensivas efectivas\n")
  } else {
    cat("✓✓ Portafolio POCO correlacionado con SPY\n")
    cat("    → Excelente diversificación\n")
    cat("    → Bajo riesgo sistemático\n")
    cat("    → Comportamiento independiente del mercado\n")
  }
  
  # Diferencia entre correlación simple y ponderada
  diff <- abs(corr_weighted - corr_simple)
  if (diff > 0.1) {
    cat(sprintf("\n⚠️  Nota: Gran diferencia entre correlación simple (%.3f) y\n", corr_simple))
    cat(sprintf("    ponderada (%.3f). Los pesos son determinantes.\n", corr_weighted))
  }
  
  # Top contribuyentes
  contributions <- correlations * valid_weights
  top_indices <- order(abs(contributions), decreasing = TRUE)[1:min(5, length(contributions))]
  
  cat("\n🔝 TOP 5 CONTRIBUYENTES A LA CORRELACIÓN:\n")
  cat("─────────────────────────────────────────────────\n")
  for (i in top_indices) {
    cat(sprintf("   %s (%.1f%% peso): corr %+.3f → contrib %+.3f\n", 
                valid_tickers[i], 
                valid_weights[i]*100,
                correlations[i],
                contributions[i]))
  }
  
  # Activos descorrelacionados
  low_corr_indices <- order(abs(correlations))[1:min(3, length(correlations))]
  
  cat("\n🟢 ACTIVOS MÁS DESCORRELACIONADOS (Diversificadores):\n")
  cat("─────────────────────────────────────────────────\n")
  for (i in low_corr_indices) {
    cat(sprintf("   %s (%.1f%% peso): corr %+.3f\n", 
                valid_tickers[i],
                valid_weights[i]*100,
                correlations[i]))
  }
  
  # Sugerencias
  cat("\n💼 SUGERENCIAS DE OPTIMIZACIÓN:\n")
  cat("─────────────────────────────────────────────────\n")
  
  if (corr_weighted > 0.6) {
    high_corr_weight <- sum(valid_weights[correlations > 0.7])
    if (high_corr_weight > 0.3) {
      cat(sprintf("• Reducir exposición a activos alta correlación (%.1f%% actual)\n", 
                  high_corr_weight*100))
    }
    low_corr_weight <- sum(valid_weights[abs(correlations) < 0.4])
    if (low_corr_weight < 0.3) {
      cat(sprintf("• Aumentar activos descorrelacionados (%.1f%% actual)\n", 
                  low_corr_weight*100))
    }
  } else {
    cat("✓ El portafolio tiene una buena diversificación\n")
  }
  
  # Crear tabla de resultados
  results <- data.frame(
    Ticker = valid_tickers,
    Peso_Pct = round(valid_weights * 100, 1),
    Correlacion = round(correlations, 3),
    Contribucion = round(contributions, 3),
    Clasificacion = ifelse(abs(correlations) > 0.7, "Alta",
                           ifelse(abs(correlations) > 0.4, "Media", "Baja")),
    stringsAsFactors = FALSE
  )
  results <- results[order(results$Contribucion, decreasing = TRUE), ]
  
  cat("\n\n📋 TABLA COMPLETA DE RESULTADOS:\n")
  cat("─────────────────────────────────────────────────\n")
  print(results, row.names = FALSE)
  
} else {
  cat("\n❌ Error: No se pudieron calcular correlaciones.\n")
  cat("   Verifica los símbolos y conexión a internet.\n")
}

cat("\n══════════════════════════════════════════════════\n")
cat("✓ Análisis completado\n")