# ================================
# Criterios de entrada para portafolio DIARIO (versión simplificada)
# Entries by Isaac Echeverri - Optimizado
# ================================
rm(list = ls())
library(quantmod)
library(TTR)
library(dplyr)

tickers <- c("GD", "LHX", "DB1.DE", "AMZN", "CME", "COO", "KR", "ESLT", "FER", 
             "PGR", "CB", "NG.L", "CTSH", "CCEP", "LMT", "GOOGL", "NOC")

fecha_final <- Sys.Date()
fecha_inicio <- fecha_final - 180  # 6 meses

evaluar_entrada <- function(ticker) {
  tryCatch({
    cat("Procesando:", ticker, "...")
    
    datos <- suppressWarnings(getSymbols(ticker, src = "yahoo",
                                         from = fecha_inicio,
                                         to = fecha_final,
                                         auto.assign = FALSE,
                                         periodicity = "daily"))
    
    if (is.null(datos) || nrow(datos) < 50) {
      stop("Datos insuficientes")
    }
    
    precio_cierre <- Cl(datos)
    volumen <- Vo(datos)
    
    # Indicadores
    rsi <- RSI(precio_cierre, n = 14)
    adx_calc <- ADX(HLC(datos), n = 14)
    adx <- adx_calc[, "ADX"]
    ema10 <- EMA(precio_cierre, n = 10)
    macd_calc <- MACD(precio_cierre, nFast = 8, nSlow = 17, nSig = 9)
    macd <- macd_calc[, "macd"]
    signal <- macd_calc[, "signal"]
    vol_ma <- SMA(volumen, n = 20)
    
    # Momentum de volumen (últimos 5 días vs anteriores 5)
    vol_reciente <- rollapply(volumen, width = 5, FUN = mean, align = "right", fill = NA)
    vol_anterior <- lag(vol_reciente, 5)
    vol_momentum <- vol_reciente / vol_anterior
    
    # Combinar
    todos <- cbind(precio_cierre, rsi, adx, ema10, macd, signal, 
                   volumen, vol_ma, vol_momentum)
    colnames(todos) <- c("Cierre", "RSI14", "ADX14", "EMA10", 
                         "MACD", "Signal", "Volumen", "VolMA20", "VolMomentum")
    
    todos <- na.omit(todos)
    
    if (nrow(todos) < 2) stop("Datos insuficientes tras indicadores")
    
    ultimo <- tail(todos, 1)
    
    cierre <- as.numeric(ultimo[1, "Cierre"])
    rsi_v <- as.numeric(ultimo[1, "RSI14"])
    adx_v <- as.numeric(ultimo[1, "ADX14"])
    ema10_v <- as.numeric(ultimo[1, "EMA10"])
    macd_v <- as.numeric(ultimo[1, "MACD"])
    signal_v <- as.numeric(ultimo[1, "Signal"])
    vol_actual <- as.numeric(ultimo[1, "Volumen"])
    vol_prom <- as.numeric(ultimo[1, "VolMA20"])
    vol_mom <- as.numeric(ultimo[1, "VolMomentum"])
    
    # Umbral ADX dinámico (percentil 30 histórico)
    adx_historico <- as.numeric(na.omit(todos[, "ADX14"]))
    umbral_adx <- quantile(adx_historico, 0.30, na.rm = TRUE)
    
    # ===== 5 CONDICIONES =====
    cond1 <- cierre > ema10_v                                          # Precio sobre EMA10
    cond2 <- rsi_v < 65                                                 # RSI no sobrecomprado
    cond3 <- macd_v > signal_v                                          # MACD positivo
    cond4 <- adx_v > umbral_adx                                          # Tendencia fuerte relativa
    cond5 <- (vol_actual > vol_prom * 0.7) && (vol_mom > 1.0)           # Volumen saludable
    
    condiciones_cumplidas <- sum(c(cond1, cond2, cond3, cond4, cond5))
    
    decision <- ifelse(condiciones_cumplidas >= 4, "✅ ENTRAR FUERTE",
                       ifelse(condiciones_cumplidas == 3, "⚠️ ENTRAR CAUTELOSO",
                              "❌ ESPERAR"))
    
    # Señales visuales simplificadas
    senales <- paste0(
      ifelse(cond1, "↗️", "↘️"), " ",
      ifelse(cond3, "🚀", "🐌"), " ",
      ifelse(cond4, "💪", "😴"), " ",
      ifelse(cond5, "📊", "📉")
    )
    
    cat(" ✓\n")
    
    data.frame(
      Ticker = ticker,
      Fecha = as.character(index(ultimo)),
      Cierre = round(cierre, 2),
      RSI14 = round(rsi_v, 1),
      ADX14 = round(adx_v, 1),
      ADX_Umbral = round(umbral_adx, 1),
      MACD = round(macd_v, 3),
      EMA10 = round(ema10_v, 2),
      VolMom = round(vol_mom, 2),
      Condiciones = condiciones_cumplidas,
      Señales = senales,
      Decisión = decision,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat(" ✗ Error:", e$message, "\n")
    data.frame(
      Ticker = ticker,
      Fecha = NA,
      Cierre = NA,
      RSI14 = NA,
      ADX14 = NA,
      ADX_Umbral = NA,
      MACD = NA,
      EMA10 = NA,
      VolMom = NA,
      Condiciones = 0,
      Señales = "",
      Decisión = paste0("⚠️ Error: ", substr(e$message, 1, 30)),
      stringsAsFactors = FALSE
    )
  })
}

# ===== EJECUCIÓN =====
cat("🔍 Analizando", length(tickers), "activos...\n\n")

resultado_lista <- list()
for (i in seq_along(tickers)) {
  resultado_lista[[i]] <- evaluar_entrada(tickers[i])
  if (i < length(tickers)) Sys.sleep(1)
}

resultado <- do.call(rbind, resultado_lista)

# Reset rownames para evitar el "30%2" raro
rownames(resultado) <- NULL

# Ordenar por condiciones
resultado <- resultado %>% arrange(desc(Condiciones))

# Mostrar
cat("\n")
print(resultado)

# Resumen
cat("\n===== RESUMEN EJECUTIVO =====\n")
cat("ENTRAR FUERTE:", sum(resultado$Decisión == "✅ ENTRAR FUERTE", na.rm = TRUE), "\n")
cat("ENTRAR CAUTELOSO:", sum(resultado$Decisión == "⚠️ ENTRAR CAUTELOSO", na.rm = TRUE), "\n")
cat("ESPERAR:", sum(resultado$Decisión == "❌ ESPERAR", na.rm = TRUE), "\n")

# Mejores oportunidades
mejores <- resultado %>% filter(Decisión %in% c("✅ ENTRAR FUERTE", "⚠️ ENTRAR CAUTELOSO"))
if (nrow(mejores) > 0) {
  cat("\n📈 MEJORES OPORTUNIDADES:\n")
  print(mejores[, c("Ticker", "Cierre", "RSI14", "ADX14", "ADX_Umbral", "Condiciones", "Decisión")])
} else {
  cat("\n⏳ No hay oportunidades claras actualmente.\n")
}

cat("\n💡 Leyenda de señales:\n")
cat("  ↗️/↘️ = Precio >/< EMA10\n")
cat("  🚀/🐌 = MACD positivo/negativo\n")
cat("  💪/😴 = ADX fuerte/débil (vs percentil 30 histórico)\n")
cat("  📊/📉 = Volumen saludable/bajo\n")