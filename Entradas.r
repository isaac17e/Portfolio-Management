# ================================
# Criterios de entrada para portafolio DIARIO (v2 - optimizado)
# Entries by Isaac Echeverri
rm(list = ls())
library(quantmod)
library(TTR)
library(dplyr)
library(zoo)

tickers <- c("EVRG", "COR", "BMY", "AMGN", "MCK", "VZ", "LMT", "WM",
             "NEE", "JNJ", "NOC")

fecha_final <- Sys.Date()
fecha_inicio <- fecha_final - 365  # 12 meses para percentiles más estables

evaluar_entrada <- function(ticker) {
  tryCatch({
    cat("Procesando:", ticker, "...")
    
    datos <- suppressWarnings(getSymbols(ticker, src = "yahoo",
                                         from = fecha_inicio,
                                         to = fecha_final,
                                         auto.assign = FALSE,
                                         periodicity = "daily"))
    
    if (is.null(datos) || nrow(datos) < 60) {
      stop("Datos insuficientes")
    }
    
    precio_cierre <- Cl(datos)
    volumen       <- Vo(datos)
    
    # ── Indicadores ─────────────────────────────────────────────────────────
    rsi    <- RSI(precio_cierre, n = 14)
    adx_calc <- ADX(HLC(datos), n = 14)
    adx    <- adx_calc[, "ADX"]
    ema9   <- EMA(precio_cierre, n = 9)   # antes EMA10
    ema21  <- EMA(precio_cierre, n = 21)  # reemplaza MACD como filtro de tendencia
    vol_ma <- SMA(volumen, n = 20)
    
    # Momentum de volumen (últimos 5 días vs anteriores 5)
    vol_reciente <- rollapply(volumen, width = 5, FUN = mean, align = "right", fill = NA)
    vol_anterior <- lag(vol_reciente, 5)
    vol_momentum <- vol_reciente / vol_anterior
    
    # RSI momentum: ¿el RSI de hoy es mayor que hace 3 barras? (impulso alcista)
    rsi_momentum <- rsi - lag(rsi, 3)
    
    # Combinar todo
    todos <- cbind(precio_cierre, rsi, adx, ema9, ema21,
                   volumen, vol_ma, vol_momentum, rsi_momentum)
    colnames(todos) <- c("Cierre", "RSI14", "ADX14", "EMA9", "EMA21",
                         "Volumen", "VolMA20", "VolMomentum", "RSI_Mom")
    
    todos <- na.omit(todos)
    
    if (nrow(todos) < 2) stop("Datos insuficientes tras indicadores")
    
    ultimo    <- tail(todos, 1)
    penultimo <- tail(todos, 2)[1, ]  # barra anterior (para detectar cruces)
    
    cierre     <- as.numeric(ultimo[, "Cierre"])
    rsi_v      <- as.numeric(ultimo[, "RSI14"])
    adx_v      <- as.numeric(ultimo[, "ADX14"])
    ema9_v     <- as.numeric(ultimo[, "EMA9"])
    ema21_v    <- as.numeric(ultimo[, "EMA21"])
    vol_actual <- as.numeric(ultimo[, "Volumen"])
    vol_prom   <- as.numeric(ultimo[, "VolMA20"])
    vol_mom    <- as.numeric(ultimo[, "VolMomentum"])
    rsi_mom_v  <- as.numeric(ultimo[, "RSI_Mom"])
    
    ema9_ant  <- as.numeric(penultimo[, "EMA9"])
    ema21_ant <- as.numeric(penultimo[, "EMA21"])
    
    # Umbral ADX dinámico (percentil 35 histórico — ligeramente más exigente)
    adx_historico <- as.numeric(na.omit(todos[, "ADX14"]))
    umbral_adx    <- quantile(adx_historico, 0.35, na.rm = TRUE)
    
    # ── 6 CONDICIONES con peso ───────────────────────────────────────────────
    cond1 <- cierre > ema9_v                                             # precio sobre EMA9
    cond2 <- ema9_v > ema21_v                                            # EMA9 sobre EMA21 (tendencia)
    cond3 <- (ema9_v > ema21_v) && (ema9_ant <= ema21_ant)               # cruce alcista reciente
    cond4 <- rsi_v >= 45 && rsi_v <= 68                                  # zona RSI saludable
    cond5 <- rsi_mom_v > 0                                               # RSI ganando impulso
    cond6 <- (vol_actual > vol_prom * 0.7) && (vol_mom > 1.0) &&
      (adx_v > umbral_adx)                                        # fuerza + participación
    
    puntaje <- sum(c(
      cond1 * 1.0,
      cond2 * 1.5,
      cond3 * 2.0,
      cond4 * 1.0,
      cond5 * 0.5,
      cond6 * 1.0
    ))
    
    # Número de condiciones booleanas cumplidas (para referencia)
    condiciones_cumplidas <- sum(c(cond1, cond2, cond3, cond4, cond5, cond6))
    
    decision <- ifelse(puntaje >= 5.0, "✅ ENTRAR FUERTE",
                       ifelse(puntaje >= 3.0, "⚠️ ENTRAR CAUTELOSO",
                              "❌ ESPERAR"))
    
    # Señales visuales
    senales <- paste0(
      ifelse(cond1, "↗️",  "↘️"),  " ",  # precio vs EMA9
      ifelse(cond2, "📈",  "📉"),  " ",  # EMA9 vs EMA21
      ifelse(cond3, "⚡",  "·"),   " ",  # cruce reciente
      ifelse(cond4, "🎯",  "⚠️"),  " ",  # RSI zona
      ifelse(cond5, "🔥",  "❄️"),  " ",  # RSI momentum
      ifelse(cond6, "💪",  "😴")         # fuerza + volumen
    )
    
    cat(" ✓\n")
    
    data.frame(
      Ticker     = ticker,
      Fecha      = as.character(index(ultimo)),
      Cierre     = round(cierre, 2),
      RSI14      = round(rsi_v, 1),
      RSI_Mom    = round(rsi_mom_v, 2),
      ADX14      = round(adx_v, 1),
      ADX_Umbral = round(umbral_adx, 1),
      EMA9       = round(ema9_v, 2),
      EMA21      = round(ema21_v, 2),
      VolMom     = round(vol_mom, 2),
      Puntaje    = round(puntaje, 1),
      Conds      = condiciones_cumplidas,
      Señales    = senales,
      Decisión   = decision,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat(" ✗ Error:", e$message, "\n")
    data.frame(
      Ticker     = ticker,
      Fecha      = NA, Cierre = NA, RSI14 = NA, RSI_Mom = NA,
      ADX14      = NA, ADX_Umbral = NA, EMA9 = NA, EMA21 = NA,
      VolMom     = NA, Puntaje = 0, Conds = 0,
      Señales    = "",
      Decisión   = paste0("⚠️ Error: ", substr(e$message, 1, 30)),
      stringsAsFactors = FALSE
    )
  })
}

# ── EJECUCIÓN ────────────────────────────────────────────────────────────────
cat("🔍 Analizando", length(tickers), "activos...\n\n")

resultado_lista <- list()
for (i in seq_along(tickers)) {
  resultado_lista[[i]] <- evaluar_entrada(tickers[i])
  if (i < length(tickers)) Sys.sleep(1)
}

resultado <- do.call(rbind, resultado_lista)
rownames(resultado) <- NULL

# Ordenar por puntaje ponderado
resultado <- resultado %>% arrange(desc(Puntaje))

cat("\n")
print(resultado)

# ── RESUMEN EJECUTIVO ────────────────────────────────────────────────────────
cat("\n===== RESUMEN EJECUTIVO =====\n")
cat("ENTRAR FUERTE:    ", sum(resultado$Decisión == "✅ ENTRAR FUERTE",    na.rm = TRUE), "\n")
cat("ENTRAR CAUTELOSO: ", sum(resultado$Decisión == "⚠️ ENTRAR CAUTELOSO", na.rm = TRUE), "\n")
cat("ESPERAR:          ", sum(resultado$Decisión == "❌ ESPERAR",           na.rm = TRUE), "\n")

mejores <- resultado %>%
  filter(Decisión %in% c("✅ ENTRAR FUERTE", "⚠️ ENTRAR CAUTELOSO"))

if (nrow(mejores) > 0) {
  cat("\n📈 MEJORES OPORTUNIDADES:\n")
  print(mejores[, c("Ticker", "Cierre", "RSI14", "RSI_Mom",
                    "EMA9", "EMA21", "ADX14", "Puntaje", "Decisión")])
} else {
  cat("\n⏳ No hay oportunidades claras actualmente.\n")
}

cat("\n💡 Leyenda de señales:\n")
cat("  ↗️/↘️  = Precio sobre/bajo EMA9\n")
cat("  📈/📉  = EMA9 sobre/bajo EMA21 (tendencia de fondo)\n")
cat("  ⚡/·   = Cruce alcista EMA9/EMA21 reciente / sin cruce\n")
cat("  🎯/⚠️  = RSI en zona saludable [45-68] / fuera\n")
cat("  🔥/❄️  = RSI ganando/perdiendo impulso (RSI Mom)\n")
cat("  💪/😴  = ADX fuerte + volumen saludable / débil\n")
cat("\n📐 Sistema de puntaje ponderado (máx 7.0):\n")
cat("  Precio > EMA9       → 1.0 pts\n")
cat("  EMA9 > EMA21        → 1.5 pts  (tendencia confirmada)\n")
cat("  Cruce EMA9/EMA21 ↑  → 2.0 pts  (señal de entrada)\n")
cat("  RSI en [45, 68]     → 1.0 pts\n")
cat("  RSI momentum +      → 0.5 pts  (bonus)\n")
cat("  ADX + Volumen OK    → 1.0 pts\n")
cat("  ─────────────────────────────\n")
cat("  ENTRAR FUERTE       ≥ 5.0 pts\n")
cat("  ENTRAR CAUTELOSO    ≥ 3.0 pts\n")