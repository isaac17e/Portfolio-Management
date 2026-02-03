# Por: Isaac Echeverri
# Rendimiento y rentabilidad con horizonte temporal extendido (1, 2 o 3 meses)
rm(list = ls())

library(quantmod)
library(tidyverse)
library(gridExtra)

# ============================================================================
# FECHA DE ANÁLISIS
# ============================================================================
fecha_cierre <- "2026-01-30"  # Última fecha con datos completos

today <- as.Date(fecha_cierre)

# ============================================================================
# CONFIGURACIÓN DE HORIZONTE TEMPORAL
# ============================================================================
# Mes inicial de análisis
inicio_mes <- 01  

# HORIZONTE TEMPORAL: ¿Cuántos meses analizar?
# Opciones: 1, 2 o 3
horizonte_meses <- 1  # Cambia según necesites

# Generar secuencia de meses a analizar
meses_analizar <- ((inicio_mes - 1) + 0:(horizonte_meses - 1)) %% 12 + 1

# ============================================================================
# MODO DE ANÁLISIS
# ============================================================================
# 1 = Solo evaluar portafolio nuevo (ignora rebalanceo)
# 0 = Aplicar rebalanceo con optimización y restricción
solo_evaluar <- 1  

# ============================================================================
# CONFIGURACIÓN DE PORTAFOLIO
# ============================================================================

# Portafolio nuevo propuesto
tickers <- c("AJG", "ABBV", "EW", "KDP", "ENTG", "FDX", "JKHY", "TJX", "NEE", 
             "ZTS", "TLT", "GLD", "COR", "PEP", "DUK", "FFIV")
weights <- c(0.114, 0.112, 0.108, 0.106, 0.104, 0.088, 0.088, 0.084, 0.062, 
             0.056, 0.036, 0.016, 0.014, 0.006, 0.004, 0.002)

# ============================================================================
# ESTRATEGIA DE REBALANCEO (Solo se usa si solo_evaluar = 0)
# ============================================================================
estrategia <- "INTERCAMBIAR"  # Cambia a "AGREGAR" si prefieres expandir

# Tickers que quieres mantener (con alta convicción)
tickers_mantener <- c("GD", "LHX", "LMT")

# RESTRICCIÓN: Peso mínimo TOTAL para estos tickers (como decimal)
# Ejemplo: 0.10 = mínimo 10% del portafolio total en estos tickers
# El código optimizará los pesos pero garantizará este mínimo
peso_minimo_mantener <- 0.10  # Ajusta según tu criterio

# Solo para modo INTERCAMBIAR: tickers del portafolio propuesto a reemplazar
tickers_a_reemplazar <- c("ABBV", "NG.L", "FER")

# ============================================================================
# REBALANCEO CON OPTIMIZACIÓN Y RESTRICCIÓN DE PESO MÍNIMO
# ============================================================================

if (solo_evaluar == 0 && length(tickers_mantener) > 0) {
  
  # Guardar portafolio original para comparación
  tickers_originales <- tickers
  weights_originales <- weights
  
  cat("\n=== REBALANCEO CON OPTIMIZACIÓN COMPLETA ===\n")
  cat("Estrategia:", estrategia, "\n")
  cat("Restricción: Peso mínimo total para tickers a mantener:", 
      round(peso_minimo_mantener * 100, 2), "%\n\n")
  
  # Crear universo combinado de tickers según estrategia
  if (estrategia == "INTERCAMBIAR") {
    cat("→ Modo: INTERCAMBIAR (reemplazar activos específicos)\n")
    cat("Tickers a mantener:", paste(tickers_mantener, collapse = ", "), "\n")
    cat("Tickers a reemplazar:", paste(tickers_a_reemplazar, collapse = ", "), "\n\n")
    
    if (length(tickers_mantener) != length(tickers_a_reemplazar)) {
      stop("ERROR: Debes especificar el mismo número de tickers a mantener y a reemplazar")
    }
    
    # Eliminar tickers a reemplazar del portafolio propuesto
    tickers_sin_reemplazar <- tickers[!tickers %in% tickers_a_reemplazar]
    weights_sin_reemplazar <- weights[!tickers %in% tickers_a_reemplazar]
    
    # Crear universo completo (mantener + resto del propuesto)
    universo_tickers <- c(tickers_mantener, tickers_sin_reemplazar)
    
    # Usar pesos del portafolio propuesto como base
    # Para tickers a mantener, usar pesos promedio como punto de partida
    peso_promedio_mantener <- mean(weights)
    weights_base_mantener <- rep(peso_promedio_mantener, length(tickers_mantener))
    universo_weights_base <- c(weights_base_mantener, weights_sin_reemplazar)
    
    cat("Reemplazo aplicado:\n")
    for (i in seq_along(tickers_mantener)) {
      cat("  ✓", tickers_a_reemplazar[i], "→", tickers_mantener[i], "\n")
    }
    cat("\n")
    
  } else if (estrategia == "AGREGAR") {
    cat("→ Modo: AGREGAR (expandir portafolio)\n")
    cat("Tickers a mantener/agregar:", paste(tickers_mantener, collapse = ", "), "\n\n")
    
    # Verificar duplicados
    tickers_ya_existentes <- tickers_mantener[tickers_mantener %in% tickers]
    tickers_nuevos_a_agregar <- tickers_mantener[!tickers_mantener %in% tickers]
    
    # Eliminar duplicados del portafolio propuesto
    tickers_filtrados <- tickers[!tickers %in% tickers_ya_existentes]
    weights_filtrados <- weights[!tickers %in% tickers_ya_existentes]
    
    # Crear universo completo
    universo_tickers <- c(tickers_mantener, tickers_filtrados)
    
    # Pesos base: usar pesos existentes para duplicados, promedio para nuevos
    weights_base_mantener <- numeric(length(tickers_mantener))
    for (i in seq_along(tickers_mantener)) {
      if (tickers_mantener[i] %in% tickers) {
        # Usar peso original
        weights_base_mantener[i] <- weights[which(tickers == tickers_mantener[i])]
      } else {
        # Usar peso promedio para nuevos
        weights_base_mantener[i] <- mean(weights)
      }
    }
    
    universo_weights_base <- c(weights_base_mantener, weights_filtrados)
    
    if (length(tickers_ya_existentes) > 0) {
      cat("  ⚠ Eliminados por duplicado:", paste(tickers_ya_existentes, collapse = ", "), "\n")
    }
    if (length(tickers_nuevos_a_agregar) > 0) {
      cat("  ✓ Agregados al portafolio:", paste(tickers_nuevos_a_agregar, collapse = ", "), "\n")
    }
    cat("\n")
  }
  
  # ========================================================================
  # OPTIMIZACIÓN CON RESTRICCIÓN DE PESO MÍNIMO
  # ========================================================================
  cat("=== OPTIMIZANDO PESOS CON RESTRICCIÓN ===\n")
  
  # Normalizar pesos base
  universo_weights_base <- universo_weights_base / sum(universo_weights_base)
  
  n_mantener <- length(tickers_mantener)
  n_total <- length(universo_tickers)
  
  # Estrategia de optimización:
  # 1. Asignar el peso mínimo requerido a los tickers a mantener
  # 2. Distribuir el resto del peso (1 - peso_minimo) proporcionalmente
  
  # Calcular peso base para cada ticker a mantener (proporcional a sus pesos originales)
  pesos_mantener_proporcional <- universo_weights_base[1:n_mantener]
  pesos_mantener_proporcional <- pesos_mantener_proporcional / sum(pesos_mantener_proporcional)
  
  # Asignar el peso mínimo proporcionalmente entre tickers a mantener
  pesos_mantener_minimo <- pesos_mantener_proporcional * peso_minimo_mantener
  
  # Peso disponible para distribución general
  peso_disponible <- 1 - peso_minimo_mantener
  
  # Distribuir peso disponible proporcionalmente entre TODOS los tickers
  weights_proporcional <- universo_weights_base * peso_disponible
  
  # Combinar: tickers a mantener reciben su mínimo + su proporción del resto
  weights_final <- weights_proporcional
  weights_final[1:n_mantener] <- weights_final[1:n_mantener] + pesos_mantener_minimo
  
  # Normalizar para asegurar suma exacta = 1
  weights_final <- weights_final / sum(weights_final)
  
  # Actualizar tickers y weights globales
  tickers <- universo_tickers
  weights <- weights_final
  
  cat("✓ Optimización completada\n")
  cat("  • Total de activos en portafolio:", n_total, "\n")
  cat("  • Peso asignado a tickers a mantener:", 
      round(sum(weights[1:n_mantener]) * 100, 2), "%")
  if (sum(weights[1:n_mantener]) > peso_minimo_mantener) {
    cat(" (mínimo requerido:", round(peso_minimo_mantener * 100, 2), "%)\n")
  } else {
    cat("\n")
  }
  cat("  • Peso en resto del portafolio:", 
      round(sum(weights[(n_mantener+1):n_total]) * 100, 2), "%\n\n")
  
  # ========================================================================
  # MOSTRAR COMPARACIÓN ANTES vs DESPUÉS
  # ========================================================================
  cat("=== COMPARACIÓN: ANTES vs DESPUÉS DEL REBALANCEO ===\n\n")
  
  cat("PORTAFOLIO PROPUESTO ORIGINAL:\n")
  original_df <- data.frame(
    Ticker = tickers_originales,
    Peso = paste0(round(weights_originales * 100, 2), "%"),
    Peso_Decimal = weights_originales
  ) %>% arrange(desc(Peso_Decimal))
  print(head(original_df %>% select(Ticker, Peso), 10))
  if (length(tickers_originales) > 10) {
    cat("... (", length(tickers_originales), "activos totales)\n")
  }
  cat("Suma:", round(sum(weights_originales) * 100, 2), "%\n\n")
  
  cat("PORTAFOLIO FINAL OPTIMIZADO:\n")
  portfolio_df <- data.frame(
    Ticker = tickers,
    Peso_Porcentaje = paste0(round(weights * 100, 2), "%"),
    Peso_Decimal = round(weights, 4),
    Tipo = ifelse(tickers %in% tickers_mantener, "MANTENER", "OPTIMIZADO")
  ) %>% arrange(desc(Peso_Decimal))
  
  print(portfolio_df %>% select(Ticker, Peso_Porcentaje, Tipo))
  
  cat("\n--- RESUMEN DEL PORTAFOLIO OPTIMIZADO ---\n")
  cat("Total de activos:", length(tickers), "\n")
  cat("Activos a mantener (con restricción):", sum(tickers %in% tickers_mantener), "\n")
  cat("Peso total en activos a mantener:", 
      round(sum(weights[tickers %in% tickers_mantener]) * 100, 2), "%")
  cat(" (mínimo requerido:", round(peso_minimo_mantener * 100, 2), "%)\n")
  cat("Suma total de pesos:", round(sum(weights), 4), "\n\n")
  
} else if (solo_evaluar == 1) {
  cat("\n=== MODO: SOLO EVALUACIÓN ===\n")
  cat("Evaluando portafolio propuesto sin aplicar rebalanceo\n")
  cat("Total de activos:", length(tickers), "\n\n")
  
  portfolio_df <- data.frame(
    Ticker = tickers,
    Peso_Porcentaje = paste0(round(weights * 100, 2), "%"),
    Peso_Decimal = round(weights, 4)
  ) %>% arrange(desc(Peso_Decimal))
  
  print(portfolio_df %>% select(Ticker, Peso_Porcentaje))
  
  cat("\n--- RESUMEN DEL PORTAFOLIO ---\n")
  cat("Suma total de pesos:", round(sum(weights), 4), "\n")
  
  if (sum(weights) < 0.99) {
    cat("⚠️ NOTA: El portafolio suma", round(sum(weights) * 100, 2), "% (queda", 
        round((1 - sum(weights)) * 100, 2), "% sin invertir)\n")
  }
  cat("\n")
  
} else {
  cat("\n=== SIN REBALANCEO ===\n")
  cat("Usando portafolio original sin modificaciones\n\n")
}

# ============================================================================
# ANÁLISIS DE RENDIMIENTOS MULTI-HORIZONTE
# ============================================================================

# Configuración de años (DINÁMICO basado en fecha_cierre)
year_inicio <- 2020  # Año desde el cual iniciar el análisis
year_current <- as.numeric(format(today, "%Y"))  # Año actual dinámico
years <- year_inicio:(year_current - 1)  # Años completos hasta el año anterior al actual

# Función para obtener nombre del mes
get_month_name <- function(month_num) {
  format(as.Date(paste("2000", sprintf("%02d", month_num), "01", sep="-")), "%B")
}

# Crear nombres de meses para títulos
nombres_meses <- sapply(meses_analizar, get_month_name)
periodo_str <- if (horizonte_meses == 1) {
  nombres_meses[1]
} else {
  paste(nombres_meses[1], "-", nombres_meses[horizonte_meses])
}

cat("\n=== ANÁLISIS MULTI-HORIZONTE ===\n")
cat("Año de proyección:", year_current, "\n")
cat("Horizonte temporal:", horizonte_meses, "mes(es)\n")
cat("Período analizado:", periodo_str, "\n")
cat("Meses incluidos:", paste(meses_analizar, collapse = ", "), "\n\n")

# Estructuras para almacenar resultados
resultados_por_horizonte <- list()
plots_list <- list()

# Analizar cada horizonte (1 mes, 2 meses acumulados, 3 meses acumulados)
for (h in 1:horizonte_meses) {
  
  cat("\n--- Analizando horizonte de", h, "mes(es) ---\n")
  meses_hasta_h <- meses_analizar[1:h]
  
  # Almacenar rendimientos por ticker y año
  rendimientos_horizonte <- matrix(NA, nrow = length(years) + 1, ncol = length(tickers))
  rownames(rendimientos_horizonte) <- c(as.character(years), as.character(year_current))
  colnames(rendimientos_horizonte) <- tickers
  
  # Calcular rendimientos acumulados para este horizonte
  for (i in seq_along(tickers)) {
    ticker <- tickers[i]
    w <- weights[i]
    
    for (year in c(years, year_current)) {
      start_date <- as.Date(paste(year, "-01-01", sep = ""))
      end_date <- if (year == year_current) today else as.Date(paste(year, "-12-31", sep = ""))
      
      tryCatch({
        data_year <- getSymbols(ticker, src = "yahoo", from = start_date, 
                                to = end_date, auto.assign = FALSE)
        
        if (nrow(data_year) > 0) {
          closes <- Cl(data_year)
          
          # Calcular rendimiento acumulado para los h meses
          precio_inicial <- NULL
          precio_final <- NULL
          
          for (mes in meses_hasta_h) {
            month_str <- sprintf("%02d", mes)
            start_month <- paste0(year, "-", month_str, "-01")
            end_month <- as.Date(paste(year, mes, "01", sep="-")) %m+% months(1) - days(1)
            end_month_str <- format(end_month, "%Y-%m-%d")
            
            month_data <- closes[paste0(start_month, "/", end_month_str)]
            
            if (nrow(month_data) > 0) {
              if (is.null(precio_inicial)) {
                precio_inicial <- as.numeric(month_data[1])
              }
              precio_final <- as.numeric(month_data[nrow(month_data)])
            }
          }
          
          # Calcular rendimiento ponderado acumulado
          if (!is.null(precio_inicial) && !is.null(precio_final) && precio_inicial > 0) {
            ret_acumulado <- (precio_final / precio_inicial - 1) * 100 * w
            rendimientos_horizonte[as.character(year), ticker] <- ret_acumulado
          }
        }
      }, error = function(e) {
        cat("  Error para", ticker, year, ":", e$message, "\n")
      })
    }
  }
  
  # Calcular rendimientos totales del portafolio por año
  rendimientos_portafolio <- rowSums(rendimientos_horizonte, na.rm = TRUE)
  
  # Separar histórico y año actual
  rend_historico <- rendimientos_portafolio[as.character(years)]
  rend_actual <- rendimientos_portafolio[as.character(year_current)]
  
  # Estadísticas
  media_historica <- mean(rend_historico, na.rm = TRUE)
  volatilidad <- sd(rend_historico, na.rm = TRUE)
  
  # Proyección ponderada (dando más peso a años recientes)
  n_years <- length(years)
  if (n_years >= 5) {
    weights_years <- seq(from = 0.1, to = 0.3, length.out = n_years)
    weights_years <- weights_years / sum(weights_years)
  } else {
    weights_years <- rep(1/n_years, n_years)
  }
  proyeccion <- sum(weights_years * rend_historico, na.rm = TRUE)
  ic_inferior <- proyeccion - 1.96 * volatilidad
  ic_superior <- proyeccion + 1.96 * volatilidad
  
  # Guardar resultados
  resultados_por_horizonte[[h]] <- list(
    horizonte = h,
    periodo = if(h == 1) nombres_meses[1] else paste(nombres_meses[1], "-", nombres_meses[h]),
    rendimientos = rend_historico,
    rendimiento_actual = rend_actual,
    media = media_historica,
    volatilidad = volatilidad,
    proyeccion = proyeccion,
    ic_inferior = ic_inferior,
    ic_superior = ic_superior
  )
  
  # Crear gráfico de barras
  year_label <- paste0(year_current, " YTD")
  df_plot <- data.frame(
    Año = c(as.character(years), year_label),
    Rendimiento = c(rend_historico, rend_actual),
    Tipo = c(rep("Histórico", length(years)), "Actual YTD")
  )
  
  # Asegurar el orden correcto de los años en el eje X
  df_plot$Año <- factor(df_plot$Año, levels = c(as.character(years), year_label))
  
  p <- ggplot(df_plot, aes(x = Año, y = Rendimiento, fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = media_historica, linetype = "dashed", 
               color = "blue", linewidth = 0.8) +
    geom_hline(yintercept = proyeccion, linetype = "dashed", 
               color = "red", linewidth = 0.8) +
    annotate("text", x = 1.5, y = media_historica, 
             label = paste("Media histórica:", round(media_historica, 2), "%"),
             vjust = -0.5, color = "blue", size = 3) +
    annotate("text", x = 1.5, y = proyeccion, 
             label = paste("Proyección", year_current, ":", round(proyeccion, 2), "%"),
             vjust = 1.5, color = "red", size = 3) +
    scale_fill_manual(values = c("Histórico" = "steelblue", "Actual YTD" = "orange")) +
    labs(
      title = paste("Rendimiento del Portafolio -", 
                    if(h == 1) nombres_meses[1] else paste(nombres_meses[1], "-", nombres_meses[h])),
      subtitle = paste("Horizonte:", h, "mes(es) |", year_current, "muestra datos parciales (YTD)"),
      x = "Año",
      y = "Rendimiento Ponderado (%)",
      fill = "Período"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  plots_list[[h]] <- p
}

# Mostrar todos los gráficos
if (horizonte_meses == 1) {
  print(plots_list[[1]])
} else if (horizonte_meses == 2) {
  grid.arrange(plots_list[[1]], plots_list[[2]], ncol = 2)
} else if (horizonte_meses == 3) {
  grid.arrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], ncol = 3)
}

# ============================================================================
# RESUMEN DE RESULTADOS
# ============================================================================

cat("\n\n=== RESUMEN DE RESULTADOS POR HORIZONTE ===\n")
cat("Portafolio optimizado con restricción de peso mínimo\n")
cat(sprintf("Período de análisis histórico: %d-%d\n", year_inicio, year_current - 1))
cat(sprintf("Año actual: %d YTD (parcial)\n\n", year_current))

for (h in 1:horizonte_meses) {
  res <- resultados_por_horizonte[[h]]
  cat(sprintf("\n--- HORIZONTE %d MES(ES): %s ---\n", h, res$periodo))
  cat(sprintf("Rendimiento promedio histórico: %.2f%%\n", res$media))
  cat(sprintf("Volatilidad (desv. estándar): %.2f%%\n", res$volatilidad))
  cat(sprintf("Rendimiento actual (%d YTD): %.2f%%\n", year_current, res$rendimiento_actual))
  cat(sprintf("\nPROYECCIÓN %d: %.2f%%\n", year_current, res$proyeccion))
  cat(sprintf("Intervalo de confianza 95%%: [%.2f%%, %.2f%%]\n", 
              res$ic_inferior, res$ic_superior))
}

# Tabla comparativa final
cat("\n\n=== TABLA COMPARATIVA DE HORIZONTES ===\n")
df_comparativo <- data.frame(
  Horizonte = paste(1:horizonte_meses, "mes(es)"),
  Período = sapply(resultados_por_horizonte, function(x) x$periodo),
  Media_Histórica = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$media)),
  Proyección = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$proyeccion)),
  IC_95_Inferior = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$ic_inferior)),
  IC_95_Superior = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$ic_superior)),
  Volatilidad = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$volatilidad))
)

# Renombrar columna de proyección dinámicamente
colnames(df_comparativo)[4] <- paste0("Proyección_", year_current)

print(df_comparativo)

cat("\n=== COMPOSICIÓN FINAL DEL PORTAFOLIO ANALIZADO ===\n")
portfolio_final_df <- data.frame(
  Ticker = tickers,
  Peso_Porcentaje = paste0(round(weights * 100, 2), "%"),
  Peso_Decimal = round(weights, 4)
) %>% arrange(desc(Peso_Decimal))

print(portfolio_final_df %>% select(Ticker, Peso_Porcentaje))

cat("\nTotal de activos:", length(tickers), "\n")
cat("Capital total invertido:", round(sum(weights) * 100, 2), "%\n")

if (solo_evaluar == 0 && length(tickers_mantener) > 0) {
  cat("  • Capital en activos con restricción:", 
      round(sum(weights[tickers %in% tickers_mantener]) * 100, 2), "%")
  cat(" (mínimo requerido:", round(peso_minimo_mantener * 100, 2), "%)\n")
  cat("  • Capital en activos optimizados:", 
      round(sum(weights[!tickers %in% tickers_mantener]) * 100, 2), "%\n")
}

if (sum(weights) < 0.99) {
  cat("  • Capital sin invertir:", round((1 - sum(weights)) * 100, 2), "%\n")
}

cat("\n=== ANÁLISIS COMPLETADO ===\n")