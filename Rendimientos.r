# Por: Isaac Echeverri
# Rendimiento y rentabilidad con horizonte temporal extendido (1, 2 o 3 meses)
rm(list = ls())

library(quantmod)
library(tidyverse)
library(gridExtra)

# ============================================================================
# FECHA DE ANÁLISIS
# ============================================================================
fecha_cierre <- "2025-12-31"  # Última fecha con datos completos

today <- as.Date(fecha_cierre)

# ============================================================================
# CONFIGURACIÓN DE HORIZONTE TEMPORAL
# ============================================================================
# Mes inicial de análisis
inicio_mes <- 01  

# HORIZONTE TEMPORAL: ¿Cuántos meses analizar?
# Opciones: 1, 2 o 3
horizonte_meses <-2  # Cambia según necesites

# Generar secuencia de meses a analizar
meses_analizar <- ((inicio_mes - 1) + 0:(horizonte_meses - 1)) %% 12 + 1

# ============================================================================
# MODO DE ANÁLISIS
# ============================================================================
# 1 = Solo evaluar portafolio nuevo (ignora rebalanceo)
# 0 = Aplicar rebalanceo (AGREGAR o INTERCAMBIAR)
solo_evaluar <- 1  

# ============================================================================
# CONFIGURACIÓN DE PORTAFOLIO
# ============================================================================

# Portafolio nuevo propuesto
tickers <- c("GD", "LHX", "DB1.DE", "AMZN", "CME", "COO", "KR", "TMUS", "JKHY", 
             "PGR", "CB", "CINF", "CTSH", "CCEP", "LMT", "GOOGL", "NOC")
weights <- c(0.122, 0.114, 0.1, 0.082, 0.08, 0.07, 0.062, 0.06, 0.056, 0.056, 
             0.044, 0.04, 0.032, 0.02, 0.016, 0.12, 0.12)

# ============================================================================
# ESTRATEGIA DE REBALANCEO (Solo se usa si solo_evaluar = 0)
# ============================================================================
estrategia <- "INTERCAMBIAR"  # Cambia a "AGREGAR" si prefieres expandir

tickers_mantener <- c("AMZN", "GOOGL")
pesos_reales_mantener <- c(0.082, 0.012)
tickers_a_reemplazar <- c("DBB", "DBA")

# ============================================================================
# REBALANCEO AUTOMÁTICO CON PESOS REALES
# ============================================================================

if (solo_evaluar == 0 && length(tickers_mantener) > 0) {
  
  if (length(pesos_reales_mantener) != length(tickers_mantener)) {
    stop("ERROR: Debes especificar un peso real para cada ticker a mantener")
  }
  
  peso_total_mantener <- sum(pesos_reales_mantener)
  peso_disponible <- 1 - peso_total_mantener
  
  if (peso_disponible < 0) {
    stop("ERROR: Los pesos reales de los tickers a mantener suman más del 100%")
  }
  
  cat("\n=== REBALANCEO DE PORTAFOLIO CON PESOS REALES ===\n")
  cat("Estrategia:", estrategia, "\n")
  cat("Capital YA invertido:", round(peso_total_mantener * 100, 2), "%\n")
  cat("Capital disponible para nuevo portafolio:", round(peso_disponible * 100, 2), "%\n\n")
  
  cat("Posiciones mantenidas (pesos reales actuales):\n")
  for (i in seq_along(tickers_mantener)) {
    cat("  •", tickers_mantener[i], ":", round(pesos_reales_mantener[i] * 100, 2), "%\n")
  }
  cat("\n")
  
  if (estrategia == "INTERCAMBIAR") {
    cat("→ Modo: INTERCAMBIAR (reemplazar activos específicos)\n")
    cat("Tickers a reemplazar:", paste(tickers_a_reemplazar, collapse = ", "), "\n\n")
    
    if (length(tickers_mantener) != length(tickers_a_reemplazar)) {
      stop("ERROR: Debes especificar el mismo número de tickers a mantener y a reemplazar")
    }
    
    tickers_sin_reemplazar <- tickers[!tickers %in% tickers_a_reemplazar]
    weights_sin_reemplazar <- weights[!tickers %in% tickers_a_reemplazar]
    
    if (length(weights_sin_reemplazar) > 0) {
      weights_sin_reemplazar <- weights_sin_reemplazar / sum(weights_sin_reemplazar) * peso_disponible
    }
    
    tickers_final <- c(tickers_mantener, tickers_sin_reemplazar)
    weights_final <- c(pesos_reales_mantener, weights_sin_reemplazar)
    
    cat("Rebalanceo aplicado:\n")
    for (i in seq_along(tickers_mantener)) {
      cat("  ✓", tickers_a_reemplazar[i], "→", tickers_mantener[i], 
          "(peso real:", round(pesos_reales_mantener[i] * 100, 2), "%)\n")
    }
    cat("\n")
    
    tickers <- tickers_final
    weights <- weights_final
    
  } else if (estrategia == "AGREGAR") {
    cat("→ Modo: AGREGAR (expandir portafolio manteniendo todos los activos)\n\n")
    
    tickers_ya_existentes <- tickers_mantener[tickers_mantener %in% tickers]
    tickers_nuevos_a_agregar <- tickers_mantener[!tickers_mantener %in% tickers]
    
    tickers_filtrados <- tickers[!tickers %in% tickers_ya_existentes]
    weights_filtrados <- weights[!tickers %in% tickers_ya_existentes]
    
    if (length(weights_filtrados) > 0) {
      weights_filtrados <- weights_filtrados / sum(weights_filtrados) * peso_disponible
    }
    
    tickers_final <- c(tickers_mantener, tickers_filtrados)
    weights_final <- c(pesos_reales_mantener, weights_filtrados)
    
    if (length(tickers_ya_existentes) > 0) {
      cat("  ⚠ Eliminados por duplicado:", paste(tickers_ya_existentes, collapse = ", "), "\n")
    }
    if (length(tickers_nuevos_a_agregar) > 0) {
      cat("  ✓ Agregados al portafolio:", paste(tickers_nuevos_a_agregar, collapse = ", "), "\n")
    }
    cat("\n")
    
    tickers <- tickers_final
    weights <- weights_final
  }
  
  suma_pesos <- sum(weights)
  if (abs(suma_pesos - 1.0) > 0.001) {
    cat("  ⚠ Ajustando normalización final (suma:", round(suma_pesos, 4), ")\n")
    weights <- weights / suma_pesos
  }
  
  cat("=== PORTAFOLIO FINAL REBALANCEADO ===\n")
  portfolio_df <- data.frame(
    Ticker = tickers,
    Peso = paste0(round(weights * 100, 2), "%"),
    Tipo = ifelse(tickers %in% tickers_mantener, "MANTENER (real)", "NUEVO"),
    Peso_Numerico = weights
  ) %>% arrange(desc(Peso_Numerico)) %>% select(-Peso_Numerico)
  print(portfolio_df)
  cat("\nTotal de activos:", length(tickers), "\n")
  cat("Capital mantenido:", round(sum(weights[tickers %in% tickers_mantener]) * 100, 2), "%\n")
  cat("Capital nuevo invertido:", round(sum(weights[!tickers %in% tickers_mantener]) * 100, 2), "%\n")
  cat("Suma total de pesos:", round(sum(weights), 4), "\n\n")
  
} else if (solo_evaluar == 1) {
  cat("\n=== MODO: SOLO EVALUACIÓN ===\n")
  cat("Evaluando portafolio propuesto sin aplicar rebalanceo\n")
  cat("Total de activos:", length(tickers), "\n\n")
  
  portfolio_df <- data.frame(
    Ticker = tickers,
    Peso = paste0(round(weights * 100, 2), "%"),
    Peso_Numerico = weights
  ) %>% arrange(desc(Peso_Numerico)) %>% select(-Peso_Numerico)
  print(portfolio_df)
  cat("\nSuma total de pesos:", round(sum(weights), 4), "\n\n")
  
} else {
  cat("\n=== SIN REBALANCEO ===\n")
  cat("Usando portafolio original sin modificaciones\n\n")
}

# ============================================================================
# ANÁLISIS DE RENDIMIENTOS MULTI-HORIZONTE
# ============================================================================

# Configuración de años
year_inicio <- 2020  # Año desde el cual iniciar el análisis
year_current <- as.numeric(format(today, "%Y"))
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
cat("Portafolio optimizado por: UTILIDAD CUADRÁTICA\n")
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
  Proyección_2025 = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$proyeccion)),
  IC_95_Inferior = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$ic_inferior)),
  IC_95_Superior = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$ic_superior)),
  Volatilidad = sprintf("%.2f%%", sapply(resultados_por_horizonte, function(x) x$volatilidad))
)
print(df_comparativo)

cat("\n=== ANÁLISIS COMPLETADO ===\n")