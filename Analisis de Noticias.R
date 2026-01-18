# News analysis with Risk Detection by Isaac Echeverri
# Cargar librerías
rm(list = ls())

library(tidyRSS)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(stringr)

# 🧠 Nombre de la empresa si no es tan obvio con el ticker
ticker <- "Cognizant"

# 📍 Palabras clave de riesgo corporativo
palabras_riesgo <- c(
  # Demandas y problemas legales
  "lawsuit", "sued", "legal action", "litigation", "class action",
  
  # Problemas financieros
  "bankruptcy", "debt", "losses", "revenue decline", "profit warning",
  "deficit", "financial trouble", "insolvency",
  
  # Problemas operativos
  "recall", "scandal", "investigation", "fraud", "misconduct",
  "breach", "hack", "data leak", "cyberattack",
  
  # Problemas de mercado
  "downgrade", "stock plunge", "shares fall", "market loss",
  "analyst downgrade", "price target cut", "bearish",
  
  # Problemas de personal/operación
  "layoffs", "restructuring", "CEO resignation", "executive departure",
  "job cuts", "workforce reduction", "plant closure",
  
  # Problemas regulatorios
  "fine", "penalty", "sanction", "regulatory", "violation",
  "compliance issue", "SEC investigation", "antitrust",
  
  # Problemas de industria
  "disruption", "competition", "market share loss", "obsolete",
  "losing ground", "threat", "challenge"
)

# Crear URL para el RSS de Google News
url <- paste0("https://news.google.com/rss/search?q=", 
              URLencode(ticker), "&hl=en-US&gl=US&ceid=US:en")

# 1. Obtener titulares
rss <- tidyfeed(url)
titulos <- rss$item_title
fechas <- rss$item_pub_date

# Verifica que haya resultados
if (length(titulos) == 0) {
  cat("❌ No se encontraron titulares para:", ticker, "\n")
} else {
  
  # 2. Análisis de sentimiento
  sentimientos <- get_nrc_sentiment(titulos)
  
  # 3. 🚨 DETECCIÓN DE RIESGOS CORPORATIVOS
  cat("\n🚨 ANÁLISIS DE RIESGOS CORPORATIVOS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  
  # Función para detectar palabras de riesgo
  detectar_riesgos <- function(texto) {
    texto_lower <- tolower(texto)
    riesgos_encontrados <- palabras_riesgo[sapply(palabras_riesgo, function(palabra) {
      grepl(palabra, texto_lower, fixed = TRUE)
    })]
    return(riesgos_encontrados)
  }
  
  # Analizar cada titular
  noticias_riesgo <- data.frame(
    titulo = titulos,
    fecha = fechas,
    riesgos = sapply(titulos, function(t) {
      riesgos <- detectar_riesgos(t)
      if(length(riesgos) > 0) paste(riesgos, collapse = ", ") else NA
    }),
    num_riesgos = sapply(titulos, function(t) length(detectar_riesgos(t))),
    stringsAsFactors = FALSE
  )
  
  # Filtrar solo noticias con señales de riesgo
  noticias_alerta <- noticias_riesgo %>% 
    filter(!is.na(riesgos)) %>%
    arrange(desc(num_riesgos))
  
  if(nrow(noticias_alerta) > 0) {
    cat("\n⚠️  Se encontraron", nrow(noticias_alerta), "noticias con señales de riesgo:\n\n")
    
    for(i in 1:min(nrow(noticias_alerta), 10)) {
      cat("🔴", i, ".", noticias_alerta$titulo[i], "\n")
      cat("   📅 Fecha:", as.character(noticias_alerta$fecha[i]), "\n")
      cat("   🏷️  Palabras clave:", noticias_alerta$riesgos[i], "\n\n")
    }
    
    # Calcular nivel de riesgo
    nivel_riesgo <- (nrow(noticias_alerta) / length(titulos)) * 100
    
    cat("\n📊 INDICADOR DE RIESGO:", round(nivel_riesgo, 1), "%\n")
    
    if(nivel_riesgo > 30) {
      cat("🔴 RIESGO ALTO - Atención inmediata requerida\n")
    } else if(nivel_riesgo > 15) {
      cat("🟠 RIESGO MODERADO - Monitorear de cerca\n")
    } else if(nivel_riesgo > 5) {
      cat("🟡 RIESGO BAJO - Vigilancia normal\n")
    } else {
      cat("🟢 RIESGO MÍNIMO - Situación estable\n")
    }
    
  } else {
    cat("\n✅ No se detectaron señales de riesgo inmediato en los titulares recientes.\n")
  }
  
  # 4. Resumen por categoría de sentimiento
  resultado <- colSums(sentimientos)
  
  # 5. Mostrar primeros titulares
  cat("\n\n📰 PRIMEROS TITULARES ANALIZADOS\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  for(i in 1:min(length(titulos), 6)) {
    cat(i, ".", titulos[i], "\n")
  }
  
  # 6. Mostrar resumen numérico
  cat("\n📊 ANÁLISIS EMOCIONAL COMPLETO:\n")
  print(resultado)
  
  # 7. Graficar análisis emocional completo
  df_sent <- data.frame(sentimiento = names(resultado), valor = resultado)
  
  # Definir colores personalizados para cada sentimiento
  colores_sent <- c(
    "positive" = "#2ecc71",    # Verde
    "negative" = "#e74c3c",    # Rojo
    "anger" = "#c0392b",       # Rojo oscuro
    "anticipation" = "#f39c12", # Naranja
    "disgust" = "#8e44ad",     # Púrpura
    "fear" = "#34495e",        # Gris oscuro
    "joy" = "#f1c40f",         # Amarillo
    "sadness" = "#3498db",     # Azul
    "surprise" = "#1abc9c",    # Turquesa
    "trust" = "#27ae60"        # Verde oscuro
  )
  
  p1 <- ggplot(df_sent, aes(x = reorder(sentimiento, valor), y = valor, fill = sentimiento)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = colores_sent) +
    labs(
      title = paste("Análisis Emocional Completo -", ticker),
      subtitle = paste(Sys.Date(), "| Total noticias analizadas:", length(titulos)),
      x = "", 
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.text.y = element_text(size = 11),
      panel.grid.major.y = element_blank()
    ) +
    geom_text(aes(label = valor), hjust = -0.3, size = 4)
  
  print(p1)
  
  cat("\n✅ Análisis completado exitosamente\n")
}