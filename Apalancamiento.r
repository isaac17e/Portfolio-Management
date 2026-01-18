# Optimum leverage by: Isaac Echeverri
rm(list = ls())

library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(corrplot)

tickers <- c("GD", "LHX", "DB1.DE", "AMZN", "CME", "COO", "KR", "TMUS", "JKHY", 
             "PGR", "CB", "CINF", "CTSH", "CCEP", "LMT", "GOOGL", "NOC")

weights <- c(0.122, 0.114, 0.1, 0.082, 0.08, 0.07, 0.062, 0.06, 0.056, 0.056, 
             0.044, 0.04, 0.032, 0.02, 0.016, 0.12, 0.12)

weights <- weights / sum(weights)

capital_total <- 10000
riesgo_max_por_operacion <- 0.01
riesgo_max_apalancado <- 0.15
exposicion_max_relativa <- 1.15
apalanc_max_por_activo <- 3.0
horizonte_dias <- 42

start_date <- Sys.Date() - 180
end_date <- Sys.Date() - 1

precios <- list()
retornos <- list()

cat("Descargando datos...\n")
for (t in tickers) {
  try({
    getSymbols(t, from = start_date, to = end_date, auto.assign = FALSE) -> temp
    precios[[t]] <- Cl(temp)
    retornos[[t]] <- dailyReturn(Cl(temp), type = "log")
  }, silent = TRUE)
}

retornos_df <- do.call(cbind, retornos)
colnames(retornos_df) <- tickers
retornos_df <- na.omit(retornos_df)

metricas <- data.frame(ticker = tickers, weight = weights, stringsAsFactors = FALSE)

vol_60 <- apply(tail(retornos_df, 60), 2, sd) * sqrt(252)
metricas$vol_anual <- vol_60

ret_ult_mes <- sapply(retornos, function(x) {
  if (nrow(x) >= 21) exp(sum(tail(x, 21))) - 1 else NA
})
metricas$momentum <- ret_ult_mes

sharpe <- sapply(retornos, function(x) {
  if (nrow(x) >= 60) {
    mean(tail(x, 60)) / sd(tail(x, 60)) * sqrt(252)
  } else NA
})
metricas$sharpe <- sharpe

mdd <- sapply(retornos, function(x) {
  if (nrow(x) >= 60) {
    cum_ret <- cumsum(tail(x, 60))
    peak <- cummax(cum_ret)
    min((cum_ret - peak) / (1 + peak))
  } else NA
})
metricas$mdd <- mdd

ret_port <- retornos_df %*% weights
correl_prom <- apply(retornos_df, 2, function(x) {
  cor(x, ret_port, use = "complete.obs")
})
metricas$correl_port <- correl_prom

metricas <- metricas %>%
  mutate(
    vol_rank = percent_rank(-vol_anual),
    mom_rank = percent_rank(momentum),
    sharpe_rank = percent_rank(sharpe),
    mdd_rank = percent_rank(-mdd),
    correl_rank = percent_rank(-abs(correl_port))
  ) %>%
  mutate(
    puntaje_apalanc = 0.3*mom_rank + 0.25*sharpe_rank + 0.2*vol_rank + 
      0.15*mdd_rank + 0.1*correl_rank
  )

umbral_apalanc <- quantile(metricas$puntaje_apalanc, 0.60, na.rm = TRUE)
candidatos <- metricas %>% filter(puntaje_apalanc >= umbral_apalanc)

candidatos <- candidatos %>%
  mutate(
    apalancamiento = 1.2 + (puntaje_apalanc - min(puntaje_apalanc)) / 
      (max(puntaje_apalanc) - min(puntaje_apalanc)) * (apalanc_max_por_activo - 1.2)
  ) %>%
  mutate(apalancamiento = pmin(apalancamiento, apalanc_max_por_activo))

candidatos$capital_base <- capital_total * candidatos$weight
candidatos$capital_apalancado <- candidatos$capital_base * candidatos$apalancamiento
candidatos$exposicion_extra <- candidatos$capital_apalancado - candidatos$capital_base
candidatos$vol_mensual <- candidatos$vol_anual / sqrt(12)
candidatos$riesgo_activo <- candidatos$capital_apalancado * candidatos$vol_mensual

exposicion_max <- capital_total * exposicion_max_relativa
exposicion_actual <- sum(candidatos$capital_apalancado, na.rm = TRUE)
riesgo_total_max <- capital_total * riesgo_max_apalancado
riesgo_total_actual <- sum(candidatos$riesgo_activo, na.rm = TRUE)

while (exposicion_actual > exposicion_max && nrow(candidatos) > 0) {
  idx_max <- which.max(candidatos$capital_apalancado)
  candidatos$apalancamiento[idx_max] <- candidatos$apalancamiento[idx_max] * 0.9
  candidatos$capital_apalancado[idx_max] <- candidatos$capital_base[idx_max] * candidatos$apalancamiento[idx_max]
  candidatos$exposicion_extra[idx_max] <- candidatos$capital_apalancado[idx_max] - candidatos$capital_base[idx_max]
  candidatos$riesgo_activo[idx_max] <- candidatos$capital_apalancado[idx_max] * candidatos$vol_mensual[idx_max]
  exposicion_actual <- sum(candidatos$capital_apalancado, na.rm = TRUE)
}

while (riesgo_total_actual > riesgo_total_max && nrow(candidatos) > 0) {
  idx_max <- which.max(candidatos$riesgo_activo)
  candidatos$apalancamiento[idx_max] <- candidatos$apalancamiento[idx_max] * 0.9
  candidatos$capital_apalancado[idx_max] <- candidatos$capital_base[idx_max] * candidatos$apalancamiento[idx_max]
  candidatos$exposicion_extra[idx_max] <- candidatos$capital_apalancado[idx_max] - candidatos$capital_base[idx_max]
  candidatos$riesgo_activo[idx_max] <- candidatos$capital_apalancado[idx_max] * candidatos$vol_mensual[idx_max]
  riesgo_total_actual <- sum(candidatos$riesgo_activo, na.rm = TRUE)
}

resultado <- metricas %>%
  left_join(select(candidatos, ticker, apalancamiento, capital_apalancado, riesgo_activo), by = "ticker") %>%
  mutate(
    apalancamiento = ifelse(is.na(apalancamiento), 1.0, apalancamiento),
    capital_apalancado = ifelse(is.na(capital_apalancado), capital_total * weight, capital_apalancado),
    riesgo_activo = ifelse(is.na(riesgo_activo), capital_total * weight * (vol_anual / sqrt(12)), riesgo_activo),
    recomendacion = ifelse(apalancamiento > 1.01, "APALANCAR", "MANTENER 1x")
  ) %>%
  arrange(desc(apalancamiento))

cat("\n", rep("=", 60), "\n")
cat("RECOMENDACIONES DE APALANCAMIENTO - 3 NOV 2025\n")
cat(rep("=", 60), "\n\n")

print(resultado %>%
        select(ticker, weight, momentum, vol_anual, sharpe, puntaje_apalanc, 
               apalancamiento, capital_apalancado, riesgo_activo, recomendacion) %>%
        mutate(across(where(is.numeric), ~round(., 3))))

cat("\n", rep("-", 50), "\n")
cat("RESUMEN DE RIESGO GLOBAL\n")
cat(rep("-", 50), "\n")
cat(sprintf("Capital total: $%s\n", format(capital_total, big.mark = ",")))
cat(sprintf("Exposición apalancada: $%s (%.1f%%)\n", 
            format(sum(resultado$capital_apalancado), big.mark = ","), 
            sum(resultado$capital_apalancado)/capital_total*100))
cat(sprintf("Riesgo total estimado (1σ): $%s (%.1f%%)\n", 
            format(sum(resultado$riesgo_activo), big.mark = ","), 
            sum(resultado$riesgo_activo)/capital_total*100))
cat(sprintf("¿Dentro de límites? Exposición ≤%.0f%%: %s | Riesgo ≤%.0f%%: %s\n", 
            exposicion_max_relativa*100,
            ifelse(sum(resultado$capital_apalancado) <= exposicion_max, "SÍ", "NO"),
            riesgo_max_apalancado*100,
            ifelse(sum(resultado$riesgo_activo) <= riesgo_total_max, "SÍ", "NO")))

apalancados <- resultado %>% filter(apalancamiento > 1.01)
cat("\nACTIVOS RECOMENDADOS PARA APALANCAR:\n")
print(apalancados %>% 
        select(ticker, apalancamiento, capital_apalancado) %>%
        mutate(capital_apalancado = paste0("$", format(round(capital_apalancado), big.mark = ","))))