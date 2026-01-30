# Peso ALTO → Percentil ALTO (relajado, permisivo)
# Peso BAJO → Percentil BAJO (estricto, filtrador)
# Cuando quiera correlacion sectorial utilizo correlation_order <- 1

# CONSERVADOR
lambda <- 4.5  # Alta aversión al riesgo
# Pesos de selección
weight_sharpe <- 0.15          # Rendimiento es terciario
weight_low_vol <- 0.65         # PRIORIDAD MÁXIMA: Estabilidad
weight_decorr <- 0.20          # Diversificación importante

n_divers_candidates <- 30
volatility_percentile <- 0.25  # SUPER estricto: solo el 25% menos volátil
correlation_percentile <- 0.50 # Estricto: solo mitad menos correlacionada

correlation_order <- 0         # Preferir baja correlación (diversificación)

# === PERFIL: CONSERVADOR-MODERADO ===
lambda <- 1  # Aversión al riesgo moderada-alta
# Pesos de selección
weight_sharpe <- 0.25          # Rendimiento empieza a importar
weight_low_vol <- 0.50         # Estabilidad sigue siendo prioritaria
weight_decorr <- 0.25          # Diversificación más valorada

n_divers_candidates <- 35
volatility_percentile <- 0.40  # Moderado-estricto: 40% menos volátil
correlation_percentile <- 0.60 # Moderado: acepta más universo
# Configura el n_drivers candidate a 35

correlation_order <- 0         # Preferir baja correlación

# === PERFIL: MODERADO ===
lambda <- 4  # Balance equilibrado riesgo-retorno
# Pesos de selección
weight_sharpe <- 0.40          # PRIORIDAD: Eficiencia (Sharpe)
weight_low_vol <- 0.35         # Volatilidad controlada pero no dominante
weight_decorr <- 0.25          # Diversificación estándar

n_divers_candidates <- 40
volatility_percentile <- 0.60  # Permisivo: acepta volatilidad media
correlation_percentile <- 0.65 # Permisivo: amplio universo

correlation_order <- 0         # Preferir baja correlación

# === PERFIL: MODERADO-AGRESIVO ===
lambda <- 3  # Baja aversión al riesgo
# Pesos de selección
weight_sharpe <- 0.50          # Rendimiento es REY
weight_low_vol <- 0.25         # Volatilidad secundaria
weight_decorr <- 0.25          # Diversificación táctica

n_divers_candidates <- 45
volatility_percentile <- 0.75  # Muy permisivo: acepta alta volatilidad
correlation_percentile <- 0.70 # Muy permisivo: universo amplio

correlation_order <- 0         # Preferir baja correlación (evita concentración)

# === PERFIL: AGRESIVO ===
lambda <- 1.2  # Aversión al riesgo mínima
# Pesos de selección
weight_sharpe <- 0.55          # Rendimiento absoluto
weight_low_vol <- 0.15         # Volatilidad casi irrelevante
weight_decorr <- 0.30          # Diversificación importante (evita catástrofe)

n_divers_candidates <- 45
volatility_percentile <- 0.85  # Extremadamente permisivo
correlation_percentile <- 0.75 # Muy permisivo

correlation_order <- 0         # Preferir baja correlación (CRÍTICO para sobrevivir)






# Después de select_optimal_candidates(): para ver la seleccion de activos
cat(sprintf("Activos post-filtro: %d\n", length(ticker_candidates)))

#Esperado
Conservador: 15-25 activos (muy selectivo)
Cons-Moderado: 25-35 activos
Moderado: 30-40 activos
Mod-Agresivo: 35-45 activos
Agresivo: 40-50 activos (casi todo pasa)

#Tickers que no estan en el broker pero que servira luego para complementar
AGG, LQD, TIP, DBC, XLB, XLC, VTV, VUG, VIG, VTWO, SCHD, SCHP, DGRO, HDV, SDY, XDS, HACK,
IRBO, DBB, DBE, DBA, BNO

# Corre esto al final de la generacion de un portafolio para analizar la concentracion
# de activos en este

cat("\n=== ⚠️ ANÁLISIS DE RIESGOS DE CONCENTRACIÓN ===\n")

concentration_risk <- pesos %>%
  arrange(desc(weight)) %>%
  mutate(
    cumulative_weight = cumsum(weight),
    rank = row_number()
  )

top3_concentration <- sum(concentration_risk$weight[1:3])
top5_concentration <- sum(concentration_risk$weight[1:5])

cat(sprintf("  ➤ Top 3 activos: %.1f%% del portafolio\n", top3_concentration * 100))
cat(sprintf("  ➤ Top 5 activos: %.1f%% del portafolio\n", top5_concentration * 100))

if (top3_concentration > 0.50) {
  cat("  ⚠️ ALERTA: Alta concentración en top 3 activos (>50%)\n")
}

# Análisis de eventos extremos
cat("\n=== 📉 ANÁLISIS DE EVENTOS EXTREMOS ===\n")

extreme_years <- yearly_mdd_valid %>%
  filter(mdd < quantile(mdd, 0.10, na.rm = TRUE)) %>%  # Peor 10%
  arrange(mdd)

cat("  Años con MDD extremo (peor 10%):\n")
print(extreme_years %>% select(year, mdd) %>% mutate(mdd_pct = sprintf("%.2f%%", mdd * 100)))

cat(sprintf("\n  ➤ En eventos extremos, espera caídas promedio de: %.2f%%\n", 
            mean(extreme_years$mdd) * 100))