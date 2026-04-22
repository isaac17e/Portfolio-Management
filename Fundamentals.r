# ============================================================================
# Fundamental analysis by: Isaac Echeverri - Versión corregida final con TTM y fixes
# ============================================================================
rm(list = ls())

library(httr)
library(jsonlite)
library(tidyverse)

# Tu API key de Alpha Vantage
API_KEY <- "80Z52RRLLBYZL7G8"

# === CONFIGURACIÓN EDITABLE ===
ticker_desde_aqui   <- "EC"
market_cap_desde_aqui <- 24350000000
use_quarterly_desde_aqui <- TRUE

# FUNCIONES AUXILIARES
get_alpha_data <- function(ticker, report_type) {
  base_url <- "https://www.alphavantage.co/query"
  response <- GET(base_url, query = list(
    `function` = report_type,
    symbol = ticker,
    apikey = API_KEY
  ))
  if(status_code(response) != 200) stop("Error API: ", status_code(response))
  data <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(data)
  if("Error Message" %in% names(parsed)) stop("API Error: ", parsed$`Error Message`)
  if("Note" %in% names(parsed)) stop("API Limit: ", parsed$Note)
  return(parsed)
}

safe_numeric <- function(x) {
  if(is.null(x) || length(x) == 0 || is.na(x) || x == "None") return(NA)
  as.numeric(x)
}

# ============================================================================
# FUNCIÓN PRINCIPAL - CON CÁLCULOS TTM
# ============================================================================

get_fundamentals <- function(ticker, market_cap = NA, use_quarterly = TRUE) {
  
  cat("Extrayendo datos para:", ticker, "\n")
  
  balance_data <- get_alpha_data(ticker, "BALANCE_SHEET")
  Sys.sleep(12)
  income_data <- get_alpha_data(ticker, "INCOME_STATEMENT")
  Sys.sleep(12)
  cashflow_data <- get_alpha_data(ticker, "CASH_FLOW")
  
  if(use_quarterly) {
    balance_q <- as.data.frame(balance_data$quarterlyReports)
    income_q <- as.data.frame(income_data$quarterlyReports)
    cashflow_q <- as.data.frame(cashflow_data$quarterlyReports)
    
    balance <- balance_q[1:min(5, nrow(balance_q)), ]
    income <- income_q[1:min(5, nrow(income_q)), ]
    cashflow <- cashflow_q[1:min(5, nrow(cashflow_q)), ]
  } else {
    balance <- as.data.frame(balance_data$annualReports)[1, ]
    income <- as.data.frame(income_data$annualReports)[1, ]
    cashflow <- as.data.frame(cashflow_data$annualReports)[1, ]
  }
  
  bal_act <- balance[1, ]
  inc_act <- income[1, ]
  cf_act <- cashflow[1, ]
  
  cash <- safe_numeric(bal_act$cashAndCashEquivalentsAtCarryingValue)
  current_assets <- safe_numeric(bal_act$totalCurrentAssets)
  current_liabilities <- safe_numeric(bal_act$totalCurrentLiabilities)
  inventory <- safe_numeric(bal_act$inventory)
  accounts_receivable <- safe_numeric(bal_act$currentNetReceivables)
  total_shareholder_equity <- safe_numeric(bal_act$totalShareholderEquity)
  
  # Cálculo de deuda más preciso (long-term noncurrent + current debt)
  long_term_debt <- safe_numeric(bal_act$longTermDebtNoncurrent)
  if(is.na(long_term_debt)) long_term_debt <- safe_numeric(bal_act$longTermDebt)
  current_debt <- safe_numeric(bal_act$currentDebt)
  if(is.na(current_debt)) current_debt <- 0
  total_debt <- long_term_debt + current_debt
  
  current_lease <- safe_numeric(bal_act$currentOperatingLeaseLiability)
  noncurrent_lease <- safe_numeric(bal_act$noncurrentOperatingLeaseLiability)
  total_leases <- sum(current_lease, noncurrent_lease, na.rm = TRUE)
  if(is.na(total_leases)) total_leases <- 0
  total_debt_sin_leases <- total_debt - total_leases
  hay_leases <- total_leases > 100000000
  
  depreciation <- safe_numeric(inc_act$depreciationAndAmortization)
  interest_expense <- safe_numeric(inc_act$interestExpense)
  
  operating_cash_flow <- safe_numeric(cf_act$operatingCashflow)
  capex <- safe_numeric(cf_act$capitalExpenditures)
  
  net_debt <- total_debt - cash
  net_debt_sin_leases <- total_debt_sin_leases - cash
  
  capital_invertido <- market_cap + net_debt
  
  # ===== CÁLCULOS TTM (últimos 4 quarters) =====
  n_quarters <- min(4, nrow(income))
  
  revenue_ttm <- sum(sapply(income[1:n_quarters, "totalRevenue"], safe_numeric), na.rm = TRUE)
  operating_income_ttm <- sum(sapply(income[1:n_quarters, "operatingIncome"], safe_numeric), na.rm = TRUE)
  depreciation_ttm <- sum(sapply(income[1:n_quarters, "depreciationAndAmortization"], safe_numeric), na.rm = TRUE)
  net_income_ttm <- sum(sapply(income[1:n_quarters, "netIncome"], safe_numeric), na.rm = TRUE)
  
  ocf_ttm <- sum(sapply(cashflow[1:n_quarters, "operatingCashflow"], safe_numeric), na.rm = TRUE)
  capex_ttm <- sum(abs(sapply(cashflow[1:n_quarters, "capitalExpenditures"], safe_numeric)), na.rm = TRUE)
  
  ebit_ttm <- operating_income_ttm
  ebitda_ttm <- ebit_ttm + depreciation_ttm
  fcf_ttm <- ocf_ttm - capex_ttm
  
  # ===== CRECIMIENTOS (YoY y QoQ sobre quarterly) =====
  revenue_yoy <- revenue_qoq <- fcf_yoy <- fcf_qoq <- NA
  
  revenue_actual <- safe_numeric(inc_act$totalRevenue)
  if(nrow(income) >= 4) {
    revenue_anio_ant <- safe_numeric(income[4, "totalRevenue"])
    revenue_yoy <- ifelse(revenue_anio_ant != 0, (revenue_actual / revenue_anio_ant - 1) * 100, NA)
  }
  if(nrow(income) >= 2) {
    revenue_ant <- safe_numeric(income[2, "totalRevenue"])
    revenue_qoq <- ifelse(revenue_ant != 0, (revenue_actual / revenue_ant - 1) * 100, NA)
  }
  
  fcf_actual <- operating_cash_flow - abs(capex)
  if(nrow(cashflow) >= 4) {
    ocf_anio_ant <- safe_numeric(cashflow[4, "operatingCashflow"])
    capex_anio_ant <- safe_numeric(cashflow[4, "capitalExpenditures"])
    fcf_anio_ant <- ocf_anio_ant - abs(capex_anio_ant)
    fcf_yoy <- ifelse(fcf_anio_ant != 0, (fcf_actual / fcf_anio_ant - 1) * 100, NA)
  }
  if(nrow(cashflow) >= 2) {
    ocf_ant <- safe_numeric(cashflow[2, "operatingCashflow"])
    capex_ant <- safe_numeric(cashflow[2, "capitalExpenditures"])
    fcf_ant <- ocf_ant - abs(capex_ant)
    fcf_qoq <- ifelse(fcf_ant != 0, (fcf_actual / fcf_ant - 1) * 100, NA)
  }
  
  # ===== ROIC y ROE sobre TTM =====
  tax_rate <- 0.21
  nopat_ttm <- ebit_ttm * (1 - tax_rate)
  roic <- ifelse(capital_invertido > 0, nopat_ttm / capital_invertido * 100, NA)
  
  roe <- ifelse(total_shareholder_equity > 0, net_income_ttm / total_shareholder_equity * 100, NA)
  
  results <- tibble(
    ticker = ticker,
    fiscal_date = bal_act$fiscalDateEnding,
    cash = cash,
    current_assets = current_assets,
    current_liabilities = current_liabilities,
    inventory = inventory,
    accounts_receivable = accounts_receivable,
    total_debt = total_debt,
    total_debt_sin_leases = total_debt_sin_leases,
    total_leases = total_leases,
    hay_leases = hay_leases,
    net_sales_quarter = revenue_actual,
    net_sales_ttm = revenue_ttm,
    interest_expense = interest_expense,
    operating_cash_flow = operating_cash_flow,
    capex = abs(capex),
    ebit_ttm = ebit_ttm,
    ebitda_ttm = ebitda_ttm,
    net_income_ttm = net_income_ttm,
    total_shareholder_equity = total_shareholder_equity,
    fcf_quarter = fcf_actual,
    fcf_ttm = fcf_ttm,
    revenue_yoy = revenue_yoy,
    revenue_qoq = revenue_qoq,
    fcf_yoy = fcf_yoy,
    fcf_qoq = fcf_qoq,
    roic = roic,
    roe = roe,
    net_debt = net_debt,
    net_debt_sin_leases = net_debt_sin_leases,
    market_cap = market_cap
  )
  
  cat("  Completado con métricas TTM y deuda corregida!\n\n")
  return(results)
}

calculate_ratios <- function(fundamentals_data) {
  fundamentals_data %>%
    mutate(
      cash_ratio = cash / current_liabilities,
      quick_ratio = (current_assets - inventory) / current_liabilities,
      cobertura_intereses = ifelse(is.na(interest_expense) | interest_expense == 0, NA, ebit_ttm / (interest_expense * 4)),
      deuda_neta_ebitda = net_debt / ebitda_ttm,
      deuda_neta_ebitda_sin_leases = net_debt_sin_leases / ebitda_ttm,
      ev_ebitda = (market_cap + net_debt) / ebitda_ttm,
      ev_ebitda_sin_leases = (market_cap + net_debt_sin_leases) / ebitda_ttm,
      dias_inventario = ifelse(net_sales_ttm > 0, inventory / (net_sales_ttm / 365), NA),
      dias_cobro = ifelse(net_sales_ttm > 0, (accounts_receivable / net_sales_ttm) * 365, NA)
    )
}

show_table_vertical <- function(data_with_ratios) {
  d <- data_with_ratios[1, ]
  hay_leases <- d$hay_leases
  
  indicadores <- c(
    "Ticker", "Fecha Fiscal", "---FUNDAMENTALES---",
    "Cash", "Current Assets", "Current Liabilities", "Inventory", "Accounts Receivable"
  )
  valores <- c(
    d$ticker, d$fiscal_date, "",
    format(d$cash, big.mark = ",", scientific = FALSE),
    format(d$current_assets, big.mark = ",", scientific = FALSE),
    format(d$current_liabilities, big.mark = ",", scientific = FALSE),
    format(d$inventory, big.mark = ",", scientific = FALSE),
    format(d$accounts_receivable, big.mark = ",", scientific = FALSE)
  )
  
  indicadores <- c(indicadores, "Total Debt")
  valores <- c(valores, format(d$total_debt, big.mark = ",", scientific = FALSE))
  if(hay_leases) {
    indicadores <- c(indicadores, "Total Debt sin leases")
    valores <- c(valores, format(d$total_debt_sin_leases, big.mark = ",", scientific = FALSE))
  }
  
  indicadores <- c(indicadores, "Revenue TTM", "EBITDA TTM", "FCF TTM", "Net Debt", "Market Cap")
  valores <- c(valores,
               format(d$net_sales_ttm, big.mark = ",", scientific = FALSE),
               format(d$ebitda_ttm, big.mark = ",", scientific = FALSE),
               format(d$fcf_ttm, big.mark = ",", scientific = FALSE),
               format(d$net_debt, big.mark = ",", scientific = FALSE),
               format(d$market_cap, big.mark = ",", scientific = FALSE))
  
  indicadores <- c(indicadores, "---CRECIMIENTOS---",
                   "Revenue YoY (%)", "Revenue QoQ (%)", "FCF YoY (%)", "FCF QoQ (%)")
  valores <- c(valores, "",
               ifelse(is.na(d$revenue_yoy), "NA", paste0(round(d$revenue_yoy, 1), "%")),
               ifelse(is.na(d$revenue_qoq), "NA", paste0(round(d$revenue_qoq, 1), "%")),
               ifelse(is.na(d$fcf_yoy), "NA", paste0(round(d$fcf_yoy, 1), "%")),
               ifelse(is.na(d$fcf_qoq), "NA", paste0(round(d$fcf_qoq, 1), "%")))
  
  indicadores <- c(indicadores, "---RENTABILIDAD---",
                   "ROIC (%)", "ROE (%)")
  valores <- c(valores, "",
               ifelse(is.na(d$roic), "NA", paste0(round(d$roic, 2), "%")),
               ifelse(is.na(d$roe), "NA", paste0(round(d$roe, 2), "%")))
  
  indicadores <- c(indicadores, "---RATIOS---",
                   "Cash Ratio", "Quick Ratio", "Cobertura de Intereses",
                   "Deuda Neta/EBITDA")
  valores <- c(valores, "",
               round(d$cash_ratio, 3), round(d$quick_ratio, 3),
               ifelse(is.na(d$cobertura_intereses), "NA", round(d$cobertura_intereses, 2)),
               round(d$deuda_neta_ebitda, 3))
  if(hay_leases) {
    indicadores <- c(indicadores, "Deuda Neta/EBITDA sin leases")
    valores <- c(valores, round(d$deuda_neta_ebitda_sin_leases, 3))
  }
  
  indicadores <- c(indicadores, "EV/EBITDA")
  valores <- c(valores, round(d$ev_ebitda, 2))
  if(hay_leases) {
    indicadores <- c(indicadores, "EV/EBITDA sin leases")
    valores <- c(valores, round(d$ev_ebitda_sin_leases, 2))
  }
  
  indicadores <- c(indicadores, "Dias de Inventario", "Dias de Cobro")
  valores <- c(valores, round(d$dias_inventario, 1), round(d$dias_cobro, 1))
  
  tabla <- data.frame(Indicador = indicadores, Valor = valores, stringsAsFactors = FALSE)
  return(tabla)
}

# ============================================================================
# EJECUCIÓN
# ============================================================================

datos <- get_fundamentals(ticker_desde_aqui, market_cap_desde_aqui, use_quarterly_desde_aqui)
datos_con_ratios <- calculate_ratios(datos)
tabla_final <- show_table_vertical(datos_con_ratios)

cat("\n=== RESULTADOS FINALES ===\n\n")
print(tabla_final, row.names = FALSE)