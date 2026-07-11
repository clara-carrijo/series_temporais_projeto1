# PROJETO 2 - SERIES TEMPORAIS

library(readr)
library(dplyr)
library(sidrar)
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(lmtest)
library(broom)
library(scales)
library(sandwich)
library(strucchange)

# baixandoo
read_csv(pmc, "dados/pmc_varejo_sidra_8880.csv")
pmc <- pmc %>%
  mutate(
    periodo = if_else(data < as.Date("2020-01-01"), "Pre-COVID", "Pos-COVID"),
    log_indice = log(indice),
    t = row_number(),
    covid_step = as.integer(data >= as.Date("2020-01-01")),
    covid_pulse_2020_04 = as.integer(data == as.Date("2020-04-01")),
    post_recovery = pmax(0, interval(as.Date("2020-01-01"), data) %/% months(1)) * covid_step
  )

serie <- ts(pmc$indice,
            start = c(year(min(pmc$data)), month(min(pmc$data))),
            frequency = 12)
log_serie <- log(serie)
serie_pre <- window(log_serie, end = c(2019, 12))
serie_pos <- window(log_serie, start = c(2020, 1))

#  descritiva inicio
resumo <- pmc %>%
  group_by(periodo) %>%
  summarise(
    n = n(),
    media = mean(indice),
    desvio = sd(indice),
    minimo = min(indice),
    maximo = max(indice),
    cv = 100 * desvio / media,
    .groups = "drop"
  )

graf_serie <- ggplot(pmc, aes(data, indice)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15) +
  labs(
    title = "Indice de volume de vendas no comercio varejista",
    subtitle = "PMC/SIDRA - numero-indice, 2022=100",
    x = NULL, y = "Numero-indice"
  ) +
  theme_minimal()

ggsave("figuras/serie_completa.png", graf_serie, width = 8, height = 4, dpi = 300)

pmc %>%
  mutate(mes_nome = month(data, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(mes_nome, indice)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Padrao sazonal mensal", x = "Mes", y = "Numero-indice") +
  theme_minimal()
ggsave("figuras/boxplot_sazonal.png", width = 8, height = 4, dpi = 300)

# Decomposicao STL
png("figuras/decomposicao_stl.png", width = 1400, height = 900, res = 180)
plot(stl(log_serie, s.window = "periodic"), main = "Decomposicao STL do log do indice")
dev.off()

# facp e fac  
serie_est <- diff(diff(log_serie, lag = 12), differences = 1)
pre_est <- diff(diff(serie_pre, lag = 12), differences = 1)
pos_est <- diff(diff(serie_pos, lag = 12), differences = 1)

adf_pre <- tseries::adf.test(na.omit(pre_est))
adf_pos <- tseries::adf.test(na.omit(pos_est))

png("figuras/fac_facp_pre.png", width = 1400, height = 700, res = 180)
par(mfrow = c(1, 2))
Acf(na.omit(pre_est), lag.max = 36, main = "FAC - pre-COVID")
Pacf(na.omit(pre_est), lag.max = 36, main = "FACP - pre-COVID")
dev.off()

png("figuras/fac_facp_pos.png", width = 1400, height = 700, res = 180)
par(mfrow = c(1, 2))
Acf(na.omit(pos_est), lag.max = 30, main = "FAC - pos-COVID")
Pacf(na.omit(pos_est), lag.max = 30, main = "FACP - pos-COVID")
dev.off()

# sarima
ajustar_modelos <- function(x) {
  candidatos <- list(
    m011_011 = Arima(x, order = c(0, 1, 1), seasonal = c(0, 1, 1), include.constant = FALSE),
    m110_011 = Arima(x, order = c(1, 1, 0), seasonal = c(0, 1, 1), include.constant = FALSE),
    m111_011 = Arima(x, order = c(1, 1, 1), seasonal = c(0, 1, 1), include.constant = FALSE),
    m020_011 = Arima(x, order = c(2, 1, 0), seasonal = c(0, 1, 1), include.constant = FALSE),
    m211_011 = Arima(x, order = c(2, 1, 1), seasonal = c(0, 1, 1), include.constant = FALSE),
    m011_110 = Arima(x, order = c(0, 1, 1), seasonal = c(1, 1, 0), include.constant = FALSE)
  )
  
  tibble(
    modelo = names(candidatos),
    ajuste = candidatos,
    AIC = map_dbl(candidatos, AIC),
    BIC = map_dbl(candidatos, BIC)
  ) %>% arrange(AIC)
}

modelos_pre <- ajustar_modelos(serie_pre)
modelos_pos <- ajustar_modelos(serie_pos)

# Modelos finail
fit_pre <- Arima(serie_pre, order = c(2, 1, 1), seasonal = c(0, 1, 1), include.constant = FALSE)
fit_pos <- Arima(serie_pos, order = c(0, 1, 1), seasonal = c(1, 1, 0), include.constant = FALSE)

# Diagnosticoss
png("figuras/diagnostico_pre.png", width = 1400, height = 900, res = 180)
checkresiduals(fit_pre)
dev.off()

png("figuras/diagnostico_pos.png", width = 1400, height = 900, res = 180)
checkresiduals(fit_pos)
dev.off()

# parte 2
modelo_intervencao <- lm(
  log_indice ~ t + covid_step + covid_pulse_2020_04 + post_recovery + factor(mes),
  data = pmc
)

intervencao_hac <- lmtest::coeftest(
  modelo_intervencao,
  vcov. = sandwich::NeweyWest(modelo_intervencao, lag = 12, prewhite = FALSE)
)


# verificando tendenccia e sazonialidade
bp <- strucchange::breakpoints(log_indice ~ t + factor(mes), data = pmc)
write_csv(tibble(breakpoint = bp$breakpoints), "tabelas/breakpoints.csv")

# modelo PRE no POS
h <- length(serie_pos)
prev_pre_para_pos <- forecast(fit_pre, h = h, level = 95)

prev_tbl <- tibble(
  data = pmc %>% filter(data >= as.Date("2020-01-01")) %>% pull(data),
  observado = as.numeric(exp(serie_pos)),
  previsto = as.numeric(exp(prev_pre_para_pos$mean)),
  li95 = as.numeric(exp(prev_pre_para_pos$lower[, 1])),
  ls95 = as.numeric(exp(prev_pre_para_pos$upper[, 1]))
)

metricas_transferencia <- prev_tbl %>%
  summarise(
    MAE = mean(abs(observado - previsto)),
    RMSE = sqrt(mean((observado - previsto)^2)),
    MAPE = mean(abs((observado - previsto) / observado)) * 100,
    cobertura_95 = mean(observado >= li95 & observado <= ls95) * 100
  )
# vamos para a previsao
prev_pos <- forecast(fit_pos, h = 12, level = 95)

autoplot(prev_pos) +
  labs(title = "Previsao ilustrativa baseada no modelo pos-COVID",
       x = NULL, y = "log(numero-indice)") +
  theme_minimal()
ggsave("figuras/previsao_pos_covid.png", width = 8, height = 4, dpi = 300)
