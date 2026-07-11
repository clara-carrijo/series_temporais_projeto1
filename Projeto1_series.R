# PROJETO 1 - SERIES TEMPORAIS
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(scales)
library(knitr)
library(broom)

# ja importei o banco e criei um arquivo antes - SALVO
serie_mensal <- serie_mensal_rmet %>%
  mutate(
    datas = as.Date(datas),
    ano = year(datas),
    mes = month(datas),
    temp_media_mes = as.numeric(temp_media_mes),
    mes_nome = factor(
      mes,
      levels = 1:12,
      labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
    )
  ) %>%
  arrange(datas)

serie_mensal_completa <- serie_mensal %>%
  complete(datas = seq.Date(min(datas), max(datas), by = "month")) %>%
  mutate(
    ano = year(datas),
    mes = month(datas),
    mes_nome = factor(
      mes,
      levels = 1:12,
      labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                 "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
    )
  ) %>%
  arrange(datas)


# descritiva começando
resumo_geral <- serie_mensal %>%
  summarise(
    data_inicio = min(datas, na.rm = TRUE),
    data_fim = max(datas, na.rm = TRUE),
    n_meses = sum(!is.na(temp_media_mes)),
    media = mean(temp_media_mes, na.rm = TRUE),
    desvio_padrao = sd(temp_media_mes, na.rm = TRUE),
    minimo = min(temp_media_mes, na.rm = TRUE),
    q1 = quantile(temp_media_mes, 0.25, na.rm = TRUE),
    mediana = median(temp_media_mes, na.rm = TRUE),
    q3 = quantile(temp_media_mes, 0.75, na.rm = TRUE),
    maximo = max(temp_media_mes, na.rm = TRUE)
  )

resumo_mensal <- serie_mensal %>%
  group_by(mes, mes_nome) %>%
  summarise(
    n = sum(!is.na(temp_media_mes)),
    media = mean(temp_media_mes, na.rm = TRUE),
    desvio_padrao = sd(temp_media_mes, na.rm = TRUE),
    minimo = min(temp_media_mes, na.rm = TRUE),
    mediana = median(temp_media_mes, na.rm = TRUE),
    maximo = max(temp_media_mes, na.rm = TRUE),
    .groups = "drop"
  )

resumo_anual <- serie_mensal %>%
  group_by(ano) %>%
  summarise(
    n_meses = sum(!is.na(temp_media_mes)),
    media_anual = mean(temp_media_mes, na.rm = TRUE),
    desvio_anual = sd(temp_media_mes, na.rm = TRUE),
    minimo_anual = min(temp_media_mes, na.rm = TRUE),
    maximo_anual = max(temp_media_mes, na.rm = TRUE),
    .groups = "drop"
  )

ano_inicio <- min(serie_mensal$ano, na.rm = TRUE)
mes_inicio <- min(serie_mensal$mes[serie_mensal$ano == ano_inicio], na.rm = TRUE)

serie_temp_ts <- ts(
  serie_mensal$temp_media_mes,
  start = c(ano_inicio, mes_inicio),
  frequency = 12
)


# Serie temporal completa
grafico_serie <- ggplot(serie_mensal, aes(x = datas, y = temp_media_mes)) +
  geom_line(linewidth = 0.45) +
  labs(
    title = "Temperatura média mensal de Brasília",
    subtitle = "INMET",
    x = "Ano",
    y = "Temperatura média mensal (graus C)"
  ) +
  theme_minimal(base_size = 12)
grafico_serie
ggsave("figuras/fig01_serie_temporal.png", grafico_serie, width = 10, height = 4.8, dpi = 300)


# Boxplot por mes - sazonalidade anual
grafico_boxplot_mes <- ggplot(serie_mensal, aes(x = mes_nome, y = temp_media_mes)) +
  geom_boxplot(outlier.alpha = 0.7) +
  labs(
    title = "Distribuição da temperatura média mensal",
    subtitle = "Por mês",
    x = "Mes",
    y = "Temperatura média mensal (graus C)"
  ) +
  theme_minimal(base_size = 12)
grafico_boxplot_mes
ggsave("figuras/fig02_boxplot_mes.png", grafico_boxplot_mes, width = 9, height = 4.8, dpi = 300)


# sazonal medio
perfil_sazonal <- serie_mensal %>%
  group_by(mes, mes_nome) %>%
  summarise(
    media_historica = mean(temp_media_mes, na.rm = TRUE),
    desvio = sd(temp_media_mes, na.rm = TRUE),
    n = sum(!is.na(temp_media_mes)),
    .groups = "drop"
  )


grafico_perfil_sazonal <- ggplot(perfil_sazonal, aes(x = mes_nome, y = media_historica, group = 1)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  labs(
    title = "Perfil sazonal da temperatura",
    subtitle = "Média da temperatura mensal por mês do ano - ao longo dos anos",
    x = "Mês",
    y = "Temperatura média dos meses (graus C)"
  ) +
  theme_minimal(base_size = 12)
perfil_sazonal
grafico_perfil_sazonal
ggsave("figuras/fig03_perfil_sazonal.png", grafico_perfil_sazonal, width = 9, height = 4.8, dpi = 300)

# Media anual longo prazo
grafico_media_anual <- ggplot(resumo_anual, aes(x = ano, y = media_anual)) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 1.4) +
  labs(
    title = "Temperatura média anual - a partir da série mensal",
    x = "Ano",
    y = "Temperatura média anual (graus C)"
  ) +
  theme_minimal(base_size = 12)
grafico_media_anual
ggsave("figuras/fig04_media_anual.png", grafico_media_anual, width = 9, height = 4.8, dpi = 300)

# Grafico sazona
png("figuras/fig05_grafico_sazonal.png", grafico_sazonal, width = 3000, height = 1800, res = 300)
grafico_sazonal <- print(
  ggseasonplot(serie_temp_ts, year.labels = TRUE, year.labels.left = TRUE) +
    labs(
      title = "Grafico sazonal da temperatura media mensal",
      x = "Mes",
      y = "Temperatura media mensal (graus C)"
    ) +
    theme_minimal(base_size = 12)
)
dev.off()

# ----------------------------------------------------------------------------
# 8. DECOMPOSICAO STL

decomp_temp <- stl(serie_temp_ts, s.window = "periodic", robust = TRUE)

png("figuras/fig06_decomposicao_stl.png", width = 3000, height = 2100, res = 300)

plot(decomp_temp, main = "Decomposição STL da temperatura média mensal - BSB")
dev.off()

componentes_stl <- as.data.frame(decomp_temp$time.series) %>%
  mutate(datas = serie_mensal$datas) %>%
  relocate(datas)



# fac e facp - estacionaraia

png("figuras/fig07_acf_pacf_original.png", width = 3300, height = 1300, res = 300)
par(mfrow = c(1, 2))
Acf(serie_temp_ts, lag.max = 60, main = "FAC - serie original")
Pacf(serie_temp_ts, lag.max = 60, main = "FACP - serie original")
par(mfrow = c(1, 1))
dev.off()

# outros testes
valor_ndiffs <- ndiffs(serie_temp_ts)
valor_nsdiffs <- nsdiffs(serie_temp_ts)

cat("ndiffs(serie_temp_ts) = ", valor_ndiffs, "\n", sep = "")
cat("nsdiffs(serie_temp_ts) = ", valor_nsdiffs, "\n\n", sep = "")

print(adf.test(na.omit(as.numeric(serie_temp_ts))))

print(kpss.test(na.omit(as.numeric(serie_temp_ts))))
sink()

# Diferencas 
serie_dif_comum <- diff(serie_temp_ts, differences = 1)
serie_dif_sazonal <- diff(serie_temp_ts, lag = 12, differences = 1)
serie_dif_comum_sazonal <- diff(diff(serie_temp_ts, lag = 12), differences = 1)

png("figuras/fig08_series_diferenciadas.png", width = 3300, height = 2500, res = 300)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot(serie_dif_comum, main = "Primeira diferenca comum", xlab = "Ano", ylab = "Diferenca")
abline(h = 0)
plot(serie_dif_sazonal, main = "Diferenca sazonal de periodo 12", xlab = "Ano", ylab = "Diferenca")
abline(h = 0)
plot(serie_dif_comum_sazonal, main = "Diferenca comum apos diferenca sazonal", xlab = "Ano", ylab = "Diferenca")
abline(h = 0)
par(mfrow = c(1, 1))
dev.off()

png("figuras/fig09_acf_pacf_diferenciada.png", width = 3300, height = 1300, res = 300)
par(mfrow = c(1, 2))
Acf(serie_dif_comum_sazonal, lag.max = 60, main = "FAC - serie diferenciada")
Pacf(serie_dif_comum_sazonal, lag.max = 60, main = "FACP - serie diferenciada")
par(mfrow = c(1, 1))
dev.off()

#treino e teste 
h_teste <- 24
n_total <- length(serie_temp_ts)

serie_treino <- head(serie_temp_ts, n_total - h_teste)
serie_teste <- tail(serie_temp_ts, h_teste)

cat("Tamanho treino: ", length(serie_treino), "\n", sep = "")
cat("Tamanho teste: ", length(serie_teste), "\n", sep = "")

# Data frame para grafico de treino/teste
df_treino_teste <- serie_mensal %>%
  mutate(amostra = if_else(row_number() <= n_total - h_teste, "Treinamento", "Teste"))

grafico_treino_teste <- ggplot(df_treino_teste, aes(x = datas, y = temp_media_mes, color = amostra)) +
  geom_line(linewidth = 0.45) +
  labs(
    title = "Separacao da serie em treinamento e teste",
    subtitle = "Os ultimos 24 meses foram reservados para validacao fora da amostra",
    x = "Ano",
    y = "Temperatura media mensal (graus C)",
    color = "Amostra"
  ) +
  theme_minimal(base_size = 12)

ggsave("figuras/fig10_treino_teste.png", grafico_treino_teste, width = 10, height = 4.8, dpi = 300)
ggsave("figuras/fig10_treino_teste.pdf", grafico_treino_teste, width = 10, height = 4.8)

# ajuste validacao 
metricas_previsao <- function(observado, previsto) {
  erro <- as.numeric(observado) - as.numeric(previsto)
  tibble(
    MAE = mean(abs(erro), na.rm = TRUE),
    RMSE = sqrt(mean(erro^2, na.rm = TRUE)),
    MAPE = mean(abs(erro / as.numeric(observado)), na.rm = TRUE) * 100
  )
}

ajustar_validar_sarima <- function(nome, ordem, sazonal, treino, teste, h) {
  tryCatch({
    usa_constante <- (ordem[2] == 0 && sazonal[2] == 0)
    
    ajuste <- Arima(
      treino,
      order = ordem,
      seasonal = list(order = sazonal, period = 12),
      method = "ML",
      include.constant = usa_constante
    )
    
    prev <- forecast(ajuste, h = h)
    met <- metricas_previsao(teste, prev$mean)
    
    tibble(
      modelo = nome,
      p = ordem[1], d = ordem[2], q = ordem[3],
      P = sazonal[1], D = sazonal[2], Q = sazonal[3],
      AIC_treino = AIC(ajuste),
      BIC_treino = BIC(ajuste),
      MAE_teste = met$MAE,
      RMSE_teste = met$RMSE,
      MAPE_teste = met$MAPE,
      convergiu = TRUE,
      mensagem = NA_character_
    )
  }, error = function(e) {
    tibble(
      modelo = nome,
      p = ordem[1], d = ordem[2], q = ordem[3],
      P = sazonal[1], D = sazonal[2], Q = sazonal[3],
      AIC_treino = NA_real_,
      BIC_treino = NA_real_,
      MAE_teste = NA_real_,
      RMSE_teste = NA_real_,
      MAPE_teste = NA_real_,
      convergiu = FALSE,
      mensagem = conditionMessage(e)
    )
  })
}

ajustar_sarima_por_linha <- function(linha, serie) {
  Arima(
    serie,
    order = c(linha$p, linha$d, linha$q),
    seasonal = list(order = c(linha$P, linha$D, linha$Q), period = 12),
    method = "ML",
    include.constant = (linha$d == 0 && linha$D == 0)
  )
}

# modelos
modelos_nivel <- tribble(
  ~modelo,                              ~p, ~d, ~q, ~P, ~D, ~Q,
  "SARIMA(0,0,0)x(1,0,1)[12]",          0,  0,  0,  1,  0,  1,
  "SARIMA(1,0,0)x(1,0,1)[12]",          1,  0,  0,  1,  0,  1,
  "SARIMA(0,0,1)x(1,0,1)[12]",          0,  0,  1,  1,  0,  1,
  "SARIMA(1,0,1)x(1,0,1)[12]",          1,  0,  1,  1,  0,  1,
  "SARIMA(2,0,0)x(1,0,1)[12]",          2,  0,  0,  1,  0,  1,
  "SARIMA(0,0,2)x(1,0,1)[12]",          0,  0,  2,  1,  0,  1,
  "SARIMA(1,0,2)x(1,0,1)[12]",          1,  0,  2,  1,  0,  1,
  "SARIMA(2,0,1)x(1,0,1)[12]",          2,  0,  1,  1,  0,  1,
  "SARIMA(2,0,2)x(1,0,1)[12]",          2,  0,  2,  1,  0,  1,
  "SARIMA(1,0,0)x(0,0,1)[12]",          1,  0,  0,  0,  0,  1,
  "SARIMA(1,0,1)x(0,0,1)[12]",          1,  0,  1,  0,  0,  1,
  "SARIMA(2,0,1)x(0,0,1)[12]",          2,  0,  1,  0,  0,  1
)

modelos_diferenciados <- tribble(
  ~modelo,                              ~p, ~d, ~q, ~P, ~D, ~Q,
  "SARIMA(0,1,1)x(0,1,1)[12]",          0,  1,  1,  0,  1,  1,
  "SARIMA(1,1,0)x(0,1,1)[12]",          1,  1,  0,  0,  1,  1,
  "SARIMA(1,1,1)x(0,1,1)[12]",          1,  1,  1,  0,  1,  1,
  "SARIMA(0,1,2)x(0,1,1)[12]",          0,  1,  2,  0,  1,  1,
  "SARIMA(2,1,0)x(0,1,1)[12]",          2,  1,  0,  0,  1,  1,
  "SARIMA(2,1,1)x(0,1,1)[12]",          2,  1,  1,  0,  1,  1,
  "SARIMA(1,1,2)x(0,1,1)[12]",          1,  1,  2,  0,  1,  1,
  "SARIMA(0,1,1)x(1,1,0)[12]",          0,  1,  1,  1,  1,  0,
  "SARIMA(1,1,0)x(1,1,0)[12]",          1,  1,  0,  1,  1,  0,
  "SARIMA(1,1,1)x(1,1,0)[12]",          1,  1,  1,  1,  1,  0,
  "SARIMA(0,1,1)x(1,1,1)[12]",          0,  1,  1,  1,  1,  1,
  "SARIMA(1,1,1)x(1,1,1)[12]",          1,  1,  1,  1,  1,  1
)

# Ajusta e valida modelos de cada grupo
comparacao_nivel <- pmap_dfr(
  modelos_nivel,
  function(modelo, p, d, q, P, D, Q) {
    ajustar_validar_sarima(
      nome = modelo,
      ordem = c(p, d, q),
      sazonal = c(P, D, Q),
      treino = serie_treino,
      teste = serie_teste,
      h = h_teste
    )
  }
) %>% arrange(BIC_treino)

comparacao_diferenciados <- pmap_dfr(
  modelos_diferenciados,
  function(modelo, p, d, q, P, D, Q) {
    ajustar_validar_sarima(
      nome = modelo,
      ordem = c(p, d, q),
      sazonal = c(P, D, Q),
      treino = serie_treino,
      teste = serie_teste,
      h = h_teste
    )
  }
) %>% arrange(BIC_treino)

write_csv(comparacao_nivel, "tabelas/modelos_nivel_d0D0.csv")
write_csv(comparacao_diferenciados, "tabelas/modelos_diferenciados_d1D1.csv")

print(comparacao_nivel)
print(comparacao_diferenciados)

# BIC de cada grupo
melhor_nivel <- comparacao_nivel %>%
  filter(convergiu) %>%
  slice_min(BIC_treino, n = 1, with_ties = FALSE) %>%
  mutate(grupo = "d=0, D=0")

melhor_diferenciado <- comparacao_diferenciados %>%
  filter(convergiu) %>%
  slice_min(BIC_treino, n = 1, with_ties = FALSE) %>%
  mutate(grupo = "d=1, D=1")

melhor_grupos <- bind_rows(melhor_nivel, melhor_diferenciado) %>%
  arrange(MAPE_teste)

# Escolhi fora da amostra.
modelo_escolhido_linha <- campeoes_grupos %>% slice(1)


print(campeoes_grupos)
print(modelo_escolhido_linha)


comparacao_todos <- bind_rows(
  comparacao_nivel %>% mutate(grupo = "d=0, D=0"),
  comparacao_diferenciados %>% mutate(grupo = "d=1, D=1")
) %>%
  filter(convergiu) %>%
  arrange(MAPE_teste)


#validando
modelo_final_treino <- ajustar_sarima_por_linha(modelo_escolhido_linha, serie_treino)
summary(modelo_final_treino)

previsao_teste <- forecast(modelo_final_treino, h = h_teste, level = 95)
metricas_final_teste <- metricas_previsao(serie_teste, previsao_teste$mean)
accuracy_final <- accuracy(previsao_teste, serie_teste)

write_csv(metricas_final_teste, "tabelas/metricas_modelo_final_teste.csv")
write_csv(as_tibble(accuracy_final, rownames = "amostra"), "tabelas/accuracy_modelo_final_teste.csv")

print(metricas_final_teste)
print(accuracy_final)

# Tabela observados x previstos no teste
datas_teste <- tail(serie_mensal$datas, h_teste)
validacao_teste_tabela <- tibble(
  datas = datas_teste,
  observado = as.numeric(serie_teste),
  previsto = as.numeric(previsao_teste$mean),
  erro = observado - previsto,
  erro_abs = abs(erro),
  erro_percentual_abs = abs(erro / observado) * 100,
  li_95 = as.numeric(previsao_teste$lower[, 1]),
  ls_95 = as.numeric(previsao_teste$upper[, 1])
)

print(validacao_teste_tabela)

png("figuras/fig11_validacao_teste.png", width = 3000, height = 1600, res = 300)
print(
  autoplot(previsao_teste) +
    autolayer(serie_teste, series = "Teste observado") +
    labs(
      title = "Validacao fora da amostra - ultimos 24 meses",
      subtitle = paste0("Modelo: ", modelo_escolhido_linha$modelo),
      x = "Ano",
      y = "Temperatura media mensal (graus C)",
      color = "Serie"
    ) +
    theme_minimal(base_size = 12)
)
dev.off()

# ajuste 
modelo_final <- ajustar_sarima_por_linha(modelo_escolhido_linha, serie_temp_ts)

print(summary(modelo_final))
sink()

print(summary(modelo_final))

# Coeficientes do modelo final
coeficientes_modelo_final <- tibble(
  parametro = names(coef(modelo_final)),
  estimativa = as.numeric(coef(modelo_final))
)

# Valores ajustados
ajustados_modelo_final <- tibble(
  datas = serie_mensal$datas,
  observado = as.numeric(serie_temp_ts),
  ajustado = as.numeric(fitted(modelo_final)),
  residuo = as.numeric(residuals(modelo_final))
)

# analise residuoos 
residuos <- residuals(modelo_final)
residuos_validos <- na.omit(residuos)

fitdf_lb <- length(coef(modelo_final))

ljung_box <- tibble(
  lag = c(12, 24, 36),
  fitdf = fitdf_lb,
  estatistica = map_dbl(lag, ~ Box.test(residuos_validos, lag = .x, type = "Ljung-Box", fitdf = fitdf_lb)$statistic),
  p_valor = map_dbl(lag, ~ Box.test(residuos_validos, lag = .x, type = "Ljung-Box", fitdf = fitdf_lb)$p.value)
)


print(ljung_box)

shapiro_residuos <- tryCatch(
  shapiro.test(as.numeric(residuos_validos)),
  error = function(e) NULL
)

print(ljung_box)
print(shapiro_residuos)
print(checkresiduals(modelo_final))


# Graficos de diagnostico
png("figuras/fig12_diagnostico_residuos.png", width = 3300, height = 2500, res = 300)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot(residuos_validos, main = "Residuos do modelo final", ylab = "Residuo", xlab = "Ano")
abline(h = 0)
Acf(residuos_validos, lag.max = 48, main = "FAC dos residuos")
hist(residuos_validos, breaks = 24, main = "Histograma dos residuos", xlab = "Residuo")
par(mfrow = c(1, 1))
dev.off()

png("figuras/fig13_qqplot_residuos.png", width = 1800, height = 1500, res = 300)
qqnorm(residuos_validos, main = "QQ-plot dos residuos do modelo final")
qqline(residuos_validos)
dev.off()

png("figuras/fig14_checkresiduals.png", width = 2600, height = 2200, res = 300)
checkresiduals(modelo_final)
dev.off()

# ultimos meses

ultima_data <- max(serie_mensal$datas, na.rm = TRUE)
fim_previsao <- as.Date("2026-12-01")

h_previsao <- interval(ultima_data, fim_previsao) %/% months(1)

if (h_previsao <= 0) {
  warning("A serie até dezembro de 2026 ou mais ")
} else {
  previsao_2026 <- forecast(modelo_final, h = h_previsao, level = 95)
  
  datas_previsao <- seq.Date(
    from = ultima_data %m+% months(1),
    by = "month",
    length.out = h_previsao
  )
  
  previsoes_tabela <- tibble(
    datas = datas_previsao,
    previsao = as.numeric(previsao_2026$mean),
    limite_inferior_95 = as.numeric(previsao_2026$lower[, 1]),
    limite_superior_95 = as.numeric(previsao_2026$upper[, 1])
  )
  
  write_csv(previsoes_tabela, "tabelas/previsoes_ate_dezembro_2026.csv")
  print(previsoes_tabela)
  
  png("figuras/fig15_previsao_2026.png", width = 3000, height = 1600, res = 300)
  print(
    autoplot(previsao_2026) +
      labs(
        title = "Previsao da temperatura media mensal ate dezembro de 2026",
        subtitle = paste0("Modelo: ", modelo_escolhido_linha$modelo),
        x = "Ano",
        y = "Temperatura media mensal (graus C)"
      ) +
      theme_minimal(base_size = 12)
  )
  dev.off()
}


