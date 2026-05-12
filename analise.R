### Análise descritiva

library(tidyverse)

respira <- readr::read_csv("data/raw/respira.csv", show_col_types = FALSE)

##grupo e sexo (sendo varáveis qualitativas nominais são descritas com a freq absoluta e a freq relativa)

table(respira$grupo)
prop.table(table(respira$grupo))*100

table(respira$sexo)
prop.table(table(respira$sexo))*100

table(respira$grupo, respira$sexo)
prop.table(table(respira$grupo, respira$sexo), margin = 1)*100

##idade (sendo variável quantitativa contínua é descrita com a média e o desvio padrão ou com a mediana e o IQR - intervalo inter quartil)

shapiro.test(respira$idade)
hist(respira$idade)

##como não apresenta distribuição normal, a idade é descrita com a mediana e com o IQR

respira %>%
  summarise(
    mediana_idade = median(idade),
    Q1 = quantile(idade, 0.25, na.rm = TRUE),
    Q3 = quantile(idade, 0.75, na.rm = TRUE),
    IQR_idade = Q3 - Q1
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    mediana_idade = median(idade),
    Q1 = quantile(idade, 0.25, na.rm = TRUE),
    Q3 = quantile(idade, 0.75, na.rm = TRUE),
    IQR_idade = Q3 - Q1
  )

##act_baseline, act_semana12, peak_flow_baseline, peak_flow_semana12 (sendo variáveis quantitativas contínuas são descritas com a média e o desvio padrão ou com a mediana e o IQR - intervalo inter quartil)
shapiro.test(respira$act_baseline)
hist(respira$act_baseline)

shapiro.test(respira$act_semana12)
hist(respira$act_semana12)

shapiro.test(respira$peak_flow_baseline)
hist(respira$peak_flow_baseline)

shapiro.test(respira$peak_flow_semana12)
hist(respira$peak_flow_semana12)

##como as variáveis apresentam distribuição normal, são descritas com a média e com o desvio padrão

respira%>%
  summarise(
    media_act_baseline = mean(act_baseline, na.rm = TRUE),
    dp_act_baseline = sd(act_baseline, na.rm = TRUE),
    
    media_act_12w = mean(act_semana12, na.rm = TRUE),
    dp_act_12w = sd(act_semana12, na.rm = TRUE)
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    media_act_baseline = mean(act_baseline, na.rm = TRUE),
    dp_act_baseline = sd(act_baseline, na.rm = TRUE),
    media_act_12w = mean(act_semana12, na.rm = TRUE),
    dp_act_12w = sd(act_semana12, na.rm = TRUE)
  )

respira%>%
  summarise(
    media_peak_flow_baseline = mean(peak_flow_baseline, na.rm = TRUE),
    dp_peak_flow_baseline = sd(peak_flow_baseline, na.rm = TRUE),
    
    media_peak_flow_12w = mean(peak_flow_semana12, na.rm = TRUE),
    dp_peak_flow_12w = sd(peak_flow_semana12, na.rm = TRUE)
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    media_peak_flow_baseline = mean(peak_flow_baseline, na.rm = TRUE),
    dp_peak_flow_baseline = sd(peak_flow_baseline, na.rm = TRUE),
    media_peak_flow_12w = mean(peak_flow_semana12, na.rm = TRUE),
    dp_peak_flow_12w = sd(peak_flow_semana12, na.rm = TRUE)
  )


##exacerbacoes_12sem sendo variável quantitativa discreta é descrita com a mediana e o IQR - intervalo inter quartil

shapiro.test(respira$exacerbacoes_12sem)
hist(respira$exacerbacoes_12sem)

respira %>%
  summarise(
    mediana_ex_12w = median(exacerbacoes_12sem, na.rm = TRUE),
    Q1_ex_12w = quantile(exacerbacoes_12sem, 0.25, na.rm = TRUE),
    Q3_ex_12w = quantile(exacerbacoes_12sem, 0.75, na.rm = TRUE),
    IQR_ex_12w = Q3_ex_12w - Q1_ex_12w
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    mediana_ex_12w = median(exacerbacoes_12sem, na.rm = TRUE),
    Q1_ex_12w = quantile(exacerbacoes_12sem, 0.25, na.rm = TRUE),
    Q3_ex_12w = quantile(exacerbacoes_12sem, 0.75, na.rm = TRUE),
    IQR_ex_12w = Q3_ex_12w - Q1_ex_12w
  )

##aqlq_semana12 sendo variável quantitativa contínua é descrita com a média e o desvio padrão ou com a mediana e o IQR - intervalo inter quartil)

shapiro.test(respira$aqlq_semana12)
hist(respira$aqlq_semana12)

##como o aqlq_semana12 apresenta distribuição normal, é descrita com a média e o desvio padrão

respira%>%
  summarise(
    media_aqlq_12w = mean(aqlq_semana12, na.rm = TRUE),
    dp_aqlq_12w = sd(aqlq_semana12, na.rm = TRUE)
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    media_aqlq_12w = mean(aqlq_semana12, na.rm = TRUE),
    dp_aqlq_12w = sd(aqlq_semana12, na.rm = TRUE)
  )

##adesao sendo variável quantitativa continua é descrita com a média e o desvio padrão ou com mediana e o IQR - intervalo inter quartil

shapiro.test(respira$adesao)
hist(respira$adesao)

##como a adesao apresenta distribuição normal, é descrita com a média e o desvio padrão

respira%>%
  summarise(
    media_adesao = mean(adesao, na.rm = TRUE),
    dp_adesao = sd(adesao, na.rm = TRUE)
  )

respira%>%
  group_by(grupo) %>%
  summarise(
    media_adesao = mean(adesao, na.rm = TRUE),
    dp_adesao = sd(adesao, na.rm = TRUE)
  )

##visualização dos dados
ggplot(respira, aes(x = grupo, y = act_semana12, fill = grupo)) +
  geom_boxplot() +
  labs(title = "Boxplot do act às 12 semanas por grupo", x = "Grupo", y = "act score") 

tabela <- respira %>%
  select(grupo, sexo, idade, act_baseline, peak_flow_baseline, aqlq_semana12, adesao) %>%
  tbl_summary(
    by = grupo,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  )

grafico <- ggplot(respira, aes(x = grupo, y = act_semana12, fill = grupo)) +
  geom_boxplot() +
  labs(
    title = "Boxplot do ACT às 12 semanas por grupo",
    x = "Grupo",
    y = "ACT score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
