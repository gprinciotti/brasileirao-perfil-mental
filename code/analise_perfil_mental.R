# packages
library(tidyverse)
library(ggrepel)
library(scales)
library(rvest)

# tema ggplot
source('code/_functions/theme_vgp.R', encoding = 'UTF-8')

# tratamento dos dados ----

# leitura dos dados pré-tratados do Transfermarkt
transfermarkt <- read_rds('data/transfermarkt_partidas_brasileirao_2015-2024.rds') |> 
  select(temporada, rodada, time_mandante, time_visitante, gols_mandante, gols_visitante, eventos) |> 
  unnest(cols = eventos) |>
  mutate(
    minuto_evento = as.numeric(minuto_evento),
    time_vencedor = case_when(
      gols_mandante > gols_visitante ~ time_mandante,
      gols_mandante < gols_visitante ~ time_visitante,
      TRUE ~ 'Empate'
    )
  )

# flag de marcação do primeiro gol
primeiro_gol <- transfermarkt |>
  filter(placar_evento == '1:0' | placar_evento == '0:1') |> 
  group_by(temporada, rodada, time_mandante, time_visitante, time_vencedor) |> 
  slice(1) |> 
  mutate(
    time_primeiro_gol = case_when(
      placar_evento == '1:0' ~ time_mandante, 
      placar_evento == '0:1' ~ time_visitante,
      TRUE ~ NA_character_
    )
  ) |>
  ungroup() |> 
  select(temporada, rodada, time_mandante, time_visitante, time_vencedor, time_primeiro_gol)

# painel expandido de partidas
painel_partidas <- primeiro_gol |> 
  pivot_longer(
    cols = c('time_mandante', 'time_visitante'),
    names_to = 'tipo_mando',
    names_prefix = 'time_',
    values_to = 'time'
  ) |> 
  mutate(
    tipo_mando = str_to_title(tipo_mando),
    pontos_ganhos = case_when(
      time == time_vencedor ~ 3,
      time_vencedor == 'Empate' ~ 1,
      TRUE ~ 0
    ),
    flag_marcou_primeiro = if_else(time == time_primeiro_gol, 1, 0),
    flag_sofreu_primeiro = if_else(time != time_primeiro_gol, 1, 0)
  )

rm(transfermarkt, primeiro_gol)

# análise geral ----

# cálculo dos indicadores de efetividade e resiliência
index <- painel_partidas |> 
  group_by(time) |> 
  summarise(
    partidas_total = n(),
    partidas_marcou_primeiro = sum(flag_marcou_primeiro, na.rm = TRUE),
    partidas_sofreu_primeiro = sum(flag_sofreu_primeiro, na.rm = TRUE),
    pontos_marcou_primeiro = sum(pontos_ganhos[flag_marcou_primeiro == 1], na.rm = TRUE),
    pontos_sofreu_primeiro = sum(pontos_ganhos[flag_sofreu_primeiro == 1], na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  mutate(
    idx_efetividade = pontos_marcou_primeiro / (partidas_marcou_primeiro * 3),
    idx_resiliencia = pontos_sofreu_primeiro / (partidas_sofreu_primeiro * 3),
    prop_marcou_primeiro = partidas_marcou_primeiro / partidas_total
  ) |> 
  arrange(desc(partidas_total)) |>
  slice(1:20)

# plot do mapa mental dos principais times do Brasileirão
times_principais <- index |> pull(time)

mediana_efetividade <- median(index$idx_efetividade, na.rm = TRUE)
mediana_resiliencia <- median(index$idx_resiliencia, na.rm = TRUE)

ggplot(data = index, mapping = aes(x = idx_resiliencia, y = idx_efetividade, label = time)) +
  geom_hline(yintercept = mediana_efetividade, linetype = 'dashed', linewidth = 0.8) +
  geom_vline(xintercept = mediana_resiliencia, linetype = 'dashed', linewidth = 0.8) +
  geom_point(mapping = aes(color = prop_marcou_primeiro), size = 4) +
  geom_text_repel(max.overlaps = Inf,  box.padding = 0.5, min.segment.length = 0) +
  labs(
    title = 'Perfil mental dos times do Brasileirão',
    subtitle = 'Relação entre Efetividade e Resiliência. A cor indica a frequência de jogos em que o time marcou o primeiro gol.',
    caption = str_glue('As linha pontilhadas representam a mediana de Resiliência ({percent(mediana_resiliencia, accuracy = 0.1)}) e Efetividade ({percent(mediana_efetividade, accuracy = 0.1)}) dos times no período analisado (2015-2024).'),
    x = 'Índice de Resiliência',
    y = 'Índice de Efetividade',
    color = 'Frequência em que\nabriu o placar:'
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.05), limits = c(0, 0.4), labels = percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05), limits = c(0.55, 0.95), labels = percent_format(accuracy = 1)) +
  scale_color_gradient2(
    low = palette_vgp[['vermelho-2']],
    mid = palette_vgp[['neve']],
    high = palette_vgp[['azul-2']],
    midpoint = 0.5,
    breaks = seq(0.35, 0.65, 0.05),
    limits = c(0.35, 0.65),
    labels = percent_format(accuracy = 1), 
    guide = guide_colorbar(barwidth = unit(15, 'lines'), barheight = unit(1, 'lines'))
  ) +
  theme_vgp() +
  theme(legend.position = 'top', panel.grid.minor = element_blank())

ggsave(
  filename = 'outputs/mapa_perfil_mental.png', plot = last_plot(), device = 'png',
  width = 28, height = 20, units = 'cm', dpi = 200, bg = 'white'
)

dev.off()

# análise por tipo de mando ----

# cálculo dos indicadores de efetividade e resiliência
index_mando <- painel_partidas |> 
  group_by(time, tipo_mando) |> 
  summarise(
    partidas_total = n(),
    partidas_marcou_primeiro = sum(flag_marcou_primeiro, na.rm = TRUE),
    partidas_sofreu_primeiro = sum(flag_sofreu_primeiro, na.rm = TRUE),
    pontos_marcou_primeiro = sum(pontos_ganhos[flag_marcou_primeiro == 1], na.rm = TRUE),
    pontos_sofreu_primeiro = sum(pontos_ganhos[flag_sofreu_primeiro == 1], na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  mutate(
    idx_efetividade = pontos_marcou_primeiro / (partidas_marcou_primeiro * 3),
    idx_resiliencia = pontos_sofreu_primeiro / (partidas_sofreu_primeiro * 3),
    prop_marcou_primeiro = partidas_marcou_primeiro / partidas_total
  ) |> 
  filter(time %in% times_principais)

# dumbell plot do índice de resiliência
ordem_times_resiliencia <- index_mando %>%
  filter(tipo_mando == 'Mandante') |> 
  arrange(idx_resiliencia) |> 
  pull(time)

ggplot(data = index_mando, mapping = aes(x = idx_resiliencia, y = factor(time, levels = ordem_times_resiliencia), color = tipo_mando)) +
  geom_vline(xintercept = mediana_resiliencia, linetype = 'dashed', linewidth = 0.5, color = palette_vgp[['cinza']]) +
  geom_line(color = palette_vgp[['cinza']], linewidth = 0.7) +
  geom_point(size = 4, show.legend = TRUE) +
  labs(
    title = 'O fator "casa" no perfil mental dos times do Brasileirão',
    subtitle = 'Índice de Resiliência em jogos como mandante versus visitante',
    caption = str_glue('A linha pontilhada representa a mediana de Resiliência ({percent(mediana_resiliencia, accuracy = 0.1)}) dos times no período analisado (2015-2024).'),
    x = 'Índice de Resiliência',
    y = '',
    color = 'Tipo de mando:'
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 0.6), labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c('Mandante' = palette_vgp[['vermelho-2']], 'Visitante' = palette_vgp[['cinza']])) +
  theme_vgp() +
  theme(legend.position = 'top')

ggsave(
  filename = 'outputs/mando_resiliencia.png', plot = last_plot(), device = 'png',
  width = 22, height = 24, units = 'cm', dpi = 200, bg = 'white'
)

dev.off()

rm(ordem_times_resiliencia, mediana_resiliencia)

# dumbell plot do índice de efetividade
ordem_times_efetividade <- index_mando %>%
  filter(tipo_mando == 'Mandante') |> 
  arrange(idx_efetividade) |> 
  pull(time)

ggplot(data = index_mando, mapping = aes(x = idx_efetividade, y = factor(time, levels = ordem_times_efetividade), color = tipo_mando)) +
  geom_vline(xintercept = mediana_efetividade, linetype = 'dashed', linewidth = 0.5, color = palette_vgp[['cinza']]) +
  geom_line(color = palette_vgp[['cinza']], linewidth = 0.7) +
  geom_point(size = 4, show.legend = TRUE) +
  labs(
    title = 'O fator "casa" no perfil mental dos times do Brasileirão',
    caption = str_glue('A linha pontilhada representa a mediana de Efetividade ({percent(mediana_efetividade, accuracy = 0.1)}) dos times no período analisado (2015-2024).'),
    subtitle = 'Índice de Efetividade em jogos como mandante versus visitante',
    x = 'Índice de Efetividade',
    y = '',
    color = 'Tipo de mando:'
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0.4, 1), labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c('Mandante' = palette_vgp[['vermelho-2']], 'Visitante' = palette_vgp[['cinza']])) +
  theme_vgp() +
  theme(legend.position = 'top')

ggsave(
  filename = 'outputs/mando_efetividade.png', plot = last_plot(), device = 'png',
  width = 22, height = 24, units = 'cm', dpi = 200, bg = 'white'
)

dev.off()

rm(ordem_times_efetividade, mediana_efetividade)

# análise por temporada ----

# cálculo dos indicadores de efetividade e resiliência
index_temporada <- painel_partidas |>
  group_by(time, temporada) |>
  summarise(
    partidas_total = n(),
    partidas_marcou_primeiro = sum(flag_marcou_primeiro, na.rm = TRUE),
    partidas_sofreu_primeiro = sum(flag_sofreu_primeiro, na.rm = TRUE),
    pontos_marcou_primeiro = sum(pontos_ganhos[flag_marcou_primeiro == 1], na.rm = TRUE),
    pontos_sofreu_primeiro = sum(pontos_ganhos[flag_sofreu_primeiro == 1], na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(
    idx_efetividade = pontos_marcou_primeiro / (partidas_marcou_primeiro * 3),
    idx_resiliencia = pontos_sofreu_primeiro / (partidas_sofreu_primeiro * 3),
    prop_marcou_primeiro = partidas_marcou_primeiro / partidas_total
  )

# construção de ranking baseado no mapa mental (média harmônica dos índices)
ranking_mental <- index_temporada |>
  mutate(harmonic_mean = 3 / (1 / idx_efetividade + 1 / idx_resiliencia + 1 / prop_marcou_primeiro)) |>
  group_by(temporada) |>
  mutate(ranking_mental = rank(-harmonic_mean, ties.method = 'min')) |>
  ungroup() |>
  select(temporada, time, ranking_mental) |>
  arrange(temporada, ranking_mental)

# extração da tabela dos campeonatos com ranking final real (fonte: Wikipedia)
extract_ranking_real <- function(ano) {
  url <- str_c('https://pt.wikipedia.org/wiki/Campeonato_Brasileiro_de_Futebol_de_', ano, '_-_S%C3%A9rie_A')
  
  read_html(url) |>
    html_element(xpath = '//table[.//th[contains(., "Pts")] and .//th[contains(., "Pts")]]') |>
    html_table(fill = TRUE) |>
    select(time = 2, ranking_real = 1) |>
    mutate(temporada = ano, .before = 'time')
}

ranking_real <- map_dfr(.x = 2015:2024, .f = extract_ranking_real)
rm(extract_ranking_real)

# merge final das bases
clean_time <- function(time) {
  time |>
    str_remove_all(pattern = '\\(C\\)') |>
    str_replace_all(pattern = '\\sMineiro', replacement = '-MG') |>
    str_replace_all(pattern = '\\sGoianiense', replacement = '-GO') |>
    str_replace_all(pattern = 'Atlético Paranaense|Athletico Paranaense', replacement = 'Athletico-PR') |>
    str_replace_all(pattern = 'Red Bull', replacement = 'RB') |>
    str_squish()
}

ranking_final <- ranking_real |>
  mutate(time = clean_time(time)) |>
  left_join(ranking_mental, by = join_by('temporada', 'time'))

rm(ranking_real, ranking_mental, clean_time)

# comparação entre rankings
ranking_final |> select(ranking_real, ranking_mental) |> cor(method = 'spearman')

ggplot(data = ranking_final, mapping = aes(x = ranking_mental, y = ranking_real)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  scale_x_continuous(breaks = seq(1, 20, 1), limits = c(1, 20)) +
  scale_y_continuous(breaks = seq(1, 20, 1), limits = c(1, 20)) +
  theme_vgp()

dev.off()
