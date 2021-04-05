# 1. Faça o mapa desse exemplo mas usando leaflet e markers no lugar de pontos:

# https://github.com/curso-r/main-visualizacao/blob/ddeec38aa31d181683a72af998f0835408c2c964/exemplos_de_aula/03-mapas-com-ggplot2.R#L136

iqa_cetesb <-
  st_read(
    "dados/shp/VWM_IQA_CETESB_2018_PTOPoint.shp",
    quiet = TRUE,
    options = "ENCODING=WINDOWS-1252"
  ) %>%
  # limpar o nome das colunas
  janitor::clean_names()


estado_sp <- geobr::read_state("SP")

ggplot() +
  geom_sf(data = estado_sp) +
  geom_sf(data = iqa_cetesb) +
  theme_bw()




# 2. Explore o shiny do {reactable} e reproduza o código dos exemplos que achar mais legais

# https://glin.github.io/reactable/articles/shiny-demo.html

# 3. [extra] O plotly também tem seu próprio jeito de fazer gráficos,
# além do ggplotly. Por exemplo:

plotly::plot_ly(
  ggplot2::economics, 
  type = "scatter", 
  mode   = 'markers',
  x = ~date, 
  y = ~pop
)



# reproduza o gráfico da linha abaixo usando essa sintaxe:

# https://github.com/curso-r/main-visualizacao/blob/ddeec38aa31d181683a72af998f0835408c2c964/exemplos_de_aula/04-miscelanea.R#L168


## https://plot.ly é uma empresa bem grande de visualização de dados.
## Eles produzem ferramentas para R, python, JavaScript
## e têm uma solução própria para fazer dashboards.

## Para essa parte, vamos nos limitar ao ggplotly(), que é um transformador
## de ggplot em plotly:

dados_idh_muni <- abjData::pnud_min %>% 
  mutate(code_muni = as.numeric(muni_id))

municipios <- geobr::read_municipality() %>% 
  filter(abbrev_state %in% c("RS", "PR", "SC")) %>% 
  inner_join(dados_idh_muni)


gg_idhm <- municipios %>% 
  ggplot(aes(x = gini, y = idhm, colour = espvida)) +
  geom_point() +
  facet_wrap(~ano)

plotly::ggplotly(gg_idhm)
