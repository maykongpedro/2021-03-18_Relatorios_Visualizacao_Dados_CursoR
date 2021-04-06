# carregar pacotes
library(tidyverse)
library(leaflet)
library(sf)

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


iqa_cetesb %>% 
  # add leaflet vazio
  leaflet() %>% 
  # add mapa
  addTiles() %>% 
  
  #  dúvida: como eu faço pra adicionar um shape do estado antes dos pontos?
  
  # add marcadores
  addMarkers(
    popup = iqa_cetesb,
    clusterOptions = markerClusterOptions()
  )


# =================================================================================#


# 2. Explore o shiny do {reactable} e reproduza o código dos exemplos que achar mais legais

# https://glin.github.io/reactable/articles/shiny-demo.html

# carrega pacotes
library(reactable)
library(shiny)

theme <- reactableTheme(borderColor = "#dfe2e5", stripedColor = "#f6f8fa", 
                        highlightColor = "#f0f5f9", cellPadding = "8px 12px")
# option 1
reactable(
  iris,
  sortable = FALSE,
  resizable = TRUE,
  showPageSizeOptions = TRUE,
  onClick = "expand",
  striped = TRUE,
  highlight = TRUE,
  compact = TRUE,
  columns = list(
    Sepal.Length = colDef(
      name = "Sepal Length",
      aggregate = "max",
      format = colFormat(suffix = " cm",
                         digits = 1)
    ),
    Sepal.Width = colDef(
      name = "Sepal Width",
      defaultSortOrder = "desc",
      aggregate = "mean",
      format = list(aggregated = colFormat(suffix = " (avg)",
                                           digits = 2)),
      cell = function(value) {
        if (value >= 3.3) {
          classes <- "tag num-high"
        } else if (value >= 3) {
          classes <- "tag num-med"
        } else {
          classes <- "tag num-low"
        }
        value <-
          format(value, nsmall = 1)
        span(class = classes, value)
      },
      footer = function(values) {
        div(tags$b("Average: "), round(mean(values), 1))
      }
    ),
    Petal.Length = colDef(name = "Petal Length",
                          aggregate = "sum"),
    Petal.Width = colDef(name = "Petal Width",
                         aggregate = "count"),
    Species = colDef(aggregate = "frequency")
  ),
  details = function(index) {
    if (index == 3) {
      tabsetPanel(tabPanel("plot", plotOutput("plot")),
                  tabPanel("subtable", reactable(iris[1:3, 1:2], fullWidth = FALSE)))
    } else if (index == 5) {
      paste("Details for row:", index)
    }
  }, 
  theme = theme
)




# option 2
theme <-
  reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px"
  )

reactable(
  iris,
  sortable = FALSE,
  resizable = TRUE,
  showPageSizeOptions = TRUE,
  onClick = "expand",
  compact = TRUE,
  groupBy = "Species",
  columns = list(
    Sepal.Length = colDef(
      name = "Sepal Length",
      aggregate = "max",
      format = colFormat(suffix = " cm",
                         digits = 1)
    ),
    Sepal.Width = colDef(
      name = "Sepal Width",
      defaultSortOrder = "desc",
      aggregate = "mean",
      format = list(aggregated = colFormat(suffix = " (avg)",
                                           digits = 2)),
      cell = function(value) {
        if (value >= 3.3) {
          classes <- "tag num-high"
        } else if (value >= 3) {
          classes <- "tag num-med"
        } else {
          classes <- "tag num-low"
        }
        value <-
          format(value, nsmall = 1)
        span(class = classes, value)
      }
    ),
    Petal.Length = colDef(name = "Petal Length",
                          aggregate = "sum"),
    Petal.Width = colDef(name = "Petal Width",
                         aggregate = "count"),
    Species = colDef(aggregate = "frequency")
  ),
  details = function(index) {
    if (index == 3) {
      tabsetPanel(tabPanel("plot", plotOutput("plot")),
                  tabPanel("subtable", reactable(iris[1:3, 1:2], fullWidth = FALSE)))
    } else if (index == 5) {
      paste("Details for row:", index)
    }
  },
  theme = theme
)


# =================================================================================#

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

# carrega pacotes
library(plotly)
library(tidyverse)

# carrega dados
dados_idh_muni <- abjData::pnud_min %>% 
  mutate(code_muni = as.numeric(muni_id))

municipios <- geobr::read_municipality() %>% 
  filter(abbrev_state %in% c("RS", "PR", "SC")) %>% 
  inner_join(dados_idh_muni)

# plota
plotly::plot_ly(
  data = municipios,
  type = "scatter",
  color = ~espvida,
  x = ~gini,
  y = ~idhm
)

# Não consegui fazer o gráfico com essa sintaxe, encontro o erro:
    # Error in `[.data.frame`(x, i) : colunas indefinidas selecionadas
