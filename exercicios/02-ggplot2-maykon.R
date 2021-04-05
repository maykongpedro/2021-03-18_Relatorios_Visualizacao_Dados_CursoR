# 1. Faça os exercícios da seção 9.2 do livro
# https://livro.curso-r.com/9-2-r-markdown.html

  #a. Leia o guia de referência do RMarkdown até o final.
      # ok  

  #b. Qual o propósito do YAML do documento?

      # Definir os metadados gerais do documento, como título, subtítulo, data e informações que 
      # correspondem ao documento inteiro.
    

  #c. Quais são as opções e resultados do parâmetro results=?
      # MARKUP: padrão defaut, mostra os resultados normalmente.
      # HIDE : knitr não vai mostrar o resultado dos códigos no documento final
      # HOLD : knitr vai mostrar os outputs aos poucos(por partes) até finalizar o chunk
      # ASIS : knitr vai msotrar os resultados sem reformatar eles (é útil se for um html, por exemplo)


# ====================================================================================


# 2. Teste os temas abaixo com os exemplos de aula:
# Dica: veja na documentação como instalar estes pacotes.

  # a) https://ewenme.github.io/ghibli/

  # load package
  library(dados)
  library(ghibli)
  library(tidyverse)

  # visualizando cores
  ghibli::ghibli_palettes$MarnieLight1

  # plotando gráfico
  pinguins %>% 
    count(especies) %>% 
    mutate(especies = fct_reorder(especies, n)) %>% 
    ggplot(aes(x = especies, y = n,fill = especies)) + 
    geom_col() +
    #ghibli::scale_fill_ghibli_d("MarnieMedium1") +
    ghibli::scale_fill_ghibli_d("KikiLight") +
    theme_minimal() 
  

  # b) https://ryo-n7.github.io/tvthemes/

  # testando com os dados de pinguins
  library(dados)
  library(tidyverse)
  library(tvthemes)
  library(extrafont)
  # para esse tipo de operação usamos o pacote forcats
  library(forcats)
  
  # ordenando do maior para o menor
  pinguins %>% 
    count(especies) %>% 
    mutate(especies = fct_reorder(especies, n)) %>% 
    ggplot(aes(x = especies, y = n,fill = especies)) + 
    geom_col() +
    #tvthemes::theme_avatar()
    #tvthemes::theme_hildaDay()
    tvthemes::theme_theLastAirbender()





  
# ====================================================================================


# 3. Escreva a label apropriada no eixo Y. Note que no eixo y
# está representada o log de alpha ao quadrado (linha 17).

  library(ggplot2)
  library(magrittr) # Carregar o pipe %>% 
  tibble::tibble(
    alpha = 1:10,
    alpha2 = log10(alpha^2)
    ) %>% 
    ggplot(aes(alpha, alpha2)) +
    geom_line() +
    labs(
      x = expression(alpha),
      y = expression(alpha^2)
    ) +
    theme_minimal(25)


# ====================================================================================


# 4. Crie um tema que você goste e nos mostre em sala

  # utilizo o tema abaixo para usar em gráficos que vou levar pro power point
  # theme para gráficos que serão usados em apresentações
  maykon_ppt_fundo_transparente <- function () 
  {
    ggthemes::theme_clean() +
      
      ggplot2::theme(
        
        # centraliza o título e deixa em negrito
        plot.title =
          element_text(
            hjust = 0.5,
            vjust = 0.5,
            face = "bold"
          ),
        
        # centraliza o subtítulo
        plot.subtitle =
          element_text(
            hjust = 0.5,
            vjust = 0.5
          ),
        
        # centraliza o título da legenda e deixa em negrito
        legend.title =
          element_text(face = "bold",
                       hjust = 0.5),
        
        # torna o fundo e o painel transparente e sem borda, a legenda também
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)
      )
    
  }


# ====================================================================================

# 5. Reproduza os gráficos das matérias abaixo:

# a) https://nucleo.jor.br/redes/2021-03-11-ciencia-dispara-twitter-divulgadores
# Reproduzir apenas o primeiro gráfico.
# Os dados e código foram disponibilizados no final da matéria.
# Primeiro, tente reproduzir sem olhar o código do ggplot, 
# rodando apenas até a linha 128:
# código: https://gist.github.com/lgelape/d854f7f23a900531e3fd4977d574e492#file-materia_engajamento_divulgadores-r-L128
# 
# O código abaixo serve para facilitar a importação dos dados.
# Caso não tenha algum destes pacotes instalado, lembre-se de instalar.

# Carregar pacotes
library(ggplot2)
library(magrittr) # Carregar o pipe %>%
library(lubridate)
library(googlesheets4)
library(dplyr)

# URL da base no google sheets - está no final da matéria
url <- "https://docs.google.com/spreadsheets/d/1SRT77C0SnPEZucaeMSWQKngE9F7Vwb4irGwDgB7CxtM/edit?usp=sharing"

# Importar a base usando a função read_sheet() do pacote googleshees4
# Será necessário realizar a autenticação com a API do google
# Ao executar essa função, acompanhe as mensagens do console.
  total_dia_brasileiros <- googlesheets4::read_sheet(url)
  
  yes# Veja a base importada
  dplyr::glimpse(total_dia_brasileiros)
  
  total_dia_brasileiros %>%
    # transformar a coluna created_at em classe data
    dplyr::mutate(created_at = lubridate::as_date(created_at)) %>%
    ggplot() +
    
    #  _____ # Comece por aqui :)
    geom_line(aes(x = created_at, y = media_15d), colour = "blue", size = 1) +
    geom_smooth(aes(x = created_at, y = media_15d), method = "lm", colour = "red", size = 1.3, se = F) +
    scale_x_date(date_breaks = "1 month",date_labels = "%b") +
    annotate(
      geom = "text",
      x = as.Date("2020-12-01"),
      y = 680,
      label = "Linha de tendência",
      colour = "red",
      size = 6
    )+
    labs(x = "", y = "Engajamento", caption = "Fonte: Twitter/Science Pulse/Análise Núcleo") +
    ggtitle(label = "Cientistas no Twitter",  subtitle = "Tendência de Engajamento diário (média móvel, 15 dias)") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.y = element_text(colour = "#848484"),
      plot.caption = element_text(colour = "#848484", hjust = 0)
    )

# esse exercício acima foi bem legal de fazer, tive que pesquisar vários itens e revisar algumas coisas,
# até conseguir deixar o mais parecido possível



# b) [EXERCÍCIO CANCELADO - NÃO FAZER]
# [DESAFIO] https://www.washingtonpost.com/graphics/2018/lifestyle/sinclair-broadcasting/
# Nesse caso, siga o passo-a-passo da matéria.
# código: https://r-journalism.com/posts/2018-07-17-sf-sinclair-map/map/
# OBS: como é de 2018, pode ser que seja necessário adaptar.


