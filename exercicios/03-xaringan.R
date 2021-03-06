# Caso não tenha lido esses capítulos,
# leia antes de fazer os exercícios:

# https://livro.curso-r.com/9-relatorios.html
# https://livro.curso-r.com/9-1-markdown.html
# https://livro.curso-r.com/9-2-r-markdown.html


# Vamos criar uma apresentação usando o pacote xaringan!
# Separamos em várias etapas para experimentar os conceitos
# mostrados no curso.
# Caso alguma etapa dê errado, anote e avise a gente sobre a dúvida, 
# mas tente continuar fazendo os outros exercícios possíveis.
# A partir do exercício 6, cada exercício é algo diferente para 
# experimentar adicionar na apresentação.


# 1. O código abaixo cria uma pasta chamada exercicios_resolucao, 
# e outra pasta dentro dela chamada xaringan. É lá que vamos organizar
# os materiais deste exercício!

fs::dir_create("exercicios_resolucao/xaringan")

# 2. Crie também as pastas para adicionar as imagens,
#  as bibliotecas e arquivos de folhas de estilo (css):

fs::dir_create("exercicios_resolucao/xaringan/img")

fs::dir_create("exercicios_resolucao/xaringan/libs")

fs::dir_create("exercicios_resolucao/xaringan/css")

# 3. Crie um arquivo R Markdown com o template `Ninja presentation`,
# e com o nome `index.html`, e salve na pasta que estamos salvando o exercicio
# (a pasta exercicios_resolucao/xaringan/  )

  # ok

# 4. Apague o conteúdo após o `YAML` 

  # ok

# 5. Aperte `Knit` e veja o resultado. Faça isso sempre, após fazer cada uma
# das etapas abaixo. Veja como ficou o resultado, e caso ocorrer um erro, 
# será mais fácil saber onde é necessário arrumar :)

  # ok

# 6. Agora vamos alterar os metadados da apresentação (YAML):
  # a. Adicione um título # ok
  # b. Adicione um subtítulo   # ok
  # c. Adicione seu nome no campo de autoria # ok
  # d. Adicione a data  # ok


# 7. Para que a sua apresentação possa ser visualizada offline,
# salve a biblioteca `remark.js` e altere o chakra no YAML.
# Confira a apresentação da aula para relembrar como fazer isso!

  # ok


## 8. Mude o estilo da sua apresentação! 

  # ok


# 9. Adicione um slide e adicione um parágrafo que esteja marcado com markdown
# Utilize negrito, itálico, links, texto formatado como código, etc.
# Caso queira relembrar, leia este material: 
# https://livro.curso-r.com/9-1-markdown.html  
# Lembre-se que para delimitar os slides utilizamos:  --

  # ok
  
  
# 10. Adicione uma tabela a partir de um data.frame. 
# (Você pode usar `mtcars`, por exemplo).
# Utilize o pacote a sua escolha para formatar a tabela.

  # ok
  

# 11. Adicione duas imagens na sua apresentação:
# Uma que esteja salva na pasta img que criamos, 
# e outra que esteja na internet, utilizando a URL da imagem.

  # ok 


# 12. Inclua um chunk de código em que a variável x recebe o valor 4, 
# a variável y recebe o valor 6 e calcula a soma de x e y.

  # ok
  

## 13. (Opcional) Utilize alguma (ou mais de uma) extensão
# utilizando o pacote xaringanExtra.
# Veja a documentação do pacote: 
# https://github.com/gadenbuie/xaringanExtra
# https://pkg.garrickadenbuie.com/xaringanExtra/
devtools::install_github("gadenbuie/xaringanExtra")

  # ok

## 13. Gere um arquivo PDF da sua apresentação usando a função 
# chrome_print() do pacote pagedown. 


## 14. Envie no classroom (opcional) para que a gente confira o resultado:
# os arquivos .pdf, .Rmd, .html, .css (caso tenha feito algum personalizado)
  


## OPCIONAL ------
  
## 15. Deixe sua apresentação online e envie o link como reso! 
# Ex. com o Netlify Drop - https://app.netlify.com/drop, 
# GitHub Pages ou Netlify.