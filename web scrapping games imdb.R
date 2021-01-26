#-----------------------------------------JOGOS-------------------------------------------#
install.packages('rvest')
library(rvest)
library(xml2)
library(tidyverse)

#read_html() comando da biblioteca xml2
#html() comando da biblioteca rvest 
#view() comando da biblioteca tibble que e do conjunto do tidyverse

site <- read_html("https://www.imdb.com/search/title/?title_type=video_game&sort=user_rating,desc&ref_=adv_prv")
site1 <- read_html("https://www.imdb.com/search/title/?title_type=video_game&sort=user_rating,desc&start=51&ref_=adv_nxt")


urls <-  site %>%
  html_nodes("h3.lister-item-header") %>%
  html_nodes("a") %>%
  html_attr("href")

urls1 <-  site1 %>%
  html_nodes("h3.lister-item-header") %>%
  html_nodes("a") %>%
  html_attr("href")

urls <- c(urls, urls1)

link <-paste('https://www.imdb.com',urls,sep ="")
link


i<- 0 
jogo <- c(NULL)
ano <- c(NULL)
nota <- c(NULL)
genero <- c(NULL)
diretor <- c(NULL)
produtora <- c(NULL)
lancamento <- c(NULL)
poster <- c(NULL)

inicio <- Sys.time()
for (x in link)
{
  i <- i+1
  j <- read_html(link[i]) %>%
    html_nodes("div.title_wrapper")%>%
    html_nodes("h1")%>%
    html_text()%>%
    gsub("^\\s+|\\s+$","",.)
  jogo <- c(jogo, j)
  
  a <- read_html(link[i]) %>%
    html_nodes("div.title_wrapper")%>%
    html_nodes("h1 a")%>%
    html_text()%>%
    gsub("^\\s+|\\s+$","",.)
  ano <- c(ano,a)
  
  n <- read_html(link[i]) %>%
    html_nodes("div.ratingValue") %>%
    html_nodes("strong span") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
  nota <- c(nota, n)
  
  g <- read_html(link[i]) %>%
    html_nodes("div.title_bar_wrapper") %>%
    html_nodes("div.titleBar") %>%
    html_nodes("div.subtext") %>%
    html_nodes("a") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
  genero <- c(genero, g[1])
  
  d <- read_html(link[i]) %>%
    html_nodes("div.plot_summary_wrapper") %>%
    html_nodes ("div.credit_summary_item") %>%
    html_nodes("a[href*='?ref_=tt_ov_dr']")%>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
  diretor <- c(diretor, d[1])
  
  p <- read_html(link[i]) %>%
    html_nodes("div#main_bottom.main") %>%
    html_nodes("div#titleDetails.article") %>%
    html_nodes("div.txt-block") %>%
    html_nodes("a[href*='?ref_=cons_tt_dt_co_1']") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
  produtora <- c(produtora, p[1])
  
  l <-read_html(link[i]) %>%
    html_nodes("div.title_wrapper") %>%
    html_nodes("div.subtext") %>%
    html_nodes("a[title*='See more release dates']") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
  lancamento <- c(lancamento, l[1])
  
  po <- read_html(link[i]) %>%
    html_nodes("div.poster")%>%
    html_nodes("a img")%>%
    html_attr("src") %>%
    gsub("^\\s+|\\s+$","",.)
  poster <- c(poster, po)
  
}
fim <-Sys.time()
difftime(fim,inicio)

games <-  data.frame(jogo = jogo, ano = ano, nota = nota, genero = genero, diretor = diretor,
                      produtora = produtora, lancamento = lancamento)

view(games)

################### Auxilio ################

#              INPUTS

# actionButton() - botao para executar uma acao.
# checkboxGroupInput() - um grupo de check boxes.
# checkboxInput() - um �nico check box.
# dateInput() - um calendario para selecao de data.
# dateRangeInput() - um par de calendários para escolher um intervalo de datas.
# fileInput() - uma ferramenta para auxiliar o upload de arquivos.
# numericInput() - Um campo para enviar números.
# radioButtons() - Um conjunto de botões para selecao.
# selectInput() - Um select box com um conjunto de opcoes.
# sliderInput() - Um slider.
# textInput() - Um campo para enviar texto.

#             OUTPUTS

# dataTableOutput() - para data frames.
# tableOutput() - para data frames/tabelas formato mais simples.
# htmlOutput() ou uiOutput() - para codigo HTML.
# imageOutput() - para imagens.
# plotOutput() - para graficos.
# tableOutput() - para tabelas.
# textOutput() - para textos.
# verbatimTextOutput() - para textos nao-formatados.
# plotlyOutput() - *graficos que envolvem a biblioteca plotly*
# uiOutput -  um elemento do UI ou HTML


#             SERVER SIDE
# renderDataTable({}) - data frames.
# renderTable({}) - data frames/tabelas formato mais simples.
# renderImage({}) - imagens.
# renderPlot({}) - graficos.
# renderPrint({}) - qualquer printed output.
# renderTable({}) - data frames, matrizes, e outras estruturas em forma de tabela.
# renderText({}) - strings.
# renderPlotly({}) - *graficos que envolvem a biblioteca plotly*  
# renderUI({}) - um elemento do UI ou HTML.
