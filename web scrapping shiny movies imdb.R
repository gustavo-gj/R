#----------------------------------FILMES----------------------------------------#
 install.packages('rvest')
 install.packages('tidyverse')
 library(rvest)
 library(xml2)
 library(tidyverse)
 library(shiny)
 
 #read_html() comando da biblioteca xml2
 #html() comando da biblioteca rvest 
  
  site <- read_html("https://www.imdb.com/chart/top?ref_=nv_mv_250")
  
  urls <-  site %>%
  html_nodes("td.titleColumn") %>%
  html_nodes("a") %>%
  html_attr("href")

  link <-paste('https://www.imdb.com',urls,sep ="")
  link
    

i<- 0
filme <- c(NULL)
ano <- c(NULL)
nota <-c(NULL)
duracao <- c(NULL)
genero <- c(NULL)
diretor <- c(NULL)
poster <-c(NULL)


inicio <- Sys.time()
 for (x in link)
  {
    i<-i+1
    
    f <- read_html(link[i]) %>%
    html_nodes("div.title_wrapper") %>%
    html_nodes("h1") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    filme <- c(filme, f)  
    
    a <- read_html(link[i]) %>%
    html_nodes("div.title_wrapper") %>%
    html_nodes("h1") %>%
    html_nodes("span a") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    ano <- c(ano, a)
    
    n <- read_html(link[i]) %>%
    html_nodes("div.ratingValue") %>%
    html_nodes("strong") %>%
    html_nodes("span") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    nota <- c(nota, n)
    
    d <- read_html(link[i]) %>%
    html_nodes("div#titleDetails.article") %>%
    html_nodes("div.txt-block") %>%
    html_nodes("time") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    duracao <- c(duracao, d[1])
    
    g <- read_html(link[i]) %>%
    html_nodes("div.title_wrapper") %>%
    html_nodes("div.subtext") %>%
    html_nodes("a[href*='genres']") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    genero <- c(genero, g[1])
    
    dir <- read_html(link[i]) %>%
    html_nodes("div.credit_summary_item") %>%
    html_nodes("a[href*='/?ref_=tt_ov_dr']") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$","",.)
    diretor <- c(diretor, dir[1])
      
    p<-read_html(link[i]) %>%
    html_nodes("div.title-overview") %>%
    html_nodes("div#title-overview-widget.heroic-overview") %>%
    html_nodes("div.poster") %>%
    html_nodes("a") %>%
    html_nodes("img") %>%
    html_attr("src") %>%
    gsub("^\\s+|\\s+$","",.)
    poster <-c(poster, p)
    
 }
fim <-Sys.time()
difftime(fim,inicio)


movies <-  data.frame(filme=filme, ano = ano,duracao = duracao ,nota = nota,
                        genero = genero, diretor = diretor, poster = poster)
view(movies)
movies$nota <- format(movies$nota, nsmall = 1)
view(movies)

ui <- fluidPage(
  headerPanel("IMDB TOP 250 RATED MOVIES"),
  
    sidebarPanel(
      selectInput(inputId = "genero", label ="Genero" , choices = c("Todos",unique(as.character(movies$genero)))),
      #sliderInput(inputId = "ano", label = "Ano", min = min(movies$ano), max=max(movies$ano), value = c(1921,2020), step = 5),
      width = 3,
      ),
  
   
  
        mainPanel(
          verbatimTextOutput(outputId =  "texto"),
          plotOutput(outputId = "plot"),
          
          fluidRow(
            column(2,uiOutput(outputId = "imagem")),
            column(2,uiOutput(outputId = "imagem2")),
            column(2,uiOutput(outputId = "imagem3")),
            column(2,uiOutput(outputId = "imagem4")),
            column(2,uiOutput(outputId = "imagem5"))
          ),
          
          
          
          tableOutput(outputId ="tabela")
                  )
  
        
      )

server <- function(input, output) {
  
      
  output$texto <- renderText({
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
  
      paste("The genre", input$genero, "has", nrow(movies), "movies in the IMDB Top 250 most ranked movies") # ntext()
    
    
  })
  
  output$plot <- renderPlot({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    movies$duracao <- as.integer(gsub("min","",movies$duracao))
   
    
    ggplot(movies, aes(x=movies$ano, y=movies$duracao, color=movies$genero)) + 
      geom_point(size=6)
    
  })
  
  
  output$imagem <- renderUI({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
    linkposter <- movies$poster

      tags$img(src = linkposter[1] ,width = "100px", height = "150px")
      
    
  })
  
  output$imagem2 <- renderUI({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
    linkposter <- movies$poster
    
    tags$img(src = linkposter[2] ,width = "100px", height = "150px")
    
  })
  
  output$imagem3 <- renderUI({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
    linkposter <- movies$poster
    
    tags$img(src = linkposter[3] ,width = "100px", height = "150px")
    
  })
  
  output$imagem4 <- renderUI({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
    linkposter <- movies$poster
    
    tags$img(src = linkposter[4] ,width = "100px", height = "150px")
    
  })
  
  output$imagem5 <- renderUI({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
    }
    
    linkposter <- movies$poster
    
    tags$img(src = linkposter[5] ,width = "100px", height = "150px")
    
  })
  
  
  output$tabela <- renderTable({
    
    if(input$genero != "Todos")
    {
      movies <- movies[movies$genero %in% input$genero,]
      
    }
    movies <- movies[,-7]
    movies
  })
  
}
  




shinyApp(ui,server)

    
      
      
   
   ################### Auxílio ################
   
   #              INPUTS
   
   # actionButton() - botao para executar uma acao.
   # checkboxGroupInput() - um grupo de check boxes.
   # checkboxInput() - um único check box.
   # dateInput() - um calendario para selecao de data.
   # dateRangeInput() - um par de calendÃ¡rios para escolher um intervalo de datas.
   # fileInput() - uma ferramenta para auxiliar o upload de arquivos.
   # numericInput() - Um campo para enviar nÃºmeros.
   # radioButtons() - Um conjunto de botÃµes para selecao.
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
  