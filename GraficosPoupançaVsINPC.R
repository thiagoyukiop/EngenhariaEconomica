library(dplyr)
library(tidyverse)


INPC <- read.csv("indice.csv", header = T, sep = ";")

INPC10anos <- INPC %>% 
  filter(Ano < 2024 & Ano > 2012) %>% 
  dplyr::select(c(Ano, TaxaAnual)) 



INPC10anos <- INPC10anos %>% 
  mutate(
    TaxaAnual = gsub("%", "", TaxaAnual),
    TaxaAnual = gsub(",", ".", TaxaAnual),
    TaxaAnual = as.numeric(TaxaAnual),
    Indicadores = "INPC"
  ) 
# Dados para o data frame
# https://www.remessaonline.com.br/blog/rendimento-da-poupanca-saiba-quanto-rende-de-juros-hoje/

Poup10anos <- data.frame(
  Ano = c(2013:2023),
  TaxaAnual = c(6.37, 7.16, 8.15, 8.30, 6.61, 4.62, 4.26, 2.11, 2.94, 7.89, 8.03)
)

Poup10anos <- Poup10anos %>% 
  mutate(Indicadores = "Poupança")

dados <- merge(INPC10anos, Poup10anos, all = TRUE)

library(shiny)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    # h1("Índices de inflação e de rendimento de aplicações"),
    # plotlyOutput("INPC_Poupanca")
    box(
      title = strong("Índices de inflação e de rendimento de aplicações"),
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      plotlyOutput("INPC_Poupanca")
    )
  ),
  controlbar = dashboardControlbar(disable = T)
)

server <- function(input, output, session) {
  output$INPC_Poupanca <- renderPlotly({
    plot_ly(
      data = dados,
      x = ~Ano,
      y = ~TaxaAnual,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~Indicadores,
      # colors = cores,
      marker = list(size = 10), # Tamanho do Marcador
      hoverinfo = "text",
      text = ~paste(
        " Indicador: ", Indicadores, "<br>",
        "Taxa Anual: ", TaxaAnual, "%"
      )
    ) %>% 
      layout(
        title = "INPC x Poupança",
        xaxis = list(
          title = "Ano",
          tickvals = unique(dados$Ano),
          ticktext = unique(dados$Ano)#,
          # showgrid = FALSE
        ),
        yaxis = list(
          title = "Taxa Anual",
          # showgrid = FALSE,
          ticksuffix = "%"
        ),
        showlegend = FALSE,
        hovermode = "x"
      )
  })
}

shinyApp(ui, server)
