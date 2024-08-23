library(dplyr)
library(tidyverse)
library(readxl)

# Dados por Mes -----------------------------------------------------------

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

INPC <- read.csv("indice.csv", header = T, sep = ";")

INPC_mes <- INPC %>%
  filter(Ano < 2024 & Ano > 2012) %>% 
  pivot_longer(
    cols = Jan:Dez,  # Define o intervalo de colunas que representam os meses
    names_to = "MES",  # Nome da nova coluna que representará o mês
    values_to = "taxa"  # Nome da coluna que conterá os valores
  ) %>%
  mutate(
    Mes = recode(MES,
                 jan = 'Jan',
                 fev = 'Fev',
                 mar = 'Mar',
                 abr = 'Abr',
                 mai = 'Mai',
                 jun = 'Jun',
                 jul = 'Jul',
                 ago = 'Ago',
                 set = 'Set',
                 out = 'Out',
                 nov = 'Nov',
                 dez = 'Dez'),
    Mes = match(MES, meses),
    taxa = gsub("%", "", taxa),
    taxa = gsub(",", ".", taxa),
    Taxa = as.numeric(taxa),
    Indicadores = "INPC"
  ) %>% dplyr::select(-c(TaxaAnual, taxa, MES))


Poup <- read_xlsx("Poupanca_2013_2023.xlsx")

Poup_mes <- Poup %>%
  pivot_longer(
    cols = Jan:Dez,  # Define o intervalo de colunas que representam os meses
    names_to = "MES",  # Nome da nova coluna que representará o mês
    values_to = "taxa"  # Nome da coluna que conterá os valores
  ) %>%
  mutate(
    Mes = recode(MES,
                 jan = 'Jan',
                 fev = 'Fev',
                 mar = 'Mar',
                 abr = 'Abr',
                 mai = 'Mai',
                 jun = 'Jun',
                 jul = 'Jul',
                 ago = 'Ago',
                 set = 'Set',
                 out = 'Out',
                 nov = 'Nov',
                 dez = 'Dez'),
    Mes = match(Mes, meses),
    taxa = as.numeric(taxa),
    Taxa = round(taxa,2),
    Indicadores = "Poupança"
  ) %>% 
  dplyr::select(-c(MES, taxa, TaxaAnual))

dados_mes <- merge(INPC_mes, Poup_mes, all = TRUE)

# Dados por Ano -----------------------------------------------------------

INPC_ano <- INPC %>% 
  filter(Ano < 2024 & Ano > 2012) %>% 
  dplyr::select(c(Ano, TaxaAnual)) %>% 
  mutate(
    TaxaAnual = gsub("%", "", TaxaAnual),
    TaxaAnual = gsub(",", ".", TaxaAnual),
    TaxaAnual = as.numeric(TaxaAnual),
    Indicadores = "INPC"
  )

Poup_ano <- Poup %>% 
  filter(Ano < 2024 & Ano > 2012) %>% 
  dplyr::select(c(Ano, TaxaAnual)) %>% 
  mutate(
    TaxaAnual = gsub("%", "", TaxaAnual),
    TaxaAnual = gsub(",", ".", TaxaAnual),
    TaxaAnual = as.numeric(TaxaAnual),
    Indicadores = "Poupança"
  )

dados_ano <- merge(INPC_ano, Poup_ano, all = TRUE)

# Dashboard ---------------------------------------------------------------

library(shiny)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tags$head(tags$style(HTML(' 
    .graficos {
    display: flex;
    width: 100%;
    # height: calc(43vh - 119px);
    height: calc(50vh - 120px);
    # height: calc(100vh - 120px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
                              ')
    )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = strong("Índices de inflação e de rendimento de aplicações"),
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          div(
            class = "graficos",
            plotlyOutput("INPC_Poupanca", height = "100%")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Heatmap INPC",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          div(
            class = "graficos",
            plotlyOutput("heatmapINPC", height = "100%")
          )
        ),
        box(
          title = "Heatmap Poupança",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          div(
            class = "graficos",
            plotlyOutput("heatmapPoup", height = "100%")
          )
        )
      )
    )
  ),
  controlbar = dashboardControlbar(disable = T)
)

server <- function(input, output, session) {
  output$INPC_Poupanca <- renderPlotly({
    plot_ly(
      data = dados_ano,
      x = ~Ano,
      y = ~TaxaAnual,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~Indicadores,
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
          tickvals = unique(dados_ano$Ano),
          ticktext = unique(dados_ano$Ano)#,
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
  
  output$heatmapINPC <- renderPlotly({
    plot_ly(
      data = dados_mes %>% filter(Indicadores == "INPC"),
      x = ~Mes,
      y = ~Ano,
      z = ~Taxa,
      type = "heatmap",
      colorscale = "Viridis",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", Mes, "<br>",
        "Ano: ", Ano, "<br>",
        "Taxa de Juros: ", Taxa, "%<br>"
      )
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_mes$Mes),
          ticktext = unique(dados_mes$Mes),
          showgrid = F
        ),
        yaxis = list(
          title = "Ano",
          tickformat = ".0f",
          tickvals = unique(floor(dados_mes$Ano)),
          ticktext = unique(floor(dados_mes$Ano)),
          showgrid = F
        ),
        legend = list(
          orientation = "h",
          y = 0.9,
          x = 0.1,
          font = list(
            size = 10
          )
        ),
        showlegend = F
      )
  })

  output$heatmapPoup <- renderPlotly({
    plot_ly(
      data = dados_mes %>% filter(Indicadores == "Poupança"),
      x = ~Mes,
      y = ~Ano,
      z = ~Taxa,
      type = "heatmap",
      colorscale = "Viridis",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", Mes, "<br>",
        "Ano: ", Ano, "<br>",
        "Taxa de Juros: ", Taxa, "%<br>"
      )
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_mes$Mes),
          ticktext = unique(dados_mes$Mes),
          showgrid = F
        ),
        yaxis = list(
          title = "Ano",
          tickformat = ".0f",
          tickvals = unique(floor(dados_mes$Ano)),
          ticktext = unique(floor(dados_mes$Ano)),
          showgrid = F
        ),
        legend = list(
          orientation = "h",
          y = 0.9,
          x = 0.1,
          font = list(
            size = 10
          )
        ),
        showlegend = F
      )
  })
  
}

shinyApp(ui, server)