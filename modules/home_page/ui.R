ui_visao_geral <- function(id) {
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = id,
    shinyjs::useShinyjs(),

    # Título principal
    tags$h1(
      "Visão Geral",
      style = "color: #002F52; border-bottom: 3px solid #C5A572; padding-bottom: 10px;
              font-weight: bold; margin-bottom: 30px; text-align: center;"
    ),

    # Linha de filtros
    fluidRow(
      column(
        width = 4,
        shinyWidgets::pickerInput(
          ns("ligas"),
          label = tags$label(class = "picker-label", "Liga:"),
          choices = c("Brasileirão A", "Premier League", "Série A (ITA)", "La Liga", "Bundesliga"),
          multiple = F,
          selected = "Brasileirão A"
        )
      )
    ),

    # TABELA
    fluidRow(

    ),
    # HISTORICO
    fluidRow(

    )
  )
}
    