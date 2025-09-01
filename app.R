rm(list = ls(all=T))
gc()

# Pacotes ======================================================================
## Modulos
library(modules)
## Shiny
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
## UI
library(bs4Dash)
library(fresh)
## Plots
library(highcharter)
library(DT)
## Handling
library(tidyverse)

# Constants ====================================================================
CONSTS <- modules::use("constants.R")

# Modulos ======================================================================
source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  for (f in files) source(f)
}

# usage
source_dir("modules/home_page")

# Settings =====================================================================
options(
  # Change R decimals
  #OutDec = ",", 
  # Change Highcharter decimals
  highcharter.lang = list(
    decimalPoint = ",",
    thousandsSep = "."
  )
)

# bs4Dash ======================================================================
header <- bs4Dash::bs4DashNavbar(
  title = bs4Dash::dashboardBrand(
    # APP TITLE (NAVBAR)
    title = CONSTS$APP_TITLE,
    # APP LOGO (NAVBAR)
    #image = CONSTS$UFMG_LOGO,
    color = "secondary",
    href = 'https://www.est.ufmg.br/portal/'
  ),
  titleWidth = "57px",
  sidebarIcon = icon("bars")
)


sidebar <- bs4Dash::dashboardSidebar(
  id = "main_sidebar",
  expandOnHover = FALSE,
  minified = FALSE,
  collapsed = TRUE,
  status = NULL,
  bs4Dash::bs4SidebarMenu(
    #SIDEBAR ITEMS
    id = "sidebar_menu",
    compact = TRUE,

    bs4Dash::bs4SidebarMenuSubItem(
      tabName = "visao_geral", text = "Visão Geral", icon = icon("square-poll-horizontal")
    ),
    bs4Dash::bs4SidebarMenuItem(
      text = "Teste",
      icon = icon("folder"),
      startExpanded = TRUE,
      # METODOLOGIA - RTM
      bs4Dash::bs4SidebarMenuSubItem(
        tabName = "teste", text = "Teste", icon = icon("book")
      )
    )
  ),
  selected = "visao_geral"
)


body <- bs4Dash::bs4DashBody(
  fresh::use_theme("gb.css"),
  #SIDEBAR PAGES
  bs4Dash::tabItems(
    # #1- VISAO GERAL
    ui_visao_geral(
      id = "visao_geral"
    )
  )
)

footer <- bs4Dash::dashboardFooter(
  left = HTML('<p style="margin: 0;"> <span style="color:green"> DEST - UFMG </span> </a></p></span>'),
  right = paste0('Versão: ', CONSTS$APP_VERSION)
)


# Main UI ======================================================================
main_ui <- function() {
  bs4Dash::bs4DashPage(
    help = NULL,
    dark = NULL,
    scrollToTop = TRUE,
    header = header,
    sidebar = sidebar,
    body = body,
    footer = footer
  )
}

ui <- tagList(
  shinyjs::useShinyjs(),
  div(id = "main_container", main_ui())
)

# Server =======================================================================
server <- function(input, output, session) {
  # # 1- VISAO GERAL
  callModule(
    module = server_visao_geral,
    id = "visao_geral"
  )
}

# ShinyApp =====================================================================
shinyApp(ui = ui, server = server)