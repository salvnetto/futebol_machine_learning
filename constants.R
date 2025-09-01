# APP INFOS
APP_VERSION <- "0.0.0"
APP_TITLE <- "DEST - Futebol"

# IMAGES
#UFMG_LOGO <- "img/ufmg_logo.png"


# DATA



# COLORS
COLORS <- list(
  '#20334a',
  '#264fec',
  '#48a2d9',
  '#C8A165',
  '#bdd448',
  '#8dc63f',
  '#346e4a',
  '#dba072',
  '#b4b4c1',
  '#D3D3D3',
  '#F3E9DC',
  '#011e38',
  '#f5f1eb'
)


# HIGHCHART THEME
THEME = highcharter::hc_theme(
  colors = COLORS,
  chart = list(
    backgroundColor = NULL
  ),
  title = list(
    style = list(color = "#333333", fontSize = "16px")
  ),
  subtitle = list(
    style = list(fontSize = "14px")
  ),
  xAxis = list(
    lineColor = "#333333", 
    tickColor = "#333333"
  ),
  yAxis = list(
    gridLineColor = "#D9D9D9"
  ),
  legend = list(
    itemStyle = list(color = "#333333")
  )
)