library(fresh)

COLORS <- list(
  azul_forte = '#011e38',
  azul_escuro = '#20334a',
  azul = '#264fec',
  azul_claro = '#48a2d9',
  rosa = '#dba072',
  dourado = '#C8A165',
  bege = '#f5f1eb',
  cinza = '#b4b4c1',
  cinza_claro = '#D3D3D3',
  branco = '#ffffff',
  bege_neutro = '#F3E9DC',
  verde_escuro = '#346e4a',
  verde = '#8dc63f',
  verde_claro = '#bdd448'
)

gb_theme <- create_theme(
  bs4dash_status(
    primary = COLORS$azul_forte, 
    secondary = COLORS$azul_escuro,
    danger = COLORS$dourado, 
    light = COLORS$bege,
    success = COLORS$verde,
    info = COLORS$azul,
    warning = COLORS$verde_escuro,
    dark = COLORS$bege
  ),
  bs4dash_layout(
    sidebar_width = "230px"
  ),
  bs4dash_sidebar_light(
    bg = COLORS$branco,
    hover_bg = COLORS$azul_escuro,
    color = COLORS$azul_escuro,
    hover_color = COLORS$branco,
    header_color = COLORS$azul_escuro,
    active_color = COLORS$branco,
    
    submenu_bg = COLORS$branco,
    submenu_hover_bg = COLORS$azul_escuro,
    submenu_color = COLORS$azul_escuro,
    submenu_hover_color = COLORS$branco,
    submenu_active_color = COLORS$branco,
    
    submenu_active_bg = COLORS$branco,
  ),
  bs4dash_yiq(
    contrasted_threshold = 200,
    text_dark = COLORS$azul_forte,
    text_light = COLORS$branco
  ),
  bs4dash_color(
    blue = COLORS$azul_escuro,
    lightblue = COLORS$azul_claro,
    navy = COLORS$azul_forte,
    cyan = COLORS$azul,
    olive = COLORS$verde,
    green = COLORS$verde_escuro,
    lime = COLORS$verde_claro,
    red = COLORS$rosa,
  ),
  bs4dash_vars(
    'dropdown-link-hover-bg' = COLORS$cinza_claro,
    'dropdown-link-active-bg' = COLORS$cinza
  ),
  output_file = "www/gb.css"
)
