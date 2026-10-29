# =====================================================================
#  GPU Hardware Performance Analytics & Recommendation Engine  (v4)
#  Pages:  Home | Manufacturer | Consumer
#  Packages: shiny, tidyverse, randomForest, DT, plotly
#  Install once:
#  install.packages(c("shiny","tidyverse","randomForest","DT","plotly"))
# =====================================================================

library(shiny)
library(tidyverse)
library(randomForest)
library(DT)
library(plotly)

# ---------------------------------------------------------------------
# 0. SETTINGS  (change the path to your CSV here - only place needed)
# ---------------------------------------------------------------------
# Keep All_GPUs.csv in the same folder as this R script.
# If needed, replace this with the full path to your CSV.
DATA_PATH <- file.path(getwd(), "All_GPUs.csv")

# Photos for the "Find by Needs" page. Put the three images in a folder called
# "www" next to this script (same folder as All_GPUs.csv).
if (dir.exists(file.path(getwd(), "www"))) addResourcePath("gpu_img", file.path(getwd(), "www"))

MFG_COLORS <- c(
  "Nvidia" = "#76B900", "AMD" = "#E4202D", "ATI" = "#FFB300",
  "Intel"  = "#0A7BD6"
)

NV_GREEN <- "#2F81F7"

# ---------------------------------------------------------------------
# 1. DATA LOADING, CLEANING & FEATURE ENGINEERING
# ---------------------------------------------------------------------
parse_num <- function(x) {
  x <- gsub("[^0-9.]", "", as.character(x))
  x[x %in% c("", ".")] <- NA
  as.numeric(x)
}

# Reads only the FIRST number in a string. Needed for values like
# "512KB(x2)", where parse_num() would glue the "2" onto 512 and give 5122.
parse_lead <- function(x) {
  out <- rep(NA_real_, length(x))
  has <- grepl("[0-9]", as.character(x))
  out[has] <- as.numeric(sub("^[^0-9]*([0-9]+\\.?[0-9]*).*$", "\\1", as.character(x)[has]))
  out
}

load_and_clean_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("CSV file not found at: ", filepath,
         "\nUpdate DATA_PATH at the top of the script.")
  }
  df <- read.csv(filepath, stringsAsFactors = FALSE, check.names = TRUE)
  
  needed <- c("Name", "Manufacturer", "Memory", "Core_Speed",
              "Max_Power", "Release_Date", "Release_Price",
              "Boost_Clock", "L2_Cache", "Memory_Bandwidth", "Memory_Bus",
              "Memory_Speed", "Pixel_Rate")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing columns in CSV: ", paste(miss, collapse = ", "))
  
  df <- df %>% mutate(across(where(is.character), trimws))
  
  df <- df %>%
    mutate(
      Clean_Power        = parse_num(Max_Power),
      Clean_Memory       = parse_lead(Memory),
      Clean_Core_Speed   = parse_lead(Core_Speed),
      Clean_Boost_Clock  = parse_lead(Boost_Clock),
      Clean_L2_Cache     = parse_lead(L2_Cache),
      # a few rows are given in MB/sec; convert them to GB/s
      Clean_Memory_Bandwidth = parse_lead(Memory_Bandwidth) /
        ifelse(grepl("MB/sec", Memory_Bandwidth), 1000, 1),
      Clean_Memory_Bus   = parse_lead(Memory_Bus),
      Clean_Memory_Speed = parse_lead(Memory_Speed),
      Clean_Texture_Rate = parse_num(Texture_Rate),
      Clean_Pixel_Rate   = parse_lead(Pixel_Rate),
      Clean_TMUs         = parse_num(TMUs),
      Clean_ROPs         = parse_num(ROPs),
      Clean_Price        = parse_num(Release_Price),
      Release_Date_Clean = as.Date(gsub("[\n\r]", "", Release_Date), format = "%d-%b-%Y"),
      Release_Year       = as.numeric(format(Release_Date_Clean, "%Y"))
    ) %>%
    # zeros are placeholders for "unknown" in this dataset
    mutate(
      Clean_Power      = ifelse(Clean_Power  <= 0, NA, Clean_Power),
      Clean_Memory     = ifelse(Clean_Memory <= 0, NA, Clean_Memory),
      Clean_Core_Speed = ifelse(Clean_Core_Speed <= 0, NA, Clean_Core_Speed),
      Clean_Boost_Clock = ifelse(Clean_Boost_Clock <= 0, NA, Clean_Boost_Clock),
      Clean_L2_Cache = ifelse(Clean_L2_Cache <= 0, NA, Clean_L2_Cache),
      Clean_Memory_Bandwidth = ifelse(Clean_Memory_Bandwidth <= 0, NA, Clean_Memory_Bandwidth),
      Clean_Memory_Bus = ifelse(Clean_Memory_Bus <= 0, NA, Clean_Memory_Bus),
      Clean_Memory_Speed = ifelse(Clean_Memory_Speed <= 0, NA, Clean_Memory_Speed),
      Clean_Texture_Rate = ifelse(Clean_Texture_Rate <= 0, NA, Clean_Texture_Rate),
      Clean_Pixel_Rate = ifelse(Clean_Pixel_Rate <= 0, NA, Clean_Pixel_Rate),
      Clean_TMUs = ifelse(Clean_TMUs <= 0, NA, Clean_TMUs),
      Clean_ROPs = ifelse(Clean_ROPs <= 0, NA, Clean_ROPs)
    ) %>%
    filter(!is.na(Name), Name != "", !is.na(Manufacturer), Manufacturer != "")
  
  df <- df %>%
    mutate(
      N_Mem  = coalesce(percent_rank(Clean_Memory), 0),
      N_Core = coalesce(percent_rank(Clean_Core_Speed), 0),
      Perf_Index      = round((N_Mem + N_Core) / 2 * 100, 1),
      Perf_per_Watt   = round(Perf_Index / Clean_Power, 3),
      Perf_per_Dollar = round(Perf_Index / Clean_Price * 100, 3),
      N_Eff   = coalesce(percent_rank(Perf_per_Watt), 0),
      N_Value = coalesce(percent_rank(Perf_per_Dollar), 0),
      N_Cheap = coalesce(1 - percent_rank(Clean_Price), 0)
    )
  df
}

gpu_data <- load_and_clean_data(DATA_PATH)

manufacturers <- sort(unique(gpu_data$Manufacturer))
mfg_palette <- {
  p <- setNames(rep("#6B7A99", length(manufacturers)), manufacturers)
  common <- intersect(names(MFG_COLORS), manufacturers)
  p[common] <- MFG_COLORS[common]
  p
}

rng <- function(x, default) {
  r <- suppressWarnings(range(x, na.rm = TRUE))
  if (!all(is.finite(r))) default else r
}
pwr_rng   <- rng(gpu_data$Clean_Power, c(0, 500))

var_choices <- c(
  "Core Speed (MHz)"       = "Clean_Core_Speed",
  "Boost Clock (MHz)"      = "Clean_Boost_Clock",
  "Memory (MB)"            = "Clean_Memory",
  "Memory Bandwidth (GB/s)"= "Clean_Memory_Bandwidth",
  "Memory Bus (bit)"       = "Clean_Memory_Bus",
  "Memory Speed (MHz)"     = "Clean_Memory_Speed",
  "L2 Cache (KB)"          = "Clean_L2_Cache",
  "Texture Rate (GTexel/s)"= "Clean_Texture_Rate",
  "Pixel Rate (GPixel/s)"  = "Clean_Pixel_Rate",
  "TMUs"                   = "Clean_TMUs",
  "ROPs"                   = "Clean_ROPs",
  "Max Power (W)"          = "Clean_Power",
  "Release Year"           = "Release_Year",
  "Price ($)"              = "Clean_Price",
  "Performance Index"      = "Perf_Index",
  "Performance per Watt"   = "Perf_per_Watt",
  "Performance per $100"   = "Perf_per_Dollar"
)
lab <- function(v) names(var_choices)[match(v, var_choices)]
short_lab <- function(v) {
  out <- ifelse(v %in% var_choices, lab(v), v)
  gsub(" \\(.*\\)", "", out)
}

# Shared ggplot theme (dark, transparent background; legend handled by plotly, see pl())
theme_gpu <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(colour = "#F0F6FC"),
      axis.text = element_text(colour = "#9DA7B3"),
      plot.title = element_text(face = "bold", colour = "#4C9AFF"),
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#3D444D"),
      strip.text = element_text(colour = "#4C9AFF", face = "bold"),
      legend.position = "bottom",
      panel.spacing = unit(1.2, "lines")
    )
}
scale_mfg_col  <- function() scale_colour_manual(values = mfg_palette)
scale_mfg_fill <- function() scale_fill_manual(values = mfg_palette)

# One place that fixes plotly legend / margin overlap and dark styling for every chart
pl <- function(p, tooltip = "all", bottom = 90, top = 60) {
  ggplotly(p, tooltip = tooltip) %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.28),
           margin = list(t = top, b = bottom, l = 70, r = 20),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(color = "#F0F6FC")) %>%
    config(displaylogo = FALSE)
}

# ---------------------------------------------------------------------
# 1b. Helper: turn one tree of the random forest into a node/edge layout
# ---------------------------------------------------------------------
layout_tree <- function(tr, max_depth = 3) {
  nodes <- list(); edges <- list(); leaf_x <- 0
  
  rec <- function(i, depth) {
    internal  <- tr$status[i] != -1
    truncated <- internal && depth >= max_depth
    kids <- integer(0)
    
    if (!internal || truncated) {
      leaf_x <<- leaf_x + 1
      x <- leaf_x
      label <- if (truncated) "..." else format(round(tr$prediction[i], 1), big.mark = ",")
      type  <- if (truncated) "Further splits (hidden)" else "Leaf = prediction"
    } else {
      l <- rec(tr$`left daughter`[i],  depth + 1)
      r <- rec(tr$`right daughter`[i], depth + 1)
      x <- (nodes[[l]]$x + nodes[[r]]$x) / 2
      v <- as.character(tr$`split var`[i])
      label <- if (v == "Manufacturer") {
        "Manufacturer\nin group?"
      } else {
        paste0(short_lab(v), "\n<= ", format(round(tr$`split point`[i], 1), big.mark = ","))
      }
      type <- "Split"
      kids <- c(l, r)
    }
    id <- length(nodes) + 1
    nodes[[id]] <<- list(id = id, x = x, y = -depth, label = label, type = type)
    for (k in kids) edges[[length(edges) + 1]] <<- list(from = id, to = k)
    id
  }
  rec(1, 0)
  
  nd <- bind_rows(lapply(nodes, as_tibble))
  ed <- bind_rows(lapply(edges, as_tibble)) %>%
    left_join(nd %>% select(id, x, y), by = c("from" = "id")) %>%
    left_join(nd %>% select(id, xend = x, yend = y), by = c("to" = "id"))
  list(nodes = nd, edges = ed)
}

# ---------------------------------------------------------------------
# 1c. UI building blocks (replace shinydashboard boxes / value boxes so the
#     menu can sit in a top navigation bar instead of a sidebar)
# ---------------------------------------------------------------------
box <- function(..., width = 12, title = NULL) {
  column(width,
         div(class = "gbox",
             if (!is.null(title)) div(class = "gbox-title", title),
             div(class = "gbox-body", ...)))
}
valueBox <- function(value, subtitle, icon = NULL, color = NULL) {
  div(class = "vbox",
      div(class = "vbox-icon", icon),
      div(class = "vbox-val", value),
      div(class = "vbox-sub", subtitle))
}
valueBoxOutput <- function(outputId, width = 4) column(width, uiOutput(outputId))
renderValueBox <- renderUI

# Use-case presets for "Find by Needs": weights for (memory, core speed).
# Score is pure performance - price is NOT part of it (budget is only a filter).
USE_CASES <- list(
  "Gaming"            = list(w = c(30, 70),
                             txt = "Ranked by performance, weighted towards core speed for high frame rates."),
  "Workstation / ML"  = list(w = c(80, 20),
                             txt = "Ranked by performance, weighted towards large memory for big models, datasets and complex scenes."),
  "Content creation"  = list(w = c(60, 40),
                             txt = "Ranked by performance, with plenty of memory and strong core speed for editing and rendering."),
  "Office / everyday" = list(w = c(50, 50),
                             txt = "Ranked by overall performance, memory and core speed weighted equally.")
)

# Performance-only ranking for "Find by Needs": budget, memory and manufacturer
# are filters; the score ignores price, efficiency and value entirely.
rank_gpus_perf <- function(budget, min_mem, mfgs, w) {
  gpu_data %>%
    filter(
      !is.na(Clean_Price),  Clean_Price  <= budget,
      !is.na(Clean_Memory), Clean_Memory >= min_mem,
      Manufacturer %in% mfgs
    ) %>%
    mutate(Score = round(100 * (w[1] * N_Mem + w[2] * N_Core) / sum(w), 1)) %>%
    arrange(desc(Score), desc(Perf_Index), desc(Clean_Memory)) %>%
    mutate(Rank = row_number())
}

# Rank every GPU that meets the requirements (no row cap)
rank_gpus <- function(budget, min_mem, max_power, mfgs, w) {
  gpu_data %>%
    filter(
      !is.na(Clean_Price),  Clean_Price  <= budget,
      !is.na(Clean_Memory), Clean_Memory >= min_mem,
      is.na(Clean_Power) | Clean_Power <= max_power,
      Manufacturer %in% mfgs
    ) %>%
    mutate(Score = round(100 * (w[1] * N_Mem + w[2] * N_Core + w[3] * N_Eff + w[4] * N_Value) / sum(w), 1)) %>%
    arrange(desc(Score)) %>%
    mutate(Rank = row_number())
}

# Top-5 card strip
top_cards_ui <- function(r) {
  top <- head(r, 5)
  fmt <- function(x, unit = "") ifelse(is.na(x), "n/a", paste0(format(round(x), big.mark = ","), unit))
  div(class = "top5-wrap",
      lapply(seq_len(nrow(top)), function(i) {
        g <- top[i, ]
        div(class = "top5-card",
            div(class = "rank", paste0("#", i, "  ", toupper(g$Manufacturer))),
            div(class = "gname", g$Name),
            div(class = "score", paste0(g$Score, " / 100")),
            div(class = "specs",
                paste0("Memory: ", fmt(g$Clean_Memory, " MB")), tags$br(),
                paste0("Core: ", fmt(g$Clean_Core_Speed, " MHz")), tags$br(),
                paste0("Power: ", fmt(g$Clean_Power, " W")), tags$br(),
                paste0("Price: $", fmt(g$Clean_Price))))
      }))
}

# ---------------------------------------------------------------------
# 2. UI  (high-contrast dark theme: bg #010409, panels #161B22, accent #1F6FEB, text #F0F6FC / #9DA7B3, borders #3D444D)
# ---------------------------------------------------------------------
# Static "Most popular GPUs" panel for Find by Needs (ignores the user's selections).
popular_gpus_ui <- function() {
  items <- list(
    list(name = "NVIDIA GeForce GTX 1080 Ti", file = "gtx_1080ti.jpg", bg = "light", tag = "Best for performance"),
    list(name = "NVIDIA GeForce GTX 1060",    file = "gtx_1060.jpg",    bg = "light", tag = "Best for budget"),
    list(name = "AMD Radeon Fury",            file = "radeon_fury.jpg", bg = "dark",  tag = "Best for AMD")
  )
  div(class = "need-photo",
      lapply(items, function(p) {
        div(class = "photo-item",
            div(class = paste("photo-frame", p$bg),
                tags$img(src = paste0("gpu_img/", p$file), alt = p$name)),
            div(class = "photo-tag", p$tag),
            div(class = "photo-name", p$name))
      }))
}

app_css <- "
  @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600;700&display=swap');
  body, h1, h2, h3, h4, h5, .navbar, label, .gbox-title {
    font-family: 'IBM Plex Sans', 'Segoe UI', Helvetica, Arial, sans-serif !important; }
  body { background: #010409; color: #F0F6FC; padding-top: 0; }
  a { color: #4C9AFF; }
  a:hover { color: #7DB5FF; }
  hr { border-top: 1px solid #3D444D; }
  h4 { color: #4C9AFF; }
  .container-fluid { padding-left: 24px; padding-right: 24px; }

  /* --- Top navigation bar --- */
  .navbar-default { background: #161B22; border: none; border-bottom: 3px solid #1F6FEB; border-radius: 0;
                    margin-bottom: 22px; }
  .navbar-default .navbar-brand { color: #4C9AFF; font-weight: 700; letter-spacing: .5px; }
  .navbar-default .navbar-brand:hover { color: #7DB5FF; }
  .navbar-default .navbar-nav > li > a { color: #F0F6FC; font-weight: 500; }
  .navbar-default .navbar-nav > li > a:hover,
  .navbar-default .navbar-nav > li > a:focus { color: #4C9AFF; background: #010409; }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus,
  .navbar-default .navbar-nav > .open > a,
  .navbar-default .navbar-nav > .open > a:hover,
  .navbar-default .navbar-nav > .open > a:focus { background: #010409; color: #4C9AFF; box-shadow: inset 0 -3px 0 #1F6FEB; }
  .navbar-default .navbar-toggle { border-color: #1F6FEB; }
  .navbar-default .navbar-toggle .icon-bar { background: #1F6FEB; }
  .dropdown-menu { background: #161B22; border: 1px solid #1F6FEB; border-radius: 0; }
  .dropdown-menu > li > a { color: #F0F6FC; }
  .dropdown-menu > li > a:hover, .dropdown-menu > .active > a, .dropdown-menu > .active > a:hover {
    background: #1F6FEB; color: #FFFFFF; }

  /* --- Cards --- */
  .gbox { background: #161B22; border-top: 3px solid #1F6FEB; border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,.6); margin-bottom: 20px; }
  .gbox-title { font-weight: 600; color: #4C9AFF; font-size: 16px; padding: 12px 15px 0 15px; }
  .gbox-body { padding: 12px 15px 15px 15px; overflow-x: auto; }

  /* --- Value boxes --- */
  .vbox { position: relative; background: #161B22; border-left: 5px solid #1F6FEB; border-radius: 4px;
          padding: 14px 70px 14px 16px; margin-bottom: 20px; min-height: 100px;
          box-shadow: 0 1px 3px rgba(0,0,0,.6); }
  .vbox-val { font-size: 22px; font-weight: 700; color: #F0F6FC; white-space: normal;
              overflow-wrap: anywhere; line-height: 1.2; }
  .vbox-sub { font-size: 13px; color: #9DA7B3; margin-top: 4px; }
  .vbox-icon { position: absolute; right: 16px; top: 16px; font-size: 40px; color: #4C9AFF; opacity: .55; }

  /* --- Forms --- */
  .form-control, .selectize-input, .selectize-dropdown { background: #010409 !important; color: #F0F6FC !important;
                                         border: 1px solid #3D444D !important; box-shadow: none; }
  .selectize-input > .item { color: #F0F6FC !important; }
  .selectize-dropdown .active { background: #1F6FEB !important; color: #FFFFFF !important; }
  .selectize-input.focus, .form-control:focus { border-color: #1F6FEB !important; }
  .selectize-control.multi .selectize-input > div { background: #123C85; color: #F0F6FC; }
  .radio label, .checkbox label { color: #F0F6FC; }
  .irs--shiny .irs-bar { background: #1F6FEB; border-color: #1F6FEB; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #1F6FEB; color: #FFFFFF; }
  .irs--shiny .irs-handle { border-color: #1F6FEB; }
  .irs--shiny .irs-line { background: #3D444D; border-color: #3D444D; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background: #3D444D; color: #9DA7B3; }
  .irs--shiny .irs-grid-text { color: #9DA7B3; }

  .btn-accent { background: #1F6FEB; border-color: #1F6FEB; color: #FFFFFF; font-weight: 700; }
  .btn-accent:hover, .btn-accent:focus { background: #388BFD; border-color: #388BFD; color: #FFFFFF; }
  .btn-default { background: #010409; color: #F0F6FC; border-color: #3D444D; }
  .btn-default:hover { background: #3D444D; color: #F0F6FC; }

  .note { color: #9DA7B3; font-size: 13px; line-height: 1.5; }

  /* --- Data tables --- */
  .dataTables_wrapper { overflow-x: auto; width: 100%; color: #F0F6FC; }
  .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_length { margin-bottom: 8px; }
  .dataTables_wrapper label, .dataTables_wrapper .dataTables_info { color: #9DA7B3; }
  table.dataTable, table.dataTable tbody tr, table.dataTable tbody td,
  table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd {
    background-color: #161B22 !important; color: #F0F6FC !important; }
  table.dataTable.stripe tbody tr.even, table.dataTable.display tbody tr.even { background-color: #010409 !important; }
  table.dataTable tbody tr:hover td { background-color: #0F2F66 !important; }
  table.dataTable thead th, table.dataTable thead td { color: #4C9AFF !important; border-bottom: 1px solid #1F6FEB !important;
                                                       background-color: #010409 !important; }
  table.dataTable.no-footer { border-bottom: 1px solid #3D444D; }
  .dataTables_wrapper .dataTables_paginate .paginate_button { color: #F0F6FC !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current,
  .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
    background: #1F6FEB !important; color: #FFFFFF !important; border: 1px solid #1F6FEB !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background: #123C85 !important; color: #F0F6FC !important;
                                                                    border: 1px solid #1F6FEB !important; }

  /* --- Home hero --- */
  .hero { background: linear-gradient(135deg, #161B22 0%, #0B2552 100%); border-left: 6px solid #1F6FEB;
          border-radius: 4px; padding: 22px 26px; margin-bottom: 20px; }
  .hero h2 { margin: 0 0 6px 0; color: #F0F6FC; font-weight: 700; }
  .hero h2 span { color: #4C9AFF; }
  .hero p { margin: 0; color: #9DA7B3; }

  /* --- What-if prediction panel --- */
  .pred-steps { display: flex; flex-wrap: wrap; gap: 10px; margin: 0 0 16px 0; }
  .pred-step { display: flex; align-items: center; gap: 8px; background: #010409; border-radius: 20px;
               padding: 6px 14px 6px 6px; font-size: 13px; color: #F0F6FC; }
  .pred-step .num { display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px;
                    border-radius: 50%; background: #1F6FEB; color: #FFFFFF; font-weight: 700; font-size: 12px; }
  .pred-empty { text-align: center; padding: 28px 12px; color: #9DA7B3; }
  .pred-empty .fa { font-size: 30px; color: #4C9AFF; margin-bottom: 8px; }
  .pred-empty-title { font-size: 16px; font-weight: 700; color: #F0F6FC; margin-bottom: 4px; }
  .pred-status { font-size: 12.5px; color: #9DA7B3; margin: 0 0 12px 0; }
  .pred-status .dot { display: inline-block; width: 8px; height: 8px; border-radius: 50%; margin-right: 6px; }
  .pred-status .ready { background: #1F6FEB; }
  .pred-status .busy  { background: #F2A900; }
  .pred-actions { display: flex; gap: 10px; margin: 4px 0 16px 0; }
  .pred-card { border-left: 5px solid #1F6FEB; background: #0A2A5E; border-radius: 6px; padding: 14px 18px; }
  .pred-card .pred-label { font-size: 12px; font-weight: 700; letter-spacing: .5px; text-transform: uppercase; color: #4C9AFF; }
  .pred-card .pred-value { font-size: 36px; font-weight: 700; color: #F0F6FC; line-height: 1.2; }
  .pred-card .pred-unit  { font-size: 16px; font-weight: 600; color: #9DA7B3; margin-left: 6px; }
  .pred-card .pred-range, .pred-card .pred-meta { font-size: 13px; color: #9DA7B3; margin-top: 4px; }
  .pred-card .pred-warn { font-size: 13px; color: #FFD27A; background: #3A2D0B; border-radius: 4px; padding: 6px 10px; margin-top: 8px; }
  .pred-error { font-size: 13.5px; color: #FFB4AB; background: #3B1512; border-radius: 4px; padding: 8px 12px; }

  /* --- Top-5 recommendation cards --- */
  .top5-wrap { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 15px; }
  .top5-card { flex: 1 1 170px; background: #0D1117; border-radius: 6px; border: 1px solid #3D444D; border-top: 4px solid #1F6FEB;
               box-shadow: 0 1px 3px rgba(0,0,0,.6); padding: 12px 14px; min-width: 170px; }
  .top5-card .rank  { font-size: 12px; font-weight: 700; color: #4C9AFF; letter-spacing: .5px; }
  .top5-card .gname { font-size: 15px; font-weight: 700; color: #F0F6FC; margin: 2px 0 6px 0;
                      overflow-wrap: anywhere; line-height: 1.25; }
  .top5-card .score { font-size: 22px; font-weight: 700; color: #4C9AFF; margin-bottom: 6px; }
  .top5-card .specs { font-size: 12.5px; color: #9DA7B3; line-height: 1.6; }

  /* --- Need summary --- */
  .need-banner { background: #0A2A5E; border-left: 5px solid #1F6FEB; border-radius: 4px;
                 padding: 12px 16px; margin-bottom: 20px; color: #F0F6FC; }
  .need-banner strong { color: #4C9AFF; }

  /* --- Popular GPU photo (Find by Needs) --- */
  .need-photo { text-align: center; }
  .need-photo .photo-frame { border-radius: 6px; padding: 8px; margin-bottom: 10px; }
  .need-photo .photo-frame.light { background: #fff; }
  .need-photo .photo-frame.dark  { background: #000000; border: 1px solid #3D444D; }
  .need-photo img { width: 100%; height: auto; max-height: 130px; object-fit: contain; display: block; }
  .need-photo .photo-name { font-size: 16px; font-weight: 700; color: #F0F6FC; line-height: 1.25; }
  .need-photo .photo-item { margin-bottom: 16px; }
  .need-photo .photo-item:last-child { margin-bottom: 0; }
  .need-photo .photo-tag  { display: inline-block; background: #0A2A5E; color: #4C9AFF; border-radius: 10px;
                            font-size: 11.5px; font-weight: 600; padding: 2px 9px; margin-bottom: 4px; }

  .hero-home { padding: 30px 32px; }
  .hero-home p { max-width: 900px; margin-top: 8px; line-height: 1.65; }
  .hero-actions { display: flex; gap: 10px; flex-wrap: wrap; margin-top: 18px; }
  .btn-outline-light { background: transparent; color: #F0F6FC; border: 1px solid #3D444D; font-weight: 700; }
  .btn-outline-light:hover { background: #0F2F66; color: #F0F6FC; border-color: #1F6FEB; }
  .home-image-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 16px; margin-bottom: 18px; }
  .home-image-card { background: #161B22; border: 1px solid #3D444D; border-radius: 8px; overflow: hidden; }
  .home-image-card img { display: block; width: 100%; height: 210px; object-fit: cover; }
  .home-image-caption { padding: 12px 16px 14px; }
  .home-image-caption h4 { color: #4C9AFF; margin-top: 0; font-weight: 700; }
  .home-image-caption p { color: #9DA7B3; margin-bottom: 0; line-height: 1.5; }
  @media (max-width: 800px) { .home-image-grid { grid-template-columns: 1fr; } .home-image-card img { height: 230px; } }

"

ui <- navbarPage(
  title = "GPU Analytics",
  id = "nav",
  collapsible = TRUE,
  header = tags$head(tags$style(HTML(app_css))),
  
  # ===================== HOME =====================
  tabPanel(
    "Home", icon = icon("home"),
    div(class = "hero hero-home",
        h2("GPU ", tags$span("Analytics")),
        p("Explore GPU hardware specifications, compare architectures, understand performance indicators, ",
          "and estimate missing specifications for hypothetical GPUs using a data-driven model."),
        div(class = "hero-actions",
            actionButton("home_visualizations", "Explore visualizations", class = "btn-accent"),
            actionButton("home_compare", "Compare GPUs", class = "btn-outline-light"))),
    fluidRow(
      column(12,
             div(class = "home-image-grid",
                 div(class = "home-image-card",
                     tags$img(src = "gpu_rtx.png", alt = "GeForce RTX graphics card"),
                     div(class = "home-image-caption", h4("GPU performance"), p("Review core speed, boost clock, memory bandwidth, cache and pixel rate."))),
                 div(class = "home-image-card",
                     tags$img(src = "gpu_intel.png", alt = "Intel graphics accelerator"),
                     div(class = "home-image-caption", h4("Compare architectures"), p("Compare specifications across manufacturers using consistent metrics."))),
                 div(class = "home-image-card",
                     tags$img(src = "gpu_black.png", alt = "Graphics card product"),
                     div(class = "home-image-caption", h4("Make informed choices"), p("Explore trade-offs between memory capacity, power, efficiency and estimated value.")))
             )
      )
    ),
    fluidRow(
      box(width = 12, title = "About this dashboard",
          p("GPU Analytics is an interactive exploration tool built around a catalogue of graphics-processing units. ",
            "It brings hardware specifications, visual analysis, side-by-side comparison, consumer-oriented recommendations, ",
            "and hypothetical GPU prediction into one place."),
          h4("What you can do"),
          tags$ul(
            tags$li(strong("Visualize specifications: "), "explore relationships with scatter plots, distributions, boxplots, trends and correlation views."),
            tags$li(strong("Compare GPUs: "), "select up to four GPUs and compare their specifications and normalized profiles."),
            tags$li(strong("Find a suitable GPU: "), "apply manufacturer, budget and hardware requirements in the Consumer tools."),
            tags$li(strong("Predict a missing specification: "), "train a Random Forest model and estimate one selected value from the other supported GPU parameters.")
          )
      )
    ),
    fluidRow(
      box(width = 6, title = "Data preparation",
          p("The application converts numeric fields that include units into usable numeric values. Release dates are parsed into years, ",
            "and zero values in selected specification columns are treated as missing when they represent unknown entries. ",
            "Rows without a GPU name or manufacturer are excluded from analysis.")
      ),
      box(width = 6, title = "Performance and recommendation methodology",
          p("The Performance Index is a simple 0–100 proxy based on percentile ranks of memory and core speed; it is not a measured gaming benchmark. ",
            "Efficiency and value indicators are derived from this index and available power or price fields. Recommendation scores combine the selected weighted components ",
            "after applying the user's constraints.")
      )
    ),
    fluidRow(
      box(width = 6, title = "Prediction model",
          p("The Random Forest model learns relationships between available GPU specifications. Model metrics are calculated on held-out rows when supported by the training workflow. ",
            "A hypothetical prediction is an estimate based on patterns in the dataset, not a guarantee of real hardware performance.")
      ),
      box(width = 6, title = "Data limitations",
          p("Predictions and recommendations are only as reliable as the source data. The supplied dataset contains missing specifications and synthetic release-price values; ",
            "therefore price-based filtering and value rankings are illustrative rather than current market advice. For real purchasing decisions, replace synthetic prices with verified prices ",
            "and validate estimates against independent benchmark results.")
      )
    ),
    fluidRow(
      box(width = 12, title = "Start exploring",
          p("Choose a section from the navigation bar: Manufacturer for visualizations and comparisons, or Consumer for requirement-based recommendations and hypothetical prediction. ",
            "The full GPU catalogue is available within the analysis tools rather than taking up space on this landing page."),
          p(class = "note", "This dashboard is intended for exploratory analysis and educational use. Estimated values should be interpreted alongside source quality and model error.")
      )
    )
  ),
  
  # ===================== MANUFACTURER =====================
  navbarMenu(
    "Manufacturer", icon = icon("industry"),
    
    # ---------- Visualizations ----------
    tabPanel(
      "Visualizations", icon = icon("chart-line"),
      fluidRow(
        box(
          width = 3, title = "Chart settings",
          selectInput("plot_type", "Chart type",
                      choices = c("Scatter Plot", "Histogram", "Density", "Boxplot",
                                  "Trend Over Time", "Correlation Matrix")),
          conditionalPanel(
            "input.plot_type != 'Boxplot' && input.plot_type != 'Trend Over Time' && input.plot_type != 'Correlation Matrix'",
            selectInput("x_var", "X variable", choices = var_choices, selected = "Clean_Memory")
          ),
          conditionalPanel(
            "input.plot_type == 'Scatter Plot' || input.plot_type == 'Boxplot' || input.plot_type == 'Trend Over Time'",
            selectInput("y_var", "Y variable", choices = var_choices, selected = "Clean_Core_Speed")
          ),
          conditionalPanel("input.plot_type == 'Scatter Plot'",
                           checkboxInput("show_trend", "Add trend line", TRUE),
                           checkboxInput("log_x", "Log scale (X)", FALSE),
                           checkboxInput("log_y", "Log scale (Y)", FALSE)),
          conditionalPanel("input.plot_type == 'Histogram'",
                           sliderInput("bins", "Number of bins", 5, 80, 30)),
          conditionalPanel("input.plot_type == 'Trend Over Time'",
                           radioButtons("trend_stat", "Statistic", c("Median" = "median", "Mean" = "mean"),
                                        inline = TRUE)),
          hr(),
          p(class = "note", "Price in this dataset is synthetic, so price-based correlations are expected to be weak. Values are parsed from the CSV text; confirm units before comparing across sources.")
        ),
        box(width = 9, title = "Chart", plotlyOutput("dynamic_plot", height = "600px"))
      )
    ),
    
    # ---------- Hypothetical prediction (Random Forest) ----------
    tabPanel(
      "Hypothetical Prediction", icon = icon("project-diagram"),
      fluidRow(
        box(
          width = 3, title = "Random Forest settings",
          selectInput("rf_target", "Step 1 - Choose the field to predict",
                      choices = c(
                        "Select a field..." = "",
                        "Core Speed (MHz)" = "Clean_Core_Speed",
                        "Boost Clock (MHz)" = "Clean_Boost_Clock",
                        "Memory (MB)" = "Clean_Memory",
                        "Memory Bandwidth (GB/s)" = "Clean_Memory_Bandwidth",
                        "Memory Bus (bit)" = "Clean_Memory_Bus",
                        "Memory Speed (MHz)" = "Clean_Memory_Speed",
                        "L2 Cache (KB)" = "Clean_L2_Cache",
                        "Pixel Rate (GPixel/s)" = "Clean_Pixel_Rate"
                      ),
                      selected = ""),
          sliderInput("rf_ntree", "Number of trees", 50, 500, 200, step = 50),
          sliderInput("rf_split", "Training share (%)", 50, 90, 70, step = 5),
          actionButton("train_model", "Retrain with these settings", icon = icon("sync"), class = "btn-accent", width = "100%"),
          hr(),
          p(class = "note", "The model trains automatically when you pick a field. Then enter the other seven specs in the box at the bottom of this page and click Predict. Only these eight specs are used.")
        ),
        column(
          width = 9,
          fluidRow(
            valueBoxOutput("vb_rmse", width = 3),
            valueBoxOutput("vb_mae",  width = 3),
            valueBoxOutput("vb_r2",   width = 3),
            valueBoxOutput("vb_oob",  width = 3)
          ),
          fluidRow(
            box(width = 6, title = "Actual vs predicted (test set)", plotlyOutput("rf_pred_plot", height = "380px")),
            box(width = 6, title = "Feature importance", plotlyOutput("rf_importance_plot", height = "380px"))
          )
        )
      ),
      fluidRow(
        box(width = 12, title = "Inside the forest: individual decision trees",
            fluidRow(
              column(4, sliderInput("tree_k", "Which tree (1 = first)", min = 1, max = 200, value = 1, step = 1)),
              column(4, sliderInput("tree_depth", "Levels to draw", min = 1, max = 5, value = 3, step = 1)),
              column(4, br(), p(class = "note", textOutput("tree_info")))
            ),
            p(class = "note",
              "Each box is a yes/no question. Go LEFT when the answer is yes (value <= split), RIGHT otherwise.",
              "Yellow boxes are leaves with the predicted value; grey '...' boxes continue deeper than the levels drawn."),
            uiOutput("rf_tree_ui"),
            hr(),
            h4("Full node table of this tree"),
            DTOutput("rf_tree_table"))
      ),
      fluidRow(
        box(width = 12, title = "Try the model: predict for a hypothetical GPU",
            uiOutput("pred_panel"))
      )
    ),
    # ---------- GPU simulator: your specs -> predicted GPU + cooling hologram ----------
    tabPanel(
      "GPU Simulator", icon = icon("cube"),
      fluidRow(
        box(width = 3, title = "Your hypothetical GPU",
            selectizeInput("sim_from", "Optional: start from a dataset GPU", choices = NULL, width = "100%",
                           options = list(placeholder = "Type a GPU name...")),
            actionButton("sim_load", "Load its values", icon = icon("download"), width = "100%"),
            hr(),
            numericInput("sim_core", "Core clock (MHz)", value = 1500, min = 100, max = 4000, step = 50),
            numericInput("sim_mem", "Memory (MB)", value = 8192, min = 16, max = 98304, step = 512),
            numericInput("sim_bus", "Memory bus (bit)", value = 256, min = 32, max = 4096, step = 32),
            numericInput("sim_power", "Board power (W)", value = 220, min = 30, max = 800, step = 10),
            fluidRow(
              column(4, numericInput("sim_dvi", "DVI", value = 1, min = 0, max = 4)),
              column(4, numericInput("sim_hdmi", "HDMI", value = 1, min = 0, max = 3)),
              column(4, numericInput("sim_dp", "DP", value = 3, min = 0, max = 4))),
            hr(),
            numericInput("sim_len", "Maximum card length (mm)", value = 300, min = 100, max = 400, step = 5),
            selectInput("sim_case", "Chassis", choices = c("Compact / SFF", "Mid-tower", "Full tower"), selected = "Mid-tower"),
            selectInput("sim_priority", "Design priority",
                        choices = c("Balanced", "Quiet operation", "Small form factor", "Maximum cooling")),
            selectInput("sim_cooler", "Cooler type allowed", choices = c("Air cooling only", "Allow liquid cooling")),
            selectInput("holo_color", "Hologram colour",
                        choices = c("Cyan" = "#00E5FF", "Cool blue" = "#4C9AFF",
                                    "Magenta" = "#FF3DF5", "Amber" = "#FFB000")),
            checkboxInput("holo_spin", "Auto-rotate + scan beam", TRUE),
            checkboxInput("holo_labels", "Show labels", TRUE),
            actionButton("sim_go", "Simulate GPU & cooling", icon = icon("bolt"), class = "btn-accent", width = "100%")),
        column(9,
               uiOutput("sim_verdict"),
               fluidRow(valueBoxOutput("sv_perf", width = 3), valueBoxOutput("sv_boost", width = 3),
                        valueBoxOutput("sv_bw", width = 3), valueBoxOutput("sv_cooler", width = 3)),
               fluidRow(box(width = 12, title = "Hologram: predicted GPU and its cooling system",
                            div(style = "background: radial-gradient(circle at 50% 62%, #07303a 0%, #050b0d 72%); border-radius: 6px;",
                                plotlyOutput("holo_plot", height = "640px"))))
        )
      ),
      fluidRow(
        box(width = 6, title = "Predicted specifications", DTOutput("sim_pred_table"),
            p(class = "note", "Random Forest estimates from your core clock, memory, bus and power. The range is the middle 80% of the forest's trees; R-squared is the model's out-of-bag score on the dataset.")),
        box(width = 6, title = "Simulated cooling system", uiOutput("sim_cooling"))
      ),
      fluidRow(box(width = 12, title = "How the simulation works", uiOutput("sim_notes")))
    ),
    # ---------- Cooling and physical design advisor ----------
    tabPanel(
      "Cooling & GPU Size", icon = icon("fan"),
      fluidRow(
        box(width = 4, title = "Hypothetical GPU design inputs",
            numericInput("design_power", "Estimated board power (W)", value = 220, min = 30, max = 800, step = 10),
            numericInput("design_length", "Maximum card length (mm)", value = 300, min = 100, max = 400, step = 5),
            selectInput("design_case", "Available case / chassis", choices = c("Compact / SFF", "Mid-tower", "Full tower"), selected = "Mid-tower"),
            selectInput("design_priority", "Design priority", choices = c("Balanced", "Quiet operation", "Small form factor", "Maximum cooling"), selected = "Balanced"),
            selectInput("design_cooler", "Cooler type allowed", choices = c("Air cooling only", "Allow liquid cooling"), selected = "Air cooling only"),
            actionButton("design_recommend", "Recommend cooling & size", icon = icon("microchip"), class = "btn-accent", width = "100%"),
            p(class = "note", "The supplied GPU dataset does not contain cooler type, card dimensions, thermal measurements, or fan-count labels. Recommendations below are engineering heuristics, not a trained classifier or validated thermal simulation.")),
        box(width = 8, title = "Manufacturing design recommendation", uiOutput("design_result"))
      ),
      
      # Box 1: Cooling options
      fluidRow(
        box(width = 12, title = "Cooling options", 
            fluidRow(
              column(4, h4("Dual / triple axial fans"), p("Typical air-cooled option for low-to-high power cards. Simpler and usually lower cost; requires a suitable heatsink and airflow path.")),
              column(4, h4("Vapor chamber + fans"), p("Useful for spreading heat across a broad heatsink on higher-power or dense designs. Adds cost and needs validation for orientation, contact pressure, and heat load.")),
              column(4, h4("Liquid cooling / AIO"), p("Consider for very high sustained heat loads or acoustic targets when the chassis supports a radiator. Higher cost, complexity, and reliability considerations."))
            ))
      ),
      
      # Box 2: Thermal baseplate options
      fluidRow(
        box(width = 12, title = "Thermal baseplate options", 
            fluidRow(
              column(4, h4("Direct-touch heatpipes"), p("Flattened copper heatpipes contact the GPU die directly. Cost-effective for low-to-mid range power (<200W), but less effective across large GPU die surfaces.")),
              column(4, h4("Solid copper baseplate"), p("Precision-machined copper plate distributes heat evenly from GPU die, VRAM, and VRM to soldered heatpipes. Standard for mid-to-high range cards (200W–350W).")),
              column(4, h4("Vapor chamber baseplate"), p("Sealed planar vacuum chamber using phase-change fluid for ultra-fast heat spreading across fin arrays. Essential for high-density, high-power designs (>350W)."))
            ))
      ),
      
      # Box 3: Power cable options
      fluidRow(
        box(width = 12, title = "Power cable options", 
            fluidRow(
              column(4, h4("Standard PCIe 8-pin / 6-pin"), p("Traditional 8-pin (150W rated) and 6-pin (75W rated) PCIe cables. Multiple dedicated cables required for high-draw cards to prevent cable overheating.")),
              column(4, h4("12V-2x6 / 12VHPWR (ATX 3.0)"), p("Compact 16-pin connector supporting up to 600W through a single cable with sideband sense pins. Replaces 3-4 bulky 8-pin cables on high-power GPUs.")),
              column(4, h4("PCIe Slot Power Only"), p("Provides up to 75W directly through the motherboard PCIe x16 expansion slot. Ideal for low-power, entry-level, or small-form-factor graphics cards without external PSU cables."))
            ))
      )
    )
  ),
  
  # ===================== CONSUMER =====================
  navbarMenu(
    "Consumer", icon = icon("user"),
    
    # ---------- Find by needs (NEW) ----------
    tabPanel(
      "Find by Needs", icon = icon("bullseye"),
      fluidRow(
        column(
          width = 3,
          box(
            width = 12, title = "Tell us what you need",
            radioButtons("need_use", "What will you use the GPU for?",
                         choices = names(USE_CASES), selected = "Gaming"),
            numericInput("need_budget", "Maximum budget ($)", value = 1300, min = 0, step = 50),
            p(class = "note", "Budget only filters out GPUs that cost more. Ranking is by performance."),
            numericInput("need_min_mem", "Minimum memory (MB)", value = 1024, min = 0, step = 512),
            selectizeInput("need_mfg", "Manufacturers", choices = manufacturers,
                           selected = manufacturers, multiple = TRUE),
            actionButton("need_go", "Find my GPUs", icon = icon("search"), class = "btn-accent", width = "100%")
          ),
          box(width = 12, title = "Most popular GPUs", popular_gpus_ui())
        ),
        column(
          width = 9,
          uiOutput("need_summary"),
          fluidRow(box(width = 12, title = "Best matches for your needs", uiOutput("need_top5"))),
          fluidRow(
            box(width = 12, title = "All matching GPUs (ranked)",
                p(class = "note", textOutput("need_count", inline = TRUE)),
                DTOutput("table_needs"), br(),
                downloadButton("dl_needs", "Download matches (CSV)", class = "btn-accent"))
          )
        )
      )
    ),
    
    # ---------- Recommender ----------
    tabPanel(
      "Recommender", icon = icon("star"),
      fluidRow(
        box(
          width = 3, title = "Your requirements",
          numericInput("rec_budget", "Budget ($)", value = 1300, min = 0, step = 50),
          numericInput("rec_min_mem", "Minimum memory (MB)", value = 2048, min = 0, step = 512),
          numericInput("rec_max_power", "Maximum power (W)", value = pwr_rng[2], min = 0, step = 10),
          selectizeInput("rec_mfg", "Manufacturers", choices = manufacturers, selected = manufacturers, multiple = TRUE),
          hr(),
          selectInput("preset", "Use case preset",
                      choices = c("Balanced", "Gaming", "Workstation / ML", "Budget value", "Energy efficient", "Custom")),
          sliderInput("w_mem",  "Weight: memory",           0, 100, 25),
          sliderInput("w_core", "Weight: core speed",       0, 100, 25),
          sliderInput("w_eff",  "Weight: power efficiency", 0, 100, 25),
          sliderInput("w_val",  "Weight: value for money",  0, 100, 25)
        ),
        column(
          width = 9,
          fluidRow(
            valueBoxOutput("vb_top",      width = 4),
            valueBoxOutput("vb_matches",  width = 4),
            valueBoxOutput("vb_topscore", width = 4)
          ),
          fluidRow(
            box(width = 12, title = "Top 5 recommended GPUs", uiOutput("rec_top5"))
          ),
          fluidRow(
            box(width = 6, title = "Top 5 by score", plotlyOutput("rec_bar", height = "340px")),
            box(width = 6, title = "Price vs score (all matches)", plotlyOutput("rec_scatter", height = "340px"))
          ),
          fluidRow(
            box(width = 12, title = "All GPUs matching your requirements (ranked)",
                p(class = "note", textOutput("rec_count", inline = TRUE)),
                DTOutput("table_recommendations"), br(),
                downloadButton("dl_recs", "Download all matches (CSV)", class = "btn-accent"))
          )
        )
      )
    ),
    
    # ---------- Compare ----------
    tabPanel(
      "Compare GPUs", icon = icon("exchange-alt"),
      fluidRow(
        box(width = 12, title = "Pick up to 4 GPUs",
            selectizeInput("cmp_gpus", NULL, choices = NULL, multiple = TRUE, width = "100%",
                           options = list(maxItems = 4, placeholder = "Start typing a GPU name...")))
      ),
      fluidRow(
        box(width = 7, title = "Specification table", DTOutput("cmp_table")),
        box(width = 5, title = "Profile (0-100, higher is better)", plotlyOutput("cmp_radar", height = "420px"))
      )
    )
  ),
  
  
)

# ---------------------------------------------------------------------
# 2b. Hologram helpers: build a wireframe 3D GPU from one CSV row
#     Real values (CSV): power, memory, memory type, bus, core clock,
#     power connectors, display outputs. Physical size is NOT in the CSV,
#     so card length / thickness / fan count are estimated from power.
# ---------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

box_edges <- function(x0, x1, y0, y1, z0, z1) {
  v <- list(c(x0,y0,z0), c(x1,y0,z0), c(x1,y1,z0), c(x0,y1,z0),
            c(x0,y0,z1), c(x1,y0,z1), c(x1,y1,z1), c(x0,y1,z1))
  e <- list(c(1,2), c(2,3), c(3,4), c(4,1), c(5,6), c(6,7),
            c(7,8), c(8,5), c(1,5), c(2,6), c(3,7), c(4,8))
  m <- do.call(rbind, lapply(e, function(i) rbind(v[[i[1]]], v[[i[2]]], c(NA, NA, NA))))
  data.frame(x = m[, 1], y = m[, 2], z = m[, 3])
}

box_mesh <- function(x0, x1, y0, y1, z0, z1) {
  list(x = c(x0,x1,x1,x0,x0,x1,x1,x0), y = c(y0,y0,y1,y1,y0,y0,y1,y1),
       z = c(z0,z0,z0,z0,z1,z1,z1,z1),
       i = c(0,0,4,4,0,0,1,1,2,2,3,3), j = c(1,2,5,6,1,5,2,6,3,7,0,4),
       k = c(2,3,6,7,5,4,6,5,7,6,4,7))
}

ring_pts <- function(cx, cy, z, r, n = 64) {
  a <- seq(0, 2 * pi, length.out = n + 1)
  rbind(data.frame(x = cx + r * cos(a), y = cy + r * sin(a), z = z),
        data.frame(x = NA_real_, y = NA_real_, z = NA_real_))
}

fan_blades <- function(cx, cy, z, r0, r1, nb = 9, sweep = 0.9) {
  t <- seq(0, 1, length.out = 10)
  do.call(rbind, lapply(seq_len(nb), function(b) {
    a0 <- 2 * pi * (b - 1) / nb
    a  <- a0 + sweep * t
    r  <- r0 + (r1 - r0) * t
    rbind(data.frame(x = cx + r * cos(a), y = cy + r * sin(a), z = z),
          data.frame(x = NA_real_, y = NA_real_, z = NA_real_))
  }))
}

mix_hex <- function(col, amt) {
  rgb_v <- grDevices::col2rgb(col)[, 1]
  v <- rgb_v * (1 - amt) + 255 * amt
  grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
}

holo_geometry <- function(s) {
  L <- s$L; D <- s$D; T <- s$T
  cx <- L * 0.42; cy <- D * 0.46
  geo <- list()
  
  # structure: PCB, shroud, bracket, PCIe edge connector
  geo$pcb    <- box_edges(0, L - 14, 0, D * 0.92, 0, 1.6)
  geo$shroud <- box_edges(14, L, 0, D * 0.97, 1.8, T)
  geo$bracket <- box_edges(-3, 0, -10, D + 2, -4, T + 2)
  pcie_x <- L * 0.20
  geo$pcie <- box_edges(pcie_x, pcie_x + 70, -11, 0, 0, 1.6)
  
  # heatsink fins inside the shroud (denser = more power)
  fin_step <- min(12, max(4, 14 - s$pw / 40))
  fx <- seq(20, L - 8, by = fin_step)
  geo$fins <- do.call(rbind, lapply(fx, function(xi) {
    data.frame(x = c(xi, xi, xi, xi, xi, NA), y = c(6, D - 6, D - 6, 6, 6, NA),
               z = c(3, 3, T - 3, T - 3, 3, NA))
  }))
  
  # fans on the top face
  geo$fans <- NULL
  if (s$nfan > 0) {
    span <- (L - 14) / s$nfan
    df <- min(D * 0.80, span - 4)
    fcx <- 14 + span * (seq_len(s$nfan) - 0.5)
    geo$fans <- do.call(rbind, lapply(fcx, function(fx0) {
      rbind(ring_pts(fx0, D / 2, T, df / 2), ring_pts(fx0, D / 2, T, df / 2 - 3),
            ring_pts(fx0, D / 2, T, df * 0.12),
            fan_blades(fx0, D / 2, T, df * 0.12, df / 2 - 3))
    }))
  }
  
  # GPU die and memory chips on the PCB
  geo$die <- box_edges(cx - 13, cx + 13, cy - 13, cy + 13, 1.6, 5)
  rx <- min(L * 0.28, 55); ry <- D * 0.36
  ang <- 2 * pi * (seq_len(s$n_chip) - 1) / s$n_chip + pi / s$n_chip
  geo$chips <- do.call(rbind, lapply(seq_along(ang), function(i) {
    px <- cx + rx * cos(ang[i]); py <- cy + ry * sin(ang[i])
    box_edges(px - 6, px + 6, py - 7, py + 7, 1.6, 3.4)
  }))
  chip_pos <- data.frame(x = cx + rx * cos(ang), y = cy + ry * sin(ang))
  
  # VRM blocks (count follows power)
  vx <- L * 0.60 + 10 * (seq_len(s$n_vrm) - 1)
  geo$vrm <- do.call(rbind, lapply(vx, function(xx) box_edges(xx, xx + 7, D * 0.74, D * 0.74 + 7, 1.6, 6)))
  
  # external power connectors on the top edge (6-pin 19 mm, 8-pin 24 mm wide)
  geo$power <- NULL
  if (length(s$pins)) {
    xr <- L - 22
    boxes <- list()
    for (p in s$pins) {
      w <- if (p == 6) 19 else if (p == 16) 22 else 24
      boxes[[length(boxes) + 1]] <- box_edges(xr - w, xr, D * 0.90, D + 8, 2, 13)
      xr <- xr - w - 4
    }
    geo$power <- do.call(rbind, boxes)
  }
  
  # display outputs on the bracket (outer face)
  ports <- c(rep("DVI", s$dvi), rep("HDMI", s$hdmi), rep("DP", s$dp))
  ports <- head(ports, 8)
  geo$ports <- NULL
  if (length(ports)) {
    len <- c(DVI = 28, HDMI = 16, DP = 16)[ports]
    sc <- min(1, (D - 8) / (sum(len) + 4 * (length(len) - 1)))
    y0 <- 4
    pb <- list()
    for (i in seq_along(len)) {
      pb[[i]] <- box_edges(-5, -3, y0, y0 + len[i] * sc, T / 2 - 5, T / 2 + 5)
      y0 <- y0 + (len[i] + 4) * sc
    }
    geo$ports <- do.call(rbind, pb)
  }
  
  # projector pad + light beams
  pad_z <- -40; pcx <- L / 2; pcy <- D / 2; pr <- L * 0.55
  geo$pad <- rbind(ring_pts(pcx, pcy, pad_z, pr), ring_pts(pcx, pcy, pad_z, pr * 0.78),
                   ring_pts(pcx, pcy, pad_z, pr * 0.45), ring_pts(pcx, pcy, pad_z, pr * 0.15))
  corners <- list(c(0, -10), c(L, -10), c(L, D + 2), c(0, D + 2))
  geo$beams <- do.call(rbind, lapply(corners, function(cn) {
    a <- atan2(cn[2] - pcy, cn[1] - pcx)
    data.frame(x = c(pcx + pr * cos(a), cn[1], NA), y = c(pcy + pr * sin(a), cn[2], NA), z = c(pad_z, 0, NA))
  }))
  
  geo$scan <- data.frame(x = c(-6, L + 6, L + 6, -6, -6), y = c(-12, -12, D + 10, D + 10, -12), z = rep(0, 5))
  geo$meta <- list(cx = cx, cy = cy, chip_pos = chip_pos, zmin = pad_z + 5, zmax = T + 50)
  geo
}

# ---------------------------------------------------------------------
# 2c. Cooling ladder + design engine (shared by the Cooling tab and the
#     Simulator). Heuristic, not ML: the CSV has no cooler data.
# ---------------------------------------------------------------------
cooler_tiers <- data.frame(
  tier = 1:11,
  name = c("Passive finned heatsink",
           "Low-profile single-fan cooler",
           "Blower (radial fan) cooler",
           "Compact dual-fan direct-touch cooler",
           "Dual-fan heatpipe cooler",
           "Dual-fan vapor-chamber cooler",
           "Triple-fan heatpipe cooler",
           "Triple-fan vapor-chamber cooler",
           "Heavy triple-fan vapor-chamber cooler",
           "Hybrid AIO liquid cooler (240 mm radiator)",
           "Hybrid AIO liquid cooler (360 mm radiator)"),
  cap = c(35, 75, 150, 180, 230, 280, 330, 400, 480, 600, 800),
  min_len = c(100, 120, 170, 190, 235, 255, 290, 310, 335, 280, 300),
  slots = c(1, 2, 2, 2, 2.5, 2.5, 2.5, 3, 3.5, 2, 2),
  liquid = c(rep(FALSE, 9), TRUE, TRUE),
  min_chassis = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3),
  fans = c("None (fanless, relies on case airflow)",
           "1x 60-70 mm fan",
           "1x radial blower (exhausts heat out the rear)",
           "2x 80 mm axial fans",
           "2x 90 mm axial fans",
           "2x 95 mm axial fans",
           "3x 90 mm axial fans",
           "3x 95 mm high-airflow axial fans",
           "3x 100 mm high-static-pressure fans",
           "2x 120 mm radiator fans + 1x 70 mm VRM/VRAM fan",
           "3x 120 mm radiator fans + 1x 70 mm VRM/VRAM fan"),
  pipes = c("None (fin stack only)",
            "1x 6 mm heatpipe",
            "2x 6 mm heatpipes + vapor-chamber plate",
            "3x 6 mm direct-touch heatpipes",
            "4x 6 mm heatpipes",
            "5x 6 mm heatpipes",
            "6x 6 mm heatpipes",
            "6-7x 6-8 mm heatpipes",
            "8x 8 mm composite heatpipes + dense fin array",
            "Liquid loop (no heatpipes needed)",
            "Liquid loop (no heatpipes needed)"),
  base = c("Machined aluminium base",
           "Copper contact plate",
           "Vapor-chamber plate",
           "Direct-touch copper heatpipes",
           "Solid copper baseplate",
           "Nickel-plated vapor chamber",
           "Solid copper baseplate",
           "Large-area vapor chamber",
           "Large-area 3.5 mm vapor chamber",
           "Micro-channel cold plate (GPU + VRAM + VRM)",
           "Full-coverage liquid block (GPU + VRAM + VRM)"),
  # numeric drawing data for the hologram
  n_fans   = c(0, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1),
  fan_mm   = c(0, 65, 75, 80, 90, 95, 90, 95, 100, 70, 70),
  pipes_n  = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0),
  vc       = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  rad_fans = c(rep(0, 9), 2, 3),
  rad_len  = c(rep(0, 9), 275, 395),
  stringsAsFactors = FALSE
)

# Smallest cooler tier that satisfies power, length, chassis, slot and
# air/liquid constraints. Returns list(error=) for invalid input.
cooling_design <- function(watts, max_len, chassis, priority, cooler) {
  if (is.null(watts) || is.na(watts) || watts < 30 || watts > 800)
    return(list(error = "Enter board power from 30 to 800 W."))
  if (is.null(max_len) || is.na(max_len) || max_len < 100 || max_len > 400)
    return(list(error = "Enter a card-length limit from 100 to 400 mm."))
  
  prio_margin    <- c("Balanced" = 1.00, "Quiet operation" = 1.25,
                      "Small form factor" = 1.00, "Maximum cooling" = 1.30)[[priority]]
  chassis_margin <- c("Compact / SFF" = 1.20, "Mid-tower" = 1.00, "Full tower" = 0.95)[[chassis]]
  eff_watts      <- watts * prio_margin * chassis_margin
  
  chassis_rank <- c("Compact / SFF" = 1, "Mid-tower" = 2, "Full tower" = 3)[[chassis]]
  max_slots <- switch(chassis, "Compact / SFF" = 2.5, "Mid-tower" = 3.5, "Full tower" = 4)
  if (priority == "Small form factor") max_slots <- min(max_slots, 2.5)
  air_only <- identical(cooler, "Air cooling only")
  
  t <- cooler_tiers
  t$ok_power   <- t$cap >= eff_watts
  t$ok_len     <- t$min_len <= max_len
  t$ok_chassis <- t$min_chassis <= chassis_rank
  t$ok_slots   <- t$slots <= max_slots
  t$ok_type    <- !(air_only & t$liquid)
  t$ok_all     <- t$ok_power & t$ok_len & t$ok_chassis & t$ok_slots & t$ok_type
  
  blockers <- character(0)
  if (any(t$ok_all)) {
    pick <- t[which(t$ok_all)[1], ]
    feasible <- TRUE
  } else {
    cand <- t[t$ok_power, ]
    if (nrow(cand) == 0) {
      pick <- t[nrow(t), ]
      blockers <- c(blockers, paste0(watts, " W (", round(eff_watts), " W with margins) exceeds the largest cooler modelled (",
                                     max(t$cap), " W). Reduce board power."))
    } else {
      pick <- cand[1, ]
      if (!pick$ok_len)
        blockers <- c(blockers, paste0("Needs a card at least ", pick$min_len, " mm long; your limit is ", max_len, " mm."))
      if (!pick$ok_chassis)
        blockers <- c(blockers, paste0("This cooler needs a ", c("Compact / SFF", "Mid-tower", "Full tower")[pick$min_chassis],
                                       " chassis or larger; you selected ", chassis, "."))
      if (!pick$ok_slots)
        blockers <- c(blockers, paste0("Needs ", pick$slots, " slots; your chassis/priority allows ", max_slots, "."))
      if (!pick$ok_type)
        blockers <- c(blockers, "Needs liquid cooling, but \"Air cooling only\" is selected.")
    }
    feasible <- FALSE
  }
  
  # Power delivery: PCIe slot 75 W, 6-pin 75 W, 8-pin 150 W, 12V-2x6 600 W
  ext <- watts - 75
  cable <- if (ext <= 0) "PCIe slot power only (no external cable)"
  else if (ext <= 75)  "1x 6-pin PCIe"
  else if (ext <= 150) "1x 8-pin PCIe"
  else if (ext <= 225) "1x 8-pin + 1x 6-pin PCIe"
  else if (ext <= 300) "2x 8-pin PCIe"
  else if (ext <= 525) "1x 12V-2x6 (up to 600 W)"
  else                 "2x 12V-2x6 connectors"
  pins <- if (ext <= 0) integer(0) else if (ext <= 75) 6L else if (ext <= 150) 8L
  else if (ext <= 225) c(8L, 6L) else if (ext <= 300) c(8L, 8L)
  else if (ext <= 525) 16L else c(16L, 16L)
  psu <- max(450, ceiling((watts + 300) / 50) * 50)
  psu_txt <- paste0(psu, " W or higher PSU", if (grepl("12V-2x6", cable)) " (ATX 3.x)" else "")
  
  headroom <- pick$cap / watts
  acoustic <- if (pick$liquid) "Pump + radiator fans; low noise at partial load"
  else if (pick$tier == 1) "Silent (no fan), but needs strong case airflow"
  else if (headroom >= 1.6) "Whisper-quiet: ~30 dBA, <= 1200 RPM"
  else if (headroom >= 1.3) "Quiet: ~30-35 dBA, <= 1500 RPM"
  else if (headroom >= 1.1) "Moderate: ~35-40 dBA, 1500-1900 RPM"
  else "Loud under sustained load: 40+ dBA, 2000+ RPM"
  
  list(watts = watts, eff_watts = round(eff_watts), chassis = chassis, priority = priority,
       max_len = max_len, pick = pick, feasible = feasible, blockers = blockers,
       cable = cable, pins = pins, psu = psu_txt, headroom = headroom, acoustic = acoustic)
}

# ---------------------------------------------------------------------
# 2d. Simulator geometry: GPU board + the cooling system chosen for it
# ---------------------------------------------------------------------
bezier3 <- function(p0, pc, p1, n = 20) {
  t <- seq(0, 1, length.out = n)
  rbind(data.frame(x = (1-t)^2*p0[1] + 2*(1-t)*t*pc[1] + t^2*p1[1],
                   y = (1-t)^2*p0[2] + 2*(1-t)*t*pc[2] + t^2*p1[2],
                   z = (1-t)^2*p0[3] + 2*(1-t)*t*pc[3] + t^2*p1[3]),
        data.frame(x = NA_real_, y = NA_real_, z = NA_real_))
}
spread <- function(a, b, n) if (n == 1) (a + b) / 2 else seq(a, b, length.out = n)

sim_geometry <- function(s, tr) {
  s$nfan <- tr$n_fans
  geo <- holo_geometry(s)
  L <- s$L; D <- s$D; T <- s$T; cx <- geo$meta$cx; cy <- geo$meta$cy
  liquid <- isTRUE(tr$liquid)
  
  # heatsink fins (air coolers only); pitch tightens with capacity
  geo$fins <- NULL
  if (!liquid) {
    step <- max(4, min(15, 16 - tr$cap / 40))
    geo$fins <- do.call(rbind, lapply(seq(20, L - 8, by = step), function(xi)
      data.frame(x = c(xi, xi, xi, xi, xi, NA), y = c(6, D - 6, D - 6, 6, 6, NA),
                 z = c(3, 3, T - 3, T - 3, 3, NA))))
  }
  
  # fans on the card
  geo$fans <- NULL
  if (tr$n_fans > 0) {
    span <- (L - 14) / tr$n_fans
    dfan <- min(tr$fan_mm, D * 0.85, span - 4)
    fcx  <- 14 + span * (seq_len(tr$n_fans) - 0.5)
    geo$fans <- do.call(rbind, lapply(fcx, function(f0)
      rbind(ring_pts(f0, D / 2, T, dfan / 2), ring_pts(f0, D / 2, T, dfan / 2 - 3),
            ring_pts(f0, D / 2, T, dfan * 0.12),
            fan_blades(f0, D / 2, T, dfan * 0.12, dfan / 2 - 3))))
  }
  
  # heatpipes: from the die, alternating left and right, arching up into the fins
  geo$pipes <- NULL
  if (tr$pipes_n > 0) {
    n <- tr$pipes_n
    ys <- cy + spread(-9, 9, n); ye <- spread(D * 0.15, D * 0.85, n)
    geo$pipes <- do.call(rbind, lapply(seq_len(n), function(i) {
      xe <- if (i %% 2 == 1) L - 12 else 20
      t <- seq(0, 1, length.out = 24)
      rbind(data.frame(x = cx + (xe - cx) * t, y = ys[i] + (ye[i] - ys[i]) * t,
                       z = 6 + (T * 0.55 - 6) * sin(t * pi / 2)),
            data.frame(x = NA_real_, y = NA_real_, z = NA_real_))
    }))
  }
  
  # base plate / vapor chamber / cold plate over the die
  hs <- if (liquid) 22 else if (tr$vc) 32 else 18
  geo$plate <- box_edges(cx - hs, cx + hs, cy - hs, cy + hs, 5, 8.5)
  geo$plate_mesh <- box_mesh(cx - hs, cx + hs, cy - hs, cy + hs, 5, 8.5)
  
  # liquid: pump block, tubes, radiator with its fans
  geo$radiator <- NULL; geo$rad_fins <- NULL; geo$rad_fans <- NULL; geo$tubes <- NULL
  xmax <- L + 6; ymax <- D + 10
  if (liquid) {
    geo$plate <- rbind(geo$plate, box_edges(cx - 22, cx + 22, cy - 22, cy + 22, 8.5, 22))
    rx0 <- L + 45; rl <- tr$rad_len; ry0 <- D / 2 - 62; ry1 <- D / 2 + 62
    geo$radiator <- box_edges(rx0, rx0 + rl, ry0, ry1, 0, 27)
    geo$rad_fins <- do.call(rbind, lapply(seq(rx0 + 6, rx0 + rl - 6, by = 7), function(xi)
      data.frame(x = c(xi, xi, xi, xi, xi, NA), y = c(ry0 + 3, ry1 - 3, ry1 - 3, ry0 + 3, ry0 + 3, NA),
                 z = c(2, 2, 25, 25, 2, NA))))
    nrf <- tr$rad_fans
    fcx <- rx0 + rl * (seq_len(nrf) - 0.5) / nrf
    geo$rad_fans <- do.call(rbind, lapply(fcx, function(f0) {
      rbind(box_edges(f0 - 60, f0 + 60, D / 2 - 60, D / 2 + 60, 27, 52),
            ring_pts(f0, D / 2, 52, 56), ring_pts(f0, D / 2, 52, 14),
            fan_blades(f0, D / 2, 52, 14, 56, nb = 7))
    }))
    geo$tubes <- rbind(
      bezier3(c(L - 22, D * 0.9, T * 0.5), c(L + 10, D + 60, T * 0.5 + 15), c(rx0 + 30, ry1, 13)),
      bezier3(c(L - 34, D * 0.9, T * 0.5), c(L - 5,  D + 75, T * 0.5 + 15), c(rx0 + 55, ry1, 13)))
    xmax <- rx0 + rl + 6; ymax <- max(D + 10, ry1 + 6)
  }
  
  # projector pad, beams and scan plane sized to everything drawn
  xmin <- -6; ymin <- -12; pad_z <- -40
  pcx <- (xmin + xmax) / 2; pcy <- (ymin + ymax) / 2
  pr <- 0.62 * max(xmax - xmin, ymax - ymin)
  geo$pad <- rbind(ring_pts(pcx, pcy, pad_z, pr), ring_pts(pcx, pcy, pad_z, pr * 0.78),
                   ring_pts(pcx, pcy, pad_z, pr * 0.45), ring_pts(pcx, pcy, pad_z, pr * 0.15))
  geo$beams <- do.call(rbind, lapply(list(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax)), function(cn) {
    a <- atan2(cn[2] - pcy, cn[1] - pcx)
    data.frame(x = c(pcx + pr * cos(a), cn[1], NA), y = c(pcy + pr * sin(a), cn[2], NA), z = c(pad_z, 0, NA))
  }))
  geo$scan <- data.frame(x = c(xmin, xmax, xmax, xmin, xmin), y = c(ymin, ymin, ymax, ymax, ymin), z = rep(0, 5))
  top <- if (liquid) 52 else T
  geo$meta$zmin <- pad_z + 5; geo$meta$zmax <- top + 55
  geo$dims <- list(L = L, D = D, T = T, top = top, xmax = xmax)
  geo
}

# Draw a geometry list as a rotating hologram (plotly + a little JS)
build_holo_plot <- function(geo, col, labels = NULL, lead = NULL) {
  hi <- mix_hex(col, 0.6); cool <- mix_hex(col, 0.85)
  m <- geo$meta; d <- geo$dims
  structure_df <- bind_rows(geo$pcb, geo$shroud, geo$bracket, geo$pcie, geo$power, geo$vrm,
                            geo$ports, geo$radiator)
  board_acc <- bind_rows(geo$die, geo$chips)
  cooling_df <- bind_rows(geo$fins, geo$fans, geo$pipes, geo$plate, geo$rad_fins,
                          geo$rad_fans, geo$tubes)
  all_df <- bind_rows(structure_df, board_acc, cooling_df)
  
  ln <- function(p, df, color, width, opacity, name = NULL, dash = "solid") {
    if (is.null(df) || nrow(df) == 0) return(p)
    add_trace(p, data = df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
              line = list(color = color, width = width, dash = dash), opacity = opacity,
              hoverinfo = "none", showlegend = FALSE, name = name, connectgaps = FALSE)
  }
  face <- function(p, b, opacity, color = col) {
    add_trace(p, type = "mesh3d", x = b$x, y = b$y, z = b$z, i = b$i, j = b$j, k = b$k,
              color = I(color), opacity = opacity, hoverinfo = "none", flatshading = TRUE, showlegend = FALSE)
  }
  
  p <- plot_ly() %>%
    face(box_mesh(0, d$L - 14, 0, d$D * 0.92, 0, 1.6), 0.18) %>%
    face(box_mesh(14, d$L, 0, d$D * 0.97, 1.8, d$T), 0.05) %>%
    face(box_mesh(m$cx - 13, m$cx + 13, m$cy - 13, m$cy + 13, 1.6, 5), 0.55, hi) %>%
    face(geo$plate_mesh, 0.40, cool) %>%
    ln(geo$pad, col, 2, 0.45) %>%
    ln(geo$beams, col, 1, 0.25, dash = "dot") %>%
    ln(all_df, col, 9, 0.10) %>%
    ln(structure_df, col, 2.5, 0.85) %>%
    ln(board_acc, hi, 3.5, 1) %>%
    ln(cooling_df, cool, 3, 1) %>%
    ln(geo$scan, "#FFFFFF", 5, 0.8, name = "scan")
  
  if (!is.null(labels)) {
    p <- p %>% ln(lead, hi, 1.5, 0.6, dash = "dot") %>%
      add_trace(data = labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text", text = ~txt,
                textfont = list(color = c(rep(hi, nrow(labels) - 1), "#FFFFFF"),
                                size = c(rep(12, nrow(labels) - 1), 15), family = "IBM Plex Sans, Arial"),
                hoverinfo = "none", showlegend = FALSE)
  }
  
  cam_ang <- -0.9
  p <- p %>% layout(
    scene = list(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), zaxis = list(visible = FALSE),
      aspectmode = "data", bgcolor = "rgba(0,0,0,0)",
      camera = list(eye = list(x = 2 * cos(cam_ang), y = 2 * sin(cam_ang), z = 0.9),
                    center = list(x = 0, y = 0, z = -0.05))),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 0, r = 0, t = 0, b = 0), showlegend = FALSE) %>%
    config(displaylogo = FALSE)
  
  js <- sprintf("
    function(el, x) {
      if (el._holoTimer) clearInterval(el._holoTimer);
      var ang = %f, rad = 2.0, ez = 0.9, paused = false, resume = null, t = 0;
      var zmin = %f, zmax = %f, idx = -1;
      for (var i = 0; i < el.data.length; i++) { if (el.data[i].name === 'scan') idx = i; }
      function hold() { paused = true; clearTimeout(resume); }
      function release() { resume = setTimeout(function() { paused = false; }, 2500); }
      el.addEventListener('mousedown', hold);  el.addEventListener('mouseup', release);
      el.addEventListener('touchstart', hold); el.addEventListener('touchend', release);
      el.on('plotly_relayout', function(e) {
        var c = e['scene.camera'];
        if (c && c.eye) { ang = Math.atan2(c.eye.y, c.eye.x); rad = Math.sqrt(c.eye.x*c.eye.x + c.eye.y*c.eye.y); ez = c.eye.z; }
      });
      el._holoTimer = setInterval(function() {
        var chk = document.getElementById('holo_spin');
        if (chk && !chk.checked) return;
        if (!document.body.contains(el) || el.offsetParent === null) return;
        t = (t + 0.015) %% 1;
        var z = zmin + t * (zmax - zmin);
        if (idx >= 0) Plotly.restyle(el, {z: [[z, z, z, z, z]]}, [idx]);
        if (!paused) {
          ang += 0.012;
          Plotly.relayout(el, {'scene.camera.eye': {x: rad*Math.cos(ang), y: rad*Math.sin(ang), z: ez}});
        }
      }, 70);
    }", cam_ang, m$zmin, m$zmax)
  htmlwidgets::onRender(p, js)
}

# Specs the simulator predicts from (core clock, memory, bus, power)
SIM_INPUTS  <- c("Clean_Core_Speed", "Clean_Memory", "Clean_Memory_Bus", "Clean_Power")
SIM_TARGETS <- c("Clean_Boost_Clock", "Clean_Memory_Speed", "Clean_Memory_Bandwidth",
                 "Clean_L2_Cache", "Clean_Pixel_Rate", "Clean_Texture_Rate", "Clean_TMUs", "Clean_ROPs")
SIM_UNITS   <- c(Clean_Boost_Clock = "MHz", Clean_Memory_Speed = "MHz", Clean_Memory_Bandwidth = "GB/s",
                 Clean_L2_Cache = "KB", Clean_Pixel_Rate = "GPixel/s", Clean_Texture_Rate = "GTexel/s",
                 Clean_TMUs = "", Clean_ROPs = "")
SIM_NAMES   <- c(Clean_Boost_Clock = "Boost clock", Clean_Memory_Speed = "Memory speed",
                 Clean_Memory_Bandwidth = "Memory bandwidth", Clean_L2_Cache = "L2 cache",
                 Clean_Pixel_Rate = "Pixel rate", Clean_Texture_Rate = "Texture rate",
                 Clean_TMUs = "TMUs", Clean_ROPs = "ROPs")

# ---------------------------------------------------------------------
# 3. SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  # ---------- Cooling & GPU Size ----------
  design_recommendation <- eventReactive(input$design_recommend, {
    cooling_design(input$design_power, input$design_length, input$design_case,
                   input$design_priority, input$design_cooler)
  })
  
  output$design_result <- renderUI({
    r <- design_recommendation()
    if (is.null(r)) return(div(class = "note", "Enter the power and size limits, then select Recommend cooling & size."))
    if (!is.null(r$error)) return(div(class = "alert alert-danger", r$error))
    pk <- r$pick
    pt <- function(label, value) p(style = "color: #F0F6FC; font-size: 14px;", strong(paste0(label, ": ")), value)
    
    tagList(
      h3(style = "color: #4C9AFF; font-weight: 700; margin-bottom: 15px;",
         if (r$feasible) "Minimum adequate design" else "Closest design (constraints not met)"),
      fluidRow(
        column(4, valueBox(pk$name, "Cooling architecture", icon = icon("snowflake"))),
        column(4, valueBox(pk$fans, "Fans / airflow", icon = icon("fan"))),
        column(4, valueBox(paste0(pk$slots, " slots, >= ", pk$min_len, " mm"), "Minimum card envelope", icon = icon("ruler-combined")))
      ),
      fluidRow(
        column(6, div(class = "gbox",
                      div(class = "gbox-title", "Thermal hardware"),
                      div(class = "gbox-body",
                          pt("Baseplate", pk$base),
                          pt("Heatpipes", pk$pipes),
                          pt("Rated capacity", paste0(pk$cap, " W (design load ", r$watts, " W, sized for ",
                                                      r$eff_watts, " W)")),
                          pt("Headroom", paste0(round((r$headroom - 1) * 100), "%"))))),
        column(6, div(class = "gbox",
                      div(class = "gbox-title", "Power & acoustics"),
                      div(class = "gbox-body",
                          pt("Power connector", r$cable),
                          pt("System PSU", r$psu),
                          pt("Estimated acoustics", r$acoustic),
                          pt("Chassis & length", paste0(r$chassis, ", up to ", r$max_len, " mm")))))
      ),
      if (r$feasible) {
        div(class = "alert alert-success",
            style = "background-color: #0B2552; border-color: #1F6FEB; color: #4C9AFF; margin-top: 10px;",
            strong("Fits your constraints. "),
            "This is the smallest, lowest-cost cooler tier that handles the load; no larger tier is needed.")
      } else {
        div(class = "alert alert-danger",
            style = "background-color: #3B1512; border-color: #FF5555; color: #FFB4AB; margin-top: 10px;",
            strong("Not feasible as specified: "),
            tags$ul(lapply(r$blockers, tags$li)))
      }
    )
  })
  # ---------- Manufacturer: GPU simulator ----------
  updateSelectizeInput(session, "sim_from", choices = sort(unique(gpu_data$Name)), server = TRUE)
  
  observeEvent(input$sim_load, {
    req(input$sim_from)
    g <- gpu_data %>% filter(Name == input$sim_from) %>% slice(1)
    req(nrow(g) == 1)
    nz <- function(x, d = NA) if (is.null(x) || is.na(x)) d else x
    updateNumericInput(session, "sim_core",  value = nz(g$Clean_Core_Speed))
    updateNumericInput(session, "sim_mem",   value = nz(g$Clean_Memory))
    updateNumericInput(session, "sim_bus",   value = nz(g$Clean_Memory_Bus))
    updateNumericInput(session, "sim_power", value = nz(g$Clean_Power))
    updateNumericInput(session, "sim_dvi",   value = nz(g$DVI_Connection, 0))
    updateNumericInput(session, "sim_hdmi",  value = nz(g$HDMI_Connection, 0))
    updateNumericInput(session, "sim_dp",    value = nz(g$DisplayPort_Connection, 0))
    showNotification("Values loaded. Edit them if you like, then press Simulate.", type = "message")
  })
  
  # One small random forest per predicted spec, trained once and cached
  sim_cache <- new.env()
  get_sim_models <- function() {
    if (!is.null(sim_cache$models)) return(sim_cache$models)
    models <- withProgress(message = "Training prediction models", value = 0, {
      lapply(SIM_TARGETS, function(tg) {
        incProgress(1 / length(SIM_TARGETS), detail = SIM_NAMES[[tg]])
        d <- gpu_data %>% select(all_of(c(tg, SIM_INPUTS))) %>% filter(complete.cases(.)) %>% as.data.frame()
        if (nrow(d) < 30) return(NULL)
        set.seed(42)
        rf <- randomForest(reformulate(SIM_INPUTS, response = tg), data = d, ntree = 150)
        list(rf = rf, n = nrow(d), r2 = tail(rf$rsq, 1))
      })
    })
    names(models) <- SIM_TARGETS
    sim_cache$models <- models
    models
  }
  
  sim_result <- eventReactive(input$sim_go, {
    core <- input$sim_core; mem <- input$sim_mem; bus <- input$sim_bus; pw <- input$sim_power
    ok <- function(x, lo, hi) !is.null(x) && !is.na(x) && x >= lo && x <= hi
    if (!ok(core, 100, 4000)) return(list(error = "Core clock must be between 100 and 4000 MHz."))
    if (!ok(mem, 16, 98304))  return(list(error = "Memory must be between 16 and 98,304 MB."))
    if (!ok(bus, 32, 4096))   return(list(error = "Memory bus must be between 32 and 4096 bit."))
    if (!ok(pw, 30, 800))     return(list(error = "Board power must be between 30 and 800 W."))
    cd <- cooling_design(pw, input$sim_len, input$sim_case, input$sim_priority, input$sim_cooler)
    if (!is.null(cd$error)) return(list(error = cd$error))
    
    models <- get_sim_models()
    nd <- data.frame(Clean_Core_Speed = core, Clean_Memory = mem, Clean_Memory_Bus = bus, Clean_Power = pw)
    preds <- bind_rows(lapply(SIM_TARGETS, function(tg) {
      m <- models[[tg]]
      if (is.null(m)) return(NULL)
      p <- predict(m$rf, nd, predict.all = TRUE)
      tv <- as.numeric(p$individual)
      data.frame(Target = tg, Spec = SIM_NAMES[[tg]], Value = as.numeric(p$aggregate)[1],
                 Low = unname(quantile(tv, 0.1)), High = unname(quantile(tv, 0.9)),
                 Unit = SIM_UNITS[[tg]], R2 = m$r2, stringsAsFactors = FALSE)
    }))
    
    n_mem  <- ecdf(na.omit(gpu_data$Clean_Memory))(mem)
    n_core <- ecdf(na.omit(gpu_data$Clean_Core_Speed))(core)
    perf   <- round((n_mem + n_core) / 2 * 100, 1)
    
    rg <- function(v) range(gpu_data[[v]], na.rm = TRUE)
    chk <- list(Clean_Core_Speed = core, Clean_Memory = mem, Clean_Memory_Bus = bus, Clean_Power = pw)
    nm  <- c(Clean_Core_Speed = "Core clock", Clean_Memory = "Memory", Clean_Memory_Bus = "Memory bus", Clean_Power = "Board power")
    warn <- character(0)
    for (v in names(chk)) {
      r <- rg(v)
      if (chk[[v]] < r[1] || chk[[v]] > r[2])
        warn <- c(warn, paste0(nm[[v]], " (", format(chk[[v]], big.mark = ","), ") is outside the dataset range (",
                               format(r[1], big.mark = ","), " to ", format(r[2], big.mark = ","),
                               "), so predictions are extrapolated."))
    }
    
    tr <- cd$pick
    s <- list(pw = pw, pins = cd$pins, conn_est = FALSE, pw_known = TRUE, bus_known = TRUE,
              L = tr$min_len, D = min(135, max(100, 95 + pw * 0.12)), T = tr$slots * 20,
              nfan = tr$n_fans, n_chip = min(16, max(2, round(bus / 32))),
              n_vrm = min(8, max(1, round(pw / 40))),
              dvi = input$sim_dvi %||% 0, hdmi = input$sim_hdmi %||% 0, dp = input$sim_dp %||% 0)
    geo <- sim_geometry(s, tr)
    list(inp = list(core = core, mem = mem, bus = bus, pw = pw), cd = cd, preds = preds, perf = perf,
         ppw = round(perf / pw, 3), warn = warn, s = s, geo = geo)
  }, ignoreNULL = FALSE)
  
  output$sim_verdict <- renderUI({
    r <- sim_result()
    if (!is.null(r$error)) return(div(class = "alert alert-danger", r$error))
    cd <- r$cd
    if (cd$feasible) {
      div(class = "alert alert-success", style = "background-color: #0B2552; border-color: #1F6FEB; color: #4C9AFF;",
          strong("FEASIBLE: "), paste0("a ", cd$pick$name, " cools ", cd$watts, " W within a ", cd$max_len, " mm, ",
                                       cd$chassis, " design (", round((cd$headroom - 1) * 100), "% thermal headroom)."),
          if (length(r$warn)) tags$ul(lapply(r$warn, tags$li)))
    } else {
      div(class = "alert alert-danger", style = "background-color: #3B1512; border-color: #FF5555; color: #FFB4AB;",
          strong("NOT FEASIBLE as specified: "), tags$ul(lapply(cd$blockers, tags$li)),
          "The hologram shows the closest cooler that handles the heat. ",
          if (length(r$warn)) tags$ul(lapply(r$warn, tags$li)))
    }
  })
  
  pred_val <- function(r, tg, digits = 0) {
    v <- r$preds$Value[r$preds$Target == tg]
    if (length(v) == 0) "n/a" else format(round(v, digits), big.mark = ",")
  }
  output$sv_perf <- renderValueBox({
    r <- sim_result(); req(is.null(r$error))
    valueBox(paste0(r$perf, " / 100"), paste0("Performance index (", r$ppw, " per W)"), icon = icon("tachometer-alt"))
  })
  output$sv_boost <- renderValueBox({
    r <- sim_result(); req(is.null(r$error))
    valueBox(paste0(pred_val(r, "Clean_Boost_Clock"), " MHz"), "Predicted boost clock", icon = icon("rocket"))
  })
  output$sv_bw <- renderValueBox({
    r <- sim_result(); req(is.null(r$error))
    valueBox(paste0(pred_val(r, "Clean_Memory_Bandwidth"), " GB/s"), "Predicted memory bandwidth", icon = icon("exchange-alt"))
  })
  output$sv_cooler <- renderValueBox({
    r <- sim_result(); req(is.null(r$error))
    valueBox(r$cd$pick$name, if (r$cd$feasible) "Cooling system (minimum adequate)" else "Closest cooler (not feasible)",
             icon = icon("snowflake"))
  })
  
  output$holo_plot <- renderPlotly({
    r <- sim_result(); req(is.null(r$error))
    cd <- r$cd; s <- r$s; geo <- r$geo; m <- geo$meta; d <- geo$dims; tr <- cd$pick
    col <- if (cd$feasible) (input$holo_color %||% "#00E5FF") else "#FF4D4D"
    
    labels <- NULL; lead <- NULL
    if (isTRUE(input$holo_labels)) {
      f0 <- function(x, u = "") paste0(format(round(x), big.mark = ","), u)
      boost <- r$preds$Value[r$preds$Target == "Clean_Boost_Clock"]
      conn <- if (length(s$pins)) paste(ifelse(s$pins == 16, "12V-2x6", paste0(s$pins, "-pin")), collapse = " + ") else "slot power only"
      zt <- d$top + 28
      cool_x <- if (tr$liquid) d$xmax - tr$rad_len / 2 - 3 else d$L * 0.78
      cool_y <- d$D / 2
      labels <- data.frame(
        x = c(m$cx, m$chip_pos$x[1], d$L - 40, -12, cool_x, d$L / 2),
        y = c(m$cy, m$chip_pos$y[1], d$D + 6, d$D / 2, cool_y, d$D / 2),
        z = c(zt, zt, zt, zt, d$top + 28, d$top + 70),
        txt = c(paste0("GPU CORE<br>", f0(r$inp$core, " MHz"), if (length(boost)) paste0("<br>boost ~", f0(boost, " MHz")) else ""),
                paste0("VRAM<br>", f0(r$inp$mem, " MB"), "<br>", r$inp$bus, "-bit, ", s$n_chip, " chips"),
                paste0("POWER ", f0(r$inp$pw, " W"), "<br>", conn),
                paste0("OUTPUTS<br>", s$dvi, "x DVI ", s$hdmi, "x HDMI ", s$dp, "x DP"),
                paste0("COOLING<br>", tr$name, "<br>", tr$cap, " W capacity"),
                paste0(if (cd$feasible) "FEASIBLE" else "NOT FEASIBLE", "  |  SIMULATED GPU")),
        stringsAsFactors = FALSE)
      lead_z0 <- c(d$T, d$T, d$T, d$T / 2, d$top)
      lead <- do.call(rbind, lapply(1:5, function(i)
        data.frame(x = c(labels$x[i], labels$x[i], NA), y = c(labels$y[i], labels$y[i], NA),
                   z = c(lead_z0[i], labels$z[i] - 6, NA))))
    }
    build_holo_plot(geo, col, labels, lead)
  })
  
  output$sim_pred_table <- renderDT({
    r <- sim_result(); req(is.null(r$error))
    d <- r$preds %>%
      transmute(Spec = ifelse(Unit == "", Spec, paste0(Spec, " (", Unit, ")")),
                Predicted = round(Value, 1),
                `Likely range` = paste0(format(round(Low, 1), big.mark = ","), " to ", format(round(High, 1), big.mark = ",")),
                `Model R-squared` = round(R2, 2))
    datatable(d, rownames = FALSE, options = list(dom = "t", ordering = FALSE, pageLength = 12))
  })
  
  output$sim_cooling <- renderUI({
    r <- sim_result(); req(is.null(r$error))
    cd <- r$cd; tr <- cd$pick
    pt <- function(label, value) p(style = "color: #F0F6FC; font-size: 14px; margin-bottom: 6px;", strong(paste0(label, ": ")), value)
    tagList(
      pt("Cooling architecture", tr$name),
      pt("Fans / airflow", tr$fans),
      pt("Baseplate", tr$base),
      pt("Heatpipes", tr$pipes),
      pt("Card envelope", paste0(tr$slots, " slots, at least ", tr$min_len, " mm long")),
      if (tr$liquid) pt("Radiator", paste0(ifelse(tr$rad_len > 300, "360", "240"), " mm radiator mounted separately in the chassis")),
      pt("Rated capacity", paste0(tr$cap, " W (design load ", cd$watts, " W, sized for ", cd$eff_watts, " W); headroom ",
                                  round((cd$headroom - 1) * 100), "%")),
      pt("Power connector", cd$cable),
      pt("System PSU", cd$psu),
      pt("Estimated acoustics", cd$acoustic))
  })
  
  output$sim_notes <- renderUI({
    r <- sim_result(); req(is.null(r$error))
    tags$ul(
      tags$li(strong("Your inputs: "), "core clock, memory, bus, board power and display outputs are used exactly as entered."),
      tags$li(strong("Predicted: "), "boost clock, memory speed, bandwidth, L2 cache, pixel rate, texture rate, TMUs and ROPs are Random Forest estimates trained on dataset GPUs where those values are known. They show typical hardware with your specs, not a guarantee."),
      tags$li(strong("Performance index: "), "the dashboard's 0-100 proxy (percentile of memory and core clock in the dataset), not a benchmark."),
      tags$li(strong("Cooling system: "), "the smallest tier on a heuristic ladder that handles your power with margin for chassis and priority and fits your length limit. It is an engineering rule of thumb, not a thermal simulation; validate with real thermal testing."),
      tags$li(strong("Hologram: "), "memory chips = bus / 32 bit; power connectors and PSU follow board power; fans, heatpipes, vapor chamber, fins and radiator come from the chosen cooler. Red means the design is not feasible."))
  })
  
  observeEvent(input$home_visualizations, updateNavbarPage(session, "nav", selected = "Visualizations"))
  observeEvent(input$home_compare, updateNavbarPage(session, "nav", selected = "Compare GPUs"))
  
  
  # ---------- Home: search + listing ----------
  home_df <- reactive({
    d <- gpu_data
    q <- tolower(trimws(if (is.null(input$search_name)) "" else input$search_name))
    if (nzchar(q)) d <- d %>% filter(grepl(q, tolower(Name), fixed = TRUE))
    d %>%
      select(Name, Manufacturer,
             `Core Speed (MHz)` = Clean_Core_Speed, `Memory (MB)` = Clean_Memory,
             `Max Power (W)` = Clean_Power, `Price ($)` = Clean_Price,
             `Release Year` = Release_Year, `Perf Index` = Perf_Index,
             `Perf per Watt` = Perf_per_Watt, `Perf per $100` = Perf_per_Dollar)
  })
  output$home_count <- renderText({
    paste0(format(nrow(home_df()), big.mark = ","), " of ",
           format(nrow(gpu_data), big.mark = ","), " GPUs shown.")
  })
  output$table_home <- renderDT({
    datatable(home_df(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE,
                             lengthMenu = list(c(15, 50, 100, -1), c("15", "50", "100", "All")))) %>%
      formatCurrency("Price ($)", digits = 0)
  })
  output$dl_home <- downloadHandler(
    filename = function() paste0("gpu_listing_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(home_df(), file, row.names = FALSE)
  )
  
  # ---------- Manufacturer: Visualizations ----------
  output$dynamic_plot <- renderPlotly({
    df <- gpu_data
    ptype <- input$plot_type
    
    if (ptype == "Scatter Plot") {
      xv <- input$x_var; yv <- input$y_var
      d <- df %>% filter(!is.na(.data[[xv]]), !is.na(.data[[yv]]))
      validate(need(nrow(d) > 1, "Not enough complete data for these two variables."))
      p <- ggplot(d, aes(x = .data[[xv]], y = .data[[yv]], colour = Manufacturer, text = Name)) +
        geom_point(alpha = 0.65, size = 2) + scale_mfg_col() + theme_gpu() +
        labs(x = lab(xv), y = lab(yv), colour = NULL)
      if (isTRUE(input$show_trend)) {
        p <- p + geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                             colour = "#FFFFFF", linetype = "dashed", linewidth = 0.7)
      }
      if (isTRUE(input$log_x)) p <- p + scale_x_log10()
      if (isTRUE(input$log_y)) p <- p + scale_y_log10()
      return(pl(p, top = 20))
      
    } else if (ptype == "Histogram") {
      xv <- input$x_var
      d <- df %>% filter(!is.na(.data[[xv]]))
      validate(need(nrow(d) > 1, "Not enough data."))
      p <- ggplot(d, aes(x = .data[[xv]], fill = Manufacturer)) +
        geom_histogram(bins = input$bins, alpha = 0.8, position = "stack") +
        scale_mfg_fill() + theme_gpu() +
        labs(x = lab(xv), y = "Count", fill = NULL)
      return(pl(p, top = 20))
      
    } else if (ptype == "Density") {
      xv <- input$x_var
      d <- df %>% filter(!is.na(.data[[xv]]))
      validate(need(nrow(d) > 5, "Not enough data."))
      p <- ggplot(d, aes(x = .data[[xv]], fill = Manufacturer, colour = Manufacturer)) +
        geom_density(alpha = 0.25) + scale_mfg_fill() + scale_mfg_col() + theme_gpu() +
        labs(x = lab(xv), y = "Density", fill = NULL, colour = NULL)
      return(pl(p, top = 20))
      
    } else if (ptype == "Boxplot") {
      yv <- input$y_var
      d <- df %>% filter(!is.na(.data[[yv]]))
      validate(need(nrow(d) > 1, "Not enough data."))
      p <- ggplot(d, aes(x = Manufacturer, y = .data[[yv]], fill = Manufacturer)) +
        geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) + scale_mfg_fill() + theme_gpu() +
        theme(legend.position = "none") +
        labs(x = NULL, y = lab(yv))
      return(pl(p, top = 20, bottom = 60) %>% layout(showlegend = FALSE))
      
    } else if (ptype == "Trend Over Time") {
      yv <- input$y_var
      fn <- if (input$trend_stat == "mean") mean else median
      d <- df %>% filter(!is.na(Release_Year), !is.na(.data[[yv]])) %>%
        group_by(Release_Year, Manufacturer) %>%
        summarise(Value = fn(.data[[yv]]), n = n(), .groups = "drop")
      validate(need(nrow(d) > 1, "Not enough data."))
      p <- ggplot(d, aes(Release_Year, Value, colour = Manufacturer, group = Manufacturer)) +
        geom_line(linewidth = 1) + geom_point(size = 2) + scale_mfg_col() + theme_gpu() +
        labs(x = "Release year", y = paste(str_to_title(input$trend_stat), lab(yv)), colour = NULL)
      return(pl(p, top = 20))
      
    } else {  # Correlation Matrix
      cols <- c("Clean_Core_Speed", "Clean_Memory", "Clean_Power", "Release_Year", "Clean_Price",
                "Perf_per_Watt")
      num <- df %>% select(all_of(cols)) %>% na.omit()
      validate(need(nrow(num) > 5, "Not enough complete rows for a correlation matrix."))
      cm <- cor(num)
      cdf <- as.data.frame(as.table(cm)) %>%
        mutate(Var1 = short_lab(as.character(Var1)), Var2 = short_lab(as.character(Var2)),
               Label = sprintf("%.2f", Freq))
      p <- ggplot(cdf, aes(Var1, Var2, fill = Freq, text = paste(Var1, "/", Var2, ":", Label))) +
        geom_tile(colour = "#161B22") +
        geom_text(aes(label = Label), size = 3.4, colour = "#000000") +
        scale_fill_gradient2(low = "#A8505A", mid = "white", high = NV_GREEN,
                             midpoint = 0, limits = c(-1, 1)) +
        theme_gpu() + theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = "r")
      return(pl(p, tooltip = "text", top = 20, bottom = 110) %>%
               layout(legend = list(orientation = "v", x = 1.02, y = 0.5)))
    }
  })
  
  # ---------- Consumer: Compare ----------
  updateSelectizeInput(session, "cmp_gpus", choices = sort(unique(gpu_data$Name)), server = TRUE)
  
  cmp_df <- reactive({
    req(length(input$cmp_gpus) > 0)
    gpu_data %>% filter(Name %in% input$cmp_gpus) %>% distinct(Name, .keep_all = TRUE)
  })
  
  output$cmp_table <- renderDT({
    d <- cmp_df() %>%
      transmute(Manufacturer,
                `Core (MHz)` = Clean_Core_Speed, `Boost (MHz)` = Clean_Boost_Clock,
                `Memory (MB)` = Clean_Memory, `Bandwidth (GB/s)` = Clean_Memory_Bandwidth,
                `Bus (bit)` = Clean_Memory_Bus, `Memory speed (MHz)` = Clean_Memory_Speed,
                `L2 cache (KB)` = Clean_L2_Cache, `Texture rate` = Clean_Texture_Rate,
                `Pixel rate` = Clean_Pixel_Rate, `TMUs` = Clean_TMUs, `ROPs` = Clean_ROPs,
                `Power (W)` = Clean_Power, `Price ($)` = Clean_Price,
                Year = Release_Year, `Perf Index` = Perf_Index,
                `Perf/W` = Perf_per_Watt, `Perf/$100` = Perf_per_Dollar)
    d <- as.data.frame(t(d), stringsAsFactors = FALSE)
    colnames(d) <- cmp_df()$Name
    datatable(d, options = list(dom = "t", ordering = FALSE, scrollX = TRUE, pageLength = 20))
  })
  
  output$cmp_radar <- renderPlotly({
    d <- cmp_df()
    axes <- c("Memory", "Core speed", "Efficiency", "Value", "Affordability")
    p <- plot_ly(type = "scatterpolar", mode = "lines", fill = "toself")
    for (i in seq_len(nrow(d))) {
      v <- 100 * c(d$N_Mem[i], d$N_Core[i], d$N_Eff[i], d$N_Value[i], d$N_Cheap[i])
      p <- add_trace(p, r = c(v, v[1]), theta = c(axes, axes[1]), name = d$Name[i], opacity = 0.6)
    }
    p %>% layout(polar = list(bgcolor = "rgba(0,0,0,0)",
                              radialaxis = list(visible = TRUE, range = c(0, 100),
                                                gridcolor = "#3D444D", tickfont = list(color = "#9DA7B3")),
                              angularaxis = list(gridcolor = "#3D444D", tickfont = list(color = "#F0F6FC"))),
                 legend = list(orientation = "h", x = 0, y = -0.2),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                 font = list(color = "#F0F6FC"),
                 margin = list(t = 40, b = 90, l = 60, r = 60)) %>%
      config(displaylogo = FALSE)
  })
  
  # ---------- Manufacturer: Random Forest ----------
  # Hypothetical GPU prediction uses exactly these eight fields.
  # One field is selected as the target; the other seven are entered by the user.
  rf_predictors <- reactive({
    numeric_specs <- c("Clean_Core_Speed", "Clean_Boost_Clock", "Clean_Memory",
                       "Clean_Memory_Bandwidth", "Clean_Memory_Bus", "Clean_Memory_Speed",
                       "Clean_L2_Cache", "Clean_Pixel_Rate")
    req(input$rf_target, nzchar(input$rf_target))
    setdiff(numeric_specs, input$rf_target)
  })
  
  # Training is a plain function so it can run both when a target is chosen
  # and when the user presses "Retrain". It returns a model list, or a
  # character string describing why training was not possible.
  fit_model <- function(target, preds, ntree, split_pct) {
    # Only GPUs with all eight specs known are used, so the model learns from
    # real values rather than median-filled gaps (Boost Clock is missing for
    # over half of the GPUs in the CSV).
    df <- gpu_data %>%
      select(all_of(c(target, preds)), Manufacturer) %>%
      filter(if_all(all_of(c(target, preds)), ~ !is.na(.x))) %>%
      mutate(Manufacturer = factor(Manufacturer))
    if (nrow(df) < 30) return("Not enough GPUs with all eight specs known to train a model.")
    
    set.seed(42)
    idx   <- sample(seq_len(nrow(df)), size = floor(split_pct / 100 * nrow(df)))
    train <- df[idx, , drop = FALSE]
    test  <- df[-idx, , drop = FALSE]
    if (nrow(test) < 2) return("The test set is too small. Increase the training share.")
    
    # Manufacturer is retained only for chart coloring, not as a prediction input.
    f  <- reformulate(preds, response = target)
    rf <- randomForest(f, data = train, ntree = ntree, importance = TRUE)
    
    pred <- predict(rf, test)
    act  <- test[[target]]
    list(
      rf = rf, target = target, preds = preds, mfg_levels = levels(df$Manufacturer),
      rmse = sqrt(mean((act - pred)^2)),
      mae  = mean(abs(act - pred)),
      r2   = 1 - sum((act - pred)^2) / sum((act - mean(act))^2),
      oob  = tail(rf$mse, 1),
      results = data.frame(Actual = act, Predicted = pred, Manufacturer = test$Manufacturer),
      ranges = lapply(df[preds], range),
      n_train = nrow(train), n_test = nrow(test)
    )
  }
  
  model_store <- reactiveVal(NULL)
  pred_state  <- reactiveVal(NULL)
  model_fit   <- reactive(model_store())
  
  train_now <- function() {
    target <- input$rf_target
    if (is.null(target) || !nzchar(target)) return(invisible(NULL))
    preds <- setdiff(c("Clean_Core_Speed", "Clean_Boost_Clock", "Clean_Memory",
                       "Clean_Memory_Bandwidth", "Clean_Memory_Bus", "Clean_Memory_Speed",
                       "Clean_L2_Cache", "Clean_Pixel_Rate"), target)
    m <- withProgress(message = paste("Training model for", short_lab(target)), value = 0.4,
                      fit_model(target, preds, input$rf_ntree, input$rf_split))
    if (is.character(m)) {
      model_store(NULL)
      showNotification(m, type = "error")
    } else {
      model_store(m)
    }
  }
  
  # choosing (or changing) the target resets everything and trains a fresh model
  observeEvent(input$rf_target, {
    model_store(NULL)
    pred_state(NULL)
    train_now()
  }, ignoreInit = TRUE)
  observeEvent(input$train_model, { pred_state(NULL); train_now() })
  
  # keep the "which tree" slider in sync with the number of trees trained
  observeEvent(model_fit(), {
    m <- model_fit(); req(m)
    n <- m$rf$ntree
    updateSliderInput(session, "tree_k", max = n, value = 1)
  })
  
  mbox <- function(value, subtitle, icon_name, colr) {
    valueBox(value, subtitle, icon = icon(icon_name), color = colr)
  }
  output$vb_rmse <- renderValueBox({ m <- model_fit(); req(m); mbox(round(m$rmse, 2), "RMSE (test)", "ruler", "teal") })
  output$vb_mae  <- renderValueBox({ m <- model_fit(); req(m); mbox(round(m$mae, 2),  "MAE (test)",  "bullseye", "blue") })
  output$vb_r2   <- renderValueBox({ m <- model_fit(); req(m); mbox(round(m$r2, 3),   "R-squared (test)", "chart-bar", "green") })
  output$vb_oob  <- renderValueBox({ m <- model_fit(); req(m); mbox(round(m$oob, 2),  "OOB mean sq. error", "tree", "orange") })
  
  output$rf_pred_plot <- renderPlotly({
    m <- model_fit(); req(m)
    lims <- range(c(m$results$Actual, m$results$Predicted))
    p <- ggplot(m$results, aes(Actual, Predicted, colour = Manufacturer)) +
      geom_abline(slope = 1, intercept = 0, colour = "#8C94A8", linetype = "dashed") +
      geom_point(alpha = 0.6, size = 2) + scale_mfg_col() + theme_gpu() +
      coord_cartesian(xlim = lims, ylim = lims) +
      labs(x = paste("Actual", lab(m$target)), y = paste("Predicted", lab(m$target)), colour = NULL)
    pl(p, top = 20)
  })
  
  output$rf_importance_plot <- renderPlotly({
    m <- model_fit(); req(m)
    imp <- importance(m$rf, type = 1)
    d <- data.frame(Feature = rownames(imp), Importance = imp[, 1]) %>%
      mutate(Feature = ifelse(Feature %in% var_choices, lab(Feature), Feature),
             Feature = fct_reorder(Feature, Importance))
    p <- ggplot(d, aes(Feature, Importance)) +
      geom_col(fill = NV_GREEN) + coord_flip() + theme_gpu() +
      labs(x = NULL, y = "% increase in MSE when permuted")
    pl(p, top = 20, bottom = 60) %>% layout(margin = list(l = 130, t = 20, b = 60, r = 20))
  })
  
  # ---- Individual trees of the forest ----
  current_tree <- reactive({
    m <- model_fit(); req(m)
    k <- min(max(1, input$tree_k), m$rf$ntree)
    getTree(m$rf, k = k, labelVar = TRUE)
  })
  
  output$tree_info <- renderText({
    m <- model_fit(); req(m)
    tr <- current_tree()
    paste0("Tree ", min(input$tree_k, m$rf$ntree), " of ", m$rf$ntree, ": ",
           nrow(tr), " nodes, ", sum(tr$status == -1), " leaves. Target: ", lab(m$target), ".")
  })
  
  output$rf_tree_ui <- renderUI({
    req(model_fit())
    w <- max(900, 2^input$tree_depth * 75)
    div(style = "overflow-x:auto;",
        plotOutput("rf_tree_plot", width = paste0(w, "px"), height = "560px"))
  })
  
  output$rf_tree_plot <- renderPlot({
    lay <- layout_tree(current_tree(), input$tree_depth)
    nd <- lay$nodes; ed <- lay$edges
    ggplot() +
      geom_segment(data = ed, aes(x = x, y = y, xend = xend, yend = yend), colour = "#8C94A8") +
      geom_label(data = nd, aes(x = x, y = y, label = label, fill = type),
                 size = 3.3, lineheight = 0.95, label.size = 0.2,
                 label.padding = unit(0.3, "lines")) +
      scale_fill_manual(values = c("Split" = "#B6D6FF", "Leaf = prediction" = "#FFE9B8",
                                   "Further splits (hidden)" = "#D9D9D9")) +
      scale_x_continuous(expand = expansion(mult = 0.05)) +
      scale_y_continuous(expand = expansion(add = 0.5)) +
      theme_void() + theme(legend.position = "bottom", legend.title = element_blank(),
                           legend.text = element_text(colour = "#F0F6FC"),
                           plot.background = element_rect(fill = "#161B22", colour = NA))
  }, res = 96, bg = "#161B22")
  
  output$rf_tree_table <- renderDT({
    tr <- current_tree()
    tr$`split var` <- ifelse(is.na(tr$`split var`), "(leaf)",
                             short_lab(as.character(tr$`split var`)))
    tr <- cbind(Node = seq_len(nrow(tr)), tr)
    datatable(tr, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("split point", "prediction"), digits = 2)
  })
  
  # ---- What-if prediction ----
  # Flow: 1) choose target  ->  2) enter the other seven specs  ->  3) Predict
  unit_map <- c(Clean_Power = "W", Clean_Core_Speed = "MHz", Clean_Boost_Clock = "MHz",
                Clean_Memory = "MB", Clean_Memory_Bandwidth = "GB/s", Clean_Memory_Bus = "bit",
                Clean_Memory_Speed = "MHz", Clean_L2_Cache = "KB", Clean_Texture_Rate = "GTexel/s",
                Clean_Pixel_Rate = "GPixel/s", Clean_TMUs = "units", Clean_ROPs = "units",
                Release_Year = "year")
  fmt_num <- function(x) format(round(x, 1), big.mark = ",", trim = TRUE, scientific = FALSE)
  
  output$pred_panel <- renderUI({
    tg <- input$rf_target
    if (is.null(tg) || !nzchar(tg)) {
      return(div(class = "pred-empty",
                 icon("hand-pointer"),
                 div(class = "pred-empty-title", "Start by choosing the field you want to predict"),
                 div(class = "note", paste0("Use Step 1 in the \"Random Forest settings\" box. ",
                                            "The model trains automatically, then the input boxes for the other seven specs appear here."))))
    }
    preds <- rf_predictors()
    inputs <- lapply(preds, function(v) {
      vals <- gpu_data[[v]]; vals <- vals[is.finite(vals)]
      ph <- if (length(vals)) paste0("e.g. ", fmt_num(median(vals)),
                                     "  (range ", fmt_num(min(vals)), " - ", fmt_num(max(vals)), ")") else ""
      column(4, textInput(paste0("pred_", v), lab(v), value = "", placeholder = ph))
    })
    tagList(
      div(class = "pred-steps",
          div(class = "pred-step", span(class = "num", "1"), span("Predicting"), strong(lab(tg))),
          div(class = "pred-step", span(class = "num", "2"),
              span(paste0("Enter the other ", length(preds), " specs"))),
          div(class = "pred-step", span(class = "num", "3"), span("Click Predict"))),
      uiOutput("pred_status"),
      do.call(fluidRow, inputs),
      div(class = "pred-actions",
          actionButton("do_predict", paste("Predict", short_lab(tg)), icon = icon("bolt"), class = "btn-accent"),
          actionButton("clear_pred", "Clear inputs", icon = icon("eraser"))),
      uiOutput("pred_result_ui")
    )
  })
  
  output$pred_status <- renderUI({
    m <- model_fit()
    if (is.null(m)) {
      div(class = "pred-status", span(class = "dot busy"), "Model is not ready yet (training or not enough data).")
    } else {
      div(class = "pred-status", span(class = "dot ready"),
          paste0("Model ready - trained on ", format(m$n_train, big.mark = ","), " GPUs, test R-squared ",
                 round(m$r2, 2), "."))
    }
  })
  
  observeEvent(input$clear_pred, {
    req(input$rf_target, nzchar(input$rf_target))
    for (v in rf_predictors()) updateTextInput(session, paste0("pred_", v), value = "")
    pred_state(NULL)
  })
  
  observeEvent(input$do_predict, {
    m <- model_fit()
    if (is.null(m) || !identical(m$target, input$rf_target)) {
      pred_state(list(ok = FALSE, msg = "The model is not ready for this field yet. Wait a moment, or press \"Retrain with these settings\"."))
      return()
    }
    raw <- vapply(m$preds, function(v) {
      x <- input[[paste0("pred_", v)]]
      if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(gsub(",", "", trimws(x))))
    }, numeric(1))
    bad <- m$preds[!is.finite(raw) | raw < 0]
    if (length(bad) > 0) {
      pred_state(list(ok = FALSE,
                      msg = paste0("Please enter a valid non-negative number for: ",
                                   paste(lab(bad), collapse = ", "), ".")))
      return()
    }
    nd <- as.data.frame(as.list(raw)); names(nd) <- m$preds
    p  <- predict(m$rf, nd, predict.all = TRUE)
    tree_vals <- as.numeric(p$individual)
    outside <- m$preds[vapply(m$preds, function(v) raw[[v]] < m$ranges[[v]][1] || raw[[v]] > m$ranges[[v]][2], logical(1))]
    pred_state(list(ok = TRUE, target = m$target, value = as.numeric(p$aggregate)[1],
                    lo = unname(quantile(tree_vals, 0.1)), hi = unname(quantile(tree_vals, 0.9)),
                    outside = outside, r2 = m$r2, rmse = m$rmse))
  })
  
  output$pred_result_ui <- renderUI({
    r <- pred_state()
    if (is.null(r)) return(NULL)
    if (!isTRUE(r$ok)) return(div(class = "pred-error", r$msg))
    unit <- unname(unit_map[r$target])
    div(class = "pred-card",
        div(class = "pred-label", paste("Predicted", lab(r$target))),
        div(class = "pred-value", fmt_num(r$value), span(class = "pred-unit", unit)),
        div(class = "pred-range", paste0("Likely range: ", fmt_num(r$lo), " to ", fmt_num(r$hi), " ", unit,
                                         " (middle 80% of the forest's trees)")),
        if (length(r$outside) > 0)
          div(class = "pred-warn", paste0("Heads up: ", paste(lab(r$outside), collapse = ", "),
                                          " is outside the range seen in the training data, so treat this estimate with caution.")),
        div(class = "pred-meta", paste0("Typical error on held-out GPUs: about ", fmt_num(r$rmse), " ", unit,
                                        " (R-squared ", round(r$r2, 2), ")."))
    )
  })
  
  # ---------- Consumer: Find by needs (NEW) ----------
  needs_result <- eventReactive(input$need_go, {
    req(input$need_use, input$need_budget, input$need_min_mem, input$need_mfg)
    list(
      use = input$need_use,
      data = rank_gpus_perf(input$need_budget, input$need_min_mem,
                            input$need_mfg, USE_CASES[[input$need_use]]$w)
    )
  }, ignoreNULL = FALSE)
  
  output$need_summary <- renderUI({
    r <- needs_result()
    div(class = "need-banner",
        strong(paste0(r$use, ": ")), USE_CASES[[r$use]]$txt)
  })
  
  output$need_top5 <- renderUI({
    r <- needs_result()$data
    validate(need(nrow(r) > 0, "No GPUs match. Raise the budget or lower the minimum memory."))
    top_cards_ui(r)
  })
  
  output$need_count <- renderText({
    paste0(format(nrow(needs_result()$data), big.mark = ","), " GPUs match - highest performance for ",
           needs_result()$use, " first.")
  })
  
  needs_table_df <- reactive({
    needs_result()$data %>%
      select(Rank, Name, Manufacturer,
             `Memory (MB)` = Clean_Memory, `Core (MHz)` = Clean_Core_Speed,
             `Power (W)` = Clean_Power, `Price ($)` = Clean_Price,
             `Perf Index` = Perf_Index, Score)
  })
  
  output$table_needs <- renderDT({
    d <- needs_table_df()
    validate(need(nrow(d) > 0, "No GPUs match your needs. Raise the budget or lower the minimum memory."))
    datatable(d, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             lengthMenu = list(c(15, 50, 100, -1), c("15", "50", "100", "All")))) %>%
      formatCurrency("Price ($)", digits = 0) %>%
      formatStyle("Score", background = styleColorBar(c(0, 100), "#1B4FB0"),
                  backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  output$dl_needs <- downloadHandler(
    filename = function() paste0("gpu_needs_matches_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(needs_table_df(), file, row.names = FALSE)
  )
  
  # ---------- Consumer: Recommender ----------
  observeEvent(input$preset, {
    w <- switch(input$preset,
                "Balanced"          = c(25, 25, 25, 25),
                "Gaming"            = c(25, 45, 10, 20),
                "Workstation / ML"  = c(60, 20, 10, 10),
                "Budget value"      = c(15, 15, 15, 55),
                "Energy efficient"  = c(10, 15, 65, 10),
                NULL)
    if (!is.null(w)) {
      updateSliderInput(session, "w_mem",  value = w[1])
      updateSliderInput(session, "w_core", value = w[2])
      updateSliderInput(session, "w_eff",  value = w[3])
      updateSliderInput(session, "w_val",  value = w[4])
    }
  })
  
  # ALL GPUs that meet the requirements, ranked (no row cap)
  recommendations <- reactive({
    wm <- input$w_mem; wc <- input$w_core; we <- input$w_eff; wv <- input$w_val
    validate(need(wm + wc + we + wv > 0, "Set at least one weight above zero."))
    req(input$rec_budget, input$rec_min_mem, input$rec_max_power, input$rec_mfg)
    rank_gpus(input$rec_budget, input$rec_min_mem, input$rec_max_power,
              input$rec_mfg, c(wm, wc, we, wv))
  })
  
  output$vb_top <- renderValueBox({
    r <- recommendations()
    valueBox(if (nrow(r) > 0) r$Name[1] else "No match", "Top recommendation",
             icon = icon("trophy"), color = "teal")
  })
  output$vb_matches <- renderValueBox({
    valueBox(format(nrow(recommendations()), big.mark = ","), "GPUs meet your requirements",
             icon = icon("filter"), color = "blue")
  })
  output$vb_topscore <- renderValueBox({
    r <- recommendations()
    valueBox(if (nrow(r) > 0) paste0(r$Score[1], " / 100") else "n/a", "Best score",
             icon = icon("sort-amount-down"), color = "green")
  })
  
  # Top 5 cards
  output$rec_top5 <- renderUI({
    r <- recommendations()
    validate(need(nrow(r) > 0, "No GPUs match. Raise the budget or lower the memory and power limits."))
    top_cards_ui(r)
  })
  
  output$rec_bar <- renderPlotly({
    r <- recommendations()
    validate(need(nrow(r) > 0, "No GPUs match."))
    d <- r %>% head(5) %>% mutate(Name = fct_reorder(Name, Score))
    p <- ggplot(d, aes(Name, Score, fill = Manufacturer,
                       text = paste0(Name, "<br>Score: ", Score, "<br>Price: $", Clean_Price))) +
      geom_col() + coord_flip() + scale_mfg_fill() + theme_gpu() +
      labs(x = NULL, y = "Score (0-100)", fill = NULL)
    pl(p, tooltip = "text", top = 20) %>% layout(margin = list(l = 200, t = 20, b = 90, r = 20))
  })
  
  output$rec_scatter <- renderPlotly({
    r <- recommendations()
    validate(need(nrow(r) > 0, "No GPUs match your requirements."))
    r <- r %>% mutate(Tier = ifelse(Rank <= 5, "Top 5", "Other matches"))
    p <- ggplot(r, aes(Clean_Price, Score, colour = Tier,
                       text = paste0(Name, "<br>Score: ", Score, "<br>Price: $", Clean_Price))) +
      geom_point(alpha = 0.7, size = 2) +
      scale_colour_manual(values = c("Top 5" = NV_GREEN, "Other matches" = "#7A7A7A")) +
      theme_gpu() + labs(x = "Price ($)", y = "Score", colour = NULL)
    pl(p, tooltip = "text", top = 20)
  })
  
  output$rec_count <- renderText({
    paste0(format(nrow(recommendations()), big.mark = ","), " GPUs match - sorted best score first.")
  })
  
  rec_table_df <- reactive({
    recommendations() %>%
      select(Rank, Name, Manufacturer,
             `Memory (MB)` = Clean_Memory, `Core (MHz)` = Clean_Core_Speed,
             `Power (W)` = Clean_Power, `Price ($)` = Clean_Price,
             `Perf/W` = Perf_per_Watt, Score)
  })
  
  output$table_recommendations <- renderDT({
    d <- rec_table_df()
    validate(need(nrow(d) > 0, "No GPUs match your requirements. Raise the budget or lower the memory and power limits."))
    datatable(d, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             lengthMenu = list(c(15, 50, 100, -1), c("15", "50", "100", "All")))) %>%
      formatCurrency("Price ($)", digits = 0) %>%
      formatStyle("Score", background = styleColorBar(c(0, 100), "#1B4FB0"),
                  backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  output$dl_recs <- downloadHandler(
    filename = function() paste0("gpu_recommendations_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rec_table_df(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)