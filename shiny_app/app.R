# =============================================================================
# Drug Discovery Dashboard — Shiny for R
# -----------------------------------------------------------------------------
# R port of dashboard2.py (Streamlit). All data is read from the parquet
# files in this app's own data/ folder, so the folder can be deployed to
# shinyapps.io as-is. To rebuild those files from the raw repo data, run
# prepare_data.R in the repo root.
#
# Packages: install.packages(c("shiny", "bslib", "nanoparquet", "plotly", "DT"))
# Run:      shiny::runApp("shiny_app")
# =============================================================================

library(shiny)
library(bslib)
library(nanoparquet)
library(plotly)
library(DT)

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# Data (read once per R process, shared by every visitor)
# -----------------------------------------------------------------------------

# Works whether R was started in shiny_app/ (runApp, shinyapps.io) or in the
# repo root (VS Code "Run Source" on this file).
data_dir <- Find(dir.exists, c("data", file.path("shiny_app", "data")))
if (is.null(data_dir)) {
  stop("Can't find the data folder. Open the drug-scoring folder in VS Code ",
       "and run run_app.R, or start R inside shiny_app/.", call. = FALSE)
}
read_tbl <- function(name) as.data.frame(read_parquet(file.path(data_dir, name)))

drugs    <- read_tbl("drugs.parquet")                 # one row per drug
links    <- read_tbl("drug_disease_targets.parquet")  # drug -> disease -> target
adverse  <- read_tbl("adverse_events.parquet")        # drug x adverse event
clinical <- read_tbl("clinical_reports.parquet")      # drug x clinical report

adverse$report_count <- suppressWarnings(as.numeric(adverse$report_count))
adverse$log_likelihood_ratio <- suppressWarnings(as.numeric(adverse$log_likelihood_ratio))

all_drugs <- sort(unique(c(drugs$drug_name, links$drug_name, adverse$drug_name, clinical$drug_name)))

# -----------------------------------------------------------------------------
# Risk score (same formula and weights as dashboard2.py)
#   0.50 * LLR / max LLR  +  0.30 * reports / max reports  +  0.20 * stage / max stage
# averaged per drug. Computed once on the full data so a drug's score doesn't
# change when you filter. "Reports" is summed over distinct adverse events;
# the raw file repeated each event once per clinical report, which inflated it.
# -----------------------------------------------------------------------------

stage_weights <- c(APPROVAL = 1, PHASE_4 = 2, PHASE_3 = 3, PHASE_2 = 4,
                   PHASE_1 = 5, EARLY_PHASE_1 = 5, UNKNOWN = 3)

compute_risk <- function(ae, cl) {
  rc  <- ifelse(is.na(ae$report_count), 0, ae$report_count)
  llr <- ifelse(is.na(ae$log_likelihood_ratio), 0, ae$log_likelihood_ratio)
  ae_part <- data.frame(
    drug_name = ae$drug_name,
    llr_norm = llr / max(llr), report_norm = rc / max(rc), rc = rc, llr = llr
  )
  per_drug <- do.call(rbind, lapply(split(ae_part, ae_part$drug_name), function(d) data.frame(
    drug_name = d$drug_name[1], llr_norm = mean(d$llr_norm), report_norm = mean(d$report_norm),
    Reports = sum(d$rc), Avg_LLR = mean(d$llr)
  )))

  w <- unname(stage_weights[as.character(cl$clinical_stage)])
  w[is.na(w)] <- 3
  stage <- tapply(w, cl$drug_name, mean) / max(w)
  per_drug$stage_norm <- unname(stage[per_drug$drug_name])
  per_drug$stage_norm[is.na(per_drug$stage_norm)] <- 3 / 5

  per_drug$Risk_Score <- 0.5 * per_drug$llr_norm + 0.3 * per_drug$report_norm + 0.2 * per_drug$stage_norm
  per_drug$Risk_Category <- cut(per_drug$Risk_Score, breaks = c(0, 0.25, 0.5, 0.75, 1),
                                labels = c("Low", "Moderate", "High", "Critical"))
  per_drug <- per_drug[order(-per_drug$Risk_Score), c("drug_name", "Risk_Score", "Risk_Category", "Reports", "Avg_LLR")]
  rownames(per_drug) <- NULL
  per_drug
}

drug_risk_all <- compute_risk(adverse, clinical)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Frequency table of a character vector, most common first
count_by <- function(x, top = Inf) {
  x <- trimws(as.character(x[!is.na(x)]))
  x <- x[nzchar(x)]
  if (!length(x)) return(data.frame(label = character(0), n = integer(0)))
  t <- sort(table(x), decreasing = TRUE)
  out <- data.frame(label = names(t), n = as.integer(t), stringsAsFactors = FALSE)
  head(out, top)
}

plain <- function(p, ...) {
  p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Source Sans 3, sans-serif", size = 13),
           margin = list(t = 30, r = 10, b = 40, l = 10), ...) |>
    config(displayModeBar = FALSE)
}

sankey <- function(from, to, value, node_color) {
  labels <- unique(c(from, to))
  plot_ly(
    type = "sankey", orientation = "h",
    node = list(label = labels, pad = 14, thickness = 16,
                color = ifelse(labels %in% from, node_color[1], node_color[2])),
    link = list(source = match(from, labels) - 1, target = match(to, labels) - 1,
                value = value, color = "rgba(120,130,150,0.25)")
  ) |> plain()
}

# Two-level sunburst / treemap: parent -> child, sized by value
hierarchy <- function(parent, child, value, type) {
  leaf_ids <- paste(parent, child, sep = " / ")
  roots <- tapply(value, parent, sum)
  plot_ly(
    type = type, branchvalues = "total",
    ids = c(names(roots), leaf_ids),
    labels = c(names(roots), child),
    parents = c(rep("", length(roots)), parent),
    values = c(unname(roots), value)
  ) |> plain(margin = list(t = 10, r = 10, b = 10, l = 10))
}

table_dt <- function(df, page = 10) {
  datatable(df, rownames = FALSE, filter = "top", class = "compact stripe hover",
            options = list(pageLength = page, scrollX = TRUE))
}

chart_card <- function(title, output, note = NULL, height = "420px") {
  card(full_screen = TRUE,
       card_header(title),
       card_body(if (!is.null(note)) p(class = "note", note), plotlyOutput(output, height = height)))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bg = "#F7F8FA", fg = "#1F2933", primary = "#2F5DA8", secondary = "#5B6675",
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "border-radius" = "0.4rem"
)

css <- "
  .note { color: #5B6675; font-size: .92rem; margin-bottom: .4rem; }
  .bslib-value-box .value-box-value { font-family: 'Source Serif 4', Georgia, serif; }
  .navbar-brand { font-family: 'Source Serif 4', Georgia, serif; font-weight: 600; }
"

ui <- page_navbar(
  title = "Drug Discovery Dashboard",
  theme = theme,
  header = tags$head(tags$style(HTML(css))),
  sidebar = sidebar(
    width = 280,
    selectizeInput("drugs", "Drugs", choices = all_drugs, multiple = TRUE,
                   options = list(placeholder = "All drugs")),
    textInput("search", "Search drug name", placeholder = "e.g. mab"),
    uiOutput("filter_note"),
    hr(),
    p(class = "note", "Filters apply to every tab. Risk scores are computed on the full data, so they stay the same when you filter.")
  ),

  nav_panel(
    "Overview",
    layout_column_wrap(
      width = 1 / 5, fill = FALSE,
      value_box("Drugs", textOutput("vb_drugs")),
      value_box("Diseases", textOutput("vb_diseases")),
      value_box("Targets", textOutput("vb_targets")),
      value_box("Clinical reports", textOutput("vb_reports")),
      value_box("Adverse events", textOutput("vb_events"))
    ),
    card(card_header("Drugs"), card_body(DTOutput("drug_table")))
  ),

  nav_panel(
    "Disease & targets",
    p(class = "note", "Link size is the number of drugs connecting a disease to a target."),
    chart_card("Disease \u2192 target", "sankey_dt", height = "460px"),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("Sunburst", "sunburst_dt"),
      chart_card("Treemap", "treemap_dt")
    ),
    chart_card("Disease \u00d7 target heatmap", "heatmap_dt"),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("Top diseases", "top_diseases"),
      chart_card("Top targets", "top_targets")
    )
  ),

  nav_panel(
    "Clinical development",
    layout_columns(
      col_widths = c(6, 6),
      chart_card("Clinical stage", "stage_funnel"),
      chart_card("Trial phase", "phase_pie")
    ),
    chart_card("Study type", "study_treemap")
  ),

  nav_panel(
    "Risk scores",
    p(class = "note", "Risk score = 0.5 \u00d7 normalised log-likelihood ratio + 0.3 \u00d7 normalised report count + 0.2 \u00d7 clinical-stage weight. A screening heuristic, not a clinical measure."),
    layout_columns(
      col_widths = c(7, 5),
      chart_card("Highest-risk drugs", "risk_bar", height = "480px"),
      chart_card("Risk category", "risk_pie", height = "480px")
    ),
    chart_card("Risk matrix", "risk_matrix"),
    card(card_header("Drug risk ranking"), card_body(DTOutput("risk_table")))
  ),

  nav_panel(
    "Adverse events",
    layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      value_box("Unique events", textOutput("ae_unique")),
      value_box("Total reports", textOutput("ae_total")),
      value_box("Average reports", textOutput("ae_avg")),
      value_box("Max LLR", textOutput("ae_maxllr"))
    ),
    chart_card("Most reported events (top 25)", "ae_bar", height = "620px"),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("Reports vs signal strength", "ae_scatter"),
      chart_card("Report count distribution", "ae_hist")
    ),
    layout_columns(
      col_widths = c(4, 8),
      chart_card("Log-likelihood ratio", "ae_box"),
      card(card_header("Event summary"), card_body(DTOutput("ae_table")))
    ),
    card(
      full_screen = TRUE,
      card_header("Drug \u2192 adverse event"),
      card_body(
        sliderInput("ae_per_drug", "Events shown per drug", min = 3, max = 20, value = 8, step = 1, width = "260px"),
        plotlyOutput("sankey_ae", height = "620px")
      )
    )
  ),

  nav_panel(
    "Data",
    layout_columns(
      col_widths = c(4, 8), fill = FALSE,
      selectInput("data_table", "Table", c(
        "Drug \u2192 disease \u2192 target links" = "links",
        "Drugs" = "drugs",
        "Adverse events" = "adverse",
        "Clinical reports" = "clinical"
      )),
      div(class = "pt-4", downloadButton("download", "Download filtered CSV"))
    ),
    card(card_body(DTOutput("data_preview")))
  ),

  nav_spacer(),
  nav_item(tags$a("Help", href = "help.html", target = "_blank", rel = "noopener"))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  selected <- reactive({
    d <- all_drugs
    if (length(input$drugs)) d <- intersect(d, input$drugs)
    s <- trimws(input$search %||% "")
    if (nzchar(s)) d <- d[grepl(tolower(s), tolower(d), fixed = TRUE)]
    d
  })

  output$filter_note <- renderUI({
    n <- length(selected())
    p(class = "note", if (n == 0) "No drugs match these filters." else sprintf("Showing %d of %d drugs.", n, length(all_drugs)))
  })

  f_drugs    <- reactive(drugs[drugs$drug_name %in% selected(), ])
  f_links    <- reactive(links[links$drug_name %in% selected(), ])
  f_adverse  <- reactive(adverse[adverse$drug_name %in% selected(), ])
  f_clinical <- reactive(clinical[clinical$drug_name %in% selected(), ])
  f_risk     <- reactive(drug_risk_all[drug_risk_all$drug_name %in% selected(), ])

  need_rows <- function(df) validate(need(nrow(df) > 0, "No data for the selected drugs."))

  # ---- Overview -------------------------------------------------------------
  fmt <- function(x) format(x, big.mark = ",")
  output$vb_drugs    <- renderText(fmt(length(selected())))
  output$vb_diseases <- renderText(fmt(length(unique(f_links()$disease_name))))
  output$vb_targets  <- renderText(fmt(length(unique(f_links()$target))))
  output$vb_reports  <- renderText(fmt(length(unique(na.omit(f_clinical()$report_id)))))
  output$vb_events   <- renderText(fmt(length(unique(f_adverse()$adverse_event_name))))

  output$drug_table <- renderDT({
    d <- f_drugs(); need_rows(d)
    tg <- tapply(links$target, links$drug_name, function(x) paste(sort(unique(x)), collapse = ", "))
    ds <- tapply(links$disease_name, links$drug_name, function(x) paste(sort(unique(x)), collapse = ", "))
    rs <- setNames(round(drug_risk_all$Risk_Score, 3), drug_risk_all$drug_name)
    out <- data.frame(
      Drug = d$drug_name, Type = d$drug_type, `Highest stage` = d$maximum_clinical_stage_summary,
      Mechanism = d$mechanism_of_action, Action = d$action_type,
      Targets = unname(tg[d$drug_name]), Diseases = unname(ds[d$drug_name]),
      `Risk score` = unname(rs[d$drug_name]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(out, rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  # ---- Disease & targets ----------------------------------------------------
  pair_counts <- reactive({
    l <- f_links(); need_rows(l)
    pc <- aggregate(drug_name ~ disease_name + target, data = l, FUN = function(x) length(unique(x)))
    names(pc)[3] <- "n"
    pc[order(-pc$n), ]
  })

  output$sankey_dt <- renderPlotly({
    pc <- head(pair_counts(), 100)
    sankey(pc$disease_name, pc$target, pc$n, c("#2F5DA8", "#3E8E7E"))
  })
  output$sunburst_dt <- renderPlotly({
    pc <- pair_counts(); hierarchy(pc$disease_name, pc$target, pc$n, "sunburst")
  })
  output$treemap_dt <- renderPlotly({
    pc <- pair_counts(); hierarchy(pc$disease_name, pc$target, pc$n, "treemap")
  })
  output$heatmap_dt <- renderPlotly({
    pc <- pair_counts()
    m <- xtabs(n ~ disease_name + target, data = pc)
    plot_ly(x = colnames(m), y = rownames(m), z = unclass(m), type = "heatmap",
            colorscale = "Viridis", hovertemplate = "%{y} \u2192 %{x}: %{z}<extra></extra>") |>
      plain(xaxis = list(title = ""), yaxis = list(title = "", automargin = TRUE))
  })
  output$top_diseases <- renderPlotly({
    l <- f_links(); need_rows(l)
    t <- count_by(unique(l[, c("disease_name", "target")])$disease_name, top = 20)
    plot_ly(t, labels = ~label, values = ~n, type = "pie", textinfo = "label+value") |>
      plain(showlegend = FALSE)
  })
  output$top_targets <- renderPlotly({
    l <- f_links(); need_rows(l)
    t <- count_by(unique(l[, c("disease_name", "target")])$target, top = 20)
    plot_ly(t, labels = ~label, parents = "", values = ~n, type = "treemap") |>
      plain(margin = list(t = 10, r = 10, b = 10, l = 10))
  })

  # ---- Clinical development ------------------------------------------------
  output$stage_funnel <- renderPlotly({
    t <- count_by(f_clinical()$clinical_stage); need_rows(t)
    plot_ly(t, x = ~n, y = ~factor(label, levels = label), type = "funnel",
            marker = list(color = "#2F5DA8")) |>
      plain(yaxis = list(title = "", automargin = TRUE))
  })
  output$phase_pie <- renderPlotly({
    t <- count_by(f_clinical()$trial_phase); need_rows(t)
    plot_ly(t, labels = ~label, values = ~n, type = "pie", hole = 0.45) |> plain()
  })
  output$study_treemap <- renderPlotly({
    t <- count_by(f_clinical()$trial_study_type, top = 20); need_rows(t)
    plot_ly(t, labels = ~label, parents = "", values = ~n, type = "treemap") |>
      plain(margin = list(t = 10, r = 10, b = 10, l = 10))
  })

  # ---- Risk scores ----------------------------------------------------------
  output$risk_bar <- renderPlotly({
    r <- head(f_risk(), 20); need_rows(r)
    r$drug_name <- factor(r$drug_name, levels = rev(r$drug_name))
    plot_ly(r, x = ~Risk_Score, y = ~drug_name, type = "bar", orientation = "h",
            marker = list(color = ~Risk_Score, colorscale = "Reds", showscale = TRUE,
                          cmin = 0, cmax = 1),
            hovertemplate = "%{y}: %{x:.3f}<extra></extra>") |>
      plain(xaxis = list(title = "Risk score", range = c(0, 1)), yaxis = list(title = ""))
  })
  output$risk_matrix <- renderPlotly({
    r <- f_risk(); need_rows(r)
    plot_ly(r, x = ~Reports, y = ~Avg_LLR, type = "scatter", mode = "markers+text",
            text = ~drug_name, textposition = "top center",
            marker = list(size = ~10 + Risk_Score * 40, color = ~Risk_Score,
                          colorscale = "Reds", showscale = TRUE, cmin = 0, cmax = 1,
                          line = list(color = "#1F2933", width = 0.5)),
            hovertemplate = "%{text}<br>Reports: %{x:,}<br>Avg LLR: %{y:.1f}<extra></extra>") |>
      plain(xaxis = list(title = "Total reports"), yaxis = list(title = "Average log-likelihood ratio"))
  })
  output$risk_pie <- renderPlotly({
    r <- f_risk(); need_rows(r)
    t <- as.data.frame(table(r$Risk_Category))
    t <- t[t$Freq > 0, ]
    cols <- c(Low = "#C9D7EA", Moderate = "#F2C38B", High = "#E07A5F", Critical = "#A4161A")
    plot_ly(t, labels = ~Var1, values = ~Freq, type = "pie", sort = FALSE,
            marker = list(colors = unname(cols[as.character(t$Var1)]))) |> plain()
  })
  output$risk_table <- renderDT({
    r <- f_risk(); need_rows(r)
    r$Risk_Score <- round(r$Risk_Score, 3); r$Avg_LLR <- round(r$Avg_LLR, 1)
    names(r) <- c("Drug", "Risk score", "Category", "Reports", "Average LLR")
    table_dt(r, page = 20)
  })

  # ---- Adverse events -------------------------------------------------------
  ae_summary <- reactive({
    a <- f_adverse()
    if (!nrow(a)) return(data.frame(adverse_event_name = character(0), report_count = numeric(0),
                                    log_likelihood_ratio = numeric(0), drugs = integer(0)))
    s <- do.call(rbind, lapply(split(a, a$adverse_event_name), function(d) data.frame(
      adverse_event_name = d$adverse_event_name[1],
      report_count = sum(d$report_count, na.rm = TRUE),
      log_likelihood_ratio = mean(d$log_likelihood_ratio, na.rm = TRUE),
      drugs = length(unique(d$drug_name))
    )))
    s[order(-s$report_count), ]
  })

  ae_stat <- function(f) renderText({
    s <- ae_summary()
    if (!nrow(s)) "\u2014" else fmt(f(s))
  })
  output$ae_unique <- ae_stat(nrow)
  output$ae_total  <- ae_stat(function(s) round(sum(s$report_count)))
  output$ae_avg    <- ae_stat(function(s) round(mean(s$report_count), 1))
  output$ae_maxllr <- ae_stat(function(s) round(max(s$log_likelihood_ratio, na.rm = TRUE), 1))

  output$ae_bar <- renderPlotly({
    s <- head(ae_summary(), 25); need_rows(s)
    s$adverse_event_name <- factor(s$adverse_event_name, levels = rev(s$adverse_event_name))
    plot_ly(s, x = ~report_count, y = ~adverse_event_name, type = "bar", orientation = "h",
            marker = list(color = ~report_count, colorscale = "Reds", showscale = FALSE),
            hovertemplate = "%{y}: %{x:,} reports<extra></extra>") |>
      plain(xaxis = list(title = "Reports"), yaxis = list(title = "", automargin = TRUE))
  })
  output$ae_scatter <- renderPlotly({
    s <- head(ae_summary(), 25); need_rows(s)
    plot_ly(s, x = ~report_count, y = ~log_likelihood_ratio, type = "scatter", mode = "markers",
            text = ~adverse_event_name,
            marker = list(size = ~8 + 30 * report_count / max(report_count),
                          color = ~log_likelihood_ratio, colorscale = "Viridis", showscale = TRUE),
            hovertemplate = "%{text}<br>Reports: %{x:,}<br>LLR: %{y:.1f}<extra></extra>") |>
      plain(xaxis = list(title = "Reports"), yaxis = list(title = "Log-likelihood ratio"))
  })
  output$ae_hist <- renderPlotly({
    s <- ae_summary(); need_rows(s)
    plot_ly(s, x = ~report_count, type = "histogram", nbinsx = 30,
            marker = list(color = "#2F5DA8")) |>
      plain(xaxis = list(title = "Reports per event"), yaxis = list(title = "Events"))
  })
  output$ae_box <- renderPlotly({
    s <- ae_summary(); need_rows(s)
    plot_ly(s, y = ~log_likelihood_ratio, type = "box", boxpoints = "outliers",
            text = ~adverse_event_name, marker = list(color = "#A4161A"), name = "") |>
      plain(yaxis = list(title = "Log-likelihood ratio"))
  })
  output$ae_table <- renderDT({
    s <- ae_summary(); need_rows(s)
    s$log_likelihood_ratio <- round(s$log_likelihood_ratio, 1)
    names(s) <- c("Adverse event", "Reports", "Average LLR", "Drugs")
    table_dt(s)
  })
  output$sankey_ae <- renderPlotly({
    a <- f_adverse(); need_rows(a)
    a <- a[order(a$drug_name, -a$report_count), ]
    a <- do.call(rbind, lapply(split(a, a$drug_name), head, input$ae_per_drug))
    sankey(a$drug_name, a$adverse_event_name, a$report_count, c("#2F5DA8", "#C0504D"))
  })

  # ---- Data -----------------------------------------------------------------
  current_table <- reactive({
    switch(input$data_table,
           links = f_links(), drugs = f_drugs(),
           adverse = f_adverse(), clinical = f_clinical())
  })
  output$data_preview <- renderDT(table_dt(current_table()))
  output$download <- downloadHandler(
    filename = function() paste0(input$data_table, "_filtered.csv"),
    content = function(file) write.csv(current_table(), file, row.names = FALSE, na = "")
  )
}

shinyApp(ui, server)
