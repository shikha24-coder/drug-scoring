# =============================================================================
# run_app.R — start the Drug Discovery Dashboard
# -----------------------------------------------------------------------------
# In VS Code: open the drug-scoring folder, then press Ctrl+Shift+B
#             (or open this file and click "Run Source" at the top right).
# In a terminal:  Rscript run_app.R
#
# Installs any missing packages the first time, then opens the app in your
# browser. Stop it with Ctrl+C in the terminal.
# =============================================================================

needed <- c("shiny", "bslib", "nanoparquet", "plotly", "DT")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Locate shiny_app/ relative to this script, whatever the working directory is
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())

app_dir <- Find(function(d) file.exists(file.path(d, "app.R")),
                c(file.path(script_dir, "shiny_app"), file.path(getwd(), "shiny_app"), getwd()))
if (is.null(app_dir)) stop("Can't find shiny_app/app.R. Open the drug-scoring folder in VS Code.")

port <- as.integer(Sys.getenv("PORT", "3838"))
message("Starting the dashboard at http://127.0.0.1:", port, "  (press Ctrl+C to stop)")
shiny::runApp(app_dir, port = port, launch.browser = interactive() || Sys.getenv("OPEN_BROWSER", "1") == "1")
