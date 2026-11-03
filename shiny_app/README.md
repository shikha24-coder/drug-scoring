# Drug Discovery Dashboard (Shiny for R)

Everything the app needs is in this folder. Full help, including a reference for every function, is in `www/help.html` (also the Help link in the app). It reads only the parquet files in `data/`:

| File | Rows | Contents |
|---|---|---|
| `drugs.parquet` | 9 | One row per drug: type, stage, mechanism, warnings, PGx |
| `drug_disease_targets.parquet` | 20 | Drug → disease → target links |
| `adverse_events.parquet` | 355 | Adverse events per drug (reports, LLR) |
| `clinical_reports.parquet` | 2,112 | Clinical reports and trials per drug |

## Run locally

```r
install.packages(c("shiny", "bslib", "nanoparquet", "plotly", "DT", "rsconnect"))
shiny::runApp("shiny_app")
```

## Deploy to shinyapps.io

```r
rsconnect::deployApp("shiny_app", appName = "drug-scoring")
```

Deploy the `shiny_app` folder only; the `data/` folder is uploaded with it.

## Updating the data

Replace `merged_drug_data.parquet` and `adverse_clinical_combined.parquet` in the
repo root, run `Rscript prepare_data.R`, then redeploy.
