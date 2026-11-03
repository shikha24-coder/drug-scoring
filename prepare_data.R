# =============================================================================
# prepare_data.R  —  run once, before deploying
# -----------------------------------------------------------------------------
# Reads the two raw parquet files in the repo root and writes small, tidy
# parquet tables into shiny_app/data/. The Shiny app reads ONLY those files,
# so the deployed app is self-contained and light.
#
# Why this step matters: the raw files are row-wise cross joins.
#   merged_drug_data.parquet         992,147 rows -> 9 drugs, 20 drug-disease-target links
#   adverse_clinical_combined.parquet 55,314 rows -> 355 adverse-event rows
#                                                    x every clinical report of the drug
# Loading them as-is costs ~2 GB of RAM and inflates any sum or count.
#
# Usage (from the repo root):
#   install.packages("nanoparquet")
#   Rscript prepare_data.R
# =============================================================================

library(nanoparquet)

in_drug     <- "merged_drug_data.parquet"
in_clinical <- "adverse_clinical_combined.parquet"
out_dir     <- file.path("shiny_app", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

collapse_unique <- function(x) {
  x <- unique(trimws(as.character(x[!is.na(x)])))
  x <- x[nzchar(x)]
  if (length(x) == 0) NA_character_ else paste(sort(x), collapse = "; ")
}

distinct_cols <- function(df, cols) {
  cols <- intersect(cols, names(df))
  out <- unique(as.data.frame(df[, cols, drop = FALSE]))
  rownames(out) <- NULL
  out
}

# ---- 1. Drug / disease / target data ---------------------------------------
cat("Reading", in_drug, "...\n")
raw <- read_parquet(in_drug, col_select = c(
  "drug_name", "chembl_id", "preferred_name", "drug_type",
  "maximum_clinical_stage_summary", "description", "synonym_count",
  "trade_name_count", "opentargets_url", "mechanism_of_action", "action_type",
  "target_symbol", "class_label", "class_level", "toxicity_class", "warning_type",
  "pgx_category", "evidence_level", "target_symbol_pgx", "phenotype_text",
  "disease_id", "disease_name", "therapeutic_area_names",
  "maximum_clinical_stage", "approved_symbol", "target_ensembl_id"
))
raw <- as.data.frame(raw)
cat("  ", format(nrow(raw), big.mark = ","), "raw rows\n")

# One row per drug-disease-target link
links <- distinct_cols(raw, c(
  "drug_name", "disease_id", "disease_name", "therapeutic_area_names",
  "maximum_clinical_stage", "approved_symbol", "target_ensembl_id"
))
names(links)[names(links) == "approved_symbol"] <- "target"
links <- links[!is.na(links$disease_name) & !is.na(links$target), ]

# One row per drug (multi-valued fields joined with "; ")
info_cols <- c(
  "chembl_id", "preferred_name", "drug_type", "maximum_clinical_stage_summary",
  "description", "synonym_count", "trade_name_count", "opentargets_url",
  "mechanism_of_action", "action_type", "target_symbol", "class_label",
  "class_level", "toxicity_class", "warning_type", "pgx_category",
  "evidence_level", "target_symbol_pgx", "phenotype_text"
)
drug_slim <- distinct_cols(raw, c("drug_name", info_cols))
drugs <- do.call(rbind, lapply(split(drug_slim, drug_slim$drug_name), function(d) {
  row <- data.frame(drug_name = d$drug_name[1], stringsAsFactors = FALSE)
  for (col in info_cols) {
    row[[col]] <- if (is.numeric(d[[col]])) max(d[[col]], na.rm = TRUE) else collapse_unique(d[[col]])
  }
  row
}))
rownames(drugs) <- NULL
rm(raw, drug_slim); invisible(gc())

# ---- 2. Adverse events + clinical reports ----------------------------------
cat("Reading", in_clinical, "...\n")
cl <- as.data.frame(read_parquet(in_clinical))
cat("  ", format(nrow(cl), big.mark = ","), "raw rows\n")

adverse <- distinct_cols(cl, c(
  "query_name_ae", "chembl_id", "adverse_event_name", "report_count",
  "log_likelihood_ratio", "critical_value", "total_significant_adverse_events"
))
names(adverse)[names(adverse) == "query_name_ae"] <- "drug_name"

# The file repeats each drug's clinical reports once per adverse event, so
# keeping the rows of one adverse event per drug recovers the report list
# exactly (including any duplicates the source itself has).
first_ae <- tapply(cl$adverse_event_name, cl$query_name_ae, function(x) x[1])
keep <- !is.na(cl$query_name_ae) &
  cl$adverse_event_name == first_ae[cl$query_name_ae]
keep[is.na(keep)] <- FALSE   # 16 rows have no drug name at all; drop them
clinical_cols <- c(
  "query_name_ae", "disease_id", "disease_name", "report_id", "report_type",
  "source", "title", "clinical_stage", "phase_from_source", "url",
  "has_expert_review", "trial_official_title", "trial_overall_status",
  "trial_phase", "trial_study_type", "trial_primary_purpose", "trial_start_date",
  "trial_why_stopped", "trial_stop_reason_categories", "trial_number_of_arms"
)
clinical <- cl[keep, intersect(clinical_cols, names(cl)), drop = FALSE]
names(clinical)[names(clinical) == "query_name_ae"] <- "drug_name"
rownames(clinical) <- NULL

# ---- 3. Write ---------------------------------------------------------------
write_parquet(drugs,    file.path(out_dir, "drugs.parquet"))
write_parquet(links,    file.path(out_dir, "drug_disease_targets.parquet"))
write_parquet(adverse,  file.path(out_dir, "adverse_events.parquet"))
write_parquet(clinical, file.path(out_dir, "clinical_reports.parquet"))

cat("\nWrote to", out_dir, ":\n")
for (f in list.files(out_dir, full.names = TRUE)) {
  cat(sprintf("  %-30s %6d rows  %6.1f KB\n", basename(f),
              nrow(read_parquet(f)), file.size(f) / 1024))
}
