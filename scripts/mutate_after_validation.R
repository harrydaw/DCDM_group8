# scripts/mutate_after_validation.R
# Post-validation mutation:
# - reports cleanliness BEFORE
# - drops rows with bad mouse_strain (categorical violations)
# - optional: drops rows with missing mouse_life_stage
# - recomputes cleanliness AFTER
# - writes mutated data + quarantine of removed rows

source("scripts/validate_merged_data.R")  # reuse read_sop, validate_*, generate_summary, etc.

mutate_after_validation = function(
    standardised_path   = "data/merged/all_clean_data__hpc_standardised.csv",
    sop_path            = "data/IMPC_SOP.csv",
    types_path          = "outputs/validation_types_ranges.csv",
    cats_path           = "outputs/validation_categoricals.csv",
    out_mutated         = "data/merged/all_clean_data_mutated.csv",
    out_quarantine      = "outputs/mutation_quarantine.csv",
    drop_missing_lifestage = FALSE
) {
  if (!file.exists(standardised_path)) stop("Standardised data not found: ", standardised_path)
  if (!file.exists(sop_path))         stop("SOP not found: ", sop_path)
  if (!file.exists(types_path))       stop("Type/range report not found: ", types_path)
  
  message("\n=== POST-VALIDATION MUTATION STEP ===")
  
  # Load data + SOP + previous reports
  df   = read.csv(standardised_path, stringsAsFactors = FALSE, check.names = FALSE)
  sop  = read_sop(sop_path)
  type_range_report = read.csv(types_path, stringsAsFactors = FALSE, check.names = FALSE)
  cat_report = if (file.exists(cats_path)) {
    read.csv(cats_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    NULL
  }
  
  message("Loaded standardised data: ", nrow(df), " rows, ", ncol(df), " columns")
  
  # Cleanliness BEFORE mutation
  summary_before = generate_summary(df, type_range_report, cat_report, sop)
  pct_clean_before = summary_before$value[summary_before$metric == "percent_rows_clean"]
  rows_issues_before = summary_before$value[summary_before$metric == "rows_with_violations"]
  
  message("\n--- BEFORE MUTATION ---")
  message(sprintf("Rows clean: %.2f%%", pct_clean_before))
  message(sprintf("Rows with issues: %d / %d",
                  rows_issues_before, nrow(df)))
  
  # ---------------------------
  # Decide which rows to drop
  # ---------------------------
  rows_to_drop = integer(0)
  reasons = list()
  
  #Drop rows with bad mouse_strain (not in allowed set)
  if (!is.null(cat_report)) {
    bad_ms = cat_report[
      cat_report$field == "mouse_strain" & cat_report$issue == "not_in_allowed_set",
      ,
      drop = FALSE
    ]
    if (nrow(bad_ms) > 0) {
      rows_to_drop = union(rows_to_drop, bad_ms$row_index)
      reasons[["bad_mouse_strain"]] = bad_ms$row_index
      message("Flagging ", nrow(bad_ms), " row(s) with invalid mouse_strain for removal.")
    } else {
      message("No invalid mouse_strain rows in categorical report.")
    }
  } else {
    message("No categorical report found; cannot identify bad mouse_strain rows from logs.")
  }
  
  #Optionally drop rows with missing mouse_life_stage
  if (drop_missing_lifestage && "mouse_life_stage" %in% names(df)) {
    miss_ls = which(is.na(df$mouse_life_stage) | df$mouse_life_stage == "")
    if (length(miss_ls)) {
      rows_to_drop = union(rows_to_drop, miss_ls)
      reasons[["missing_mouse_life_stage"]] = miss_ls
      message("Flagging ", length(miss_ls),
              " row(s) with missing mouse_life_stage for removal.")
    } else {
      message("No rows with missing mouse_life_stage.")
    }
  }
  
  rows_to_drop = sort(unique(rows_to_drop))
  message("\nTotal rows flagged for removal: ", length(rows_to_drop))
  
  # =======================================
  # Build quarantine + mutated data
  # =======================================
  quarantine_df = NULL
  if (length(rows_to_drop) > 0) {
    quarantine_df = df[rows_to_drop, , drop = FALSE]
    
    # add a simple issue tag per row (can be multiple)
    issue_tag = character(length(rows_to_drop))
    names(reasons) = names(reasons)
    for (nm in names(reasons)) {
      issue_tag[rows_to_drop %in% reasons[[nm]]] =
        ifelse(issue_tag[rows_to_drop %in% reasons[[nm]]] == "",
               nm,
               paste(issue_tag[rows_to_drop %in% reasons[[nm]]], nm, sep = ";"))
    }
    
    quarantine_df = cbind(
      mutation_issue = issue_tag,
      row_index_original = rows_to_drop,
      quarantine_df
    )
    
    df_mut = df[-rows_to_drop, , drop = FALSE]
  } else {
    df_mut = df
  }
  
  message("Rows remaining after mutation: ", nrow(df_mut))
  # =========================================
  # Recalculate cleanliness AFTER mutation
  # ========================================
  # Re-run validations on mutated data
  message("\nRecomputing validation on mutated data...")
  type_range_after = validate_types_ranges(df_mut, sop)
  allowed_sets     = parse_allowed_sets(sop)
  cat_after        = validate_allowed_sets(df_mut, allowed_sets)
  
  summary_after = generate_summary(df_mut, type_range_after, cat_after, sop)
  pct_clean_after = summary_after$value[summary_after$metric == "percent_rows_clean"]
  rows_issues_after = summary_after$value[summary_after$metric == "rows_with_violations"]
  
  message("\n--- AFTER MUTATION ---")
  message(sprintf("Rows clean: %.2f%%", pct_clean_after))
  message(sprintf("Rows with issues: %d / %d",
                  rows_issues_after, nrow(df_mut)))
  
  # Write outputs
  dir.create(dirname(out_mutated), recursive = TRUE, showWarnings = FALSE)
  write.csv(df_mut, out_mutated, row.names = FALSE, na = "")
  message("\nMutated data written to: ", out_mutated)
  
  if (!is.null(quarantine_df)) {
    dir.create(dirname(out_quarantine), recursive = TRUE, showWarnings = FALSE)
    write.csv(quarantine_df, out_quarantine, row.names = FALSE, na = "")
    message("Quarantine file written to: ", out_quarantine,
            " (", nrow(quarantine_df), " rows)")
  } else {
    message("No rows removed; no quarantine file written.")
  }
  
  invisible(list(
    before = summary_before,
    after  = summary_after
  ))
}