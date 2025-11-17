# scripts/clean_parameter_procedure.R
# Cleaning utilities for IMPC metadata tables:
# - IMPC_parameter_description
# - IMPC_procedure
#
# Design:
# - Row-preserving tabular cleaning (NOT key/value like clean_one_file)
# - Trim whitespace, normalise IDs, optional title-casing names
# - Drop rows with missing IDs
# - Collapse duplicates by ID (keep first), log counts
# - Output clean CSVs + brief logs

#  Shared helpers 
#==============================
# just capitalised the first letter of the string
cap_first = function(x) { 
  ifelse(
    is.na(x) | x == "",
    x,
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  )
}

# trims whitespace from character values
trim_char_cols = function(df) {
  is_char = vapply(df, is.character, logical(1))
  df[is_char] = lapply(df[is_char], trimws)
  df
}

# new function to write logs
.write_log = function(log_df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(log_df, path, row.names = FALSE, na = "")
}

# Start of real fucntions
# ===========================================
clean_parameter_descriptions = function(
    in_path    = "data/IMPC_parameter_description.txt",
    out_path   = "data/clean/IMPC_parameter_description_clean.csv",
    log_path   = "outputs/logs/IMPC_parameter_description_clean_log.csv",
    sep        = "\t",
    titlecase_names = TRUE # optional in case you want to maintain formatting
) {
  if (!file.exists(in_path)) stop("Parameter description file not found: ", in_path)
  
  message("\nCleaning parameter descriptions:")
  message("Reading: ", in_path)
  
  raw = tryCatch(
    {
      read.table(
        in_path,
        header = TRUE,
        sep = sep,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        quote = "",
        comment.char = ""
      )
    },
    error = function(e) stop("Failed to read parameter description file: ", e$message)
  )
  
  if (!nrow(raw)) stop("Parameter description file has zero rows: ", in_path)
  
  # Basic trimming on all character columns
  df = trim_char_cols(raw)
  
  # Check for key columns
  key_col  = "parameter_id"
  name_col = "parameter_name"
  proc_col = "procedure_id"
  
  missing_key_cols = setdiff(key_col, names(df))
  if (length(missing_key_cols)) {
    stop("Parameter description file is missing required column(s): ",
         paste(missing_key_cols, collapse = ", "))
  }
  
  # Canonicalise IDs
  df[[key_col]]  = toupper(df[[key_col]])
  if (proc_col %in% names(df)) {
    df[[proc_col]] = toupper(df[[proc_col]])
  }
  
  # Optional: nicer naming for readability
  if (titlecase_names && name_col %in% names(df)) {
    df[[name_col]] = cap_first(tolower(df[[name_col]]))
  }
  
  # Stats before dropping
  n_in          = nrow(df)
  n_missing_key = sum(is.na(df[[key_col]]) | df[[key_col]] == "")
  
  # Drop rows missing the primary key
  if (n_missing_key > 0) {
    message("Dropping ", n_missing_key, " rows with missing ", key_col)
    df = df[!(is.na(df[[key_col]]) | df[[key_col]] == ""), , drop = FALSE]
  }
  
  # Safety check - ensure rows remain after dropping
  if (nrow(df) == 0) {
    stop("No valid rows remaining after dropping missing ", key_col)
  }
  
  # Collapse duplicates on parameter_id, keeping first
  dup_idx = duplicated(df[[key_col]])
  n_dup   = sum(dup_idx)
  
  if (n_dup > 0) {
    dup_pct = round(100 * n_dup / n_in, 1)
    message("Collapsing ", n_dup, " duplicate ", key_col, " entries (", dup_pct, "% of input)")
    
    # Warning if high duplicate rate
    if (dup_pct > 10) {
      warning("High duplicate rate (", dup_pct, "%) - verify input data quality")
    }
    
    # NEW: Log which IDs were duplicated
    dup_ids = df[[key_col]][dup_idx]
    dup_counts = table(dup_ids)
    dup_log_path = sub("\\.csv$", "_duplicates.csv", log_path)
    dup_log_df = data.frame(
      id = names(dup_counts),
      duplicate_count = as.integer(dup_counts),
      stringsAsFactors = FALSE
    )
    .write_log(dup_log_df, dup_log_path)
    message("Duplicate details written to: ", dup_log_path)
    
    df = df[!dup_idx, , drop = FALSE]
  }
  
  n_out = nrow(df)
  
  # Write cleaned table
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, out_path, row.names = FALSE, na = "")
  message("Clean parameter descriptions written to: ", out_path)
  
  # Log summary
  log_df = data.frame(
    metric = c(
      "rows_input",
      "rows_missing_parameter_id_dropped",
      "duplicate_parameter_id_collapsed",
      "rows_output"
    ),
    value = c(
      n_in,
      n_missing_key,
      n_dup,
      n_out
    ),
    stringsAsFactors = FALSE
  )
  .write_log(log_df, log_path)
  message("Log written to: ", log_path)
  
  # Summary message
  message("\n=== Summary ===")
  message(sprintf("Input:  %d rows", n_in))
  message(sprintf("Output: %d rows (%.1f%% retained)", n_out, 100 * n_out / n_in))
  message("================\n")
  
  invisible(df)
}


clean_procedures = function(
    in_path   = "data/IMPC_procedure.txt",
    out_path  = "data/clean/IMPC_procedure_clean.csv",
    log_path  = "outputs/logs/IMPC_procedure_clean_log.csv",
    sep       = "\t",
    titlecase_names = TRUE  # option agian if you want to maintain exact formatting
) {
  if (!file.exists(in_path)) stop("Procedure file not found: ", in_path)
  
  message("\n=== Cleaning procedures ===")
  message("Reading: ", in_path)
  
  raw = tryCatch(
    {
      read.table(
        in_path,
        header = TRUE,
        sep = sep,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        quote = "",
        comment.char = ""
      )
    },
    error = function(e) stop("Failed to read procedure file: ", e$message)
  )
  
  if (!nrow(raw)) stop("Procedure file has zero rows: ", in_path)
  
  df = trim_char_cols(raw)
  
  # Key columns (adjust if your actual names differ)
  key_col  = "procedure_id"
  name_col = "procedure_name"
  
  missing_key_cols = setdiff(key_col, names(df))
  if (length(missing_key_cols)) {
    stop("Procedure file is missing required column(s): ",
         paste(missing_key_cols, collapse = ", "))
  }
  
  # Canonicalise IDs
  df[[key_col]] = toupper(df[[key_col]])
  
  # Optional: nicer naming
  if (titlecase_names && name_col %in% names(df)) {
    df[[name_col]] = cap_first(tolower(df[[name_col]]))
  }
  
  # Stats
  n_in          = nrow(df)
  n_missing_key = sum(is.na(df[[key_col]]) | df[[key_col]] == "")
  
  # Drop rows missing key
  if (n_missing_key > 0) {
    message("Dropping ", n_missing_key, " rows with missing ", key_col)
    df = df[!(is.na(df[[key_col]]) | df[[key_col]] == ""), , drop = FALSE]
  }
  
  # Safety check
  if (nrow(df) == 0) {
    stop("No valid rows remaining after dropping missing ", key_col)
  }
  
  # Collapse duplicates
  dup_idx = duplicated(df[[key_col]])
  n_dup   = sum(dup_idx)
  
  if (n_dup > 0) {
    dup_pct = round(100 * n_dup / n_in, 1)
    message("Collapsing ", n_dup, " duplicate ", key_col, " entries (", dup_pct, "% of input)")
    
    # Warn if high duplicate rate
    if (dup_pct > 10) {
      warning("High duplicate rate (", dup_pct, "%) - verify input data quality")
    }
    
    #Log which IDs were duplicated
    dup_ids = df[[key_col]][dup_idx]
    dup_counts = table(dup_ids)
    dup_log_path = sub("\\.csv$", "_duplicates.csv", log_path)
    dup_log_df = data.frame(
      id = names(dup_counts),
      duplicate_count = as.integer(dup_counts),
      stringsAsFactors = FALSE
    )
    .write_log(dup_log_df, dup_log_path)
    message("Duplicate details written to: ", dup_log_path)
    
    df = df[!dup_idx, , drop = FALSE]
  }
  
  n_out = nrow(df)
  
  # Write cleaned table
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, out_path, row.names = FALSE, na = "")
  message("Clean procedures written to: ", out_path)
  
  # Log summary
  log_df = data.frame(
    metric = c(
      "rows_input",
      "rows_missing_procedure_id_dropped",
      "duplicate_procedure_id_collapsed",
      "rows_output"
    ),
    value = c(
      n_in,
      n_missing_key,
      n_dup,
      n_out
    ),
    stringsAsFactors = FALSE
  )
  .write_log(log_df, log_path)
  message("Log written to: ", log_path)
  
  #Summary message
  message("\n=== Summary ===")
  message(sprintf("Input:  %d rows", n_in))
  message(sprintf("Output: %d rows (%.1f%% retained)", n_out, 100 * n_out / n_in))
  message("================\n")
}
```