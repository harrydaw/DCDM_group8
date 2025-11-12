# script/clean_one_file.R
# Minimal cleaner for ONE file at a time
# Steps:
# - Parse key/value lines
# - Normalise keys + values
# - Keep first duplicate key (log it)
# - Add missing expected keys as NA
# - Write a 1-row cleaned CSV and a small log CSV

# Creating some helper functions!

# normalise a key: trim, lowercase, non-alnum -> underscore, trim edge underscores
# global substitution (gsub)
norm_key <- function(x) {
  x <- trimws(tolower(x)) #trimes "white space" from the start or end of the value and makes it all lowercase
  x <- gsub("[^a-z0-9]+", "_", x) # repleces any non alpha-num characters with _
  x <- gsub("^_+|_+$", "", x) # removes underscores from the start or end 
  x
}

# normalise a value: trim, turn literal "na"/"NA" into NA
norm_value <- function(x) {
  if (is.na(x)) return(NA_character_) # if already NA, leave as is
  x <- trimws(x) 
  if (tolower(x) == "na") return(NA_character_) # if someone has types NA in any format, convert to real NA
  x
}

# validate pvalue: must be numeric in [0,1]; otherwise set NA and log
validate_pvalue <- function(rec, issues) {
  if (is.null(rec$pvalue)) return(list(rec = rec, issues = issues)) # checks if there is no pvalue and amends issues
  raw <- rec$pvalue
  num <- suppressWarnings(as.numeric(raw))
  bad_num <- is.na(num) & !is.na(raw)
  oor <- !is.na(num) & (num < 0 | num > 1) # checks if pvalue [0,1]
  if (isTRUE(bad_num) || isTRUE(oor)) {
    issues <- c(issues, sprintf("pvalue invalid (%s) -> set NA", raw)) # saves bad pvalue issues
    rec$pvalue <- NA_real_
  } else {
    rec$pvalue <- num
  }
  list(rec = rec, issues = issues)
}
# Simple title case function
cap_first = function(x) {
  ifelse(
    is.na(x) | x == "",
    x,
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  )
}


# ===============================
# Start of the big function!
# ================================
clean_one_file <- function(in_path, # path to file, REQUIRED
                           out_csv = NULL, # optional output location
                           out_log = NULL, # optional log output location
                           expected_keys = c( 
                             "gene_accession_id","gene_symbol","mouse_strain","mouse_life_stage",
                             "parameter_id","pvalue","parameter_name","analysis_id"
                           )) {
  if (!file.exists(in_path)) stop("Input not found: ", in_path) # Checks path is valid
  
  lines <- readLines(in_path, warn = FALSE)
  if (!length(lines)) stop("File is empty: ", in_path) # stops if file is empty
  
  # initialising tracking variables
  issues <- character(0) # no. problems found
  seen <- character(0) # tracks which keys have already been encountered
  rec <- list() # places to store key-value pairs
  
  # Parse each line like "key,value" or "key<TAB>value"
  # Turn this into a list of its parts
  for (ln in lines) {
    parts <- strsplit(ln, "[,\t]", perl = TRUE)[[1]]
    if (length(parts) < 2) next # skips lines that don't have at least 2 parts (a key and a value)
    key <- norm_key(parts[1]) # normalises the first "part" (key)
    val <- norm_value(paste(parts[-1], collapse = " ")) # Combines all other parts (value(s)) with spaces and normalises 
    if (key %in% seen) {
      issues <- c(issues, sprintf("duplicate key ignored: %s", key)) # tracks and then ignores any duplicate keys
      next
    }
    seen <- c(seen, key) # adds the key to the seen list (to compare to)
    rec[[key]] <- val # store the key-value pair
  }
  
  # Log unexpected keys (we won’t include them in the clean output)
  unexpected <- setdiff(names(rec), expected_keys) #setdiff(A, B) finds elements in A that are not in B
  if (length(unexpected)) {
    issues <- c(issues, sprintf("unexpected keys ignored: %s", paste(unexpected, collapse = ", "))) # logs unexpected keys
  }
  
  # Ensure all expected keys exist (fill missing with NA)
  missing <- setdiff(expected_keys, names(rec))
  if (length(missing)) {
    issues <- c(issues, sprintf("missing keys set NA: %s", paste(missing, collapse = ", ")))
    for (k in missing) rec[[k]] <- NA_character_ # adds an NA to the position of the missing vector
  }
  
  # P value validation
  result <- validate_pvalue(rec,issues)
  rec <- result$rec # saving outputs 
  issues <- result$issues
  
  # Build a 1-row data.frame of lists in fixed column order
  row <- as.data.frame(as.list(rec[expected_keys]), stringsAsFactors = FALSE) # creates dataFrame in exact order of expected_keys
  
  # ============= Canonicalisation (hard coded) ==============================
  if ("mouse_strain" %in% names(row)) {
    # collapse internal spaces only; keep slashes (e.g. C57BL)
    row$mouse_strain = gsub("\\s+", "", row$mouse_strain)
  }
  
  if ("gene_accession_id" %in% names(row)) {
    # uppercase letters, keep digits/colons (e.g. MGI:12345)
    row$gene_accession_id = toupper(row$gene_accession_id)
  }
  
  if ("gene_symbol" %in% names(row)) {
    # title casing gene symbol (e.g. Satb2)
    row$gene_symbol = cap_first(tolower(row$gene_symbol))
  }
  
  if ("mouse_life_stage" %in% names(row)) {
    # title casing mouse life stage (e.g. Early adult)
    row$mouse_life_stage = cap_first(tolower(row$mouse_life_stage))
  }
  
  if ("parameter_id" %in% names(row)) {
    # upper casijng param ID (e.g. IMPC_GRS_009_001)
    row$parameter_id = toupper(row$parameter_id)
  }
  
  if ("parameter_name" %in% names(row)) {
    # title casing parameter name (e.g. Forelimb and hindlimb grip strength measurement mean)
    row$parameter_name = cap_first(tolower(row$parameter_name))
  }
  
  # ===================================================================
  
  # Default output paths
  base <- tools::file_path_sans_ext(basename(in_path)) # takes filename without path or extension
  if (is.null(out_csv)) out_csv <- file.path("data/clean", paste0(base, "_clean.csv")) # creates relevant files with _clean addition
  if (is.null(out_log)) out_log <- file.path("outputs/logs", paste0(base, "_log.csv")) # saves logs (if applicable)
  
  # create directories if they don't already exist
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_log), recursive = TRUE, showWarnings = FALSE)
  
  # Write out our clean 1 line input
  write.csv(row, out_csv, row.names = FALSE, na = "") 
  
  # Saving any issues if they exist, per file
  if (!length(issues)) issues <- "OK"
  log_df <- data.frame(file = basename(in_path), issue = issues, stringsAsFactors = FALSE)
  write.csv(log_df, out_log, row.names = FALSE, na = "")
  
  invisible(list(out_csv = out_csv, out_log = out_log)) # returns the value but doesn't automatically print
}