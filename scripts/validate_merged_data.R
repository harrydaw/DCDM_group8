# scripts/validate_merged_data.R
# Validate a merged, tidy dataset against IMPC_SOP.csv (default) and output a report
  # NO MUTATION
# - Type & range/length checks for every field
# - Allowed-set checks parsed from SOP "Values are ..." remarks (flexible)
# Outputs two reports in outputs/: validation_types_ranges.csv, validation_categoricals.csv

# Helper functions
read_sop = function(sop_path){
  if (!file.exists(sop_path)) stop("No SOP file was found in: ", sop_path) # checks file exists
  sop = read.csv(sop_path, stringsAsFactors = FALSE, check.names = FALSE)
  needed = c("dataField","dataType","minValue","maxValue","exampleValues","remarks")
  miss = setdiff(needed, names(sop)) # compares needed columns to what's actually there
  if (length(miss)) stop("SOP is missing the following columns: ", paste(miss, collapse = ", ")) # tells you what's missing (if applicable)
  sop # prints out sop if successful
}
# No check for "allowedValues" yet because some keys don't have it 

# Here comes the "allowedValues" parsing
parse_allowed_sets = function(sop) {
  allowed = vector("list", nrow(sop)) # creates an empty list to store alloed values if they are found
  names(allowed) = sop$dataField
  
  # Check if there's a dedicated allowed Values column
  has_allowed_col = "allowedValues" %in% names(sop)
  
  # start of for loop to check through each row in SOP
  for (i in seq_len(nrow(sop))) {
    # First checks if ideal "allowedValues" column exists
    if (has_allowed_col) { 
      av = sop$allowedValues[i]
      if (!is.na(av) && nzchar(trimws(av))) { # "if NOT NA or empty" trim white space before and after
        vals = trimws(unlist(strsplit(av, ";", fixed = TRUE))) # saves all values separated by semi-colons
        vals = vals[nzchar(vals)] # saves any non-zero elements
        if (length(vals)) { #as long as AV isn't empty, save the list of allowed values
          allowed[[i]] = vals
          next  # Found it in dedicated column, move to next field
      # E.g.
         #allowedValues = "C57BL; B6J; BALB"
         #allowed[["mouse_strain"]] = c("C57BL", "B6J", "BALB")
        }
      }
    }
    
    # More realistic (and for our data) allowedValues doesn't exist, so checks for specific patterns in "remarks column"
    rm = sop$remarks[i]
    if (is.na(rm) || !nzchar(rm)) next # skip if NA or zero-length
    
    # List of some realistic syntaxes for remarks
    # (?i) means case-insenitive search
    patterns = c(
      "(?i).*values\\s+are\\s*:?\\s*(.*)$",        # "Values are:" or "values are"
      "(?i).*allowed\\s+values\\s*:?\\s*(.*)$",    # "Allowed values:"
      "(?i).*valid\\s+values\\s*:?\\s*(.*)$",      # "Valid values:"
      "(?i).*permitted\\s+values\\s*:?\\s*(.*)$"   # "Permitted values:"
    )
    
   # checks through each row to see if any of the patterns exist at the end of the remark
     for (pattern in patterns) {
      m = regexpr(pattern, rm, perl = TRUE)
      if (m[1] >= 0) {
        tail = sub(pattern, "\\1", rm, perl = TRUE)
        vals = trimws(unlist(strsplit(tail, "[;,]"))) # trims whitespace again and saves values whether they are split by a , or a ;
        vals = vals[nzchar(vals)] # saves all non-zero elements
        if (length(vals)) { # Saves output as long as some values were found
          allowed[[i]] = vals
          break
        }
      }
    }
  }
  # Drop empty entries (if applicable)
  allowed = allowed[!vapply(allowed, is.null, logical(1))] # creates a vector of Trues and Falses based on whether or not the entry is null (to be able to remove the ones that are)
                                                          # logical(1) 
  # Report what we found
  if (length(allowed) > 0) {
    message("Found allowed value sets for ", length(allowed), " field(s):")
    for (fld in names(allowed)) {
      message("  - ", fld, ": ", length(allowed[[fld]]), " allowed values") # lists how many allowed values were found for each field
    }
  } else {
    message("No allowed value sets found in SOP")
  }
  
  allowed
}
# Finally the end of that function!
# Result: we have a list of any allowed values for each key:
  # allowed[["mouse_strain"]] = c("C57BL/6", "B6J", "BALB/c")
  # For each key that has allowed values listed

# Removes ALL white spaces and makes lower case for comparisons
norm_value = function(x) tolower(gsub("\\s+", "", x))

# Saves field, data type and min/max from the sop for each key
# Actual validation functions
# - Validating types and ranges
# - Validating allowed sets extracted in parse_allowed_values()
validate_types_ranges = function(df, sop) {
  out = list()
  for (i in seq_len(nrow(sop))) {
    fld   = sop$dataField[i]
    dtype = tolower(sop$dataType[i]) # handling different cases
    minv  = sop$minValue[i]
    maxv  = sop$maxValue[i]
    
    # Checks if the field already exists and skips if so
    if (!fld %in% names(df)) { 
      out[[length(out) + 1]] = data.frame(
        field = fld, check = "missing_column_in_data",
        metric = "count", value = 1, stringsAsFactors = FALSE
      )
      next
    }
    
    col = df[[fld]]

    # comparing number values to min max
    if (dtype == "float") {
      num = suppressWarnings(as.numeric(col))
      non_numeric = sum(is.na(num) & !is.na(col)) # set non_numeric true if num successfully became NA and the original value wasn't already NA
      out_of_range = sum(!is.na(num) & (num < minv | num > maxv)) # If num was successfully converted to a number but it's out of range
      out[[length(out) + 1]] = data.frame(field=fld, check="numeric", metric="non_numeric", value=non_numeric, stringsAsFactors=FALSE) # saves +1 non numeric value
      out[[length(out) + 1]] = data.frame(field=fld, check="numeric", metric="out_of_range", value=out_of_range, stringsAsFactors=FALSE) # saves +1 out=of-range value
    }
    # comparing string length values to min max
    else if (dtype == "string") {
    col_chr = ifelse(is.na(col), "", as.character(col))
    L = nchar(col_chr, type = "chars", allowNA = FALSE, keepNA = FALSE) # saves njumber of characters in this value
    too_short = sum(L < minv & col_chr != "") # too short & NOT NA
    too_long  = sum(L > maxv) # too long
    blanks    = sum(col_chr == "") # number of NAs
    out[[length(out) + 1]] = data.frame(field=fld, check="string_length", metric="too_short", value=too_short, stringsAsFactors=FALSE)
    out[[length(out) + 1]] = data.frame(field=fld, check="string_length", metric="too_long",  value=too_long,  stringsAsFactors=FALSE) # save each type of validation failure
    out[[length(out) + 1]] = data.frame(field=fld, check="string_length", metric="blank_or_na", value=blanks, stringsAsFactors=FALSE)
  }
    else {
      out[[length(out) + 1]] = data.frame(field=fld, check="unknown_dtype", metric=dtype, value=NA_integer_, stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, out) # Bind all of our output data frames into one big one
}

# Other validation function:
validate_allowed_sets = function(df, allowed_sets) {
  if (!length(allowed_sets)) return(NULL) # If nothing in allowed sets, return nothing
  rows = list() # set-up
  for (fld in names(allowed_sets)) {
    if (!fld %in% names(df)) next # If value is in our "allowed values", move on
    x = df[[fld]]
    okset = norm_value(allowed_sets[[fld]]) # Save values that passed validation
    bad = !is.na(x) & !(norm_value(x) %in% okset) # If not NA and not in allowed set, save as "bad" value
    if (any(bad)) {
      rows[[length(rows) + 1]] = data.frame(
        field = fld,
        row_index = which(bad),
        value = x[bad],
        issue = "not_in_allowed_set",
        stringsAsFactors = FALSE      # Save all info about the bad value
      )
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)  # Save all of the bad values as one big data frame
}

# Extra function for indexing rows with type/range issues
rows_with_type_range_issues = function(df, sop) {
  index = integer(0)
  for (i in seq_len(nrow(sop))) {
    fld   = sop$dataField[i]
    dtype = tolower(sop$dataType[i])
    minv  = sop$minValue[i]
    maxv  = sop$maxValue[i]
    if (!fld %in% names(df)) next
    
    col = df[[fld]]
    
    if (dtype == "float") {
      num = suppressWarnings(as.numeric(col))
      bad = (is.na(num) & !is.na(col)) | (!is.na(num) & (num < minv | num > maxv))
      index = union(index, which(bad))
    } else if (dtype == "string") {
      col_chr = ifelse(is.na(col), "", as.character(col))
      L = nchar(col_chr, type = "chars", allowNA = FALSE, keepNA = FALSE)
      bad = (L < minv & col_chr != "") | (L > maxv) | (col_chr == "")
      index = union(index, which(bad))
    } else {
      next
    }
  }
  index
}


# Summary function of previous steps
generate_summary = function(df, type_range_report, cat_report, sop) { 
  total_rows = nrow(df)
  total_fields = ncol(df)
 
  # Count fields with any categorical issues
  fields_with_type_issues = length(unique(type_range_report$field[type_range_report$value > 0]))
  
  fields_with_cat_issues = 0
  if (!is.null(cat_report)) {
    fields_with_cat_issues = length(unique(cat_report$field)) # saves all unique issues
  }
  
  # Count total violations
  total_type_violations = sum(type_range_report$value, na.rm = TRUE)
  total_cat_violations = if (!is.null(cat_report)) nrow(cat_report) else 0
  
  # Count rows with ANY violation
  problematic_rows = integer(0)
  
  #rows failing categorical checks
  if (!is.null(cat_report)) {
    problematic_rows <- unique(cat_report$row_index)
  }
  
  #rows failing type/range/length checks
  problematic_rows <- unique(c(
    problematic_rows,
    rows_with_type_range_issues(df, sop)
  ))
  
  rows_with_issues <- length(problematic_rows)
  
  # Calculate percentages
  pct_rows_clean = round(100 * (total_rows - rows_with_issues) / total_rows, 2) # round percentage of clean rows to two digits
  pct_fields_clean = round(100 * (total_fields - fields_with_type_issues - fields_with_cat_issues) / total_fields, 2) # " for fields
  
  # Check for missing expected fields
  expected_fields = sop$dataField
  missing_from_data = setdiff(expected_fields, names(df)) # compares expected vs. observed and saves missing ones
  unexpected_in_data = setdiff(names(df), expected_fields) # compares expected and observed and saves unexpected extras
  
  # Create summary data frame
  summary_df = data.frame(
    metric = c(
      "total_rows",
      "total_fields",
      "rows_with_violations",
      "rows_clean",
      "percent_rows_clean",
      "fields_with_type_violations",
      "fields_with_categorical_violations",
      "fields_clean",
      "percent_fields_clean",
      "total_type_range_violations",
      "total_categorical_violations",
      "missing_expected_fields",
      "unexpected_extra_fields"
    ),
    value = c(
      total_rows,
      total_fields,
      rows_with_issues,
      total_rows - rows_with_issues,
      pct_rows_clean,
      fields_with_type_issues,
      fields_with_cat_issues,
      total_fields - fields_with_type_issues - fields_with_cat_issues,
      pct_fields_clean,
      total_type_violations,
      total_cat_violations,
      length(missing_from_data),
      length(unexpected_in_data)
    ),
    details = c(
      "Number of data rows",
      "Number of data columns",
      "Rows containing at least one violation",
      "Rows with no violations",
      "Percentage of clean rows",
      "Fields with type/range/length issues",
      "Fields with categorical value issues",
      "Fields with no violations",
      "Percentage of clean fields",
      "Total type/range/length violations",
      "Total categorical violations",
      if (length(missing_from_data)) paste(missing_from_data, collapse="; ") else "None", # missing/unexpected fields existed, paste the mwith ; separation
      if (length(unexpected_in_data)) paste(unexpected_in_data, collapse="; ") else "None"
    ),
    stringsAsFactors = FALSE
  )
  
  summary_df
# Example:
 # metric,value,details
 # total_rows,47,Number of data rows
 # total_fields,9,Number of data columns
 # rows_with_violations,3,Rows containing at least one violation
 # rows_clean,44,Rows with no violations
 # percent_rows_clean,93.62,Percentage of clean rows
 # fields_with_type_violations,2,Fields with type/range/length issues
 # fields_with_categorical_violations,1,Fields with categorical value issues
 # fields_clean,6,Fields with no violations
 # percent_fields_clean,66.67,Percentage of clean fields
 # total_type_range_violations,8,Total type/range/length violations
 # total_categorical_violations,5,Total categorical violations
 # missing_expected_fields,1,mouse_strain
 # unexpected_extra_fields,0,None
}

# Function for standardising values with allowed_sets to the exact matched value
standardise_categoricals = function(df, allowed_sets) {
  if (!length(allowed_sets)) return(df)
  
  corrections_made = 0
  corrections_log = list() # setup to track changes
  
  for (fld in names(allowed_sets)) {
    if (!fld %in% names(df)) next # if the field is not in our row, move on
    
    x = df[[fld]]
    allowed = allowed_sets[[fld]] # the specific allowed set for our current field
    
    #Create lookup: normalised version -> proper version
    lookup = setNames(allowed, norm_value(allowed)) 
    
    #For each value in our data, find its standardised match
    for (i in seq_along(x)) {
      if (is.na(x[i])) next  # Skip NA values
      
      normalised = norm_value(x[i])
      
      # If we find a match, replace with the proper standardised value
      if (normalised %in% names(lookup)) {
        if (x[i] != lookup[[normalised]]) {  # Only if it's actually different to save resources
          message(sprintf("  Standardising '%s' -> '%s' in field '%s', row %d", 
                          x[i], lookup[[normalised]], fld, i))
          
          # Logging any corrections made
          corrections_log[[length(corrections_log)+1]] = data.frame(
            field = fld,
            row_index = i,
            original_value = x[i],
            standardised_value = lookup[[normalised]],
            stringsAsFactors = FALSE
          )
        
           df[[fld]][i] = lookup[[normalised]]
          corrections_made = corrections_made + 1 # log the change
        }
      }
    }
  }
  
  if (corrections_made > 0) {
    message(sprintf("\nTotal corrections made: %d", corrections_made))
  } else {
    message("\nNo standardisation corrections needed")
  }
  
  list(df = df, log = corrections_log)
}

#Generate normalisation summary report
generate_normalisation_summary = function(corrections_log) {
  if (length(corrections_log) == 0) {
    return(data.frame(
      field = character(0),
      row_index = integer(0),
      original_value = character(0),
      standardised_value = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, corrections_log)
}

# ==================================
# FINALLY the main function
# ==================================

validate_cleaned = function(
    cleaned_path = "data/merged/all_clean_data.csv",     # Sensible defaults
    sop_path     = "data/IMPC_SOP.csv",
    out_types    = "outputs/validation_types_ranges.csv",
    out_cats     = "outputs/validation_categoricals.csv",
    out_summary  = "outputs/validation_summary.csv" 
   ) {
  
  if (!file.exists(cleaned_path)) stop("Merged cleaned data not found: ", cleaned_path)
  
  message("\n======================================")
  message("VALIDATION REPORT")
  message("======================================\n")
  
  message("Reading data...")
  df  = read.csv(cleaned_path, stringsAsFactors = FALSE, check.names = FALSE)
  message("  Loaded ", nrow(df), " rows, ", ncol(df), " columns") # Loading in our data and summarising size
  
  message("\nReading SOP...") # Reading the SOP
  sop = read_sop(sop_path)
  message("  SOP defines ", nrow(sop), " expected fields") # Prints no. expected fields
  
  dir.create("outputs", showWarnings = FALSE)
  
  # Type/range/length summary
  message("\nValidating data types and ranges...")
  type_range_report = validate_types_ranges(df, sop)
  write.csv(type_range_report, out_types, row.names = FALSE, na = "")
  message("-- Successfully wrote: ", out_types) # Type error report csv
  
  # Allowed sets (parsed from allowedValues or Remarks column)
  message("\nParsing allowed value sets from SOP...")
  allowed_sets = parse_allowed_sets(sop)
  
  # Print allowed value sets for each key
  if (length(allowed_sets)) {
    message("Allowed values found:")
    for (fld in names(allowed_sets)) {
      vals <- paste(head(allowed_sets[[fld]], 5), collapse = ", ")
      if (length(allowed_sets[[fld]]) > 5) vals <- paste0(vals, ", ...")
      message("  - ", fld, ": ", vals)
    }
  } else {
    message("No allowed values found.")
  }
 
  message("\nStandardising categorical values...")
  standardisation_result = standardise_categoricals(df, allowed_sets)
  df = standardisation_result$df
  
  #Save normalisation summary
  norm_summary = generate_normalisation_summary(standardisation_result$log)
  if (nrow(norm_summary) > 0) {
    norm_summary_path = "outputs/normalisation_summary.csv"
    write.csv(norm_summary, norm_summary_path, row.names = FALSE, na = "")
    message("-- Successfully wrote: ", norm_summary_path)
  }
  
  message("\nValidating categorical values...")
  cat_report = validate_allowed_sets(df, allowed_sets)
  if (!is.null(cat_report)) {
    write.csv(cat_report, out_cats, row.names = FALSE, na = "")
    message("-- Successfully wrote: ", out_cats, " (", nrow(cat_report), " violations)")
  } else {
    message("-- No categorical violations found")
  }
  
  #Save the corrected data
  message("\nSaving standardised data...")
  corrected_path = sub("\\.csv$", "_standardised.csv", cleaned_path)
  write.csv(df, corrected_path, row.names = FALSE, na = "")
  message("-- Successfully wrote: ", corrected_path)
  
  # We have now:
  # - Parsed allowed values
  # - Validated categorical values
  # - Reported on how many violations were found

  # Generate and write our summary report
  message("\nGenerating summary report...")
  summary_report = generate_summary(df, type_range_report, cat_report, sop)
  write.csv(summary_report, out_summary, row.names = FALSE, na = "")
  message("-- Successfully Wrote: ", out_summary)
  
  # Print key findings to console for immediate readability
  message("\n======================================")
  message("KEY FINDINGS!")
  message("======================================")
  
  # Grabbing specific numbers from the summary report by searching though $metric
  pct_clean = summary_report$value[summary_report$metric == "percent_rows_clean"]
  rows_issues = summary_report$value[summary_report$metric == "rows_with_violations"]
  total_violations = summary_report$value[summary_report$metric == "total_type_range_violations"] +
    summary_report$value[summary_report$metric == "total_categorical_violations"]
  
  # Concise print outs
  message(sprintf("Data Quality: %.1f%% of rows are clean", pct_clean)) # sprint f allows for text formatting
  message(sprintf("Total Violations: %d", total_violations))            # %.1f%% = 1 decimal place
  message(sprintf("Rows with Issues: %d / %d", rows_issues, nrow(df)))  # %d = integer  rows with issues(int) / number of rows(int)
  
  if (total_violations > 0) {
    message("\n-- Review the detailed reports in outputs/ for specific violations")
  } else {
    message("\n-- All data passed validation! (yippee)")
  }
  
  message("======================================\n")
}










