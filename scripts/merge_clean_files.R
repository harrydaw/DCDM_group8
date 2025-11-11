#scripts/merge_clean_files

merge_clean_files <- function(in_dir="data/clean",
                              out_file="data/merged/all_clean_data.csv",
                              pattern="_clean\\.csv$") {
  # checking input directory actually exists
  if (!dir.exists(in_dir)) stop("Input directory not found: ", in_dir)
  
  # Find all cleaned CSV files and read them in
  files <- list.files(in_dir, pattern = pattern, full.names=TRUE) # full.names give full path
  
  # check if files were found
  if(length(files) == 0) {
    stop("No cleaned files found matching pattern '",pattern,"' in ", in_dir)
  }
  
  message("Found ", length(files), " cleaned file(s) to merge...")
  
  
  # create an empty list to store each file's data
  all_data <- list()
  
  for (i in seq_along(files)) {
    file_name <- basename(files[i])
    file_path <- files[i]
    message("Reading [", i, "/", length(files), "]: ", file_name) # progress bar
      
      # Read the CSV
    df <- tryCatch({
      read.csv(file_path, stringsAsFactors = FALSE, na.strings = "")
     }, error = function(e) {
      warning("Failed to read ", file_name, ": ", e$message)
      return(NULL)
    })
    
    # store successful reads
    if (!is.null(df)) all_data[[length(all_data) + 1]] <- df
    }
    
  # Remove any NULL entries (failed reads)
  all_data <- all_data[!sapply(all_data, is.null)]
    
  if (length(all_data) == 0) {
    stop("No files could be successfully read!") #output for empty directory
    }
    
  # Combine all rows into one data frame
  merged_df <- do.call(rbind, all_data)
    
  message("\nMerged ", nrow(merged_df), " rows from ", length(all_data), " file(s)")
    
  # Add warning for failed files
  n_failed <- length(files) - length(all_data)
  if (n_failed > 0) {
    message("Warning: ", n_failed, " file(s) could not be read (check warnings above)")
    }
    
  # Create output directory if needed
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    
  # Write merged file
  write.csv(merged_df, out_file, row.names = FALSE, na = "")
  message("Merged data saved to: ", out_file)
    
  # Return the merged data frame invisibly
  invisible(merged_df)
}
  