# scripts/clean_all_files.R
# Batch processor for cleaning multiple files

#sourcing the core cleaning function

source("scripts/clean_one_file.R")

# Clean all files in a folder
clean_all_files <- function(in_dir = "dat/raw",
                            out_csv_dir = "data/clean",
                            out_log_dir = "outputs/logs",
                            pattern = "\\.csv$",
                            expected_keys = c(
                              "gene_accession_id","gene_symbol","mouse_strain","mouse_life_stage",
                              "parameter_id","pvalue","parameter_name","analysis_id"
                            )) {
  # Check input directory exists
  if (!dir.exists(in_dir)) stop("Input directory not found: ", in_dir)
  
  # Find all files matching the pattern
  files <- list.files(in_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    message("No files found matching pattern '", pattern, "' in ", in_dir)
    return(invisible(NULL))
  }
  
  message("Found ", length(files), " file(s) to process...")
  
  # Create output directories (if necessary)
  dir.create(out_csv_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_log_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Process each file
  results <- list()
  for (i in seq_along(files)) {
    file_path <- files[i]
    file_name <- basename(file_path)
    
    message("Processing [", i, "/", length(files), "]: ", file_name)
    
    # Build output paths
    base <- tools::file_path_sans_ext(file_name)
    out_csv <- file.path(out_csv_dir, paste0(base, "_clean.csv"))
    out_log <- file.path(out_log_dir, paste0(base, "_log.csv"))
    
    # Try to clean the file, catch any errors
    result <- tryCatch({
      clean_one_file(file_path, out_csv = out_csv, out_log = out_log, 
                     expected_keys = expected_keys)
      list(file = file_name, status = "success", error = NA)
    }, error = function(e) {
      list(file = file_name, status = "failed", error = as.character(e))
    })
    
    results[[i]] <- result
  }
  
  # Create summary
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  n_success <- sum(results_df$status == "success")
  n_failed <- sum(results_df$status == "failed")
  
  message("\n--- Summary ---")
  message("Successfully processed: ", n_success, " file(s)")
  message("Failed: ", n_failed, " file(s)")
  
  if (n_failed > 0) {
    message("\nFailed files:")
    failed <- results_df[results_df$status == "failed", ]
    for (i in 1:nrow(failed)) {
      message("  - ", failed$file[i], ": ", failed$error[i])
    }
  }
  
  # Save processing summary to CSV
  summary_file <- file.path(out_log_dir, paste0("_SUMMARY_", 
                                                format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                                ".csv"))
  write.csv(results_df, summary_file, row.names = FALSE, na = "")
  message("\nSummary saved to: ", summary_file)
  
  invisible(results_df)
}  

                          

