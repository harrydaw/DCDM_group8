#scripts/merge_clean_files

merge_clean_files <- function(in_dir="data/clean",#
                              out_file="data/merged/all_clean_data.csv",
                              pattern="_clean\\.csv$") {
  # checnking input directory actually exists
  if (!dir.exists(in_dir)) stop("Input directory not found: ", in_dir)
  
  # Find all cleaned CSV files and read them in
  files <- list.files(in_dir, pattern = pattern, full.names=TRUE) # full.names give full path
  
  if(length(files) == 0) {
    stop("No cleaned files found matching pattern '",pattern,"' in ", in_dir)
  }
  
  message("Found ", length(files), " cleaned file(s) to merge...")
  
  
  # create an empty list to store each file's data
  all_data <- list()
  
  for (i in seq_along(files)) {
    file_name <- basename(files[i])
    message("Reading [", i, "/" length(files), "]: ", file_name)
    
    
  }
  
}