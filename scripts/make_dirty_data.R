# scripts/make_dirty_data.R

# Generating 10 deliberately messay IMPC-style CSVs for cleaning tests
# Each file will contan key-value pairs and random errors

dir.create("data/raw_dirty",recursive = TRUE, showWarnings = FALSE)

# reusibility
set.seed(09102003)


for (i in 1:10) {
  # Step 1: Create a normal "valid"-looking record
  kv = c(
    gene_accession_id = sprintf("MGI:%d", sample(2000000:9999999, 1)),
    gene_symbol       = sample(c("Mgst2", "mgst2", "MGST-2"), 1),
    mouse_strain      = sample(c("C57BL", "C57bl", "C57 BL"), 1),
    mouse_life_stage  = sample(c("Early adult", "earlyadult"), 1),
    parameter_id      = sample(c("IMPC_HEM_001_001", "impc_HEM_001_001"), 1),
    pvalue            = as.character(round(runif(1), 6)),
    parameter_name    = sample(c("White blood cell count", "White cell count"), 1),
    analysis_id       = sprintf("fake_%02d", i)
  )
  
  # Step 2: Randomly choose two error types to inject into each file
  issues = sample(
    c("none","missing_key","duplicate_key","unexpected_key","bad_pvalue","spaces","na_text"),
    size = 2, replace = FALSE
  )
  
  # Step 3: Applying the chosen errors
  # Drop a random key entirely (simulating missing data)
  if ("missing_key" %in% issues) kv <- kv[-sample(seq_along(kv), 1)]
  
  # Duplicate one key with a slightly different value (inconsistent formatting)
  if ("duplicate_key" %in% issues) {
    dup <- sample(names(kv), 1)
    kv <- c(kv, setNames(paste0(kv[[dup]], "_dup"), dup))
  }
  
  # Add an unexpected key/value pair (something we don’t expect in real data)
  if ("unexpected_key" %in% issues) kv <- c(kv, junk = sample(c("foo","bar","42"), 1))
  
  # Replace pvalue with something invalid (text or out-of-range number)
  if ("bad_pvalue" %in% issues && "pvalue" %in% names(kv))
    kv["pvalue"] <- sample(c("not_a_number", "-0.1", "1.1"), 1)
  
  # Add extra spaces around one random field (common formatting mess)
  if ("spaces" %in% issues)
    kv[sample(names(kv), 1)] <- paste0("  ", kv[sample(names(kv), 1)], "  ")
  
  # Replace a random value with literal "NA" or "na" (can be misinterpreted)
  if ("na_text" %in% issues)
    kv[sample(names(kv), 1)] <- sample(c("NA","na"), 1)
  
  # Step 4: Shuffle the order of rows (real files may not be consistent)
  kv <- kv[sample(seq_along(kv))]
  
  # Step 5: Write the messy record to a CSV
  out <- file.path("data/raw_dirty", sprintf("fake_%02d.csv", i))
  writeLines(sprintf("%s,%s", names(kv), kv), out)
  
}

message("Created 10 messy CSVS in data/raw_dirty")