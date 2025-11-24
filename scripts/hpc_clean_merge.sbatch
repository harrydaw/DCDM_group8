#!/bin/bash
#SBATCH --job-name=impc_clean
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=logs/impc_clean_%j.out
#SBATCH --error=logs/impc_clean_%j.err

set -euo pipefail

module load r/4.5.1-gcc-13.2.0-withx-rmath-standalone-python-3.11.6

REPO_DIR=/scratch/grp/msc_appbio/DCDM/Group8/DCDM_group8
IN_DIR=/scratch/grp/msc_appbio/DCDM/Group8/data
OUT_CSV_DIR=${REPO_DIR}/data/clean_hpc
OUT_LOG_DIR=${REPO_DIR}/outputs/logs_hpc
MERGED=${REPO_DIR}/data/merged/all_clean_data_hpc.csv

mkdir -p "$OUT_CSV_DIR" "$OUT_LOG_DIR" "${REPO_DIR}/logs" "$(dirname "$MERGED")"

# 1) Clean every file in IN_DIR -> one-row CSV + per-file log
Rscript -e "source('${REPO_DIR}/scripts/clean_all_files.R'); \
            clean_all_files(in_dir='${IN_DIR}', out_csv_dir='${OUT_CSV_DIR}', out_log_dir='${OUT_LOG_DIR}')"

# 2) Merge the cleaned rows into a single table
Rscript -e "source('${REPO_DIR}/scripts/merge_clean_files.R'); \
            merge_clean_files(in_dir='${OUT_CSV_DIR}', out_file='${MERGED}')"

echo "Done. Clean rows: ${OUT_CSV_DIR}  Logs: ${OUT_LOG_DIR}  Merged: ${MERGED}"
