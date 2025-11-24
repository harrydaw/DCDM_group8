#!/bin/bash
#SBATCH --job-name=impc_validate
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=logs/impc_validate_%j.out
#SBATCH --error=logs/impc_validate_%j.err

set -euo pipefail

module load r/4.5.1-gcc-13.2.0-withx-rmath-standalone-python-3.11.6

REPO_DIR=/scratch/grp/msc_appbio/DCDM/Group8/DCDM_group8
MERGED=${REPO_DIR}/data/merged/all_clean_data_hpc.csv

# 3) Validate the clean single table
Rscript -e "source('${REPO_DIR}/scripts/validate_merged_data.R'); \
            validate_cleaned(cleaned_path='${MERGED}')"

echo "Done."
