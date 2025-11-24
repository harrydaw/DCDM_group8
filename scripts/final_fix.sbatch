#!/bin/bash
#SBATCH --job-name=impc_final
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=logs/impc_final_%j.out
#SBATCH --error=logs/impc_final_%j.err

set -euo pipefail

module load r/4.5.1-gcc-13.2.0-withx-rmath-standalone-python-3.11.6

REPO_DIR=/scratch/grp/msc_appbio/DCDM/Group8/DCDM_group8

Rscript -e "source('${REPO_DIR}/scripts/final_corrupt_row_removal.R')"


