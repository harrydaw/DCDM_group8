#!/bin/bash
#SBATCH --job-name=impc_metadata_clean
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=logs/impc_metadata_clean_%j.out
#SBATCH --error=logs/impc_metadata_clean_%j.err

set -euo pipefail

module load r/4.5.1-gcc-13.2.0-withx-rmath-standalone-python-3.11.6

REPO_DIR=/scratch/grp/msc_appbio/DCDM/Group8/DCDM_group8

# 1) Clean parameter descriptions and procedures
Rscript -e "source('${REPO_DIR}/scripts/clean_parameter_procedure.R'); \
            clean_parameter_descriptions(in_path='${REPO_DIR}/data/IMPC_parameter_description.txt', \
                                        out_path='${REPO_DIR}/data/clean/IMPC_parameter_description_clean.csv', \
                                        log_path='${REPO_DIR}/outputs/logs/IMPC_parameter_description_clean_log.csv'); \
            clean_procedures(in_path='${REPO_DIR}/data/IMPC_procedure.txt', \
                             out_path='${REPO_DIR}/data/clean/IMPC_procedure_clean.csv', \
                             log_path='${REPO_DIR}/outputs/logs/IMPC_procedure_clean_log.csv')"

echo "Metadata cleaning done."
