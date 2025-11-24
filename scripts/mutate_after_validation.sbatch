#!/bin/bash
#SBATCH --job-name=impc_mutate
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --output=logs/impc_mutate_%j.out
#SBATCH --error=logs/impc_mutate_%j.err

set -euo pipefail

module load r/4.5.1-gcc-13.2.0-withx-rmath-standalone-python-3.11.6

REPO_DIR=/scratch/grp/msc_appbio/DCDM/Group8/DCDM_group8

# MUTATION STEP
Rscript -e "source('${REPO_DIR}/scripts/mutate_after_validation.R'); \
            mutate_after_validation( \
              drop_missing_lifestage=FALSE \
            )"

echo 'Mutation step completed.'
