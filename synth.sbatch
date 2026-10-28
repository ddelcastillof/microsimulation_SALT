#!/bin/bash

#SBATCH --job-name=synth_endes
#SBATCH --account=none
#SBATCH --partition=cpu
#SBATCH --output=logs/%x-%j.out
#SBATCH --error=logs/%x-%j.err
#SBATCH --time=1-00:00:00
#SBATCH --mem=64G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk
#SBATCH --mail-type=ALL

module purge
module load R/4.6.1
export PATH="$PATH:/mnt/home/guid/software/R/bin"
export R_LIBS_USER="/mnt/home/dd198b/.R/4.6.1/"

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "R: $(which R) (R_HOME $(R RHOME))"; R --version | head -n 1
Rscript -e '.libPaths()'

cd "${SLURM_SUBMIT_DIR}"

Rscript -e 'source("R/synth-generation.R")
generate_synth(arrow::read_parquet(here::here("data", "endes-clean", "adults-clean.parquet")),
               n = 24690000)'
status=$?
echo "synthesis exit status: $status"

exit $status
