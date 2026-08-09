#!/bin/bash
#
# Run the SALT microsimulation on SLURM. See README.md for usage.

#SBATCH --job-name=run_simulation
#SBATCH --account=none
#SBATCH --output=logs/%x-%j.out
#SBATCH --error=logs/%x-%j.err
#SBATCH --partition=nodes
#SBATCH --time=1-00:00:00
#SBATCH --mem=8G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk
#SBATCH --mail-type=ALL

module purge
module load apps/miniforge
module load apps/R/4.4.1/gcc-8.5.0+openblas-0.3.28

source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version

cd "${SLURM_SUBMIT_DIR}"

Rscript run_microsim.R
status=$?
echo "simulation exit status: $status"

exit $status