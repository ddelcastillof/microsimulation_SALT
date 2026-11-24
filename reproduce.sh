#!/bin/bash

#SBATCH --job-name=test_libs
#SBATCH --partition=cpu
#SBATCH --output=logs/%x-%j.out
#SBATCH --error=logs/%x-%j.err
#SBATCH --time=00:10:00
#SBATCH --mem=8G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk
#SBATCH --mail-type=ALL

set -euo pipefail
module purge
module load R/4.6.1
export PATH="$PATH:/mnt/home/guid/software/R/bin"
export R_LIBS_USER="/mnt/home/dd198b/.R/4.6.1"

echo "R: $(which R)  Rscript: $(which Rscript)  R_HOME: $(R RHOME)"; R --version | head -n 1
echo "shell R_LIBS_USER=${R_LIBS_USER}"

cd "${SLURM_SUBMIT_DIR}"

# 1. Library paths with site/user Renviron and Rprofile skipped
Rscript --vanilla -e 'print(.libPaths())'
# 2. Library paths as R normally starts, with no workaround applied
Rscript -e '
cat("R_LIBS_USER in R:", Sys.getenv("R_LIBS_USER"), "\n")
cat("R_LIBS / R_LIBS_SITE:", Sys.getenv("R_LIBS"), "|", Sys.getenv("R_LIBS_SITE"), "\n")
cat("Renviron.site exists:", file.exists(file.path(R.home("etc"), "Renviron.site")), "\n")
cat("Rprofile.site exists:", file.exists(file.path(R.home("etc"), "Rprofile.site")), "\n")
cat("lib dir exists:", dir.exists(Sys.getenv("R_LIBS_USER")), "\n")
print(.libPaths())
library(rlang)
'
status=$?
echo "library test exit status: $status"