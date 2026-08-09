#!/usr/bin/env bash
#
# Run the SALT microsimulation locally. See README.md for usage.

# Everything below lives in one brace group that exits before its closing brace.
# Bash otherwise reads a script incrementally, resuming at a saved byte offset
# after each command; saving this file in place during the ~25 min model run
# shifts that offset and the remainder gets parsed mid-line ("syntax error near
# unexpected token"). The brace group forces the whole file to be parsed up front.
{
set -euo pipefail

# --- resolve repo root from this script's location, not the caller's cwd ---
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

usage() {
  cat <<'EOF'
Usage: run_local.sh [options]

Options:
  -n, --psa N        PSA draws (0 disables).            [default: 1000]
  -s, --seed N       RNG seed.                          [default: 42]
  -o, --no-owsa      Skip the one-way sensitivity pass.
  -q, --quick        Smoke run: --psa 0 --no-owsa.
  -r, --render       Render salt_results.qmd after a successful model run.
      --threads N    BLAS/OpenMP threads.               [default: 1]
  -h, --help         Show this message.

Flags override the corresponding SALT_* environment variables.
See README.md for what the model does and how the SLURM path differs.
EOF
}

# --- defaults: mirror run_microsim.R, but let a pre-set env var win ---
psa_n="${SALT_PSA_N:-1000}"
seed="${SALT_SEED:-42}"
owsa="${SALT_OWSA:-TRUE}"
threads=1
render=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--psa)     psa_n="${2:?--psa needs a value}"; shift 2 ;;
    -s|--seed)    seed="${2:?--seed needs a value}";  shift 2 ;;
    --threads)    threads="${2:?--threads needs a value}"; shift 2 ;;
    -o|--no-owsa) owsa=FALSE; shift ;;
    -q|--quick)   psa_n=0; owsa=FALSE; shift ;;
    -r|--render)  render=1; shift ;;
    -h|--help)    usage; exit 0 ;;
    *)            echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

for v in psa_n seed threads; do
  [[ "${!v}" =~ ^[0-9]+$ ]] || { echo "--${v/_n/} must be a non-negative integer, got '${!v}'" >&2; exit 2; }
done

# --- preflight ---
command -v Rscript >/dev/null || { echo "Rscript not found on PATH." >&2; exit 127; }
[[ -f salt_results.qmd ]] || { echo "salt_results.qmd missing; here::i_am() will fail." >&2; exit 1; }
[[ -f run_microsim.R  ]] || { echo "run_microsim.R missing." >&2; exit 1; }
if (( render )); then
  command -v quarto >/dev/null || { echo "--render given but quarto is not on PATH." >&2; exit 127; }
fi

mkdir -p logs output

export SALT_PSA_N="$psa_n"
export SALT_OWSA="$owsa"
export SALT_SEED="$seed"

# Pin BLAS/OpenMP so results stay bit-comparable with the cluster run, where
# simulate.sbatch pins the same four variables. The model is single-threaded;
# multi-threaded BLAS only changes floating-point reduction order.
export OMP_NUM_THREADS="$threads"
export OPENBLAS_NUM_THREADS="$threads"
export MKL_NUM_THREADS="$threads"

stamp="$(date +%Y%m%d-%H%M%S)"
log="logs/microsim-${stamp}.log"

{
  echo "SALT microsimulation - local run"
  echo "started : $(date '+%F %T')"
  echo "root    : $ROOT"
  echo "R       : $(Rscript -e 'cat(R.version.string)')"
  echo "config  : SALT_PSA_N=$psa_n  SALT_OWSA=$owsa  SALT_SEED=$seed  threads=$threads"
  echo "log     : $log"
  echo "------------------------------------------------------------"
} | tee "$log"

start=$SECONDS

# -e would abort before the status line below, so lift it for this call only.
set +e
Rscript run_microsim.R 2>&1 | tee -a "$log"
status="${PIPESTATUS[0]}"
set -e

elapsed=$(( SECONDS - start ))

{
  echo "------------------------------------------------------------"
  printf 'model exit status: %s (%dm %02ds)\n' "$status" $(( elapsed / 60 )) $(( elapsed % 60 ))
} | tee -a "$log"

if (( status != 0 )); then
  echo "Model failed; skipping render. See $log" | tee -a "$log"
  exit "$status"
fi

if (( render )); then
  echo "Rendering salt_results.qmd ..." | tee -a "$log"
  set +e
  quarto render salt_results.qmd 2>&1 | tee -a "$log"
  status="${PIPESTATUS[0]}"
  set -e
  (( status == 0 )) || { echo "Render failed (status $status). See $log" | tee -a "$log"; exit "$status"; }
  echo "Report written to reports/" | tee -a "$log"
fi

echo "Done. Results in output/cea_results.rds" | tee -a "$log"

exit 0
}
