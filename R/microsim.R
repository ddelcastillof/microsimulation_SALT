# --------------------------------------------------------------
# Microsimulation engine for the SALT model
# --------------------------------------------------------------

print("Loading microsimulation engine")

# samplev: vectorised sampling of the next state for each individual.
# P is an n_i x n_s probability matrix (rows sum to 1); states names the
# columns. Returns a character vector of sampled states.
#
# The row-wise cumulative sum is a single matrix product against an upper
# triangular matrix of ones: (P %*% U)[i, j] = sum over k <= j of P[i, k],
# which is the row cumsum by definition. That replaces t(apply(P, 1, cumsum)),
# an implicit per-row R loop that ran 2 x n_t times per draw -- negligible at
# the trial's 6 cycles, but not at a lifetime horizon's ~197.
samplev <- function(P, states) {
  u   <- runif(nrow(P))
  cp  <- P %*% upper.tri(diag(ncol(P)), diag = TRUE)
  idx <- rowSums(u > cp) + 1L
  states[idx]
}

# MicroSim: run the microsimulation for one arm.
#   si      - sim_input list
#   params  - get_params() list
#   arm     - "Intervention" or "Control"
#   probs, costs, effects - injected module functions
# Returns the state trace plus mean discounted cost, DALYs and QALYs.
MicroSim <- function(si, params, arm, probs, costs, effects) {
  set.seed(params$seed)
  n_i <- length(si$state_0)
  n_t <- params$n_t
  cl  <- params$cycle_length
  v_n <- params$state_names

  m_M <- matrix(NA_character_, n_i, n_t + 1L,
                dimnames = list(si$ids, paste0("cycle_", 0:n_t)))
  m_M[, 1L] <- si$state_0

  m_C   <- matrix(0, n_i, n_t + 1L)   # costs per cycle
  m_YLD <- matrix(0, n_i, n_t + 1L)   # years lived with disability
  m_Q   <- matrix(0, n_i, n_t + 1L)   # QALYs per cycle

  e0 <- effects(m_M[, 1L], params)
  m_YLD[, 1L] <- e0$yld
  m_Q[, 1L]   <- e0$qaly
  # cycle 0 carries no transition cost; state cost only
  m_C[, 1L]   <- costs(m_M[, 1L], si, t = 1L, arm = arm, params = params)

  for (t in 1:n_t) {
    P <- probs(m_M[, t], si, t, arm, params)
    m_M[, t + 1L] <- samplev(P, v_n)
    m_C[, t + 1L] <- costs(m_M[, t + 1L], si, t, arm, params)
    e <- effects(m_M[, t + 1L], params)
    m_YLD[, t + 1L] <- e$yld
    m_Q[, t + 1L]   <- e$qaly
  }

  # --- Years of Life Lost ---------------------------------------------------
  # max.col finds the first Dead column directly; the previous apply() over
  # rows was a per-individual R loop over an n_i x (n_t + 1) character matrix.
  is_dead     <- m_M == "Dead"
  ever_dead   <- rowSums(is_dead) > 0L
  death_cycle <- ifelse(ever_dead, max.col(is_dead, ties.method = "first") - 1L,
                        NA_integer_)

  # discount weights at each cycle's fractional year
  yrs <- (0:n_t) * cl
  w_c <- 1 / (1 + params$d_c) ^ yrs
  w_e <- 1 / (1 + params$d_e) ^ yrs

  # A death forfeits the STANDARD life expectancy at the age it occurred, not
  # the remaining in-horizon cycles. The horizon-based version silently made
  # YLL a function of where the model happened to stop, which at a lifetime
  # horizon credits a death at 60 with 40 lost years against the GBD
  # standard's 26 -- inflating DALYs averted precisely where the intervention
  # acts. si$age_0 supplies baseline age; when it is absent (older fixtures)
  # the horizon-based definition is kept so those callers are unaffected.
  #
  # Note that YLL can now extend beyond the horizon while QALYs cannot. That
  # is the standard DALY convention rather than an inconsistency, but it is
  # worth stating whenever these two are reported side by side.
  if (is.null(si$age_0)) {
    yll <- ifelse(is.na(death_cycle), 0, (n_t - death_cycle) * cl)
  } else {
    yll <- ifelse(is.na(death_cycle), 0,
                  reference_sle(si$age_0 + death_cycle * cl))
  }

  # Discount YLL from the cycle of death, not from the end of the horizon.
  # The lost years are a stream starting at death, so they are discounted as a
  # continuous annuity over that many years and then brought back to time zero
  # at the death cycle's own weight. Using w_e[n_t + 1L] for everyone applied
  # the horizon-end weight to every death alike -- at 2.5 years that weight is
  # 0.93 and the error hides; at 82 years it is 0.089 and it would erase YLL
  # almost entirely regardless of when death occurred.
  d_e <- params$d_e
  annuity <- if (d_e == 0) yll else (1 - (1 + d_e) ^ (-yll)) / log(1 + d_e)
  w_death <- ifelse(is.na(death_cycle), 0, w_e[pmin(death_cycle + 1L, n_t + 1L)])
  yll_disc <- annuity * w_death

  cost <- as.numeric(m_C %*% w_c)
  daly <- as.numeric(m_YLD %*% w_e) + yll_disc
  qaly <- as.numeric(m_Q %*% w_e)

  list(
    m_M       = m_M,
    cost      = cost,  daly = daly,  qaly = qaly,
    mean_cost = mean(cost),
    mean_daly = mean(daly),
    mean_qaly = mean(qaly)
  )
}
