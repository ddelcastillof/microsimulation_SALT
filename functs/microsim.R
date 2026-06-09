# --------------------------------------------------------------
# Microsimulation engine for the SALT model
# --------------------------------------------------------------

print("Loading microsimulation engine")

# samplev: vectorised sampling of the next state for each individual.
# P is an n_i x n_s probability matrix (rows sum to 1); states names the
# columns. Returns a character vector of sampled states.
samplev <- function(P, states) {
  u   <- runif(nrow(P))
  cp  <- t(apply(P, 1L, cumsum))
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

  # Years of Life Lost: a death forfeits the remaining in-horizon life-years
  death_cycle <- apply(m_M, 1L, function(r) {
    d <- which(r == "Dead"); if (length(d)) min(d) - 1L else NA_integer_
  })
  yll <- ifelse(is.na(death_cycle), 0, (n_t - death_cycle) * cl)

  # discount weights at each cycle's fractional year
  yrs <- (0:n_t) * cl
  w_c <- 1 / (1 + params$d_c) ^ yrs
  w_e <- 1 / (1 + params$d_e) ^ yrs

  cost <- as.numeric(m_C %*% w_c)
  daly <- as.numeric(m_YLD %*% w_e) + yll * w_e[n_t + 1L]
  qaly <- as.numeric(m_Q %*% w_e)

  list(
    m_M       = m_M,
    cost      = cost,  daly = daly,  qaly = qaly,
    mean_cost = mean(cost),
    mean_daly = mean(daly),
    mean_qaly = mean(qaly)
  )
}
