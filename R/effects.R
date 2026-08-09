#--------------------------------
# Effects module (DALYs and QALYs)
#--------------------------------

print("Loading effects functions")

# effects: per-cycle Years Lived with Disability (YLD) and QALYs for the
# state occupied in a cycle. Years of Life Lost (YLL) depends on the death
# cycle relative to the horizon and is computed by MicroSim() from the
# state trace, not here.
effects <- function(v_state, params) {
  cl   <- params$cycle_length
  dw   <- params$dw
  util <- params$utility
  n_i  <- length(v_state)

  yld <- numeric(n_i)
  yld[v_state == "CVD"]     <- dw["CVD"] * cl
  yld[v_state == "PostCVD"] <- dw["PostCVD"] * cl

  qaly <- as.numeric(util[v_state]) * cl

  list(yld = yld, qaly = qaly)
}
