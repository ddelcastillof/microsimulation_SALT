#--------------------------------
# Cost-effectiveness module
#--------------------------------

print("Loading cost-effectiveness functions")

# run_cea: incremental cost-effectiveness of Intervention vs Control.
# `res` is a list with elements `Intervention` and `Control`, each carrying
# mean_cost, mean_daly and mean_qaly (from MicroSim()). DALYs averted is a
# reduction, so it is Control - Intervention; QALYs gained is the reverse.
run_cea <- function(res) {
  d_cost       <- res$Intervention$mean_cost - res$Control$mean_cost
  daly_averted <- res$Control$mean_daly - res$Intervention$mean_daly
  qaly_gained  <- res$Intervention$mean_qaly - res$Control$mean_qaly

  dominant <- d_cost < 0 & daly_averted > 0

  list(
    d_cost       = d_cost,
    daly_averted = daly_averted,
    qaly_gained  = qaly_gained,
    icer_daly    = d_cost / daly_averted,
    icer_qaly    = d_cost / qaly_gained,
    dominant     = dominant,
    summary      = data.table::data.table(
      arm   = c("Intervention", "Control"),
      cost  = c(res$Intervention$mean_cost, res$Control$mean_cost),
      daly  = c(res$Intervention$mean_daly, res$Control$mean_daly),
      qaly  = c(res$Intervention$mean_qaly, res$Control$mean_qaly)
    )
  )
}

# ce_plane: a single-point CE plane for the deterministic base case.
ce_plane <- function(cea) {
  require(ggplot2)
  ggplot(data.frame(e = cea$daly_averted, c = cea$d_cost),
         aes(e, c)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(size = 3, colour = "#1A5276") +
    labs(x = "DALYs averted (Intervention vs Control)",
         y = "Incremental cost",
         title = "Cost-effectiveness plane (base case)") +
    theme_minimal(base_size = 11)
}
