# -----------------
# Helper Functions
# -----------------

print("Loading helper functions")

# Exponential and Logarithmic Transformations
expit <- function(x) exp(x)/(1+exp(x))

logit <- function(x) log(x/(1-x))

# Functions Defining Contrasts of Interests and Inverting those Contrasts
RDFunc <- function(a,b) a-b
RDInvApply <- function(out,effect) out-effect
logRRFunc <- function(a,b) log(a/b)
logRRInvApply <- function(out,effect) out*exp(-1*effect)
logORFunc <- function(a,b) log(a*(1-b)/((1-a)*b))
logORInvApply <- function(out,effect) expit(-1*effect+log(out/(1-out)))

# Create a vector with each cluster's first intervention period, by observation
StartTimeVec <- function(Periods, Clusters, Trts) {
  StartTimes <- tapply(Periods[Trts==1], Clusters[Trts==1], FUN=min)
  NumPds <- tapply(Periods, Clusters, FUN=length)
  return(rep(as.vector(StartTimes), times=NumPds))
}

# Missingness functions
# Function to create a graph of missingness across participants and waves
missing_graph <- function(data) {
  part_ord <- data[
    , .(n_missing = sum(is.na(time)), village = codigovilla[1L]),
    by = codigo
  ][order(village, -n_missing), codigo]
  
  missing_grid <- data[
    , .(missing = is.na(time), village = codigovilla),
    by = .(codigo, wave)
  ]
  missing_grid[, codigo := factor(codigo, levels = part_ord)]
  
  wave_summary <- missing_grid[, .(pct_missing = mean(missing) * 100), by = wave]
  
  p_tiles <- ggplot(missing_grid, aes(x = factor(wave), y = codigo, fill = missing)) +
    geom_raster() +
    scale_fill_manual(
      values = c("FALSE" = "#1A5276", "TRUE" = "#E74C3C"),
      labels = c("Observed", "Missing")
    ) +
    facet_grid(village ~ ., scales = "free_y", space = "free_y") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "Visit missingness across participants and waves",
         subtitle = "Grouped by village · sorted by number of missing visits within village") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y     = element_blank(),
      axis.ticks.y    = element_blank(),
      strip.text.y    = element_text(angle = 0, size = 7, hjust = 0),
      panel.spacing.y = unit(0.25, "lines"),
      panel.grid      = element_blank(),
      legend.position = "top",
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(colour = "grey50", size = 9)
    )
  
  p_bar <- ggplot(wave_summary, aes(x = factor(wave), y = pct_missing)) +
    geom_col(fill = "#E74C3C", width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", pct_missing)),
              vjust = -0.4, size = 3, colour = "grey30") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = "Wave", y = "% missing")
part_ord <- data_prelim[
  , .(n_missing = sum(is.na(time)), village = codigovilla[1L]),
  by = codigo
][order(village, -n_missing), codigo]
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )
  
  require(patchwork)
  return(p_tiles / p_bar + plot_layout(heights = c(5, 1)))
}