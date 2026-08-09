#-----------------------
# Descriptive statistics
#-----------------------

# Set up the environment
here::i_am("salt_results.qmd")

# Load cleaning functions
source(here::here("R", "cleaning.R"))
source(here::here("R", "helpers.R"))

# Importing data for descriptive statistics
print("Importing data for descriptive statistics")

data_prelim <- clean_long()

id_cols <- c("codigo", "codigogen", "codigovilla", "codigovivienda",
             "codigofam", "codigopersona", "entvilla", "wave", "time")

#------------------------------
# 1. Exploring data missingness
#------------------------------

# Across all participants, which visits are missing?
print("Exploring data missingness across all participants")

missing_visits <- data_prelim[, .(Missing = sum(is.na(time))), by=codigo]

print("Missing visits across all participants:")

print(missing_visits)

# Which visits are missing across all participants?
print("Exploring data missingness across all visits")

missing_participants <- data_prelim[is.na(time), .(missing_waves = list(wave)), by = codigo]

# Create a grid of all participants and waves to visualize missingness
# Sort participants by village, then most-missing first within each village

missing_grid <- missing_graph(data_prelim)

#------------------------------
# 1. Exploring covariates
#------------------------------

#---------------------------------------
# Explore mortality variable consistency
#---------------------------------------

# Explore reliability of pvivo and f_muerte before introducing to model
print("Exploring pvivo × f_muerte consistency (wave-aware)")

# 1. Cross-tab at wave level: pvivo × has death date
death_wave <- data_prelim[, .(pvivo, has_date = !is.na(f_muerte)), by = .(codigo, wave)]
print(death_wave[, .N, by = .(pvivo, has_date)])

# 2. Inconsistency A — marked dead but no date, with wave
no_date_dead <- death_wave[pvivo == "No" & !has_date]
cat("Dead but no f_muerte:", nrow(no_date_dead), "observations\n")
print(no_date_dead[order(codigo, wave)])

# 3. Inconsistency B — has date but not marked dead, with wave
date_alive <- death_wave[pvivo != "No" & has_date]
cat("Has f_muerte but pvivo != dead:", nrow(date_alive), "observations\n")
print(date_alive[order(codigo, wave)])

# 3b. Of those participants: did any show pvivo == "No" at an intermediate wave?
alive_with_date_ids <- unique(date_alive$codigo)
pvivo_seq_flagged <- data_prelim[
  codigo %in% alive_with_date_ids,
  .(codigo, wave, fecha, pvivo, f_muerte)
][order(codigo, wave)]
cat("Full pvivo × f_muerte sequence for flagged participants:\n")
print(pvivo_seq_flagged)
cat("With intermediate death wave:",
    uniqueN(pvivo_seq_flagged[pvivo == "No", codigo]), "of", length(alive_with_date_ids), "\n")

# 4. True pvivo reversals: dead → alive only (cummax monotonicity check)
pvivo_nonmono <- data_prelim[
  !is.na(pvivo),
  .(wave, dead = as.integer(pvivo == "No")),
  by = codigo
][order(codigo, wave)][
  , valid := (dead >= cummax(dead)),
  by = codigo
][valid == FALSE]
cat("True pvivo reversals (dead→alive):", uniqueN(pvivo_nonmono$codigo), "participants\n")
print(pvivo_nonmono[, .(codigo, wave, dead)])

# 5. f_muerte inconsistency — show date at each wave for affected participants
date_seq <- data_prelim[
  !is.na(f_muerte), .(wave, f_muerte), by = codigo
][, n_dates := uniqueN(f_muerte), by = codigo][n_dates > 1][order(codigo, wave)]
cat("Participants with >1 f_muerte date:", uniqueN(date_seq$codigo), "\n")
print(date_seq)

# 6. Cleaning with information from diagnostics
die_between_0_1 <- c("012-086-01", "107-091-01", "020-153-02", "018-034-01")
die_between_1_2 <- c("012-035-01", "012-038-03")

# the 020-153-02 corrections now live in clean_long(), so they reach the model
# rather than only this script

#---------------------------------------------
# Explore cardiovascular risk factor variables
#---------------------------------------------

#-----------------------
# 2. Descriptive statistics
#-----------------------

cols_keep <- c("codigo", "codigovilla", "codigofam", "wave", "intervencion", "entvilla", 
               "edad", "sexo", "bmi", "sbp", "dbp", "smoking1", "ht5", "pool_cvd")

#1. Age distribution
print("Descriptive statistics: age distribution at baseline")

age_dist_baseline <- data_prelim[wave %in% c(0)][, .(mean_age = mean(edad1, na.rm = TRUE),
                             median_age = median(edad1, na.rm = TRUE),
                             age_sd = sd(edad1, na.rm = TRUE),
                             age_min = min(edad1, na.rm = TRUE),
                             age_max = max(edad1, na.rm = TRUE),
                             age_q1 = quantile(edad1, 0.25, na.rm = TRUE),
                             age_q3 = quantile(edad1, 0.75, na.rm = TRUE))]
print(age_dist_baseline)

# 1.1. How many under 40 
n_under_40 <- data_prelim[wave == 0 & edad1 < 40, .N]
n_total <- data_prelim[wave == 0, .N]
cat("Number of participants under 40 at baseline:", n_under_40, "out of", n_total, "\n")

# 1.2. How many over 74
n_over_74 <- data_prelim[wave == 0 & edad1 > 74, .N]
cat("Number of participants over 74 at baseline:", n_over_74, "out of", n_total, "\n")

# hist of age distribution at baseline
age_hist <- ggplot(data_prelim[wave == 0], aes(x = edad1)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution at Baseline", x = "Age", y = "Count") +
  theme_minimal()
print(age_hist)