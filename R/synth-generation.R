#-----------------------------------------------------------------
# Synthetic adult population of Peru from cleaning_endes()'s table
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing synthetic data generation functions")

# syn() reproduces the ENDES sample, which over-represents small departments and adults of
# small households (one adult is interviewed per household); Peso15_AMAS undoes both. So k
# synthetic adults, each with a synthetic weight, are drawn n times with probability
# proportional to it, and the equal-weight population goes to `out` in 1M-row parquet files.
# The table's column order is the visit order

generate_synth <- function(data, n, k = 5e5, out = here::here("data", "synth-endes")) {
  # require() only warns, so a missing package would surface after hours of fitting
  stopifnot(require(synthpop, lib.loc = '/mnt/home/dd198b/.R/4.6.1'), 
            require(data.table, lib.loc = '/mnt/home/dd198b/.R/4.6.1'), 
            require(purrr, lib.loc = '/mnt/home/dd198b/.R/4.6.1'), 
            require(arrow, lib.loc = '/mnt/home/dd198b/.R/4.6.1'))
  # The patch below, and .claude/synthpop-reference.md, describe this version's internals
  stopifnot("synthpop 1.9-3 expected" = packageVersion("synthpop") == "1.9.3")
  data <- as.data.table(data)
  stopifnot("Peso15_AMAS must be complete and positive" = isTRUE(min(data$Peso15_AMAS) > 0))
  geo <- c("prov_key", "SHDISTRI")
  x <- data[, !..geo]

  # syn() turns perfectly correlated columns into copies of the first (pairwise-complete, so
  # sparse columns qualify), but checks only once fitting starts. Same check, before it
  stopifnot("Collinear columns: drop them in cleaning_endes()" = is.null(synthpop:::collinear.out(
    as.data.frame(map(x, \(v) if (is.factor(v)) addNA(v, ifany = TRUE) else v)))))

  # remove.lindep.syn() drops a neighbour of the intended column after its first drop
  # (synthpop 1.9-3), and paired measurements share NA flags (weight/height, BP readings).
  # Trees need no pruning, so every predictor is kept for this call
  lindep <- synthpop:::remove.lindep.syn
  assignInNamespace("remove.lindep.syn", \(x, y, ...) rep(TRUE, ncol(x)), "synthpop")
  on.exit(assignInNamespace("remove.lindep.syn", lindep, "synthpop"))

  # xval = 0 and no surrogates skip rpart work synthpop never reads (7x faster, same trees).
  # Peak memory grows ~30 KB per synthetic row, hence k = 5e5 within the job's 32G
  sds <- syn(x, k = k, seed = 2025L, print.flag = FALSE,
             cart.xval = 0, cart.maxsurrogate = 0, cart.maxcompete = 0)
  stopifnot("syn() made collinear copies: drop those columns in cleaning_endes()" =
              !"collinear" %in% sds$method)
  s <- setDT(sds$syn)

  # synthpop's nested sampling ignores weights and area, so province and district come from
  # the original adults of the same department, area and natural region, drawn with
  # probability proportional to their weight (the department alone if that cell is empty)
  g <- data[, .(HV024, HV025, SHREGION, prov_key, SHDISTRI, w = Peso15_AMAS)]
  s[, (geo) := {
    m <- .N
    o <- g[.BY, on = .(HV024, HV025, SHREGION), nomatch = NULL]
    if (!nrow(o)) o <- g[.BY[1], on = "HV024"]
    o[sample.int(nrow(o), m, TRUE, o$w), .(prov_key, SHDISTRI)]
  }, by = .(HV024, HV025, SHREGION)]

  # Sorted, so each base adult's copies sit together and compress: subsample the output at
  # random, never with head()
  idx <- sort(sample.int(k, n, replace = TRUE, prob = s$Peso15_AMAS))
  s[, c("Peso15_AMAS", "HV005") := NULL]
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(out, "^part-\\d+\\.parquet$", full.names = TRUE))
  iwalk(split(idx, ceiling(seq_along(idx) / 1e6)), \(i, part) {
    write_parquet(s[i], file.path(out, sprintf("part-%03d.parquet", as.integer(part))))
  })
  invisible(out)
}
