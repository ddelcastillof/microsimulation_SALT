#-----------------------------------------------------------------
# Corrected relational dictionary of the ENDES parquets
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing functions to document the ENDES parquets...")

# building_endes() writes nine parquets to data/endes-proc. This file records how they
# relate (grain, keys, which module joined on which column) and where the INEI 2025
# dictionaries in .claude/data-dictionary/ describe a key or a type wrongly. Variable
# labels stay in the PDFs; only relations and their corrections live here.
#
# Order of use: building_endes(), then write_endes_dictionary(). Rebuilding the parquets
# drops the metadata, so run write_endes_dictionary() again after every build.
# read_endes_dictionary() reads it back from any one of the nine files.

# The dictionary as an R list. Column lists are wrapped in I() so the JSON keeps them as
# arrays even when they hold one name. Evidence counts come from key-only checks on
# ENDES 2025: they never looked at the values of a variable

endes_dictionary <- function() {
  keys <- \(...) I(c(...))

  # Where a table's columns come from. `on` maps the module's column to this table's
  src <- \(source, join, on = NULL, note = NULL) {
    out <- list(source = source, join = join)
    if(!is.null(on)) out$on <- as.list(on)
    if(!is.null(note)) out$note <- note
    out
  }

  # A link from this table to another one. Rows with an NA key, or a key in `exclude`,
  # point nowhere
  link <- \(columns, table, references, nullable = FALSE, exclude = NULL) {
    out <- list(columns = I(columns), table = table, references = I(references),
                nullable = nullable)
    if(!is.null(exclude)) out$exclude <- I(exclude)
    out
  }

  drop <- \(columns, from, reason) list(columns = I(columns), from = from, reason = reason)

  # A place where the INEI dictionary is wrong or silent, and what the parquets do instead
  fix <- \(variables, modules, dictionary_says, corrected, evidence) {
    list(variables = I(variables), modules = I(modules), dictionary_says = dictionary_says,
         corrected = corrected, evidence = evidence)
  }

  mef_modules <- c("RE223132", "RE516171", "RE758081", "REC42", "REC84DV", "REC91")
  ps_modules <- c("PS_BECA18", "PS_COMEDOR", "PS_CUNAMAS_VISITA", "PS_PENSION65",
                  "PS_QALIWARMA", "PS_TRABAJA", "PS_VL", "PS_WAWAWASI")

  list(
    version = "1.0.0",
    date = "2026-10-23",
    survey = "ENDES 2025 (INEI)",
    built_by = "building_endes() in R/endes-builder.R",
    source_dictionaries = ".claude/data-dictionary/",
    usage = paste("Written by write_endes_dictionary() into every parquet's metadata",
                  "(keys 'endes_dictionary' and 'endes_table'). Read it with",
                  "read_endes_dictionary(path)."),

    tables = list(
      compiled = list(
        grain = paste("Person selected for the health questionnaire (CSALUD01): one per",
                      "household, aged 15 or more. Saved before filtering non-respondents"),
        base_module = "CSALUD01",
        primary_key = keys("HHID", "QSNUMERO"),
        alternate_keys = list(keys("HHID")),
        sources = list(
          src("CSALUD01", "base"),
          src("persons", "left", c(HHID = "HHID", HVIDX = "QSNUMERO"),
              "roster line, household, RECH4, RECH5 and social programmes"),
          src(paste(c("REC0111", mef_modules), collapse = ", "), "left",
              c(HHID = "HHID", V003 = "QSNUMERO"),
              "MEF modules; NA when the selected person is not an interviewed woman")
        ),
        foreign_keys = list(
          link(c("HHID", "QSNUMERO"), "persons", c("HHID", "HVIDX")),
          link("HHID", "households", "HHID"),
          link("CASEID", "women", "CASEID", nullable = TRUE)
        ),
        dropped = list(
          drop("ID1", "persons and the MEF modules", "survey year; CSALUD01's copy is kept"),
          drop("UBIGEO", "REC0111", "equal to RECH0's UBIGEO, which persons carries")
        )
      ),

      households = list(
        grain = "Household. RECH0 and RECH23 list the same households",
        base_module = "RECH23",
        primary_key = keys("HHID"),
        sources = list(
          src("RECH23", "base"),
          src("RECH0", "right join onto RECH23", c(HHID = "HHID")),
          src("PS_HOGAR", "left", c(HHID = "HHID"),
              "household-level social programmes; only households with the health questionnaire")
        ),
        foreign_keys = list(),
        dropped = list(
          drop("ID1", "RECH23", "survey year; RECH0's copy is kept"),
          drop(c("ID1", "QHCLUSTER", "QHNUMBER", "QHHOME"), "PS_HOGAR",
               "survey year, and copies of RECH0's HV001, HV002 and HV002A")
        )
      ),

      persons = list(
        grain = "Household roster member (RECH1), every age",
        base_module = "RECH1",
        primary_key = keys("HHID", "HVIDX"),
        sources = list(
          src("RECH1", "base"),
          src("households", "right join onto RECH1", c(HHID = "HHID")),
          src("RECH4", "left", c(HHID = "HHID", IDXH4 = "HVIDX"), "health insurance"),
          src("RECH5", "left", c(HHID = "HHID", HA0 = "HVIDX"),
              "anaemia and anthropometry of women aged 12-49"),
          src(paste(ps_modules, collapse = ", "), "left", c(HHID = "HHID", HVIDX = "HVIDX"),
              "one table per social programme")
        ),
        foreign_keys = list(link("HHID", "households", "HHID")),
        dropped = list(
          drop("ID1", "RECH1, RECH4, RECH5 and the PS modules", "survey year; RECH0's copy is kept"),
          drop(c("QHCLUSTER", "QHNUMBER", "QHHOME"), "PS modules",
               "copies of RECH0's HV001, HV002 and HV002A")
        )
      ),

      women = list(
        grain = "Woman interviewed with the individual questionnaire (MEF, 15-49; REC0111)",
        base_module = "REC0111",
        primary_key = keys("CASEID"),
        alternate_keys = list(keys("HHID", "V003")),
        sources = list(
          src("REC0111", "base"),
          src(paste(mef_modules, collapse = ", "), "left", c(CASEID = "CASEID")),
          src("persons", "left", c(HHID = "HHID", HVIDX = "V003"),
              "her roster line and household")
        ),
        foreign_keys = list(
          link(c("HHID", "V003"), "persons", c("HHID", "HVIDX")),
          link("HHID", "households", "HHID")
        ),
        dropped = list(
          drop("ID1", "the MEF modules and persons", "survey year; REC0111's copy is kept"),
          drop("UBIGEO", "REC0111", "equal to RECH0's UBIGEO, which persons carries")
        )
      ),

      children = list(
        grain = paste("Child on the household roster in CSALUD08 (0-11 years) or RECH6",
                      "(under 5), joined on the child's roster line"),
        base_module = "CSALUD08 full join RECH6",
        primary_key = keys("HHID", "QS801"),
        sources = list(
          src("CSALUD08", "base"),
          src("RECH6", "full", c(HHID = "HHID", HC0 = "QS801"), "anthropometry and anaemia"),
          src("persons", "left", c(HHID = "HHID", HVIDX = "QS801"), "roster line and household")
        ),
        foreign_keys = list(
          link(c("HHID", "QS801"), "persons", c("HHID", "HVIDX")),
          link("HHID", "households", "HHID"),
          link(c("HHID", "HC60"), "women", c("HHID", "V003"), nullable = TRUE,
               exclude = c(993L, 994L, 995L))
        ),
        dropped = list(
          drop("ID1", "RECH6 and persons", "survey year; CSALUD08's copy is kept")
        ),
        notes = I(c(
          paste("A child only in RECH6 has no CSALUD08 row: its ID1, QHCLUSTER, QHNUMBER and",
                "QHHOME are filled from RECH0's ID1, HV001, HV002 and HV002A"),
          "HC60 codes 993-995 mean the mother is not an interviewed woman of this household"
        ))
      ),

      births = list(
        grain = "Birth in the birth history (REC21) of an interviewed woman",
        base_module = "REC21",
        primary_key = keys("CASEID", "BIDX"),
        alternate_keys = list(keys("CASEID", "BORD")),
        sources = list(
          src("REC21", "base"),
          src("DIT", "left", c(CASEID = "CASEID", BIDX = "BIDX")),
          src("REC41", "left", c(CASEID = "CASEID", MIDX = "BIDX")),
          src("REC43", "left", c(CASEID = "CASEID", HIDX = "BIDX")),
          src("REC44", "left", c(CASEID = "CASEID", HWIDX = "BIDX")),
          src("REC94", "left", c(CASEID = "CASEID", IDX94 = "BIDX")),
          src("REC95", "left", c(CASEID = "CASEID", IDX95 = "BIDX")),
          src("REC93DVdisciplina", "left", c(CASEID = "CASEID", Q1035NO = "BORD"),
              "child discipline; QCOL93 is kept but does not link to REC21"),
          src("REC0111", "left", c(CASEID = "CASEID"), "adds the mother's HHID and V003")
        ),
        foreign_keys = list(
          link("CASEID", "women", "CASEID"),
          link(c("HHID", "V003"), "persons", c("HHID", "HVIDX"))
        ),
        dropped = list(
          drop("ID1", "every joined module", "survey year; REC21's copy is kept"),
          drop("BORD", "DIT", "equal to REC21's BORD for the same BIDX")
        ),
        notes = I(paste("358 REC93DVdisciplina children (328 women) have a CASEID that is in",
                        "neither REC0111 nor REC21, so they are not in this table"))
      ),

      siblings = list(
        grain = "Sibling of an interviewed woman (REC83, maternal mortality)",
        base_module = "REC83",
        primary_key = keys("CASEID", "MMIDX"),
        sources = list(
          src("REC83", "base"),
          src("REC0111", "left", c(CASEID = "CASEID"), "adds the woman's HHID and V003")
        ),
        foreign_keys = list(
          link("CASEID", "women", "CASEID"),
          link(c("HHID", "V003"), "persons", c("HHID", "HVIDX"))
        ),
        dropped = list()
      ),

      calendar = list(
        grain = "Calendar column (VCOL) of an interviewed woman (REC82)",
        base_module = "REC82",
        primary_key = keys("CASEID", "VCOL"),
        sources = list(
          src("REC82", "base"),
          src("REC0111", "left", c(CASEID = "CASEID"), "adds the woman's HHID and V003")
        ),
        foreign_keys = list(
          link("CASEID", "women", "CASEID"),
          link(c("HHID", "V003"), "persons", c("HHID", "HVIDX"))
        ),
        dropped = list(),
        notes = I("VCAL holds 80 positions, one per month; its blanks are data, not padding")
      ),

      deaths = list(
        grain = "Household member who died (RECHM)",
        base_module = "RECHM",
        primary_key = keys("HHID", "NRO_ORDEN_ID"),
        sources = list(src("RECHM", "base")),
        foreign_keys = list(link("HHID", "households", "HHID")),
        dropped = list(),
        notes = I("NRO_ORDEN_ID numbers the deceased; the dictionary gives no RECH1 roster line for them")
      )
    ),

    corrections = list(
      fix("HHID", "every household-level module",
          "A 15 (N 15 in RECH5), range 000100101:542905001",
          paste("The CSVs store it as an integer. The parquets store it as nine digits with",
                "leading zeros, INEI's own spelling"),
          "Equals the first nine characters of CASEID for 36,260 of 36,260 REC0111 women"),
      fix("CASEID", c("REC0111", mef_modules, "REC21", "DIT", "REC41", "REC43", "REC44",
                      "REC82", "REC83", "REC93DVdisciplina", "REC94", "REC95"),
          "A 18",
          paste("Stored without its padding: 12 characters, HHID (9) followed by V003",
                "right-aligned in 3. REC93DVdisciplina names the column 'caseid'"),
          "CASEID == HHID + formatC(V003, width = 3) for 36,260 of 36,260 REC0111 women"),
      fix("QSNUMERO", "CSALUD01",
          "Número de la persona seleccionada",
          "Roster line of the selected person: RECH1's HVIDX",
          paste("0 of 33,569 selected persons sit on a child's line in CSALUD08 or RECH6;",
                "shifted by +1 or -1 they would hit 9,326 or 1,299. Equals QS20C where QS20C",
                "is present (30,435 rows)")),
      fix("V003", "REC0111",
          "Número de línea de entrevistada",
          "Roster line of the woman: RECH1's HVIDX",
          paste("Built into CASEID; 0 women on a child's line; 36,260 of 36,260 found in",
                "RECH5 (HA0); 18,290 of 18,290 RECH6 mother pointers (HC60 in 1:20) find her")),
      fix(c("MIDX", "HIDX", "HWIDX", "IDX94", "IDX95"),
          c("REC41", "REC43", "REC44", "REC94", "REC95"),
          paste("Orden de historia de nacimiento: the same words the dictionary uses for",
                "REC21's BORD"),
          "Birth-history index counted from the most recent birth: REC21's BIDX, not BORD",
          "Against DIT, which holds both: 0.954 of 17,138 rows match as BIDX, 0.396 as BORD"),
      fix("Q1035NO", "REC93DVdisciplina",
          "Número de orden del niño (historia de nacimiento)",
          paste("REC21's BORD, the opposite of the other child modules. QCOL93, the",
                "dictionary's key, counts the woman's children and does not link to REC21"),
          "Against DIT: 1.000 of 14,518 rows match as BORD, 0.393 as BIDX"),
      fix(c("QHCLUSTER", "QHNUMBER", "QHHOME"), c("CSALUD01", "CSALUD08", "PS_HOGAR", ps_modules),
          "Conglomerado, Vivienda seleccionada, Hogar seleccionado",
          "The same identifiers as RECH0's HV001, HV002 and HV002A",
          "Equal in 100% of rows of every module that carries them"),
      fix(c("QS801", "HC0", "HC60"), c("CSALUD08", "RECH6"),
          paste("QS801: Nro. de orden niña/niño; HC0: Número de orden en el hogar;",
                "HC60: Número de orden de la madre en el hogar"),
          paste("QS801 and HC0 are the child's RECH1 line. HC60 in 1:20 is the mother's line,",
                "REC0111's V003; 993-995 are codes, not lines"),
          "All QS801 and HC0 found in RECH1; 18,290 of 18,290 HC60 pointers found in REC0111"),
      fix(c("IDXH4", "HA0"), c("RECH4", "RECH5"),
          "Número de orden (del listado) del hogar",
          "RECH1's HVIDX, as the dictionary says",
          "All rows found in RECH1"),
      fix(c("UBIGEO", "CODCCPP", "NCONGLOME1"), c("RECH0", "REC0111"),
          "A 6, A 4 and A 7 (NCONGLOME1 is named NCONGLOME in the dictionary)",
          paste("The CSVs drop the leading zeros; the parquets pad them back. REC0111's",
                "UBIGEO is dropped as a copy of RECH0's"),
          "REC0111's UBIGEO equals RECH0's for 36,260 of 36,260 women"),
      fix(c("QS407", "QFIN", "QS8INICIO", "HA62"), c("CSALUD01", "RECH5"),
          "A (text)",
          "Read as text: fread would guess them as numbers or, when empty, as logical",
          "Dictionary type"),
      fix("VCAL", "REC82",
          "Calendario, A 80, values A:Z and 0:9",
          paste("Positional, one character per month. Read without stripping white space",
                "and padded to 80 on the right; an all-blank calendar is NA"),
          paste("Every one of the 109,634 non-blank calendars starts with blanks and none",
                "ends with them; stripping had shifted every month")),
      fix("blank text", "every module",
          "Not described",
          "A blank or quoted \"\" text field is NA, not an empty answer",
          "Fifteen text columns held \"\" for questions not asked, e.g. QS27"),
      fix(c("Peso15_AMAS", "QH100B", "V393"), c("CSALUD01", "PS_HOGAR", "RE223132"),
          "PESO15_AMAS in the dictionary; QH100B and V393 are not listed",
          "Kept with the CSV's names",
          "Column names of the CSVs"),
      fix("ID1", "every module",
          "Año",
          "One value, the same in every module; each table keeps its base module's copy",
          "A single ID1 value across the 35 modules")
    )
  )
}

# Checks the dictionary against the parquets, reading key columns only: every declared
# column exists, every primary and alternate key is unique, and every foreign key finds
# its parent row. Stops naming the relation that fails

validate_endes_dictionary <- function(dict = endes_dictionary(),
                                      dir = here::here("data", "endes-proc")) {
  library(data.table)
  library(purrr)
  library(arrow)

  path <- \(t) file.path(dir, paste0(t, ".parquet"))
  read_keys <- \(t, cols) as.data.table(read_parquet(path(t), col_select = unique(cols)))

  missing_files <- names(dict$tables)[!file.exists(path(names(dict$tables)))]
  if(length(missing_files) > 0) stop("Parquets missing from ", dir, ": ", paste(missing_files, collapse = ", "))

  iwalk(dict$tables, \(tb, t) {
    pk_sets <- c(list(tb$primary_key), tb$alternate_keys)
    declared <- unique(c(unlist(pk_sets), unlist(map(tb$foreign_keys, "columns"))))
    absent <- setdiff(declared, names(open_dataset(path(t))))
    if(length(absent) > 0) stop(t, " lacks the columns its dictionary declares: ", paste(absent, collapse = ", "))

    walk(pk_sets, \(k) {
      k <- as.character(k)
      x <- read_keys(t, k)
      if(anyNA(x) || anyDuplicated(x, by = k) > 0) stop(t, " is not unique on ", paste(k, collapse = " + "))
    })

    walk(tb$foreign_keys, \(fk) {
      cols <- as.character(fk$columns)
      refs <- as.character(fk$references)
      x <- read_keys(t, cols)
      if(!fk$nullable && anyNA(x)) stop(t, ": NA in the non-nullable link ", paste(cols, collapse = " + "))
      x <- na.omit(x)
      if(!is.null(fk$exclude)) x <- x[!x[[last(cols)]] %in% fk$exclude]
      n <- nrow(x[!read_keys(fk$table, refs), on = set_names(refs, cols)])
      if(n > 0) {
        stop(t, " (", paste(cols, collapse = ", "), ") -> ", fk$table, " (",
             paste(refs, collapse = ", "), "): ", n, " rows with no parent")
      }
    })
  })

  message("ENDES dictionary matches the parquets in ", dir)
  invisible(TRUE)
}

# Writes the dictionary into every parquet's metadata, after validating it. Each file
# gets the whole dictionary, so any one of them documents the set, plus its own table
# name. The table goes to a temporary file first and replaces the original only once
# written, so a failed write leaves the parquet as it was

write_endes_dictionary <- function(dict = endes_dictionary(),
                                   dir = here::here("data", "endes-proc")) {
  library(purrr)
  library(arrow)

  validate_endes_dictionary(dict, dir)
  json <- as.character(jsonlite::toJSON(dict, auto_unbox = TRUE))

  walk(names(dict$tables), \(t) {
    f <- file.path(dir, paste0(t, ".parquet"))
    tmp <- paste0(f, ".tmp")
    tab <- read_parquet(f, as_data_frame = FALSE)
    tab$metadata$endes_dictionary <- json
    tab$metadata$endes_table <- t
    write_parquet(tab, sink = tmp)
    if(!file.rename(tmp, f)) {
      unlink(tmp)
      stop("Could not replace ", f)
    }
  })

  message("ENDES dictionary ", dict$version, " written to ", length(dict$tables),
          " parquets in ", dir)
  invisible(json)
}

# Reads the dictionary back from one parquet's footer, without loading its rows

read_endes_dictionary <- function(path) {
  meta <- arrow::open_dataset(path)$schema$metadata
  if(is.null(meta$endes_dictionary)) {
    stop(path, " has no ENDES dictionary; run write_endes_dictionary() after building_endes()")
  }
  list(table = meta$endes_table,
       dictionary = jsonlite::fromJSON(meta$endes_dictionary, simplifyDataFrame = FALSE,
                                       simplifyMatrix = FALSE))
}
