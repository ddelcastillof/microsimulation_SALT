#-----------------------------------------------------------------
# Cleaning and preprocessing ENDES data before synthetic creation
# Author: Darwin Del Castillo
#-----------------------------------------------------------------

print("Importing functions to build ENDES datasets...")

# Compiling all ENDES files

building_endes <- function() {
  library(data.table)
  library(purrr)
  library(arrow)

  endes_raw <- here::here("data", "endes-raw")
  endes_proc <- here::here("data", "endes-proc")

  if(!dir.exists(endes_raw)) {
    message("Creating input folder for raw ENDES data: ", endes_raw)
    dir.create(endes_raw, recursive = TRUE)
  }

  if(!dir.exists(endes_proc)) {
    message("Creating output folder for processed ENDES data: ", endes_proc)
    dir.create(endes_proc, recursive = TRUE)
  }

  # Are there files in the ENDES raw directory?
  check_endes_files <- list.files(endes_raw, pattern = "\\.csv$", ignore.case = TRUE)
  
  if(length(check_endes_files) == 0) {
    stop("No ENDES CSV files found in: ", endes_raw, "\n",
         "  Add the ENDES 2025 module CSV files downloaded from the INEI ",
         "microdata portal, so you can create the synthetic population.")
  }

  message("Folders and files ready. Compiling original ENDES dataset")

  # Identification key (Llave de identificación) of each module, from the ENDES 2025 dictionaries.
  endes_keys <- list(
    # Household
    RECH0             = "HHID",
    RECH23            = "HHID",
    PS_HOGAR          = "HHID",
    # Household member, by roster line
    RECH1             = c("HHID", "HVIDX"),
    RECH4             = c("HHID", "IDXH4"),
    RECH5             = c("HHID", "HA0"),
    RECH6             = c("HHID", "HC0"),
    PS_BECA18         = c("HHID", "HVIDX"),
    PS_COMEDOR        = c("HHID", "HVIDX"),
    PS_CUNAMAS_VISITA = c("HHID", "HVIDX"),
    PS_PENSION65      = c("HHID", "HVIDX"),
    PS_QALIWARMA      = c("HHID", "HVIDX"),
    PS_TRABAJA        = c("HHID", "HVIDX"),
    PS_VL             = c("HHID", "HVIDX"),
    PS_WAWAWASI       = c("HHID", "HVIDX"),
    CSALUD01          = c("HHID", "QSNUMERO"),
    CSALUD08          = c("HHID", "QS801"),
    # Deceased household member
    RECHM             = c("HHID", "NRO_ORDEN_ID"),
    # Interviewed woman (MEF)
    REC0111           = "CASEID",
    RE223132          = "CASEID",
    RE516171          = "CASEID",
    RE758081          = "CASEID",
    REC42             = "CASEID",
    REC84DV           = "CASEID",
    REC91             = "CASEID",
    # Birth or child of an interviewed woman
    REC21             = c("CASEID", "BIDX"),
    DIT               = c("CASEID", "BIDX"),
    REC41             = c("CASEID", "MIDX"),
    REC43             = c("CASEID", "HIDX"),
    REC44             = c("CASEID", "HWIDX"),
    REC94             = c("CASEID", "IDX94"),
    REC95             = c("CASEID", "IDX95"),
    REC93DVdisciplina = c("CASEID", "QCOL93"),
    # Sibling of an interviewed woman, and her calendar columns
    REC83             = c("CASEID", "MMIDX"),
    REC82             = c("CASEID", "VCOL")
  )

  # Module name = file name minus the survey-year suffix. 'Programas Sociales x Hogar' is
  # not a syntactic name, so it becomes PS_HOGAR
  endes_files <- set_names(check_endes_files,
                           sub("_[0-9]{4}\\.csv$", "", check_endes_files, ignore.case = TRUE))
  names(endes_files)[names(endes_files) == "Programas Sociales x Hogar"] <- "PS_HOGAR"

  # Two survey years in the folder give two files the same module name, and [[ would
  # silently read the first one. One year per run until the pipeline pools years
  stopifnot("Two files map to the same module: keep one survey year in data/endes-raw" =
              !anyDuplicated(names(endes_files)))

  missing_modules <- setdiff(names(endes_keys), names(endes_files))
  if(length(missing_modules) > 0) {
    stop("ENDES modules missing from ", endes_raw, ": ", paste(missing_modules, collapse = ", "))
  }

  # Dictionary type A (character) columns that fread guesses as numbers, or as logical
  # when empty. Codes whose leading zeros matter are padded back to their dictionary length
  code_width <- c(UBIGEO = 6L, CODCCPP = 4L, NCONGLOME1 = 7L)
  char_cols <- c(names(code_width), "QS407", "QFIN", "QS8INICIO", "HA62")

  # Importing ENDES tables
  walk(names(endes_keys), \(m) {
    f <- file.path(endes_raw, endes_files[[m]])
    as_char <- intersect(char_cols, names(fread(f, nrows = 0)))
    # VCAL (REC82) is positional, one character per month, so its leading blanks are data.
    # REC82 is the only module read without stripping white space
    dt <- fread(f, colClasses = list(character = as_char), strip.white = m != "REC82")

    # REC93DVdisciplina spells its key in lower case
    setnames(dt, "caseid", "CASEID", skip_absent = TRUE)

    # Unstripped, REC82's CASEID keeps the padding every other module loses
    if(m == "REC82") dt[, CASEID := trimws(CASEID)]

    # Blank text is a question not asked, not an answer. fread keeps a quoted "" as "",
    # which na.strings cannot catch, so blanks become NA after reading
    walk(names(dt)[map_lgl(dt, is.character)], \(v) {
      set(dt, i = which(trimws(dt[[v]]) == ""), j = v, value = NA_character_)
    })

    # The CSV drops VCAL's trailing blanks; pad back to the dictionary's 80 positions
    if(m == "REC82") {
      dt[!is.na(VCAL), VCAL := sprintf("%-80s", VCAL)]
      stopifnot("VCAL is wider than its 80 positions" = all(nchar(dt$VCAL) == 80L, na.rm = TRUE))
    }

    # HHID is A15 in the dictionaries (N15 in RECH5's) but every module stores it as a
    # number. INEI's own spelling, the one inside CASEID, is nine digits with leading zeros
    if("HHID" %in% names(dt)) {
      stopifnot("HHID no longer arrives as an integer" = is.integer(dt$HHID))
      dt[, HHID := formatC(HHID, width = 9, flag = "0")]
      stopifnot("HHID is not nine digits long" = all(nchar(dt$HHID) == 9L))
    }

    walk(intersect(names(code_width), names(dt)), \(v) {
      x <- dt[[v]]
      x <- fifelse(is.na(x), NA_character_,
                   paste0(strrep("0", pmax(code_width[[v]] - nchar(x), 0L)), x))
      if(any(nchar(x) != code_width[[v]], na.rm = TRUE)) {
        stop(v, " in ", m, " is wider than its dictionary length of ", code_width[[v]])
      }
      set(dt, j = v, value = x)
    })

    # Columns empty in every row come back logical; the dictionary codes them N
    walk(names(dt)[map_lgl(dt, is.logical)], \(v) set(dt, j = v, value = as.integer(dt[[v]])))

    assign(m, dt, envir = globalenv())
  })

  # Each module must be unique on its key, or the joins below would duplicate rows
  iwalk(endes_keys, \(k, m) {
    if(anyDuplicated(get(m), by = k) > 0) stop(m, " is not unique on ", paste(k, collapse = " + "))
  })

  # Columns every joined module repeats: survey year and the health questionnaire's
  # sample design. Joined modules drop them; each level's base table keeps its own
  drop_shared <- \(dt) dt[, setdiff(names(dt), c("ID1", "QHCLUSTER", "QHNUMBER", "QHHOME")),
                          with = FALSE]

  # Joined module with its own index column renamed to the one it matches in the base table
  rekey <- \(m, to) setnames(drop_shared(get(m)), last(endes_keys[[m]]), to)

  # Every row of x must find its parent, or the join below would drop it or leave the
  # parent's columns NA. `on` as in data.table: x's columns as names, parent's as values
  assert_covered <- \(x, parent, on, what) {
    n <- nrow(x[!parent, on = on])
    if(n > 0) stop(what, ": ", n, " rows with no match in their parent table")
  }

  # Merging datasets
  # Household: dwelling (RECH23) and household-level social programmes (PS_HOGAR)
  assert_covered(RECH0, RECH23, "HHID", "RECH0 households missing from RECH23")
  assert_covered(RECH23, RECH0, "HHID", "RECH23 households missing from RECH0")
  assert_covered(PS_HOGAR, RECH23, "HHID", "PS_HOGAR")
  assert_covered(RECHM, RECH23, "HHID", "RECHM")
  households <- RECH0[RECH23[, !"ID1"], on = "HHID"]
  households <- merge(households, drop_shared(PS_HOGAR), by = "HHID", all.x = TRUE)

  # Household member: roster (RECH1), health insurance (RECH4), anaemia and anthropometry
  # of women 12-49 (RECH5) and one table per social programme, all keyed on the roster
  # line. A module row with no roster line would be missing from every table
  member_modules <- c("RECH4", "RECH5",
                      grep("^PS_(?!HOGAR$)", names(endes_keys), value = TRUE, perl = TRUE))
  assert_covered(RECH1, RECH23, "HHID", "RECH1")
  persons <- households[RECH1[, !"ID1"], on = "HHID"]
  persons <- reduce(member_modules, \(acc, m) {
    dt <- rekey(m, "HVIDX")
    assert_covered(dt, RECH1, c("HHID", "HVIDX"), m)
    merge(acc, dt, by = c("HHID", "HVIDX"), all.x = TRUE)
  }, .init = persons)

  # Interviewed woman: REC0111 plus every module keyed on CASEID alone. Her roster line
  # (V003) links her to RECH1. Key-only proof that V003 is that line: CASEID is HHID
  # followed by V003, and no woman aged 15-49 sits on a child's line
  stopifnot("REC0111 repeats a roster line (HHID + V003)" =
              anyDuplicated(REC0111, by = c("HHID", "V003")) == 0,
            "CASEID is not HHID followed by V003" =
              REC0111[, all(CASEID == paste0(HHID, formatC(V003, width = 3)))],
            "An interviewed woman sits on a child's line (CSALUD08)" =
              nrow(REC0111[CSALUD08, on = .(HHID, V003 = QS801), nomatch = NULL]) == 0,
            "An interviewed woman sits on a child's line (RECH6)" =
              nrow(REC0111[RECH6, on = .(HHID, V003 = HC0), nomatch = NULL]) == 0)
  assert_covered(REC0111, RECH1, c(HHID = "HHID", V003 = "HVIDX"), "REC0111")
  woman_modules <- c("RE223132", "RE516171", "RE758081", "REC42", "REC84DV", "REC91")
  women <- reduce(woman_modules, \(acc, m) {
    assert_covered(get(m), REC0111, "CASEID", m)
    merge(acc, drop_shared(get(m)), by = "CASEID", all.x = TRUE)
  }, .init = REC0111)

  # RECH0 already holds the household's UBIGEO
  women_full <- merge(women[, !"UBIGEO"], persons[, !"ID1"], by.x = c("HHID", "V003"),
                      by.y = c("HHID", "HVIDX"), all.x = TRUE)

  # CSALUD01 uses QSNUMERO. Check it against QS20C before joining. The dictionary calls it
  # the selected person's number; key-only proof that it is the roster line: no adult (15+)
  # sits on a child's line, while QSNUMERO shifted by one does on thousands
  stopifnot("QSNUMERO disagrees with QS20C, the selected person's code" =
              CSALUD01[!is.na(QS20C), all(QSNUMERO == QS20C)],
            "A selected adult sits on a child's line (CSALUD08)" =
              nrow(CSALUD01[CSALUD08, on = .(HHID, QSNUMERO = QS801), nomatch = NULL]) == 0,
            "A selected adult sits on a child's line (RECH6)" =
              nrow(CSALUD01[RECH6, on = .(HHID, QSNUMERO = HC0), nomatch = NULL]) == 0)
  assert_covered(CSALUD01, RECH1, c(HHID = "HHID", QSNUMERO = "HVIDX"), "CSALUD01")

  # Selected adult + their roster line and household. persons already carries RECH5's
  # anaemia and anthropometry
  adults <- merge(CSALUD01, persons[, !"ID1"], by.x = c("HHID", "QSNUMERO"),
                  by.y = c("HHID", "HVIDX"), all.x = TRUE)

  # Selected adult who is also an interviewed woman: her MEF modules
  full <- merge(adults, women[, !c("ID1", "UBIGEO")], by.x = c("HHID", "QSNUMERO"),
                by.y = c("HHID", "V003"), all.x = TRUE)

  # Child in the household: CSALUD08 (0-11 years) and RECH6 (under 5) both key on the
  # child's roster line
  assert_covered(CSALUD08, RECH1, c(HHID = "HHID", QS801 = "HVIDX"), "CSALUD08")
  assert_covered(RECH6, RECH1, c(HHID = "HHID", HC0 = "HVIDX"), "RECH6")
  children <- merge(CSALUD08, drop_shared(RECH6), by.x = c("HHID", "QS801"),
                    by.y = c("HHID", "HC0"), all = TRUE)
  children <- merge(children, persons[, !"ID1"], by.x = c("HHID", "QS801"),
                    by.y = c("HHID", "HVIDX"), all.x = TRUE)

  # A child only in RECH6 has no CSALUD08 row for the survey year and sample design.
  # QHCLUSTER, QHNUMBER and QHHOME are RECH0's HV001, HV002 and HV002A, so fill from there
  stopifnot("QHCLUSTER/QHNUMBER/QHHOME disagree with HV001/HV002/HV002A" =
              children[!is.na(QHCLUSTER),
                       all(QHCLUSTER == HV001 & QHNUMBER == HV002 & QHHOME == HV002A)])
  children[, `:=`(QHCLUSTER = fcoalesce(QHCLUSTER, HV001),
                  QHNUMBER  = fcoalesce(QHNUMBER, HV002),
                  QHHOME    = fcoalesce(QHHOME, HV002A))]
  children[RECH0, on = "HHID", ID1 := fcoalesce(ID1, i.ID1)]

  # Birth of an interviewed woman. DIT, REC41, REC43, REC44, REC94 and REC95 index REC21's
  # birth history (BIDX); a row with no birth in REC21 means that mapping is wrong.
  # The dictionaries describe MIDX, HIDX, HWIDX and IDX94 in BORD's words ("orden de
  # historia de nacimiento"), yet they match DIT's BIDX, not its BORD
  stopifnot("DIT's BORD disagrees with REC21's for the same BIDX" =
              DIT[REC21, on = c("CASEID", "BIDX"), nomatch = NULL, all(BORD == i.BORD)])
  assert_covered(REC21, REC0111, "CASEID", "REC21")
  birth_modules <- c("DIT", "REC41", "REC43", "REC44", "REC94", "REC95")
  births <- reduce(birth_modules, \(acc, m) {
    dt <- rekey(m, "BIDX")
    assert_covered(dt, REC21, c("CASEID", "BIDX"), m)
    # DIT's copy of BORD was checked above
    merge(acc, dt[, setdiff(names(dt), "BORD"), with = FALSE], by = c("CASEID", "BIDX"),
          all.x = TRUE)
  }, .init = REC21)

  # REC93DVdisciplina gives the child's birth-history line (Q1035NO), which REC21 stores as
  # BORD: Q1035NO matches DIT's BORD, not its BIDX
  discipline <- setnames(drop_shared(REC93DVdisciplina), "Q1035NO", "BORD")

  # Some REC93DVdisciplina women are missing from REC0111, so the left join from REC21
  # leaves their children out. Every other child must find its line in REC21
  in_rec0111 <- discipline$CASEID %in% REC0111$CASEID
  message("REC93DVdisciplina children whose mother is not in REC0111, left out: ",
          sum(!in_rec0111))
  stopifnot("REC21 repeats or lacks a birth-history line (CASEID + BORD)" =
              anyDuplicated(REC21, by = c("CASEID", "BORD")) == 0 && !anyNA(REC21$BORD),
            "REC93DVdisciplina repeats a birth-history line within a woman" =
              anyDuplicated(discipline, by = c("CASEID", "BORD")) == 0)
  assert_covered(discipline[in_rec0111], REC21, c("CASEID", "BORD"), "REC93DVdisciplina")
  births <- merge(births, discipline, by = c("CASEID", "BORD"), all.x = TRUE)

  # Woman's household and roster line, so births, siblings and calendar link back to
  # women and households
  mother <- REC0111[, .(CASEID, HHID, V003)]
  births <- merge(births, mother, by = "CASEID", all.x = TRUE)
  assert_covered(REC83, REC0111, "CASEID", "REC83")
  assert_covered(REC82, REC0111, "CASEID", "REC82")
  siblings <- merge(REC83, mother, by = "CASEID", all.x = TRUE)
  calendar <- merge(REC82, mother, by = "CASEID", all.x = TRUE)

  # Saving files before filtering for non-respondents. compiled is CSALUD01's selected
  # adult; every other table holds one row per unit of its own level
  endes_tables <- list(compiled = full, households = households, persons = persons,
                       women = women_full, children = children, births = births,
                       siblings = siblings, calendar = calendar, deaths = RECHM)

  # merge() leaves each table keyed and arrow stores that key in the file. A reader that
  # reorders rows would inherit a false key, so the files carry none
  walk(endes_tables, \(dt) {
    setkey(dt, NULL)
    setindex(dt, NULL)
  })
  stopifnot("A join left suffixed duplicate columns (.x, .y or i.)" =
              !any(grepl("\\.(x|y)$|^i\\.", unlist(map(endes_tables, names)))))

  iwalk(endes_tables, \(dt, name) {
    arrow::write_parquet(dt, sink = file.path(endes_proc, paste0(name, ".parquet")))
  })

  # The raw modules were only needed to build the tables; free their memory
  rm(list = names(endes_keys), envir = globalenv())
}