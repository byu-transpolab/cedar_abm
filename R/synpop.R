#' Run populationsim
#'
run_populationsim <- function(write_result, data_path, out_path){

  out <- system2("sh/runpopsim.sh")
  if(out != 0){
    stop("Error running popsim")
  } else {
    p_file <- file.path(out_path, "synthetic_persons.csv")
    return(p_file)
  }

}

#' Write out populationsim files
#'
#'
#' @param meta
#' @param tract_controls
#' @param bg_control
#' @param bg
#' @param seed
#' @param path Path to output folder
#'
write_popsim <- function(meta, tract_controls, bg_control, bg, seed, path){
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  # Controls
  write_csv(meta, file.path(path, "control_totals_meta.csv"))
  write_csv(tract_controls, file.path(path, "control_totals_tract.csv"))
  write_csv(bg_control |> rename(BLOCKGROUP = GEOID), file.path(path, "control_totals_bg.csv"))

  # Seed
  write_csv(seed$households, file.path(path, "seed_households.csv"))
  write_csv(seed$persons, file.path(path, "seed_persons.csv"))

  # Crosswalk
  bg |>
    transmute(
      BLOCKGROUP = GEOID,
      TRACT = substr(GEOID, 1, 11),
      PUMA,
      REGION = 1
    ) |>
    write_csv(file.path(path, "crosswalk.csv"))

  return(TRUE)
}


#' Make geographic crosswalk file
#'


#' Make seed tables from input data
#'
#' @param pp_seed_file path to a persons seed file
#' @param crosswalk
#'
#'
make_seed <- function(pp_seed_file, pumas){

  pums_files <- dir("data/pums")
  # Get hh pums files -----
  h_files <- pums_files[substr(pums_files, 1, 1) == "h"]
  seed_hh <- lapply(h_files, function(h){
    read_csv(
      file.path("data/pums", h),
      col_types = list(SERIALNO = col_character(), NP = col_integer(),
                       PUMA = col_character(),
                       FINCP = col_number(), ADJINC = col_number(),
                       WGTP = col_number())
    ) |>
      mutate(PUMA = str_c(ST, PUMA)) |>
      # remove households from outside the region
      filter(PUMA %in% pumas)
  }) |>
    bind_rows() |>
    # remove empty households
    filter(NP > 0) |> filter(WGTP > 0) |>
    mutate(
      # create unique hh_id field
      hh_id = row_number(),
      # compute adjusted numeric income
      HHINCADJ = FINCP * ADJINC/10^6
    ) |>
    # apply replacement function to key variables
    mutate_at(.vars = vars(NP, WIF, WGTP, HHINCADJ), replace_na)


  # Get per pums files -----
  p_files <- pums_files[substr(pums_files, 1, 1) == "p"]
  seed_per <- lapply(p_files, function(p){
    pums_persons <- read_csv(
      file.path("data/pums", p),
      col_types = list(SERIALNO = col_character(), PWGTP = col_number())
    )
  }) |>
    bind_rows() |>

    # join hhid field, and only keep households we filtered down to.
    inner_join(seed_hh |> select(SERIALNO, hh_id, WGTP)) |>
    # replace NA values with something less stupid
    mutate_at(.vars = vars(PWGTP, AGEP), replace_na)


  list("persons" = seed_per, "households" = seed_hh)
}


get_nhh_controls <- function(acsvars, mycounties){

  raw_hh <- get_acs("block group", variables = "B25001_001",
          state = substr(mycounties, 1, 2),
          county = substr(mycounties, 3, 5), year = 2019)

  raw_hh |>
    select(GEOID, numhh = estimate)

}

#' A function to replace NA values with non-missing stupid numbers
#'
#'
replace_na <- function(x) {
  ifelse(is.na(x),-8,x)
}

#' Get size work variables
#'
#' @param acvars
#' @param mycounties
#'
#' Because these two values come in the same table, we will build them together.
#' In both cases the top-line category contains all households with that many
#' workers / persons or more.
#'
get_sizework_controls <- function(acsvars, mycounties){
  swvars <- str_c("B08202_", sprintf("%03d", c(2:5, 6, 9, 13, 18)))
  raw_sw <- get_acs("tract", variables = swvars, state = substr(mycounties, 1, 2),
                    county = substr(mycounties, 3, 5), year = 2019)

  size_work <- raw_sw |>
    left_join(acsvars, by = c("variable" = "name")) |>
    separate(label, c("VAR", "total", "label"), sep = "!!") |>
    select(GEOID, label, estimate)

  works <- size_work |>
    filter(grepl("work", label)) |>
    mutate(
      num_work = str_extract(label, "\\d+"),
      workcat = case_when(
        num_work == 1 ~ "HHWORK1",
        num_work == 2 ~ "HHWORK2",
        num_work == 3 ~ "HHWORK3",
        TRUE ~ "HHWORK0"
      )
    ) |>
    group_by(GEOID, workcat) |> summarize(count = as.integer(sum(estimate)))

  sizes <- size_work |>
    filter(!grepl("work", label)) |>
    mutate(
      num_size = str_extract(label, "\\d+"),
      sizecat = str_c("HHSIZE", num_size)
    ) |>
    group_by(GEOID, sizecat) |> summarize(count = as.integer(sum(estimate)))

  list( "sizes" = sizes, "works" =  works )
}

#' Get age variables
#'
#' @param acsvars
#' @param mycounties
#'
#' This is the number of people in each age category.
#'
get_age_controls <- function(acsvars, mycounties){
  agevars <- str_c("B01001_", sprintf("%03d", c(3:25, 27:49)))
  raw_ages <- get_acs("tract", variables = agevars, state = substr(mycounties, 1, 2),  county = substr(mycounties, 3, 5), year = 2019)

  ages <- raw_ages |>
    left_join(acsvars, by = c("variable" = "name")) |>
    separate(label, c("VAR", "total", "sex", "age"), sep = "!!") |>
    select(GEOID, sex, age, estimate)  |>

    # regroup age categories
    mutate(
      numage = as.numeric(substr(age, 1, 2)),
      agecat = case_when(
        numage %in% c(15:24) ~ "PAGE1",
        numage %in% c(25:54) ~ "PAGE2",
        numage %in% c(55:64) ~ "PAGE3",
        numage %in% c(65:99) ~ "PAGE4",
        TRUE ~ "PAGE0" # children less than 15 not categorized in demo
      )
    ) |>

    # consolidate men and women
    group_by(GEOID, agecat) |>
    summarise(count = as.integer(sum(estimate)))

  ages
}

#' Get age variables
#'
#' @param acsvars
#' @param mycounties
#'
#' This is the household income variable, which is categorized as follows:
#'   - <$15k
#'   - \$15k - \$30k
#'   - \$30k - \$60k
#'   - > $60k
#'
get_income_controls <- function(acsvars, mycounties){
  incvars <- str_c("B19001_", sprintf("%03d", c(2:17)))
  raw_incs <- get_acs("tract", variables = incvars, state = substr(mycounties, 1, 2),  county = substr(mycounties, 3, 5), year = 2019)

  incs <- raw_incs |>
    left_join(acsvars, by = c("variable" = "name")) |>
    separate(label, c("VAR", "total", "income"), sep = "!!") |>
    select(GEOID, income, estimate)  |>
    # regroup income categories
    mutate(
      numinc  = stringr::str_extract(income, "\\d+"),
      inccat = case_when(
        numinc <  15 ~ "HHINC1",
        numinc <  30 ~ "HHINC2",
        numinc <  60 ~ "HHINC3",
        numinc >= 60 ~ "HHINC4",
        TRUE ~ as.character(NA)
      )
    ) |>
    group_by(GEOID, inccat) |>
    summarise(count = as.integer(sum(estimate)))

  incs
}

#' Get age variables
#'
#' @param mytracts
#' @param ages
#' @param incs
#' @param sizes includes workers
#'
#' @details  When all of the controls have been gathered, we can put them into one large table.
#'
make_controls <- function(mytracts, ages, incs, sizes){
  tibble(TRACT = mytracts) |>
    left_join(ages  |> spread(agecat,  count), by = c("TRACT" = "GEOID")) |>
    left_join(incs  |> spread(inccat,  count), by = c("TRACT" = "GEOID")) |>
    left_join(sizes$works |> spread(workcat, count), by = c("TRACT" = "GEOID")) |>
    left_join(sizes$sizes |> spread(sizecat, count), by = c("TRACT" = "GEOID"))
}

#' make meta information
#'
#' @param tract_controls
#' PopulationSim requires (we think) at least some region-level controls. We will
#' simply sum up the total population in the controls data to work with this.
get_meta <- function(tract_controls){
  tract_controls |>
    summarise(
      REGION = 1,
      totalPOP = sum(PAGE0) +  sum(PAGE1) +  sum(PAGE2) +
        sum(PAGE3) +  sum(PAGE4))
}
