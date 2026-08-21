load_eplet_registry <- function(eplets_path = '[file path to save eplets table]',
                                filename = "eplets",
                                print_version = TRUE,
                                delete = FALSE) {

  file_path <- paste0(eplets_path, '/', filename, '.rds')

  if (delete) {
    unlink(file_path)
    return(invisible())
  }

  if (file.exists(eplets_path)) {
    df_eplet <- readRDS(file_path)
    if (print_version) {
      message_version(df_eplet)
    }

    return(invisible(df_eplet))
  }

  if (rlang::is_interactive() && scrape_permission() == 2) {
    return(invisible())
  }

  df_eplet <- scrape_eplet_registry(eplets_path, filename)
  if (print_version) {
    message_version(df_eplet)
  }
  invisible(df_eplet)
}

fetch_registry_version <- function() {
  registry_url <- "https://www.epregistry.com.br"
  version_text <- rvest::read_html(registry_url) |>
    rvest::html_elements("#description > h2 > div > h4") |>
    rvest::html_text2()

  invisible(c(
    date = stringr::str_extract(
      version_text,
      r"((?<=Software version: )[\d-]+)"
    ),
    db = stringr::str_extract(
      version_text,
      r"((?<=IPD-IMGT/HLA: )[\d.]+)"
    ),
    url = registry_url
  ))
}

message_version <- function(df_eplet) {
  message(
    stringr::str_glue(
      "Loaded Eplet Registry table\n",
      "IPD-IMGT/HLA version {attr(df_eplet, 'db')} ({attr(df_eplet, 'date')}),",
      "\ndownloaded from {attr(df_eplet, 'url')}"
    )
  )
}

scrape_permission <- function() {
  q_title <- paste(
    "Do you want to download the HLA Eplet Registry tables",
    "(to lookup which eplets occur on which alleles, and vice versa?)"
  )

  utils::menu(choices = c("Yes", "No"), title = q_title)
}

scrape_eplet_registry <- function(eplets_path, filename) {
  base_url <- "https://www.epregistry.com.br/databases/"
  locus_groups <- c("ABC", "DRB", "DQ", "DP", "DRDQDP")
  # CSS selector paths to the individual columns
  # (scraping entire table with rvest::html_table resulted in misaligned rows/
  # columns)
  base_path <- "#table-result > div > table > tbody > tr > td:nth-child"
  col_paths <- c(
    id = "(1)",
    name = "(2)",
    description = "(3)",
    evidence = "(4)",
    exposition = "(5)",
    status = "(6)",
    alleles = "(9)"
  )
  col_paths[] <- paste0(base_path, col_paths)

  scrape_column <- function(page_html, col_path) {
    rvest::html_elements(page_html, col_path) |>
      rvest::html_text2()
  }

  # scrape all columns, add each to a list, and store the database used
  scrape_table <- function(base_url, locus_group, col_paths) {
    Sys.sleep(0.5) # wait a little between scrapes
    page_html <- rvest::read_html(paste0(base_url, locus_group))

    purrr::map(col_paths, \(x) scrape_column(page_html, x)) |>
      purrr::list_assign(locus_group = locus_group)
  }

  # for each locus group (i.e. page), scrape all columns, store in another list
  df <- purrr::map(locus_groups,
    \(x) scrape_table(base_url, x, col_paths),
    .progress = "Collecting tables from HLA Eplet Registry website"
  ) |>
    purrr::map(tidyr::as_tibble) |> # make a dataframe out of each scraped db
    purrr::list_rbind() |> # combine into one dataframe
    dplyr::mutate(residue_type = dplyr::case_when(
      stringr::str_detect(.data$name, "\\+") ~ "reactivity pattern",
      .default = "eplet"
    ), .after = "name") |>
    # de-duplicate duplicate eplet names by adding locus group in []
    dplyr::add_count(.data$name, name = "n_name") |>
    dplyr::mutate(name = dplyr::if_else(.data$n_name > 1,
      stringr::str_c(.data$name, "[", .data$locus_group, "]"),
      .data$name
    )) |>
    dplyr::group_by(.data$id) |> # one row per allele
    tidyr::separate_longer_delim("alleles", delim = ",") |>
    dplyr::filter(.data$alleles != "") |> # get rid of trailing comma
    # clean up whitespace at start/end
    dplyr::ungroup() |>
    dplyr::select(!c("n_name")) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~ stringr::str_trim(.x)
    )) |>
    # exposition is empty string for reactivity patterns
    dplyr::mutate(exposition = dplyr::na_if(.data$exposition, "")) |>
    # evidence is empty for eplets not in paper
    dplyr::mutate(evidence = dplyr::na_if(.data$evidence, ""))

  registry_info <- fetch_registry_version()
  attr(df, "date") <- registry_info[["date"]]
  attr(df, "db") <- registry_info[["db"]]
  attr(df, "url") <- registry_info[["url"]]

  openxlsx::write.xlsx(df, paste0(eplets_path, '/', filename, '.xlsx'),overwrite = TRUE)
  df
}