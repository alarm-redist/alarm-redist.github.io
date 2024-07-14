lapply(cli::cli_progress_along(state.abb), function(x) {

  state <- state.abb[x]
  if (state %in% c('AK', 'DE', 'VT', 'ND', 'SD', 'WY')) {
    return(NULL)
  }
  fs::dir_create(glue::glue("fifty-states/{state}_cd_2020"))
  #setwd(glue::glue("fifty-states/{state}_cd_2020"))


  fs::file_copy(
    'fifty-states/template.qmd',
    glue::glue("fifty-states/{state}_cd_2020/index.qmd"),
    overwrite = TRUE
  )

  readLines(glue::glue("fifty-states/{state}_cd_2020/index.qmd")) |>
    stringr::str_replace('AL_cd_2020', glue::glue("{state}_cd_2020")) |>
    stringr::str_replace('Alabama', state.name[x]) |>
    xfun::write_utf8(glue::glue("fifty-states/{state}_cd_2020/index.qmd"))
})
