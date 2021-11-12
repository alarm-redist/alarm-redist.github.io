fifty_states <- function(analysis,
                         title = glue::glue("{censable::match_name(substr(analysis, 1, 2))} Congressional Districts"),
                         desc = glue::glue("Simulation Analysis for Congressional Districts in {substr(analysis, 1, 2)}")
) {
    abbr <- substr(analysis, 1, 2)
    fs::file_copy(path = 'fifty-states/DS_cd_2020.Rmd', glue::glue('fifty-states/{analysis}.Rmd'))
    file <- readLines(con = glue::glue('fifty-states/{analysis}.Rmd'))
    file[2] <- paste0("title: ", title)
    file[4] <- paste0("    ", desc)
    file[6] <- glue::glue("    analysis: '{abbr}_cd_2020'")
    writeLines(text = file, con = glue::glue('fifty-states/{analysis}.Rmd'))
}
