render_state_page = function(slug) {
    state_name = state.name[state.abb == substr(slug, 1, 2)]
    dir.create(here("_fifty-states", slug), showWarnings=FALSE)
    rmarkdown::render(here("_fifty-states/template.Rmd"),
                      params=list(slug=slug, state=state_name),
                      output_file=paste0(slug, "/index.html"))
    rmarkdown::render_site(here())
}
