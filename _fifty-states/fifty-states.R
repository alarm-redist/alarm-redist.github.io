PAL_COAST = c("#7BAEA0", "#386276", "#3A4332", "#7A7D6F", "#D9B96E", "#BED4F0")
PAL_LARCH = c("#D2A554", "#626B5D", "#8C8F9E", "#858753", "#A4BADF", "#D3BEAF")
PAL = PAL_COAST[c(5, 1, 2, 4, 3, 6)]
GOP_DEM <- c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0",
             "#EBC2BC",  "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA",
             "#9FB6DE", "#82A0D2", "#638BC6", "#3D77BB", "#0063B1")

lbl_party = function(x) {
    if_else(x == 0.5, "Even",
            paste0(if_else(x < 0.5, "R+", "D+"), number(200*abs(x-0.5), 1)))
}

scale_fill_party_b <- function(name="Democratic share", midpoint=0.5, limits=0:1,
                               labels=lbl_party, oob=squish, ...) {
    scale_fill_steps2(name=name, ..., low = GOP_DEM[1], high = GOP_DEM[15],
                      midpoint=midpoint, limits=limits, labels=labels, oob=oob)
}
scale_fill_party_c <- function(name="Democratic share", midpoint=0.5, limits=0:1,
                               labels=lbl_party, oob=squish, ...) {
    scale_fill_gradient2(name=name, ..., low = GOP_DEM[1], high = GOP_DEM[15],
                         midpoint=midpoint, limits=limits, labels=labels, oob=oob)
}
scale_color_party_c <- function(name="Democratic share", midpoint=0.5, limits=0:1,
                                labels=lbl_party, oob=squish, ...) {
    scale_color_gradient2(name=name, ..., low = GOP_DEM[1], high = GOP_DEM[15],
                          midpoint=midpoint, limits=limits, labels=labels, oob=oob)
}
scale_color_party_d = function(...) {
    scale_color_manual(..., values=c(GOP_DEM[2], GOP_DEM[14]),
                       labels=c("Rep.", "Dem."))
}

#' Plot Congressional Districts
#'
#' @param map redist_map object
#' @param pl redist_plans object
#' @param county unqouted county name
#' @param abbr string, state abbreviation
#' @param city boolean. Plot cities? Default is FALSE.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' #TODO
plot_cds = function(map, pl, county, abbr, qty="plan", city=FALSE, coverage=TRUE) {
    if (n_distinct(pl) > 6)
        plan = redist:::color_graph(get_adj(map), as.integer(pl))
    else
        plan = pl
    places = suppressMessages(tigris::places(abbr, cb=TRUE))
    if (city) {
        cities = arrange(places, desc(ALAND)) %>%
            filter(LSAD == "25") %>%
            head(4) %>%
            st_centroid() %>%
            suppressWarnings()
    }

    if (qty == "dem") {
        qty = expr(dem)
        scale = scale_fill_party_b("Two-party\nvote margin", limits=c(0.35, 0.65))
    } else {
        qty = expr(.plan)
        scale = scale_fill_manual(values=PAL, guide="none")
    }

    cty_val = rlang::eval_tidy(rlang::enquo(county), map)
    if (n_distinct(cty_val) == nrow(map)) county = 1L

    counties = map %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by({{ county }}) %>%
        summarize(is_coverage=coverage)
    map %>%
        mutate(.plan = as.factor(plan),
               .distr = as.integer(pl),
               dvote = map$ndv,
               rvote = map$nrv) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(.plan = .plan[1],
                  dem = 1 / (1 + sum(rvote, na.rm=T) / sum(dvote, na.rm=T)),
                  is_coverage=coverage) %>%
    ggplot(aes(fill={{ qty }})) +
        geom_sf(size=0.0) +
        geom_sf(data=places, inherit.aes=FALSE, fill="#00000033", color=NA) +
        geom_sf(fill=NA, size=0.4, color="black") +
        geom_sf(data=counties, inherit.aes=FALSE, fill=NA, size=0.45, color="#ffffff2A") +
        {if (city) ggrepel::geom_text_repel(aes(label=NAME, geometry=geometry),
                                            data=cities, color="#000000", fontface="bold",
                                            size=3.5, inherit.aes=FALSE, stat="sf_coordinates")} +
        scale +
        theme_void() +
        theme(legend.key.size=unit(0.75, "cm"))
}

download_dataverse = function(slug) {
    files <- dataset_files(doi)
    file_names <- map_chr(files, function(x) x$label)

    if (!any(str_detect(file_names, slug))) stop("`", slug, "` not available yet.")

    load_file = function(suffix="stats.tab") {
        ext = strsplit(suffix, "\\.")[[1]][2]
        if (ext == "tab") ext = "csv"

        tf <- tempfile(fileext=ext)
        get_file_by_name(filename=paste0(slug, "_", suffix), dataset=doi) %>%
            writeBin(tf)

        if (ext == "rds") {
            read_rds(tf)
        } else if (ext == "csv") {
            read_csv(tf, col_types=cols(draw="f"), show_col_types=FALSE)
        } else if (ext == "html") {
            rvest::read_html(tf, encoding="utf-8")
        } else {
            tf
        }
    }

    map = load_file("map.rds")
    plans = load_file("plans.rds")
    stats = load_file("stats.tab")
    doc = load_file("doc.html")
    plans = left_join(plans, stats, by=c("draw", "district", "total_pop"))

    list(map=map, plans=plans, doc=doc)
}


render_state_page = function(slugs) {
    walk(slugs, function(slug) {
        state_name = state.name[state.abb == substr(slug, 1, 2)]
        dir.create(here("_fifty-states", slug), showWarnings=FALSE)
        rmarkdown::render(here("_fifty-states/template.Rmd"),
                          params=list(slug=slug, state=state_name),
                          output_file=paste0(slug, "/index.html"),
                          quiet=TRUE)
        rmarkdown::render_site(here(), quiet=TRUE)
        cat("Rendered `", slug, "`\n", sep="")
    })
}
