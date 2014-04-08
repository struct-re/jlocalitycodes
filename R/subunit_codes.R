#' @name subunit_codes
#' @title Get information on all sub-units of a locality
#' @param code A single JIS prefecture or municipality code.
#' @param date A Date object.
#' @param class Administrative class to filter for. Omit the ‘sacs:’
#' prefix.
#' @param sub.prefs Logical. Whether to include sub-prefectures
#' (shichou, shinkoukyoku) in the search. They are excluded by
#' default.
#' @return A data frame.
subunit_codes <- function(code, date = Sys.Date(), class = NULL, sub.prefs = FALSE) {
    stopifnot(is_valid_code(code))
    if (class(date) != 'Date') date <- as.Date(date)
    stopifnot(is.null(class) | length(class) == 1 & is_valid_class(class))
    if (nchar(code) == 2) code <- paste0(code, '000')

    ## Build and send SPARQL query:
    query <- paste0("SELECT DISTINCT ?code ?label ?class ?city WHERE {",
                    "?item rdfs:label ?label ;\n",
                    "      dcterms:identifier ?code ;\n",
                    "      sacs:administrativeClass ?class ;\n",
                    "      dcterms:issued ?from ;\n",
                    "      dcterms:isPartOf ?x .\n",
                    "{ { ?x dcterms:identifier \"", code, "\" }\n",
                    "  UNION\n",
                    "  { ?x dcterms:isPartOf ?y .\n",
                    "    ?y dcterms:identifier \"", code, "\" }\n",
                    "} .\n",
                    "OPTIONAL {\n",
                    "  ?x sacs:administrativeClass sacs:DesignatedCity ;\n",
                    "     rdfs:label ?city } .\n",
                    "OPTIONAL { ?item dcterms:valid ?to } .\n",
                    ifelse(sub.prefs,
                           "",
                           "FILTER (?class != sacs:SubPrefecture)\n"),
                    ifelse(is.null(class),
                           "",
                           paste0("FILTER (?class = sacs:", class, ")\n")),
                    "FILTER (!BOUND(?city) || LANG(?city) = \"ja\")",
                    "FILTER (?from <= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    "FILTER (!bound(?to) || ?to >= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    "}\n")
    res <- nstac_sparql_send(query)

    ## postprocessing
    res <- nstac_sparql_split_langs(res, 'label')
    res$class <- strip_sacs_namespace(res$class)
    res$city.ja  <- strip_lang_tagging(res$city)
    ## workaround to avoid the 250-row limit
    dccodes <- desig_city_codes()
    res[, 'city.en']      <- dccodes[match(res$city.ja, dccodes$label.ja),
                                     'label.en']
    res[, 'city.ja-hrkt'] <- dccodes[match(res$city.ja, dccodes$label.ja),
                                     'label.ja-hrkt']
    res[, 'city'] <- NULL

    ## reorder columns
    res <- res[c('code', 'label.ja', 'label.en', 'label.ja-hrkt',
                 'class', 'city.ja', 'city.en', 'city.ja-hrkt',
                 'label.ja.display', 'label.en.display')]

    res
}
