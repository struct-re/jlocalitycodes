#' @name lookup_codes
#' @title Look up information on specific JIS prefecture and
#' municipality codes.
#'
#' @param codes A vector of JIS prefecture or municipality codes.
#' @param date A Date object.
#' @return A data frame like that returned by
#' \code{\link{lookup_names}}.
#' @export
lookup_codes <- function(codes, date=Sys.Date()) {
    stopifnot(all(is_valid_code(codes)))
    if (class(date) != 'Date') date <- as.Date(date)

    ## Preprocess codes:
    res <- data.frame(code = NULL)
    ## any prefecture names requested? handle them quickly
    prefs <- prefecture_codes()
    codes.prefs <- grep("000$", codes)
    if (length(codes.prefs) > 0) {
        pref.data         <- prefs[prefs$code %in%
                                   substr(codes[codes.prefs], 1, 2), ]
        pref.data$code    <- paste0(pref.data$code, "000")
        pref.data$full.ja <- pref.data$prefecture.ja <- pref.data$label.ja

        res   <- rbind.fill(res, pref.data)
        codes <- codes[-codes.prefs]
        if (length(codes) == 0) {
            return(res)
        }
    }

    ## Build and send SPARQL query:
    query <- paste0("SELECT ?code ?label ?container",
                    " WHERE {\n{",
                    paste0("?item dcterms:identifier \"", codes,
                           collapse="\"} UNION {"),
                    "\"} .\n",
                    "?item rdfs:label ?label ;\n",
                    "      dcterms:identifier ?code ;\n",
                    "      dcterms:isPartOf ?cont ;\n",
                    "      dcterms:issued ?from .\n",
                    "?cont rdfs:label ?container .\n",
                    "OPTIONAL { ?item dcterms:valid ?to } .\n",
                    "FILTER (?from <= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    "FILTER (!bound(?to) || ?to >= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    "FILTER (langMatches(lang(?container), \"ja\") &&",
                    "(!langMatches(lang(?container), \"ja-hrkt\"))) .\n",
                    "}\n",
                    "ORDER BY ?code")
    res2 <- nstac_sparql_send(query)
    
    if (nrow(res2) > 0) {
        ## cleanup
        res2 <- unique(res2)
        res2 <- nstac_sparql_split_langs(res2, 'label')
        res2 <- with(res2, data.frame(
            code            = code,
            label.ja        = label.ja,
            label.en        = label.en,
            'label.ja-hrkt' = res2[, 'label.ja-hrkt'],
            subunit.ja      = gsub('(\"|@.+$)', '', container),
            prefecture.ja   = prefs$label.ja[match(substr(code, 1, 2), prefs$code)],
            check.names = FALSE))

        res2$subunit.ja[grep('(郡|市)', res2$subunit.ja, invert=TRUE)] <- NA
        res2$full.ja <- with(res2, paste00(prefecture.ja, subunit.ja, label.ja))
        
        ## merge `res2` with `res`
        res <- rbind.fill(res, res2)
    }

    res
}
