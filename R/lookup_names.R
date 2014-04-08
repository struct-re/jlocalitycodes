#' @name lookup_names
#' @title Look up JIS prefecture or municipality codes matching a set
#' of place names.
#'
#' @details Look up the local authority codes for each place name in a
#' vector at a given date (default: today). If more than one code is
#' found for a given place name, they are all included in the return
#' value, but a warning is output. To enable filtering, provide
#' unambiguous place names in Japanese script, including the
#' prefecture and where applicable, the district (gun) or designated
#' city (see \code{\link{desig_city_codes}}).
#' @param names Vector of \code{character}. Locality names can be
#' given in Japanese or Roman script (macrons omitted!) as long as the
#' suffix is included, eg ‘Akiruno-shi‘.
#' @param date Date object. If omitted, today’s date will be used.
#' @return A data frame.
#' @export
lookup_names <- function(names, date=Sys.Date()) {

    ## Auxiliary functions
    ## ===================

    filter_duplicates <- function(res, extra.info) {
        dupes <- unique(res$label.ja[duplicated(res$label.ja)])
        if (length(dupes) > 0 & nrow(extra.info) > 0) {
            for (d in dupes) {
                requested <- extra.info[!is.na(extra.info$name) &
                                        extra.info$name == d, ]
                if (nrow(requested) > 0) {
                    ## delete non-requested duplicates of name `d`
                    bad <- (res$label.ja == d & !(res$full.ja %in% requested$full))
                    ## debug:
                    ## message("Deleting spurious results: ",
                    ##         paste(res[bad, 'full.ja'], collapse = ", "))
                    res <- res[!bad, ]
                }
            }
        }
        res
    }

    ## FIXME also parse those given as, eg "Nagoya-shi Higashi-ku"
    parse_full_names <- function(names) {
        search <- regmatches(names, regexec(
            "(.+[都道府県])(.+[郡市]|)(.+[市区町村])$",
            names))
        info <- data.frame(
            name = rep(NA, length(names)),
            pref = rep(NA, length(names)),
            dist = rep(NA, length(names)),
            full = rep(NA, length(names))
            )
        for (i in seq_along(search)) {
            matches <- search[[i]]
            if (length(matches) > 0) {
                info$name[i] <- matches[4]
                info$pref[i] <- matches[2]
                if (nchar(matches[3]) > 0) {
                    info$dist[i] <- matches[3]
                }
            }
        }
        info$full <- with(info, paste00(pref, dist, name, na.rm = TRUE))
        info
    }

    ## Main code
    ## =========

    if (class(date) != 'Date') {
        date <- as.Date(date)
    }

    prefs <- prefecture_codes()

    ## Preprocess names:
    res <- data.frame(code = NULL)

    ## any prefecture codes requested? handle them quickly
    names.prefs <- grep("[都道府県]$", names)
    if (length(names.prefs) > 0) {
        pref.data <- prefs[prefs$label.ja %in% names[names.prefs], ]
        pref.data$code <- paste0(pref.data$code, "000")
        pref.data$full.ja <- pref.data$prefecture.ja <- pref.data$label.ja
        res   <- rbind.fill(res, pref.data)
        names <- names[-names.prefs]
    }

    if (length(names) == 0) return(res)

    ## any wards of designated cities that are missing the prefecture
    ## name? if yes, auto-complete
    dcward.re   <- "^([^都道府県]+市).+区$"
    dcwards.idx <- grep(dcward.re, names)
    if (length(dcwards.idx) > 0) {
        dcwards     <- regmatches(names, regexec(dcward.re, names))
        city.names  <- lapply(dcwards[dcwards.idx], function (x) x[2])
        dcities     <- desig_city_codes(date)
        names[dcwards.idx] <- paste0(
            dcities$prefecture.ja[match(city.names, dcities$label.ja)],
            names[dcwards.idx])
    }

    ## reduce the fully-specified names, saving the information for
    ## later filtering
    extra.info       <- parse_full_names(names)
    shortened.idx    <- !is.na(extra.info$name)
    names[shortened.idx] <- extra.info[shortened.idx, 'name']

    ## done, "names" contains only potential municipality labels
    stopifnot(length(grep("[都道府県]", names)) == 0)

    ## FIXME: more efficient filtering by including the containing
    ## district/desig-city label inside the query, where possible,
    ## which would reduce the risk of hitting the endpoint's 250-row
    ## hard limit.

    ## Build and send SPARQL query:
    query <- paste0("SELECT DISTINCT ?code ?label ?subunit",
                    " WHERE {\n{",
                    paste0("?item rdfs:label \"", unique(names),
                           collapse="\"} UNION {"),
                    "\"} .\n",
                    "?item rdfs:label ?label ;\n",
                    "      dcterms:identifier ?code ;\n",
                    "      dcterms:isPartOf ?cont ;\n",
                    "      dcterms:issued ?from .\n",
                    "?cont rdfs:label ?subunit .\n",
                    "OPTIONAL { ?item dcterms:valid ?to } .\n",
                    "FILTER (?from <= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    "FILTER (!bound(?to) || ?to >= \"", date,
                    "T00:00:00+09:00\"^^xsd:date) .\n",
                    #"FILTER (langMatches(lang(?subunit), \"ja\") &&",
                    #"(!langMatches(lang(?subunit), \"ja-hrkt\"))) .\n",
                    "}\n",
                    "ORDER BY ?code")
    res2 <- nstac_sparql_send(query)

    if (nrow(res2) > 0) {
        ## cleanup
        res2 <- unique(res2)
        res2 <- nstac_sparql_split_langs(res2, 'label')
        res2 <- nstac_sparql_split_langs(res2, 'subunit')
        p.ix <- match(extract_prefectures(res2$code), prefs$code)
        ## add some columns, fix the order of the existing columns
        res2 <- with(res2, data.frame(
            code            = code,
            label.ja        = label.ja,
            label.en        = label.en,
            'label.ja-hrkt' = res2[, 'label.ja-hrkt'],
            subunit.ja      = subunit.ja,
            subunit.en      = subunit.en,
            'subunit.ja-hrkt' = res2[, 'subunit.ja-hrkt'],
            prefecture.ja   = prefs[p.ix, 'label.ja'],
            prefecture.en   = prefs[p.ix, 'label.en'],
            'prefecture.ja-hrkt' = prefs[p.ix, 'label.ja-hrkt'],
            check.names = FALSE))
        res2[grep('(郡|市)', res2$subunit.ja, invert=TRUE),
             c('subunit.ja', 'subunit.en', 'subunit.ja-hrkt')] <- c(NA, NA, NA)
        #'subunit.ja'] <- NA
        res2$full.ja <- with(res2, paste00(prefecture.ja, subunit.ja, label.ja))

        ## merge `res2` with `res`
        res <- rbind.fill(res, res2)
        
        ## filter out any accidental duplicates (ie those for which a
        ## disambiguating JA name was given)
        res <- filter_duplicates(res, extra.info)

        ## any more duplicates left? throw a warning
        dupes2 <- duplicated(res$label.en) |
            duplicated(res$label.en, fromLast = TRUE)
        if (any(dupes2)) {
            warning(paste0(
                "Some place names were shared by several localities:\n  - ",
                paste00(res[dupes2, 'code'], ' ',
                        res[dupes2, 'label.en'], ' (',
                        res[dupes2, 'prefecture.en'],
                        ifelse(is.na(res[dupes2, 'subunit.en']), '', ' '),
                        res[dupes2, 'subunit.en'],
                        ')',
                        collapse="\n  - "),
                "\n",
                "  If these are not the desired results, ",
                "try specifying potentially\n  ambiguous ",
                "locality names in full in Japanese script",
                ", by including the\n  prefecture and any ",
                "district (郡) or designated city ",
                "(政令指定都市) names."))
        }
    }

    res
}

lookup_name <- function(name, date = Sys.Date()) {
    stopifnot(length(name) == 1)
    lookup_names(name, date)
}
