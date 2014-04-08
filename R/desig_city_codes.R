#' @name desig_city_codes
#' @title Get the JIS codes and names for all Japanese 'designated
#' cities' at a given date.
#'
#' Designated cities (seireishiteitoshi) are the major cities, as
#' designated by a government ordinance, that are subdivided into
#' non-autonomous wards (ku). Tokyo is not a designated city, but a
#' prefecture comprising (among other municipalities) 23 city-level
#' so-called special wards.
#' 
#' @param date a Date object.
#' @return JIS codes and names of all Japanese "ordinance-designated
#' cities" (a list of major cities other than Tokyo) at the given
#' date.
#' @export
desig_city_codes <- function(date = Sys.Date()) {
    if (class(date) != 'Date') date <- as.Date(date)
    cached(paste0('des_city_codes_', date), function() {
        query <- paste0("SELECT ?code ?label",
                        " WHERE {\n",
                        "?item rdfs:label ?label ;\n",
                        "      dcterms:identifier ?code ;\n",
                        "      sacs:administrativeClass sacs:DesignatedCity ;\n",
                        "      dcterms:issued ?from .\n",
                        "OPTIONAL { ?item dcterms:valid ?to } .\n",
                        "FILTER (?from <= \"", date,
                        "T00:00:00+09:00\"^^xsd:date) .\n",
                        "FILTER (!bound(?to) || ?to >= \"", date,
                        "T00:00:00+09:00\"^^xsd:date) .\n",
                        "}")
        res <- (SPARQL(endpoint, query, curl_args=list(.encoding="UTF-8")))$result
        res <- nstac_sparql_split_langs(res, 'label')
        res$prefecture.ja <- extract_prefectures(res$code, as.names = TRUE)
        res
    })
}

## part of my toolchain is choking on UTF-8 R code, I don't know why.
## 政令指定都市コード一覧 <- desig_city_codes
