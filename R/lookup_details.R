#' @name lookup_details
#' @title Look up detailed information on a singe JIS prefecture or
#' municipality code.
#'
#' @param code A single JIS prefecture or municipality code.
#' @return A data frame with an extensive summary of the information
#' about the given locality code contained in the NSTAC database.
#' @importFrom SPARQL SPARQL
#' @importFrom plyr rbind.fill
#' @export
lookup_details <- function(code) {
    stopifnot(is_valid_code(code))

    query <- paste0("SELECT DISTINCT ?code ?label ?class ",
                    "?location ?locationCode ?locationClass ?from ?to ",
                    "?formerly ?formerCode ?later ?laterCode ",
                    "?creationEvent\n",
                    "WHERE {\n",
                    "?item rdfs:label ?label ;\n",
                    "      dcterms:identifier \"", code, "\" ;\n",
                    "      dcterms:identifier ?code ;\n",
                    "      sacs:administrativeClass ?class ;\n",
                    "      dcterms:issued ?from .\n",
                    "OPTIONAL { ?item dcterms:isPartOf ?x .\n",
                    "           ?x rdfs:label ?location ;\n",
                    "              dcterms:identifier ?locationCode ;\n",
                    "              sacs:administrativeClass ?locationClass } .\n",
                    "OPTIONAL { ?item org:resultedFrom ?e .\n",
                    "           ?e rdfs:label ?creationEvent } .\n",
                    "OPTIONAL { ?item sacs:previousMunicipality ?p .\n",
                    "           ?p rdfs:label ?formerly ;\n",
                    "              dcterms:identifier ?formerCode } .\n",
                    "OPTIONAL { ?item sacs:succeedingMunicipality ?s .\n",
                    "           ?s rdfs:label ?later ;\n",
                    "              dcterms:identifier ?laterCode } .\n",
                    "OPTIONAL { ?item dcterms:valid ?to } .\n",
                    "FILTER (!bound(?formerly) || (langMatches(lang(?formerly), \"ja\") &&",
                    "(!langMatches(lang(?formerly), \"ja-hrkt\")))) .\n",
                    "FILTER (!bound(?later) || (langMatches(lang(?later), \"ja\") &&",
                    "(!langMatches(lang(?later), \"ja-hrkt\")))) .\n",
                   "} ORDER BY ?from")
    res <- nstac_sparql_send(query)
    if (nrow(res) > 0) {
        res          <- nstac_sparql_split_langs(res, 'label')
        res          <- nstac_sparql_split_langs(res, 'location')
        res$class    <- strip_sacs_namespace(res$class)
        res$locationClass <- strip_sacs_namespace(res$locationClass)
        res$formerly <- strip_lang_tagging(res$formerly)
        res$later    <- strip_lang_tagging(res$later)
        res$creationEvent <- strip_lang_tagging(res$creationEvent)
        res$from     <- as.Date(as.POSIXct(res$from, origin="1970-01-01"))
        res$to       <- as.Date(as.POSIXct(res$to, origin="1970-01-01"))
        ## adjust column order and names:
        res          <- with(res, data.frame(
            code        = code,
            label.ja    = label.ja,
            label.en    = label.en,
            'label.ja-hrkt' = res[, 'label.ja-hrkt'],
            class       = class,
            valid.from  = from,
            valid.until = to,
            formerly    = formerly,
            former.code = formerCode,
            later       = later,
            later.code  = laterCode,
            chng.event  = creationEvent,
            parent.code = locationCode,
            parent.ja   = location.ja,
            parent.en   = location.en,
            'parent.ja-hrkt' = res[, 'location.ja-hrkt'],
            parent.class= locationClass, 
            check.names = FALSE
            ))
    }

    res
}
