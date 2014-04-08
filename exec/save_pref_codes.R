library(SPARQL)
endpoint <- "http://statdb.nstac.go.jp/lod/sparql"

split_langs <- function(result) {
    ## select only one row of the non-label data
    output <- result[grep("@ja$", result$label), ]
    ## `output$label` now contains only one language, we will drop it
    ## later but it keeps the dataframe together

    ## separate `result`'s labels into columns of `output` by language
    for (lang.tag in c("ja", "en", "ja-hrkt")) {
        col.name  <- paste("label", lang.tag, sep=".")
        indexes   <- grep(paste0(lang.tag, '$'), result$label)
        if (length(indexes) == nrow(output)) {
            name.data <- gsub("(\"|@[-A-Za-z]+$)", "",
                              result$label[indexes])
            output[[col.name]] <- name.data
        }
    }
    output[, "label"] <- NULL
    output
}

query <- paste("SELECT ?code ?label",
               "WHERE {
                  ?item sacs:administrativeClass sacs:Prefecture ;
                        dcterms:identifier ?code ;
                        rdfs:label ?label .",
               "} ORDER BY ?code")

result <- (SPARQL(endpoint, query, curl_args=list(.encoding="UTF-8")))$result
output <- split_langs(result)
output$code <- sub('000', '', output$code)

saveRDS(object=output, file="inst/extdata/prefectures.rds")

