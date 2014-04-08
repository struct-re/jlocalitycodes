#' @importFrom SPARQL SPARQL

endpoint <- "http://statdb.nstac.go.jp/lod/sparql"

.jlc_cache <- new.env()

cached <- function(key, generator) {
    if (!exists(key, envir = .jlc_cache)) {
        value <- do.call(generator, list())
        assign(key, value, envir = .jlc_cache)
    }

    get(key, envir = .jlc_cache)
}

is_valid_code <- function(code) {
    (length(grep("^\\d+$", code)) > 0) &
        (nchar(code) == 5 | (nchar(code) == 2 & as.numeric(code) <= 47))
}

is_valid_class <- function(class) {
    all(class %in% c('Prefecture', 'SubPrefecture', 'District',
                     'SpecialWardsArea', 'Municipality', 'City',
                     'DesignatedCity', 'CoreCity', 'SpecialCity',
                     'SpecialWard', 'Ward', 'Town', 'Village',
                     'AdministrativeDivision'))
}

nstac_sparql_split_langs <- function(df, col.name) {
    ## select only one row of the non-label data
    output <- df[grep("@ja$", df[, col.name]), ]
    ## `output$col` now contains only one arbitrarily-chosen
    ## language, but we don't drop it yet else the data.frame
    ## collapses into a vector when `code` is the only other column

    ## Now separate the labels by language
    for (lang.tag in c("ja", "en", "ja-hrkt")) {
        col.name.new  <- paste(col.name, lang.tag, sep=".")
        indexes       <- grep(paste0(lang.tag, '$'), df[, col.name])
        if (length(indexes) == nrow(output)) {
            name.data <- strip_lang_tagging(df[indexes, col.name])
            output[[col.name.new]] <- name.data
        }
    }
    output[, col.name] <- NULL

    output
}

strip_sacs_namespace <- function(str) {
    gsub('(<http:.+#|>)', "", str)
}

strip_lang_tagging <- function(str) {
    gsub("(\"|@[-A-Za-z]+$)", "", str)
}

paste00 <- function(..., collapse = NULL, na.rm = TRUE) {
    na2blank <- function(x) {
        vapply(as.character(x), function(c) ifelse(is.na(c), '', c), '')
    }
    if (na.rm == TRUE) {
        argz <- lapply(list(...), na2blank)
        argz[['collapse']] <- collapse
        do.call(paste0, argz)
    } else {
        paste0(..., collapse = collapse)
    }
}

#' @name extract_prefectures
#' @title Extract prefecture information from JIS locality codes.
#' @param codes Vector of JIS locality codes.
#' @param as.names Logical. If set to \code{TRUE}, this function
#' returns prefecture names. By default, it returns two-digit JIS
#' prefecture codes.
#' @param lang Desired language for prefecture names.
#' @return A vector with the prefecture of each code in \code{codes}.
#' @export
extract_prefectures <- function(codes, as.names = FALSE, lang = 'ja') {
    lang    <- match.arg(lang, c('en', 'ja', 'ja-hrkt'))
    p.data  <- prefecture_codes()
    p.codes <- substr(codes, 1, 2)
    if (as.names) {
        col.name <- paste('label', lang, sep = '.')
        p.data[match(p.codes, p.data$code), col.name]
    } else {
        p.codes
    }
}

drop_languages <- function(df, langs) {
    for (lang in langs) {
        re <- paste0('^.+\\.', lang, '$')
        drop.cols <- grep(re, names(df), value = TRUE)
        df <- df[!(names(df) %in% drop.cols)]
    }
    df
}

nstac_sparql_send <- function(query) {
    message("Querying SPARQL endpoint at ", endpoint)
    res <- SPARQL(endpoint,
                  query,
                  curl_args = list(.encoding = 'UTF-8'))$result
    if (nrow(res) == 250) {
        warning("Server limit reached. Only the first ", 
                "250 results were returned.")
    }
    res
}
