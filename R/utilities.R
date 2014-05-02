#' @importFrom SPARQL SPARQL

endpoint <- "http://statdb.nstac.go.jp/lod/sparql"

.jlc_cache <- new.env()

insert_col_at <- function(df, index, x, name) {
    stopifnot(index <= ncol(df) + 1)
    newdf = df
    old.names = names(df)
    new.name = name; i = 1;
    while(new.name %in% old.names) {
        new.name = paste(name, i, sep = ".")
        i = i + 1
    }
    newdf[[new.name]] = x
    new.names = append(old.names, new.name, after = (index - 1))

    newdf[, new.names]
}

cached <- function(key, generator) {
    if (!exists(key, envir = .jlc_cache)) {
        value <- do.call(generator, list())
        assign(key, value, envir = .jlc_cache)
    }

    get(key, envir = .jlc_cache)
}

is_valid_code <- function(code) {
    is_valid_jlocalitycode(code)
}

is_valid_class <- function(class) {
    all(class %in% c('Prefecture', 'SubPrefecture', 'District',
                     'SpecialWardsArea', 'Municipality', 'City',
                     'DesignatedCity', 'CoreCity', 'SpecialCity',
                     'SpecialWard', 'Ward', 'Town', 'Village',
                     'AdministrativeDivision'))
}

nstac_sparql_split_langs <- function(df, col.names) {
    old.col.names = names(df)
    unchanged.cols = old.col.names[!(old.col.names %in% col.names)]
    newdf = df
    N = ncol(df)
    for (c in col.names) {
        i = which(old.col.names == c)
        if (length(i) != 1) stop('Column name not found or ambiguous: ', c)
        langs   = unique(extract_lang_tags(df[, c]))
        nnotNA  = sum(!is.na(df[, c]))
        nlangs  = length(langs)
        if (nnotNA > 0) {
            if (nlangs < 1)
                stop(paste0(c, 'has zero language-tagged elements.'))
            if ((nnotNA %% nlangs) != 0)
                stop(paste0('The number of non-NA elements of ', c, ' is not a multiple of the number of languages ', length(langs), '!'))
            j = i
            for (l in langs) {
                select = is.na(df[, c]) | grepl(paste0("@", l, "$"), df[, c])
                ## only true on the first iteration
                if (nrow(newdf) > sum(select)) newdf <- df[select, ]
                newvec = strip_lang_tagging(df[select, c])
                newdf = insert_col_at(newdf, j, newvec, name = paste(c, l, sep = "."))
                j = j + 1
            }
        }
        newdf[, c] <- NULL
    }
    row.names(newdf) <- NULL
    if (length(setdiff(unique(df[, unchanged.cols]), unique(newdf[, unchanged.cols]))) != 0) {
        warning("Data loss occurred while splitting languages!")
    }
    newdf
}

strip_sacs_namespace <- function(str) {
    gsub('(<http:.+#|>)', "", str)
}

strip_lang_tagging <- function(str) {
    gsub("(\"|@[-A-Za-z]+$)", "", str)
}

extract_lang_tags <- function(str) {
    res = gsub("@", "", gsub("^[^@]+|\"", "", str), fixed = T)
    res[!is.na(res)]
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
