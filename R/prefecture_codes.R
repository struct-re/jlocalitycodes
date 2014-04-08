#' @name prefecture_codes
#' @title Get the JIS codes and names for all 47 Japanese prefectures
#' (retrieved from disk).
#' 
#' @return A data frame containing codes and names for all
#' prefectures. Codes are trimmed to 2 digits.
#' @export
prefecture_codes <- function() {
    cached('pref_codes', function() {
        readRDS(system.file('extdata', 'prefectures.rds',
                            package = 'jlocalitycodes'))
    })
}

prefecture_code <- function(name) {
    with(prefecture_codes(), code[label.ja == name | label.en == name])
}

## FIXME UTF-8 in toolchain
## 都道府県コード一覧 <- prefecture_codes
## 都道府県コード <- prefecture_code
