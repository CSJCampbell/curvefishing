
#' @title Convert mana cost to mana value
#' @description Calculate mana value, ignoring coloured mana.
#' @param cost character vector containing coloured mana requirements
#' (W, U, B, R, G). Hybrid coloured mana is also supported
#' (e.g. wu, ub, ub, ur, br, bg, rg, rw, gw, gu).
#' @return numeric vector
#' @importFrom stringr str_detect str_extract str_extract_all str_replace str_replace_all
#' str_split str_count
#' @export
#' @examples
#' cost_to_mana_value(cost = c("RR", "2R", "RGU", "11", "1rw"))

cost_to_mana_value <- function(cost) {
    # handle mana cost with digits greater than 9.
    cost <- replace_two_digit(string = cost)
    # handle hybrid mana
    cost <- replace_hybrid(string = cost, replacement = "1")
    # basic mana cost
    cost <- str_replace_all(string = cost, pattern = "[WUBRGX]", replacement = "1")
    cost_list <- str_split(string = cost, pattern = "")
    vapply(X = cost_list, FUN = function(x) { sum(as.numeric(x), na.rm = TRUE) }, FUN.VALUE = numeric(1))
}

#' @noRd
#' handle mana cost with digits greater than 9.
#' @examples
#' curvefishing:::replace_two_digit(string = c("RR", "2R", "RGU", "11", "1rw", "gugu"))

replace_two_digit <- function(string, pattern = "[1-9][0-9]") {
    is_two_digits <- str_detect(string = string, pattern = pattern)
    if (any(is_two_digits)) {
        for (two in which(is_two_digits)) {
            two_digits <- as.numeric(str_extract(string = string[two], pattern = pattern))
            num_fives <- two_digits %/% 5
            num_remainder <- two_digits %% 5
            string[two] <- str_replace(
                string = string[two],
                pattern = pattern,
                replacement = paste0(c(rep(5, times = num_fives), if (num_remainder > 0) { num_remainder } else { "" }), collapse = ""))
        }
    }
    string
}

is_hybrid <- function(x, pattern = "[wubrg]{2,5}?") {
    str_detect(string = x, pattern = pattern)
}

#' @noRd
#' handle hybrid mana
#' @examples
#' curvefishing:::replace_hybrid(string = c("RR", "2R", "RGU", "11", "1rw", "gugu"))

replace_hybrid <- function(string, replacement = NULL, which = 1L, pattern = "[wubrg]{2,5}?") {
    is_hybrid_str <- is_hybrid(x = string, pattern = pattern)
    if (any(is_hybrid_str)) {
        for (hyb in which(is_hybrid_str)) {
            hybrid <- str_extract_all(string = string[hyb], pattern = pattern)[[1L]]
            for (sym in seq_along(hybrid)) {
                if (is.null(replacement)) {
                    replace <- casefold(str_split(string = hybrid[sym], pattern = "")[[1]][which], upper = TRUE)
                } else {
                    replace <- replacement
                }
                string[hyb] <- str_replace(
                    string = string[hyb],
                    pattern = pattern,
                    replacement = replace)
            }
        }
    }
    string
}

#' @title Have lands with sufficient coloured mana been played?
#' @description Is coloured mana requirement met?
#' @param cost character vector containing coloured mana requirements
#' (W, U, B, R, G)
#' @param resource numeric vector with quantity of coloured mana resource available
#' @return logical vector of length cost
#' @examples
#' curvefishing:::has_resource(cost = c("RR", "2R", "RGU", "11"), resource = c(0, 0, 0, 1, 0))

has_resource <- function(cost, resource) {
    cost_df <- get_mana(data.frame(cost = cost, stringsAsFactors = FALSE))
    has_each <- as.matrix(cost_df[-1]) <= matrix(resource, ncol = 5, nrow = nrow(cost_df), byrow = TRUE)
    apply(X = has_each, MARGIN = 1L, FUN = all)
}
