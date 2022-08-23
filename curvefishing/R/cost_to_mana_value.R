
#' @title Convert mana cost to mana value
#' @description Calculate mana value, ignoring coloured mana.
#' X is always counted as 1 as many spells do not provide
#' value when cast for 0.
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
    cost <- str_replace_all(string = cost, pattern = "[A-Za-z]", replacement = "0")
    vapply(X = strtoi(cost), FUN = sum_int, FUN.VALUE = numeric(1))
}

sum_int <- function(x) sum(floor(x / 10^(seq_len(nchar(x)) - 1)) %% 10)

#' handle mana cost with digits greater than 9.
#' @examples
#' curvefishing:::replace_two_digit(string = c("RR", "2R", "RGU", "11", "1rw", "gugu"))
#' @noRd

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


#' handle hybrid mana
#' @examples
#' curvefishing:::replace_hybrid(string = c("RR", "2R", "RGU", "11", "1rw", "gugu"))
#' @noRd

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
    cost_tab <- get_mana_cols_tab(cost)
    has_each <- cost_tab <= matrix(resource, ncol = ncol(cost_tab), nrow = 5, byrow = FALSE)
    apply(X = has_each, MARGIN = 2L, FUN = all)
}

#' @noRd
#' @param x character vector containing WUBRG mana symbols
#' @return numeric vector length 5
#' @examples curvefishing:::count_mana(c("R", "R"))

count_mana <- function(x) {
    res <- numeric(5)
    names(res) <- c("W", "U", "B", "R", "G")
    for (mana in names(res)) {
        res[[mana]] <- sum(x == mana)
    }
    res
}

#' @noRd
#' @description Count mana symbols in cost
#' @param cost character vector containing WUBRG mana symbols
#' @return numeric matrix with 5 rows corresponding to WUBRG,
#' and columns equal to length cost
#' @examples curvefishing:::get_mana_cols_tab(cost = c("RR", "2R", "RGU", "11"))

get_mana_cols_tab <- function(cost) {
    cost_list <- str_split(string = cost, pattern = "")
    vapply(X = cost_list, FUN = count_mana, FUN.VALUE = numeric(5))
}

#' @noRd
#' @return A deck data.frame with columns named type and cost,
#' and also columns w, u, b, r, g counting the number of each mana symbol in cost.
#' @seealso get_mana_cols_tab
#' @examples
#' curvefishing:::get_mana(deck = shuffle_deck(sligh))

get_mana <- function(deck) {
    cols <- c("W", "U", "B", "R", "G")
    for (cl in cols) {
        deck[[casefold(cl, upper = FALSE)]] <- str_count(string = deck$cost, pattern = cl)
    }
    return(deck)
}
