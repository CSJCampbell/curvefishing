
#' @title Convert mana cost to mana value
#' @description Calculate mana value, ignoring coloured mana.
#' X is always counted as 1 as many spells do not provide
#' value when cast for 0.
#' @param cost character vector containing coloured mana requirements
#' (W, U, B, R, G). Hybrid coloured mana is also supported
#' (e.g. wu, ub, ub, ur, br, bg, rg, rw, gw, gu).
#' Alternatively, a costs object
#' @return numeric vector
#' @importFrom stringr str_detect str_extract str_extract_all str_replace str_replace_all
#' str_split str_count
#' @export
#' @examples
#' cost_to_mana_value(cost = c("wub", "1rw", "2R", "UBRG", "11"))
#' cost_to_mana_value(cost = costs(c("wub", "1rw", "2R", "UBRG", "11")))

cost_to_mana_value <- function(cost) {
    if (is.character(cost)) {
        costs <- costs(cost)
    } else {
        costs <- cost
    }
    vapply(X = costs, FUN = function(x) {
        nums <- suppressWarnings(as.numeric(x))
        sum(nums, na.rm = TRUE) + sum(is.na(nums))
    }, FUN.VALUE = numeric(1L))
}

sum_int <- function(x) sum(floor(x / 10^(seq_len(nchar(x)) - 1)) %% 10)


#' @describeIn get_combinations calculates opportunities.
#' @title Have lands with sufficient coloured mana been played?
#' @description Is coloured mana requirement met?
#' @inheritParams get_combinations
#' @param resource numeric vector with quantity of coloured mana resource available
#' @return logical vector of length cost
#' @examples
#' curvefishing:::has_resource(deck = sligh, resource = c(0, 0, 0, 1, 0))

has_resource <- function(deck, resource) {
    if (!all(c("w", "u", "b", "r", "g") %in% colnames(deck))) {
        deck <- get_mana(deck = deck)
    }
    has_each <- as.matrix(deck[, c("w", "u", "b", "r", "g")]) <= matrix(resource,
        nrow = nrow(deck), ncol = 5, byrow = TRUE)
    apply(X = has_each, MARGIN = 1L, FUN = all)
}


#' @noRd
#' @return A deck data.frame with columns named type and cost,
#' and also columns w, u, b, r, g counting the number of each mana symbol in cost.
#' @seealso get_mana_cols_tab
#' @examples
#' curvefishing:::get_mana(deck = shuffle_deck(sligh))

get_mana <- function(deck) {
    if (!"costs" %in% names(deck)) {
        deck$costs <- costs(deck$cost)
    }
    cols <- c("W", "U", "B", "R", "G")
    for (cl in cols) {
        deck[[casefold(cl, upper = FALSE)]] <- vapply(X = deck$costs,
            FUN = function(x) sum(x == cl), numeric(1L))
    }
    return(deck)
}
