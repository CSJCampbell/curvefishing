

#' @title Shuffle a deck or decklist
#' @description Randomise a deck or decklist.
#' Decklist will be expanded using column number if present.
#' @inheritParams go_fish
#' @return A deck data.frame with the same columns,
#' but rows randomised. If hand is provided, these rows will be first.
#' @importFrom stats runif
#' @export
#' @examples
#' shuffle_deck(sligh)
#' shuffle_deck(sligh, hand = "Mountain")
#' shuffle_deck(sligh, hand = character(0))

shuffle_deck <- function(decklist, hand = NULL) {
    stopifnot(is_deck(decklist))
    if (!is.character(decklist$cost)) { decklist$cost <- as.character(decklist$cost) }
    is_costs <- vapply(decklist, FUN = function(x) "costs" %in% class(x), FUN.VALUE = logical(1L))
    if (!is.null(hand)) {
        if (!"name" %in% colnames(decklist)) {
            stop("when using argument hand, 'name' must be present in decklist")
        }
        hand_in_deck <- hand %in% decklist$name
        if (!all(hand_in_deck)) {
            stop(paste0(hand[!hand_in_deck], collapse = ", "), "found in hand, but not in decklist")
        }
    } else {
        hand <- character(0)
    }
    if (is_decklist(decklist)) {
        number_of_cards <- sum(decklist$number)
        deck <- as.data.frame(select(slice(decklist, rep(seq_len(n()), times = number)), -number))
    } else {
        number_of_cards <- nrow(decklist)
        deck <- decklist
    }
    ind <- order(runif(number_of_cards))
    if (length(hand) > 0L) {
        hand_ind <- pick_x_from_y(x = hand, y = deck$name)
        ind <- c(hand_ind, ind[!ind %in% hand_ind])
    }
    deck <- deck[ind, ]
    rownames(deck) <- NULL
    if (any(is_costs)) {
        for (cc in which(is_costs)) {
            class(deck[[cc]]) <- "costs"
            attr(deck[[cc]], which = "is_hybrid") <- is_hybrid(deck$cost)
        }
    }
    return(deck)
}

#' Create partial match sort order to pick hand out of deck
#' @examples
#' pick_x_from_y(x = 1:2, y = 3:1) # [1] 3 2
#' pick_x_from_y(x = c(1, 1, 2), y = 3:1) # [1] 3 0 2
#' pick_x_from_y(x = c(1, 1, 2, 1, 2), y = c(1, 3:1, 1:3)) # [1] 1 4 3 5 6
#' @noRd

pick_x_from_y <- function(x, y) {
    ind <- numeric(length = length(x))
    for (i in seq_along(ind)) {
        ii <- which(y %in% x[i] & (!(seq_along(y) %in% ind)))
        if (length(ii) == 0L) { ii <- 0L } else {
            if (length(ii) > 1L) { ii <- ii[1L] }
        }
        ind[i] <- ii
    }
    ind
}

#' @describeIn shuffle_deck A deck must have columns named type and cost.
#' type must have no missing values, and value land or spell.
#' Expected values of cost include 0-9, X, and WUBRG, but no other letters.
#' @param x an object to test
#' @export
#' @examples
#' is_deck(mtcars)
#' is_deck(sligh)

is_deck <- function(x) {
    is.data.frame(x) &&
        all(c("type", "cost") %in% colnames(x)) &&
        all(x$type %in% c("land", "spell")) &&
        sum(is.na(x$type)) == 0L &&
        sum(is.na(x$cost)) == 0L &&
        !any(str_detect(string = x$cost, pattern = "[ACDEFH-QSTVYZacdefh-qstvxyz]"))
}


#' @describeIn shuffle_deck A decklist is a deck with an additional column 'number'
#' which indicates the number of each card to shuffle into the deck.
#' @export
#' @examples
#' is_decklist(mtcars)
#' is_decklist(sligh)

is_decklist <- function(x) {
    is_deck(x) &&
        "number" %in% colnames(x) &&
        is.numeric(x$number)
}

