

#' @title Shuffle a deck or decklist
#' @description Randomise a deck or decklist.
#' @param decklist data frame with columns type, cost and optionally number
#' @return A deck data.frame with columns named type and cost.
#' @importFrom stats runif
#' @export
#' @examples
#' shuffle_deck(sligh)

shuffle_deck <- function(decklist) {
    stopifnot(is_deck(decklist))
    if (is_decklist(decklist)) {
        number_of_cards <- sum(decklist$number)
        deck <- matrix(nrow = number_of_cards, ncol = 2,
                       dimnames = list(NULL, c("type", "cost")))
        pos <- 0L
        for (i in seq_len(nrow(decklist))) {
            deck[seq.int(from = pos + 1, length.out = decklist$number[i]), ] <- rep(c(
                decklist$type[i], decklist$cost[i]), each = decklist$number[i])
            pos <- pos + decklist$number[i]
        }
        deck <- as.data.frame(deck, stringsAsFactors = FALSE)
        decklist$number <- NULL
        deck <- full_join(x = deck, y = decklist, by = c("type", "cost"))
    } else {
        number_of_cards <- nrow(decklist)
        deck <- decklist
    }
    deck[order(runif(number_of_cards)), ]
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
        !any(str_detect(string = x$cost, pattern = "[ACDEFHIJKLMNOPQSTVYZ]"))
}


#' @describeIn shuffle_deck A decklist is a deck with an additional column 'number'
#' which indicates the number of each card to shuffle into the deck.
#' @export
#' @examples
#' is_decklist(mtcars)
#' is_decklist(sligh)

is_decklist <- function(x) {
    is.data.frame(x) &&
        all(c("number", "type", "cost") %in% colnames(x)) &&
        is_deck(x) &&
        is.numeric(x$number)
}

