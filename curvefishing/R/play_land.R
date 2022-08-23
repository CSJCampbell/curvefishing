
#' @title Play land
#' @description Set the turn played column of a deck with the
#' turn based on the TRUE value found in lands_this_turn,
#' or using the whichland argument to set the index.
#' @param deck data frame with columns, "type", "cost",
#' and optionally number, describing deck to be analysed
#' @param turn single numeric, turn identifier(default 1)
#' @param whichland single numeric indicating which land row to play
#' (default NULL). When not set, a column named lands_this_turn is expected.
#' @return A deck data.frame with columns named type and cost.
#' @export
#' @examples
#' d1 <- shuffle_deck(sligh)
#' d1$lands_this_turn <- seq_len(nrow(d1)) == which(d1$type == "land")[1]
#' d1$turn <- NA
#' head(play_land(deck = d1))
#' head(play_land(deck = d1, turn = 2, whichland = which(d1$type == "land")[2]))

play_land <- function(deck, turn = 1L, whichland = NULL) {
    stopifnot(is_deck(deck))
    stopifnot("turn" %in% colnames(deck))
    if (is.null(whichland)) {
        stopifnot("lands_this_turn" %in% colnames(deck))
    } else {
        stopifnot(is.numeric(whichland))
        deck$lands_this_turn <- seq_len(nrow(deck)) == whichland
    }
    n_lands <- sum(deck$lands_this_turn & is.na(deck$turn))
    if (n_lands != 1L) {
        warning("expected one land to play, but found ", n_lands)
    }
    if (deck[deck$lands_this_turn & is.na(deck$turn), "type"] != "land") {
        warning("selected card is not land")
    }
    deck[deck$lands_this_turn & is.na(deck$turn), "turn"] <- turn
    return(deck)
}
