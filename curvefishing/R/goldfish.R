
#' @title Count Opportunities to play cards by Goldfishing a Deck
#' @description Create a column opportunities, which counts the number of
#' turns on which it would have been possible to play a spell.
#' One land is played each turn, attempt to maximise the opportunity
#' each turn.
#' @param deck data frame with columns, "type" and "cost".
#' @inheritParams go_fish
#' @return A deck data.frame with columns named type and cost.
#' Columns mana_value, opportunities, turn, cards_this_turn,
#' lands_last_turn, w, u, b, r, g are also returned.
#' @export
#' @examples
#' goldfish(deck = shuffle_deck(sligh))

goldfish <- function(deck, turns = 7L, play = TRUE, handsize = 7L) {
    stopifnot(is_deck(deck))
    if (!"mana_value" %in% names(deck)) {
        deck$mana_value <- cost_to_mana_value(cost = deck$cost)
    }
    deck$opportunities <- 0L
    deck$turn <- NA_integer_
    deck$cards_this_turn <- FALSE
    deck$lands_this_turn <- FALSE
    deck <- get_mana(deck)

    num_lands <- sum(deck$type == "land", na.rm = TRUE)
    num_lands_played <- 0L
    # initial cards
    deck$cards_this_turn <- seq_len(nrow(deck)) %in% seq_len(handsize + as.numeric(!play) - 1L)
    for (turn in seq_len(turns)) {
        if (num_lands_played >= num_lands) { break }
        # add drawn card, avoiding previously seen cards
        deck$cards_this_turn[which.min(deck$cards_this_turn)] <- TRUE
        deck$lands_this_turn <- deck$type == "land" & deck$cards_this_turn & is.na(deck$turn)
        if (any(deck$lands_this_turn)) {
            if (sum(deck$lands_this_turn) > 1L) {
                deck_turn <- deck
                deck_turn$opportunities <- 0L
                opps <- numeric(sum(deck_turn$lands_this_turn))
                lands <- which(deck_turn$lands_this_turn)
                for (ind in seq_along(lands)) {
                    possible_hand <- opportunities(play_land(deck_turn, whichland = lands[ind]))
                    opps[ind] <- sum(possible_hand$opportunities)
                }
                deck$lands_this_turn <- seq_len(nrow(deck)) == which(deck$lands_this_turn)[which.max(opps)]
            }
            deck <- play_land(deck, turn = turn)
            deck <- opportunities(deck, updateall = FALSE)
        }
        num_lands_played <- num_lands_played + 1L
    }
    return(deck)
}
