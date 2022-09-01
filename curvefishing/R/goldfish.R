
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

goldfish <- function(deck, turns = 7L, play = TRUE,
    handsize = 7L) {
    stopifnot(is_deck(deck))
    if (!is.character(deck$cost)) { deck$cost <- as.character(deck$cost) }
    if (!"costs" %in% colnames(deck)) {
        deck$costs <- costs(deck$cost)
    }
    if (!"hybrid_costs" %in% colnames(deck)) {
        deck$hybrid_costs <- hybrid_costs(deck$costs)
    }
    if (!"mana_value" %in% colnames(deck)) {
        deck$mana_value <- cost_to_mana_value(cost = deck$costs)
    }
    # hybrid mana does not show up yet
    if (!all(c("w", "u", "b", "r", "g") %in% colnames(deck))) {
        deck <- get_mana(deck = deck)
    }
    deck$opportunities <- 0L
    deck$turn <- NA_integer_
    deck$cards_this_turn <- FALSE
    deck$lands_this_turn <- FALSE
    num_lands <- sum(deck$type == "land", na.rm = TRUE)
    # initial cards
    deck$cards_this_turn <- seq_len(nrow(deck)) %in% seq_len(handsize + as.numeric(!play) - 1L)
    for (turn in seq_len(turns)) {
        if (sum(!is.na(deck$turn)) >= num_lands) { break }
        # add drawn card, avoiding previously seen cards
        deck$cards_this_turn[which.min(deck$cards_this_turn)] <- TRUE
        deck$lands_this_turn <- deck$type == "land" & deck$cards_this_turn & is.na(deck$turn)
        # handle hybrid permutations for spells that can be cast this turn
        deck$hybrid_this_turn <- is_hybrid(deck$hybrid_costs) &
            deck$cards_this_turn &
            # NOTE to handle lands that can add more than 1 mana, this line must be changed
            # same logic in `get_combinations`
            deck$mana_value <= sum(!is.na(deck$turn))
        if (any(deck$lands_this_turn)) {
            # choose which land to play
            if (sum(deck$lands_this_turn) > 1L) {
                # copy of whole deck necessary in case we need to search for lands
                deck_turn <- deck
                # new opportunities this turn
                deck_turn$opportunities <- 0L
                opps <- numeric(sum(deck_turn$lands_this_turn))
                lands <- which(deck_turn$lands_this_turn)
                # which land should be played
                for (ind in seq_along(lands)) {
                    # each land gets played before each call to opportunities
                    possible_hand <- opportunities(play_land(deck_turn,
                            whichland = lands[ind]))
                    opps[ind] <- sum(possible_hand$opportunities)
                }
                deck$lands_this_turn <- seq_len(nrow(deck)) == which(deck$lands_this_turn)[which.max(opps)]
            }
            # play only or chosen land
            deck <- play_land(deck, turn = turn)
            deck <- opportunities(deck, updateall = FALSE)
        }
    }
    return(deck)
}
