
#' @title Analyse how many cards can be played by Goldfishing a Deck
#' @description Calculate the "fish" metric, by estimating the
#' number of opportunities to play cards over a number of turns.
#' An opportunity to play is where a card's mana value
#' and coloured mana requirements are met by lands that have be played
#' each turn. Estimation of opportunities to play by simulating
#' nsim replicates, and counting the total number of opportunities for
#' spells over multiple turns.
#' @param decklist data frame with columns, "type", "cost",
#' and optionally "number", "is_basic", "is_tapped", "is_search_basic" and "name",
#' describing deck to be analysed
#' @param turns number of turns to analyse, default 7
#' @param play is the player playing first (skip first draw)
#' or second (draw as usual), default TRUE
#' @param handsize number of cards in starting hand, default 7
#' @param nsim number of simulations to perform (default 100).
#' @param hand character vector of cards from column name that make up the
#' starting hand (handsize will be ignored), or NULL, the default.
#' @return fish single numeric with bootstrap simulations as attribute 'fishing'
#' @export
#' @examples
#' f1 <- go_fish(decklist = sligh, nsim = 10)
#' f1
#' plot(f1)

go_fish <- function(decklist, turns = 7L, play = TRUE, handsize = 7L,
    nsim = 100L, hand = NULL) {
    if (!is.null(hand)) {
        stopifnot(is.character(hand))
        handsize <- length(hand)
    }
    fishing <- numeric(length = nsim)
    deck <- shuffle_deck(decklist, hand = hand)
    deck$costs <- costs(deck$cost)
    deck$hybrid_costs <- hybrid_costs(deck$costs)
    deck$mana_value <- cost_to_mana_value(cost = deck$costs)
    if (!"is_tapped" %in% colnames(deck)) {
        deck$is_tapped <- FALSE
    }
    if (!"is_search_basic" %in% colnames(deck)) {
        deck$is_search_basic <- FALSE
    }
    if (!"is_basic" %in% colnames(deck)) {
        is_named_basic <- TRUE
        if ("name" %in% colnames(deck)) {
            is_named_basic <- deck$name %in% c("Plains", "Island", "Swamp", "Mountain", "Forest")
        }
        deck$is_basic <- deck$type == "land" &
            deck$cost %in% c("W", "U", "B", "R", "G") &
            !deck$is_tapped &
            is_named_basic
    }
    deck <- get_mana(deck = deck)
    for (fsh in seq_along(fishing)) {
        deck_f <- goldfish(deck = shuffle_deck(deck, hand = hand),
            turns = turns, play = play, handsize = handsize)
        fishing[fsh] <- sum(deck_f$opportunities, na.rm = TRUE)
    }
    fish(fishing, turns = turns)
}

#' @describeIn go_fish Returns an object of class fish.
#' @param x An object
#' @importFrom stats median quantile
#' @export
#' @examples
#' fish(1:3)

fish <- function(x, turns = 7L) {
    fish <- median(x)
    attr(x = fish, which = "fishing") <- x
    attr(x = fish, which = "turns") <- turns
    class(fish) <- "fish"
    return(fish)
}

#' @method print fish
#' @export

print.fish <- function(x, ...) {
    print(data.frame("><>" = c(x), row.names = "", check.names = FALSE))
    invisible(x)
}

#' @method quantile fish
#' @export

quantile.fish <- function(x, na.rm = TRUE, ...) {
    quantile(x = attr(x = x, which = "fishing"), na.rm = na.rm, ...)
}

#' @method mean fish
#' @export

mean.fish <- function(x, na.rm = TRUE, ...) {
    mean(x = attr(x = x, which = "fishing"), na.rm = na.rm, ...)
}

#' @noRd
#' @describeIn plot The \code{plot} method
#' creates a \pkg{graphics} plot for the fish class.
#' @importFrom graphics plot points text
#' @method plot fish
#' @export
#' @examples
#' f1 <- go_fish(decklist = sligh, nsim = 20)
#' plot(f1)

plot.fish <- function(x, xlab = "Probability", ylab = "Opportunities", ...) {
    fishing <- attr(x = x, which = "fishing")
    plot(x = c(0, seq_along(fishing) / length(fishing), 1),
         y = c(min(fishing), sort(fishing), max(fishing)), type = "s", xlab = xlab, ylab = ylab, ...)
    points(x = 0.5, y = c(x), ...)
    text(x = 0.5, y = c(x) * 0.8, labels = c(x), ...)
}

#' @name sligh
#' @title Decklist by Jay Schneider and Paul Sligh
#' @description Decklist data frame
#' @docType data
#' @format Data frame with columns:\itemize{
#'     \item number integer number of each card
#'     \item name an optional column of card names
#'     \item type card type (land or spell)
#'     \item cost character column specifying coloured and generic mana cost
#' }
#' @examples
#' sligh

NULL
