
#' @title Analyse how many cards can be played by Goldfishing a Deck
#' @description Calculate the "fish" metric, by estimating the
#' number of opportunities to play cards over a number of turns.
#' An opportunity to play is where a card's mana value
#' and coloured mana requirements are met by lands that have be played
#' each turn. Estimation of opportunities to play by simulating
#' nsim replicates, and counting the total number of opportunities for
#' spells over multiple turns.
#' @param decklist data frame with columns, "type", "cost",
#' and optionally "number", describing deck to be analysed
#' @param turns number of turns to analyse, default 7
#' @param play is the player playing first (skip first draw) or second (draw as usual), default TRUE
#' @param handsize number of cards in starting hand, default 7
#' @param nsim number of simulations to perform (default 100).
#' @return fish single numeric with bootstrap simulations as attribute 'fishing'
#' @export
#' @examples
#' f1 <- go_fish(decklist = sligh, nsim = 10)
#' f1
#' plot(f1)

go_fish <- function(decklist, turns = 7L, play = TRUE, handsize = 7L, nsim = 100L) {
    fishing <- numeric(length = nsim)
    deck <- shuffle_deck(decklist)
    for (fsh in seq_along(fishing)) {
        deck <- goldfish(deck = shuffle_deck(deck),
            turns = turns, play = play, handsize = handsize)
        fishing[fsh] <- sum(deck$opportunities, na.rm = TRUE)
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

print.fish <- function(x, ...) {
    print(data.frame("><>" = c(x), row.names = "", check.names = FALSE))
    invisible(x)
}

quantile.fish <- function(x, na.rm = TRUE, ...) {
    quantile(x = attr(x = x, which = "fishing"), na.rm = na.rm, ...)
}

mean.fish <- function(x, na.rm = TRUE, ...) {
    mean(x = attr(x = x, which = "fishing"), na.rm = na.rm, ...)
}

#' @noRd
#' @describeIn plot The \code{plot} method
#' creates a \pkg{graphics} plot for the fish class.
#' @importFrom graphics plot points text
#' @method plot fish
#' @examples
#' f1 <- go_fish(decklist = sligh, nsim = 20)
#' plot(f1)

plot.fish <- function(x, xlab = "Probability", ylab = "Opportunities", ...) {
    fishing <- attr(x = x, which = "fishing")
    plot(x = seq_along(fishing) / length(fishing),
         y = sort(fishing), type = "s", xlab = xlab, ylab = ylab, ...)
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
