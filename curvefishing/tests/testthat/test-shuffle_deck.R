
test_that("check shuffle_deck", {
    expect_error(shuffle_deck(data.frame()))
    d1 <- data.frame(cost = "1", type = "land")
    expect_equal(shuffle_deck(d1), d1)
    d1 <- data.frame(
        cost = c("1", "R", "W", "RW"),
        type = c("spell", "spell", "land", "land"),
        is_tapped = c(FALSE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE)
    set.seed(3239)
    expect_equivalent(shuffle_deck(d1), d1[4:1, ])
    d1$number <- c(2, 4, 3, 1)
    set.seed(7039)
    expect_equivalent(shuffle_deck(d1),
        expected = d1[c(3, 2, 4, 3, 3, 2, 1, 1, 2, 2), c("cost", "type", "is_tapped")])
    d1$number <- c(2, 0, 3, 1)
    set.seed(9831)
    expect_equivalent(shuffle_deck(d1),
        expected = d1[c(1, 1, 4, 3, 3, 3), c("cost", "type", "is_tapped")])
    d2 <- data.frame(cost = c("R", "rw", "uw", "rgrg"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    set.seed(3676)
    d3 <- shuffle_deck(d2)
    expect_equivalent(d3[-1], d2[-1])
})

test_that("check is_deck", {
    expect_equal(is_deck(mtcars), FALSE)
    expect_equal(is_deck(sligh), TRUE)
})

test_that("check is_decklist", {
    expect_equal(is_decklist(mtcars), FALSE)
    expect_equal(is_decklist(sligh), TRUE)
})
