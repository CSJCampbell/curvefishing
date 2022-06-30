
test_that("check shuffle_deck", {
    expect_error(shuffle_deck(data.frame()))
    d1 <- data.frame(cost = "1", type = "land")
    expect_equal(shuffle_deck(d1), d1)
    d1 <- data.frame(
        cost = c("1", "R", "W", "RW"),
        type = c("spell", "spell", "land", "land"),
        is_tapped = c(FALSE, FALSE, FALSE, TRUE))
    set.seed(3239)
    expect_equal(shuffle_deck(d1), d1[4:1, ])
})

test_that("check is_deck", {
    expect_equal(is_deck(mtcars), FALSE)
    expect_equal(is_deck(sligh), TRUE)
})

test_that("check is_decklist", {
    expect_equal(is_decklist(mtcars), FALSE)
    expect_equal(is_decklist(sligh), TRUE)
})
