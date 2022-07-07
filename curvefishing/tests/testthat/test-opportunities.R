
test_that("check opportunities", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- opportunities(d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
    d1$is_tapped <- d1$lands_this_turn
    d2 <- opportunities(d1)
    expect_equal(d2$opportunities, c(0, 0, 0, 0))
    expect_true(all(!d2$is_tapped))
    d3 <- data.frame(
        name = 1:20, cost = "R", type = c("spell", "land"),
        turn = c(NA, 1, NA, 2, rep(NA, times = 16)),
        cards_this_turn = rep(c(TRUE, FALSE), times = c(10, 10)),
        stringsAsFactors = FALSE)
    expect_equal(opportunities(deck = d3)$opportunities,
        expected = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    d3$cost <- c("R", "G")
    expect_equal(opportunities(deck = d3)$opportunities,
        expected = rep(0L, times = 20))
    d4 <- data.frame(cost = c("R", "R", "rw", "brbg"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d5 <- opportunities(d4)
    expect_equal(d5$opportunities, c(0, 1, 1, 0))
})

test_that("check get_combinations", {
    d1 <- data.frame(cost = c("R", "rw", "uw"),
        cards_this_turn = TRUE,
        is_tapped = FALSE,
        mana_value = 1L,
        turn = c(1L, NA, NA),
        stringsAsFactors = FALSE)
    p2 <- curvefishing:::get_combinations(deck = d1)
    expect_equal(length(p2), expected = 4L)
    expect_equal(p2[[1L]]$cost, expected = c("R", "R", "U"))
    expect_equal(p2[[2L]]$cost, expected = c("R", "W", "U"))
    d3 <- data.frame(
        name = 1:9, cost = c("wr", "ur", "wu"), type = c("spell", "land", "spell"),
        turn = c(NA, 1, NA, NA, 2, rep(NA, times = 4)),
        cards_this_turn = rep(c(TRUE, FALSE), times = c(6, 3)),
        is_tapped = FALSE,
        mana_value = 1L,
        stringsAsFactors = FALSE)
    p4 <- curvefishing:::get_combinations(deck = d3)
    expect_equal(length(p4), expected = 64L)
    expect_equal(p4[[1L]]$cost,
        expected = c("W", "U", "W", "W", "U", "W", "wr", "ur", "wu"))
})

test_that("check get_combinations", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        cards_this_turn = TRUE,
        is_tapped = FALSE,
        mana_value = c(1, 1, 1, 2),
        opportunities = 0,
        stringsAsFactors = FALSE)
    d2 <- curvefishing:::get_opportunities(deck = d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
})
