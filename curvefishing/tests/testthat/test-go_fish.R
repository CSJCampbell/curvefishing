
test_that("check go_fish", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    f2 <- go_fish(d1, nsim = 2)
    expect_equal(c(f2), 2)
    expect_equal(attr(f2, which = "fishing"), c(2, 2))
    expect_equal(c(go_fish(decklist = data.frame(cost = 1:20, type = "spell"), nsim = 2)), 0L)
    set.seed(34573)
    expect_equal(c(go_fish(decklist = data.frame(
        name = 1:20, cost = "R", type = c("spell", "land"),
        stringsAsFactors = FALSE), nsim = 2)), 32.5)
    expect_equal(c(go_fish(decklist = data.frame(
        name = 1:20, cost = c("R", "G"), type = c("spell", "land"),
        stringsAsFactors = FALSE), nsim = 2)), 0L)
    d3 <- data.frame(cost = c("R", "rw", "uw", "rgrg"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    f4 <- go_fish(d3, nsim = 2)
    expect_equal(c(f4), 1)
    d5 <- data.frame(cost = rep(c("11", "W", "U", "B", "R", "G"), times = 2),
        name = letters[rep(1:6, times = 2)],
        type = rep(c("spell", rep("land", 5)), times = 2), stringsAsFactors = FALSE)
    f6 <- go_fish(d5, nsim = 2, hand = c("a", "a"))
    expect_equal(c(f6), 0)
    # be able to play each spell as you draw it
    # T1 = 0, T2 = 1, T3 = 3, etc.
    d7 <- data.frame(cost = "R",
        name = letters[rep(1:2, each = 7)],
        type = rep(c("spell", "land"), each = 7), stringsAsFactors = FALSE)
    f8 <- go_fish(d7, nsim = 2, hand = rep("b", times = 7))
    expect_equal(c(f8), 21)
})
