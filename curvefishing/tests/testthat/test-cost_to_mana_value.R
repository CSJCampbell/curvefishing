
test_that("check cost_to_mana_value", {
    expect_equal(cost_to_mana_value(
            cost = c("RR", "2R", "RGU", "11")),
        expected = c(2, 3, 3, 11))
    expect_equal(cost_to_mana_value(cost = "11B"),
        expected = 12)
    expect_equal(cost_to_mana_value(
            cost = c("wu", "ub", "ub", "ur", "br", "bg", "rg", "rw", "gw", "gu")),
        expected = rep(1, times = 10))
    expect_equal(cost_to_mana_value(
            cost = c("rw", "2rw", "1bgbg", "1brRrg")),
        expected = c(1, 3, 3, 4))
    expect_equal(cost_to_mana_value(
            cost = c("R", "s")),
        expected = c(1, 0))
})

test_that("check replace_two_digit", {
    expect_equal(curvefishing:::replace_two_digit(
            string = c("9R", "10R", "11", "99")),
        expected = c("9R", "55R", "551", "55555555555555555554"))
})

test_that("check replace_hybrid", {
    expect_equal(curvefishing:::replace_hybrid(
            string = c("rw", "2rw", "1bgbg", "1brRrg")),
        expected = c("R", "2R", "1BB", "1BRR"))
    expect_equal(curvefishing:::replace_hybrid(
            string = c("rw", "2rw", "1bgbg", "1brRrg"), which = 2),
        expected = c("W", "2W", "1GG", "1RRG"))
    expect_equal(curvefishing:::replace_hybrid(
            string = c("rw", "2rw", "1bgbg", "1brRrg"), replacement = "1"),
        expected = c("1", "21", "111", "11R1"))
})

test_that("check has_resource", {
    expect_equal(curvefishing:::has_resource(
            cost = c("RR", "2R", "RGU", "11"),
            resource = c(0, 0, 0, 1, 0)),
        expected = c(FALSE, TRUE, FALSE, TRUE))
})

test_that("check get_mana", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    o1 <- curvefishing:::get_mana(d1)
    expect_equal(o1$w, expected = c(0, 0, 0, 0))
    expect_equal(o1$r, expected = c(1, 1, 0, 2))
})
