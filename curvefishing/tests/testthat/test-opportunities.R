
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
    # multi-colour hybrid
    d6 <- data.frame(type = c("spell", "spell", "land", "spell"),
        cost = c("uw", "rgrg", "R", "rw"), turn = c(NA, NA, 1L, NA),
        cards_this_turn = c(TRUE, TRUE, TRUE, TRUE),
        is_tapped = c(FALSE, FALSE, FALSE, FALSE),
        mana_value = c(1, 2, 1, 1), is_search_basic = c(FALSE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
    do6 <- opportunities(deck = d6)
    expect_equal(do6$opportunities, expected = c(0, 0, 0, 1))
    expect_equivalent(do6$costs, expected = list("U", c("rg", "rg"), "R", "R"))
})

test_that("check get_combinations", {
    d1 <- data.frame(cost = c("R", "rw", "uw"),
        type = c("land", "spell", "spell"),
        cards_this_turn = TRUE,
        is_tapped = FALSE,
        mana_value = 1L,
        turn = c(1L, NA, NA),
        is_basic = c(TRUE, FALSE, FALSE),
        is_search_basic = FALSE,
        stringsAsFactors = FALSE)
    p2 <- curvefishing:::get_combinations(deck = d1)
    expect_equal(length(p2), expected = 4L)
    expect_equivalent(p2[[1L]]$costs, expected = list("R", "R", "U"))
    expect_equivalent(p2[[2L]]$costs, expected = list("R", "W", "U"))
    d3 <- data.frame(
        name = 1:9,
        cost = c("wr", "ur", "wu"),
        type = c("spell", "land", "spell"),
        turn = c(NA, 1, NA, NA, 2, rep(NA, times = 4)),
        cards_this_turn = rep(c(TRUE, FALSE), times = c(6, 3)),
        is_tapped = FALSE,
        is_basic = FALSE,
        is_search_basic = FALSE,
        mana_value = 1L,
        stringsAsFactors = FALSE)
    p4 <- curvefishing:::get_combinations(deck = d3)
    expect_equal(length(p4), expected = 64L)
    expect_equivalent(p4[[1L]]$costs,
        expected = list("W", "U", "W", "W", "U", "W", "wr", "ur", "wu"))
    # search for R or W basic that is in deck
    d5 <- data.frame(cost = c("rw", "R", "R", "R"),
        type = c("land", "spell", "spell", "land"),
        cards_this_turn = c(TRUE, TRUE, TRUE, FALSE),
        is_tapped = c(TRUE, FALSE, FALSE, FALSE),
        turn = c(1L, NA, NA, NA),
        mana_value = 1L,
        is_search_basic = c(TRUE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE)
    d6 <- data.frame(cost = c("0", "R", "R", "R"),
        mana_value = c(0L, 1L, 1L, 1L),
        turn = c(1L, NA, NA, 1L),
        stringsAsFactors = FALSE)
    p6 <- curvefishing:::get_combinations(deck = d5)
    expect_equal(p6[[1]][, c("cost", "mana_value", "turn")], expected = d6)
    # search for R or W basic that is not in deck
    d5[4L, 1L] <- "U"
    d7 <- data.frame(cost = c("rw", "R", "R", "U"),
        mana_value = c(1L, 1L, 1L, 1L),
        turn = c(1L, NA, NA, NA),
        stringsAsFactors = FALSE)
    p7 <- curvefishing:::get_combinations(deck = d5)
    expect_equal(p7[[1]][, c("cost", "mana_value", "turn")], expected = d7)
    # multi-colour hybrid
    d8 <- data.frame(type = c("spell", "spell", "land", "spell"),
        cost = c("uw", "rgrg", "R", "rw"), turn = c(NA, NA, 1L, NA),
        cards_this_turn = c(TRUE, TRUE, TRUE, TRUE),
        is_tapped = c(FALSE, FALSE, FALSE, FALSE),
        mana_value = c(1, 2, 1, 1), is_search_basic = c(FALSE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
    p8 <- curvefishing:::get_combinations(deck = d8)
    expect_equivalent(p8[[2]]$costs, expected = list("W", c("rg", "rg"), "R", "R"))
    # double multi-colour hybrid
    d9 <- rbind(d8, list("land", "G", 2L, TRUE, FALSE, 1, FALSE, TRUE))
    p9 <- curvefishing:::get_combinations(deck = d9)
    expect_equal(length(p9), 16L)
    expect_equivalent(p9[[6]]$costs, expected = list("W", c("R", "G"), "R", "R", "G"))
    # double multi-colour hybrid plus cost
    d10 <- rbind(d9, list(c("spell", "land"), c("1wrrg", "G"), c(NA, 3L),
        c(TRUE, TRUE), c(FALSE, FALSE), c(3, 1), c(FALSE, FALSE), c(FALSE, TRUE)))
    p10 <- curvefishing:::get_combinations(deck = d10)
    expect_equal(length(p10), 64L)
    expect_equivalent(p10[[6]]$costs, expected = list("W", c("R", "G"), "R", "R", "G", c("1", "W", "R"), "G"))
})

test_that("check get_searchable", {
    d1 <- data.frame(cost = c("rw", "R", "R", "R"),
        type = c("land", "spell", "spell", "land"),
        cards_this_turn = c(TRUE, TRUE, TRUE, FALSE),
        is_tapped = c(TRUE, FALSE, FALSE, FALSE),
        turn = c(1L, NA, NA, NA),
        is_search_basic = c(TRUE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE)
    d2 <- curvefishing:::get_searchable(deck = d1)
    expect_equal(length(d2), 4L)
    expect_equal(d2[[1]], "R")
    d5 <- d3 <- data.frame(cost = c("wg", "rw", "uw", "rgrg", "W", "G"),
        type = c("land", "spell", "spell", "spell", "land", "land"),
        cards_this_turn = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
        is_tapped = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        turn = c(1L, NA, NA, NA, NA, NA),
        is_search_basic = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
        stringsAsFactors = FALSE)
    d4 <- curvefishing:::get_searchable(deck = d3)
    expect_equivalent(d4, list(c("W", "G"), "rw", "uw", c("rg", "rg"), character(0), character(0)))
    # no searchable lands
    d5[5:6, 1] <- "B"
    d6 <- curvefishing:::get_searchable(deck = d5)
    expect_equivalent(d6, list(character(0), "rw", "uw", c("rg", "rg"), character(0), character(0)))
    # search land has been used
    d7 <- data.frame(cost = c("0", "wg", "rw", "uw", "rgrg", "W", "G"),
        type = c("land", "land",  "spell", "spell", "spell", "land", "land"),
        cards_this_turn = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
        is_tapped = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        turn = c(1L, NA, NA, NA, NA, 1, NA),
        is_search_basic = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
        stringsAsFactors = FALSE)
    d8 <- curvefishing:::get_searchable(deck = d7)
    expect_equivalent(d8, list(character(0), "G", "rw", "uw", c("rg", "rg"), character(0), character(0)))
})

test_that("check get_opportunities", {
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
