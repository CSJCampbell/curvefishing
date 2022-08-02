
test_that("check goldfish", {
    d1 <- data.frame(cost = c("R", "R", "1", "RR"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d2 <- goldfish(deck = d1)
    expect_equal(d2$opportunities, c(0, 1, 1, 0))
    d3 <- data.frame(cost = c("W", "W", "W", "ub", "WW"),
        type = c("land", "land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA, NA),
        lands_this_turn = c(TRUE, TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d4 <- goldfish(d3)
    expect_equal(d4$opportunities, c(0L, 0L, 2L, 0L, 1L))
    d5 <- data.frame(cost = c("rw", "uw", "rw", "ub", "WW"),
        type = c("land", "land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA, NA),
        lands_this_turn = c(TRUE, TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d6 <- goldfish(d5)
    expect_equal(d6$opportunities, c(0L, 0L, 2L, 1L, 0L))
    d7 <- data.frame(cost = c("R", "rw", "uw", "rgrg"),
        type = c("land", "spell", "spell", "spell"),
        turn = c(1, NA, NA, NA),
        lands_this_turn = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    set.seed(5347)
    d8 <- goldfish(shuffle_deck(d7))
    expect_equal(d8$opportunities, c(0L, 0L, 1L, 0L))
    d9 <- data.frame(cost = c("R", "rw", "uw", "rgrg", "W", "G"),
        type = c("land", "spell", "spell", "spell", "land", "land"),
        stringsAsFactors = FALSE)
    d10 <- goldfish(d9, hand = 4L)
    expect_equal(d10[c("cost", "type")], d9)
    expect_equal(d10$opportunities, c(0, 3, 2, 1, 0, 0))
    expect_equal(d10$turn, c(1, NA, NA, NA, 2, 3))
    # search land enters and taps, finding tapped land
    d11 <- data.frame(cost = c("wg", "rw", "uw", "rgrg", "W", "G"),
        type = c("land", "spell", "spell", "spell", "land", "land"),
        is_tapped = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        is_search_basic = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d12 <- goldfish(d11, hand = 4L)
    expect_equal(d12[c("cost", "type")], d11[c("cost", "type")])
    expect_equal(d12$opportunities, c(0, 1, 1, 0, 0, 0))
    expect_equal(d12$turn, c(1, NA, NA, NA, 1, 2))
    d13 <- data.frame(
        name = c("Maestros Theater", "Raffine's Tower",
            "Plains", "Halo Scarab", "Forest", "Plains", "Swamp", "Raffine's Informant",
            "Island", "Mysterious Limousine", "Dapper Shieldmate", "Plains",
            "Mountain", "Spara's Adjudicators", "Swamp", "Shattered Seraph",
            "Out of the Way", "Echo Inspector", "Spara's Adjudicators", "Hostile Takeover",
            "Spara's Adjudicators", "Plains", "Halo Scarab", "Disciplined Duelist",
            "Majestic Metamorphosis", "Ominous Parcel", "Glamorous Outlaw",
            "Sleep with the Fishes", "Plains", "Island", "Raffine's Informant",
            "Island", "Swamp", "Rooftop Nuisance", "Obscura Storefront",
            "Backup Agent", "Swamp", "Swamp", "Dapper Shieldmate", "Hold for Ransom"),
        type = c("land", "land", "land", "spell", "land", "land",
            "land", "spell", "land", "spell", "spell", "land", "land", "spell",
            "land", "spell", "spell", "spell", "spell", "spell", "spell",
            "land", "spell", "spell", "spell", "spell", "spell", "spell",
            "land", "land", "spell", "land", "land", "spell", "land", "spell",
            "land", "land", "spell", "spell"),
        cost = c("ubr", "wub", "W",
            "2", "G", "W", "B", "1W", "U", "3WW", "3W", "W", "R", "2GWU",
            "B", "4WUB", "3U", "3U", "2GWU", "2UBR", "2GWU", "W", "2", "GWU",
            "2U", "1", "3UBR", "2UU", "W", "U", "1W", "U", "B", "2U", "wub",
            "1W", "B", "B", "3W", "1W"),
        is_tapped = c(TRUE, TRUE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE),
        is_search_basic = c(TRUE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
        is_basic = c(FALSE, FALSE, TRUE, FALSE, TRUE,
            TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
            TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
            TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
        stringsAsFactors = FALSE)
    d14 <- goldfish(d13)
    expect_equal(d14$opportunities[1:15],
            c(0, 0, 0, 6, 0,
              0, 0, 6, 0, 3,
              3, 0, 0, 1, 0))
})
