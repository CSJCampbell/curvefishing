
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(curl)
library(jsonlite)
library(curvefishing)
library(digest)
library(ggplot2)

if (!file.exists("pr1.rda")) {
    source("rank_ratings_20220626.R")
} else {
    load("pr1.rda")
    est_gamma <- 22
}

# download from https://www.17lands.com/public_datasets
url_name <- "https://17lands-public.s3.amazonaws.com/analysis_data/game_data"
game_file_zipped_name <- "game_data_public.HBG.PremierDraft.csv.gz"
if (!file.exists(game_file_zipped_name)) {
    download.file(url = file.path(url_name, game_file_zipped_name),
        destfile = game_file_zipped_name)
}
game1 <- read_csv(file = gzfile(game_file_zipped_name))

# premier draft is BO1
# remove where game_number = 2
# actually, on discussion with devs, these look fine to leave in
game1 <- filter(game1, game_number != 2)

# this is _slow_
# handle restarts
if (file.exists("out1.rda")) { 
    load("out1.rda")
} else {
    out1 <- matrix(NA_real_, nrow = nrow(game1), ncol = 10, 
        dimnames = list(NULL, c(
            "mean_fish", 
            "q1_fish", "median_fish", "q3_fish", 
            "num_plains", "num_island", "num_swamp", 
            "num_mountain", "num_forest", "num_non_basic")))
}
if (file.exists("deck1.rda")) { 
    load("deck1.rda")
} else {
    deck1 <- character(length = nrow(game1))
}
is_ne_zero <- function(x) sum(x) != 0
if_null_zero <- function(x) { 
    x[is.na(x)] <- 0
    x 
}
if_null_blank <- function(x) { 
    x[is.na(x)] <- ""
    x
}

#' @examples 
#' mana_cost_to_cost(x = c("{1}", "{3}{W} // {W}", "{1}{B}")) == c("1", "3W", "1B")
mana_cost_to_cost <- function(x) {
    x1 <- str_split(string = str_remove_all(
        string = x, pattern = "\\{|\\}"), pattern = " // ")
    is_2x <- vapply(X = x1, FUN = function(x) length(x) > 1L, FUN.VALUE = logical(1))
    if (any(is_2x)) {
        for (ii in which(is_2x)) {
            x1[[ii]] <- x1[[ii]][which.min(cost_to_mana_value(x1[[ii]]))]
        }
    }
    unlist(x1)
}
db0 <- matrix("FALSE", nrow = 600, ncol = 8, 
    dimnames = list(NULL, c(
        "name", "type", "cost", "is_tapped", "is_basic", "type_line", "mana_cost", "oracle_text")))

if (file.exists("db1.rda")) {
    load("db1.rda")
} else {
    # set up database of cards
    db2 <- db0
    db2[1:5, "name"] <- c("Plains", "Island", "Swamp", "Mountain", "Forest")
    db2[1:5, "type"] <- "land"
    db2[1:5, "cost"] <- c("W", "U", "B", "R", "G")
    db2[1:5, "is_basic"] <- "TRUE"
    db2[1:5, "type_line"] <- c("Basic Land — Plains", "Basic Land — Island", "Basic Land — Swamp", "Basic Land — Mountain", "Basic Land — Forest")
    
    # sort card names so we can check response agrees with cards
    cards_in_set <- colnames(select(game1, contains("deck_"))) %>% 
        str_remove(pattern = "^deck_(A-){0,1}")
    cards_in_set <- cards_in_set[!cards_in_set %in% c("Plains", "Island", "Swamp", "Mountain", "Forest")] %>% 
        sort
    for (i in 1:8) {
        cards <- na.omit(cards_in_set[seq_len(35)+(i-1)*35])
        con_i <- curl(
            url = paste0("https://api.scryfall.com/cards/search?q=", 
                URLencode(paste0('!"', paste0(cards, collapse = '" or !"'), '"'), 
                    reserved = TRUE)))
        res <- fromJSON(con_i)
        res_i <- as.data.frame(res$data) %>% arrange(name) %>% 
            mutate(short_name = str_remove(name, pattern = " // .*$"))
        use_rows <- which(db2[, 1] == "FALSE")[seq_along(cards)]
        if (sum(cards != res_i$short_name) > 0) { stop("response is not ordered to match cards") }
        db2[use_rows, "name"] <- res_i$short_name
        db2[use_rows, "type_line"] <- res_i$type_line
        db2[use_rows, "type"] <- as.character(
            factor(
                str_detect(string = res_i$type_line, pattern = "Land"), 
                levels = c("TRUE", "FALSE"), labels = c("land", "spell")))
        db2[use_rows, "oracle_text"] <- if_null_blank(res_i$oracle_text)
        db2[use_rows, "is_tapped"] <- db2[use_rows, "type"] == "land" & 
            str_detect(string = db2[use_rows, "oracle_text"], pattern = "tapped")
        db2[use_rows, "mana_cost"] <- res_i$mana_cost
        cost <- mana_cost_to_cost(res_i$mana_cost)
        if (any(cost == "")) {
            # handling for Gates
            if (any(cost == "" & db2[use_rows, "type"] == "land")) {
                message("attempting to handle non-basic land cost in iter ", i)
                cost_land <- character(length = length(use_rows))
                for (mana in c("W", "U", "B", "R", "G", "C")) {
                    add_mana <- str_detect(string = db2[use_rows, "oracle_text"], 
                                           pattern = paste0("\\{T\\}: Add \\{", mana, "\\}"))
                    if (any(add_mana)) {
                        if (mana == "C") { mana <- "1" }
                        cost[cost == "" & db2[use_rows, "type"] == "land" & add_mana] <- mana
                    }
                }
                if (any(cost == "")) { 
                    stop("unhandled missing cost for land in game ", i) 
                }
            } else {
                stop("unhandled missing cost for spell in game ", i)
            }
        }
        db2[use_rows, "cost"] <- cost
        # polite
        Sys.sleep(5)
    }
}
db2 <- db2[db2[, 1] != "FALSE", ]
db1 <- db2
if (!file.exists("hbg_analysis.csv")) {
    # write out results column names
    cat(paste0(paste0(c(
        "draft_id", "draft_time", "deck_digest", "mean_fish",
        "q1_fish", "median_fish", "q3_fish",
        "num_plains", "num_island", "num_swamp",
        "num_mountain", "num_forest", "num_non_basic",
        "match_number", "rank", "opp_rank", "on_play",
        "num_mulligans", "won", "user_game_win_rate_bucket"), collapse = ","), "\n"), 
        file = "hbg_analysis.csv")
}

# perform fish for each unique deck
# when restarting, manually ignore previously analysed rows
for (i in seq_len(nrow(game1))[-(1:46296)]) {
    
    message("game ", i, " of ", nrow(game1), " at ", Sys.time())
    
    # pivot deck row to table
    game_i <- slice(game1, i)
    deck_i <- select(game_i, contains("deck_")) %>% 
        select(where(is_ne_zero)) %>% 
        pivot_longer(everything(), names_to = "name", values_to = "number") %>% 
        mutate(name = str_remove(string = name, pattern = "^deck_(A-){0,1}"))
    if (sum(deck_i$number) < 40) warning("fewer than 40 cards in deck ", i)
    
    # look up card info if not in db
    cards_not_in_db <- deck_i$name[!deck_i$name %in% db1[, "name"]] %>% sort
    
    if (length(cards_not_in_db) > 0L) {
        message("getting ", length(cards_not_in_db), " new card definitions at ", Sys.time())
        is_blank <- which(db1[, "name"] == "FALSE")
        if (sum(is_blank) < length(cards_not_in_db)) {
            db1 <- rbind(db1, db0)
            is_blank <- which(db1[, "name"] == "FALSE")
        }
        use_rows <- is_blank[seq_along(cards_not_in_db)]
        db1[use_rows, "name"] <- cards_not_in_db
        
        # get card info
        # https://scryfall.com/docs/api/cards/search
        con_i <- curl(
            url = paste0("https://api.scryfall.com/cards/search?q=", 
                URLencode(paste0('!"', paste0(cards_not_in_db, collapse = '" or !"'), '"'), 
                    reserved = TRUE)))
        res <- fromJSON(con_i)
        try(close(con_i), silent = TRUE)
        res_i <- as.data.frame(res$data) %>% arrange(name)
        # TODO check rows always align
        db1[use_rows, "type_line"] <- res_i$type_line
        db1[use_rows, "type"] <- as.character(factor(str_detect(string = res_i$type_line, pattern = "Land"), 
            levels = c("TRUE", "FALSE"), labels = c("land", "spell")))
        # TODO handle multiple missing oracle text blocks
        db1[use_rows, "oracle_text"] <- if_null_blank(res_i$oracle_text)
        db1[use_rows, "is_tapped"] <- db1[use_rows, "type"] == "land" & 
            str_detect(string = db1[use_rows, "oracle_text"], pattern = "tapped")
        db1[use_rows, "mana_cost"] <- res_i$mana_cost
        cost <- mana_cost_to_cost(res_i$mana_cost)
        if (any(cost == "")) {
            # handling for Gates
            if (any(cost == "" & db1[use_rows, "type"] == "land")) {
                message("attempting to handle non-basic land cost in game ", i)
                cost_land <- character(length = length(use_rows))
                for (mana in c("W", "U", "B", "R", "G", "C")) {
                    add_mana <- str_detect(string = db1[use_rows, "oracle_text"], 
                        pattern = paste0("\\{T\\}: Add \\{", mana, "\\}"))
                    if (any(add_mana)) {
                        if (mana == "C") { mana <- "1" }
                        cost[cost == "" & db1[use_rows, "type"] == "land" & add_mana] <- mana
                    }
                }
                if (any(cost == "")) { 
                    stop("unhandled missing cost for land in game ", i) 
                }
            } else {
                stop("unhandled missing cost for spell in game ", i)
            }
        }
        db1[use_rows, "cost"] <- cost
    }
    
    deck_i <- left_join(x = deck_i, 
        y = transmute(as.data.frame(db1, stringsAsFactors = FALSE), name, type, cost, 
            is_tapped = as.logical(is_tapped), 
            is_basic = as.logical(is_basic)), 
        by = "name")
    
    # if we have seen identical deck before, use cached results, else get fresh results
    digest_i <- digest(object = deck_i)
    if (digest_i %in% deck1) {
        out1[i, c(
            "mean_fish", 
            "q1_fish", "median_fish", "q3_fish", 
            "num_plains", "num_island", "num_swamp",
            "num_mountain", "num_forest", "num_non_basic")] <- out1[which.max(deck1 %in% digest_i), c(
                "mean_fish", 
                "q1_fish", "median_fish", "q3_fish", 
                "num_plains", "num_island", "num_swamp",
                "num_mountain", "num_forest", "num_non_basic")]
    } else {
        f_i <- go_fish(decklist = deck_i, nsim = 2000)
        ff_i <- attr(f_i, which = "fishing")
        out1[i, c("mean_fish", 
                  "q1_fish", "median_fish", "q3_fish")] <- c(
                  mean(ff_i), 
                  quantile(ff_i, probs = c(0.25, 0.5, 0.75)))
        
        out1[i, c("num_plains", "num_island", "num_swamp",
                  "num_mountain", "num_forest", "num_non_basic")] <- c(
                  if_null_zero(deck_i$number[deck_i$name == "Plains"]), 
                  if_null_zero(deck_i$number[deck_i$name == "Island"]), 
                  if_null_zero(deck_i$number[deck_i$name == "Swamp"]), 
                  if_null_zero(deck_i$number[deck_i$name == "Mountain"]), 
                  if_null_zero(deck_i$number[deck_i$name == "Forest"]), 
                  if_null_zero(sum(deck_i$number[deck_i$type == "land" & !deck_i$is_basic])))
    }
    deck1[i] <- digest_i
    
    message("writing results for game ", i, " at ", Sys.time())
    
    cat(
        paste0(paste0(c(
                game_i$"draft_id", as.character(game_i$"draft_time"), digest_i, 
                out1[i, , drop = TRUE],
                game_i$"match_number", game_i$"rank", game_i$"opp_rank", game_i$"on_play",
                game_i$"num_mulligans", game_i$"won", game_i$"user_game_win_rate_bucket"), 
            collapse = ","), "\n"), 
        file = "hbg_analysis.csv", 
        append = TRUE)
}

save(db1, file = "db1.rda")
save(deck1, file = "deck1.rda")
save(out1, file = "out1.rda")

game_fish <- read_csv("hbg_analysis.csv")

quantile(game_fish$draft_time)

# clean up opp ranks to match rank
game_fish <- game_fish %>% mutate(
    date = as.Date(draft_time),
    rank = replace(rank, is.na(rank), "none"),
    opp_rank = str_replace(
        string = str_replace(
            string =
                str_replace(
                    string = str_replace(
                        string = str_replace(
                            string = str_replace(string = replace(opp_rank, is.na(opp_rank), "none"),
                                pattern = "^Platinum.*", replacement = "platinum"),
                            pattern = "^Diamond.*", replacement = "diamond"),
                        pattern = "^Mythic.*", replacement = "mythic"),
                    pattern = "^Gold.*", replacement = "gold"),
            pattern = "^Bronze.*", replacement = "bronze"),
        pattern = "^Silver.*", replacement = "silver"))

game_fish <- game_fish %>% mutate(
    rank_pred_win = predict(pr1, 
        newdata = data.frame(day = 19, rank, opp_rank), 
        gamma = ((on_play) - 0.5) * 2 * est_gamma))

# # some decks have more than 1 draft id
# # for the most part it seems that this is an incorrect split of the same draft into 2 draft ids
# repeat_decks <- game_fish %>% 
#     group_by(deck_digest) %>% 
#     summarise(`N drafts` = n_distinct(draft_id)) %>% 
#     filter(`N drafts` > 1L)

# game_fish %>% 
#     semi_join(y = slice(repeat_decks, 1L), by = "deck_digest") %>% View


dd1 <- summarise(group_by(game_fish, deck_digest, mean_fish), 
          n_games = n(), 
          win_rate = sum(won) / n(), 
          rank_pred_win_rate = sum(rank_pred_win > 0.5) / n(),
          mulligan_rate = sum(num_mulligans > 0) / n(),
          play_plains = c("N", "Y")[(num_plains[1L] > 4) + 1], 
          play_island = c("N", "Y")[(num_island[1L] > 4) + 1], 
          play_swamp = c("N", "Y")[(num_swamp[1L] > 4) + 1], 
          play_mountain = c("N", "Y")[(num_mountain[1L] > 4) + 1], 
          play_forest = c("N", "Y")[(num_forest[1L] > 4) + 1], 
          lost = n() - sum(won), 
          won = sum(won), .groups = "drop") %>% 
    arrange(desc(win_rate)) 

# possible probabilities (excluding some decks with duplicate records)
#' @title map predictions into probability space
#' @param x numeric vector of probabilities
#' @param n max number of possible games that can be played
#' @return numeric vector of possible probabilities
#' @examples 
#' pred_to_prob(x = c(0, 0.1, 0.5, 0.65, 0.79))
pred_to_prob <- function(x, n = 9) {
    prob_vals <- sort(unique(unlist(lapply(seq_len(n), 
        FUN = function(z) seq_len(z + 1) - 1)) / rep(seq_len(n), times = seq_len(n) + 1)))
    prob_vals[cut(x, breaks = c(-1, diff(prob_vals) / 2, 1) + c(0, prob_vals))]
}

# opportunities does not seem to predict mulligan rate
ggplot(data = dd1, 
       mapping = aes(x = mean_fish, y = mulligan_rate)) + 
    geom_point(alpha = 0.2) + 
    geom_smooth() + 
    theme_bw()

lm1 <- glm(cbind(won, lost) ~ I(mean_fish / 17) + rank_pred_win_rate + 
        mulligan_rate + play_plains + 
        play_island + play_swamp + 
        play_mountain + play_forest, data = dd1, family = binomial)
summary(lm1)

f1 <- seq(from = 12, to = 22, by = 0.1)
df1 <- expand.grid(
    mean_fish = f1, 
    rank_pred_win_rate = 0.5,
    mulligan_rate = c(0, 1/4), 
    play_plains = "N", 
    play_island = c("N", "Y"), 
    play_swamp = "N", 
    play_mountain = "N", 
    play_forest = "N")
preds <- cbind(
    df1, 
    win_rate = predict(lm1, 
        newdata = df1, 
        type = "response"))

ggplot(data = dd1, 
    mapping = aes(x = mean_fish, y = win_rate)) + 
    geom_hline(yintercept = 0.5, colour = "blue") +
    geom_point(alpha = 0.1) + 
    geom_line(data = preds, colour = "red") + 
    # keep 1 * no island 2 = 2
    # mull 2 * no island 2 = 4
    # keep 1 * island 3 = 3
    # mull 2 * island 3 = 6
    facet_grid(. ~ c("", "Keep 7", "Islands Keep 7", "Mulligan(s)", "", "Islands Mulligan(s)")[((mulligan_rate > 0) + 1) * ((play_island == "Y") + 2)]) + 
    theme_bw() +
    theme(strip.text.x = element_text(size = 8)) + 
    xlab("Opportunities") +
    ylab("Win Rate")
ggsave("win_rate1.png", width = 5, height = 4)

brk <- c(8, 16, 17, 18, 19, 26)
dd1 %>% 
    group_by(mean_fish = cut(mean_fish, breaks = brk)) %>% 
    ggplot(mapping = aes(x = mean_fish, y = win_rate)) + 
    geom_violin() + 
    stat_summary(fun = mean, geom = "point", colour = "red") + 
    theme_bw()

dd1 %>% 
    group_by(mean_fish = cut(mean_fish, breaks = brk)) %>% 
    filter(!is.na(mean_fish)) %>%
    summarise(`N decks` = n(), `Mean Win Rate` = mean(win_rate))

dd1 <- mutate(dd1, 
    pred_win_rate = pred_to_prob(predict(lm1, newdata = dd1, type = "response")))

ggplot(filter(dd1, (lost + won) > 2), 
    mapping = aes(x = pred_win_rate, y = win_rate)) +
    geom_point(alpha = 0.01) +
    geom_abline() +
    coord_fixed() +
    theme_bw()

fc1 <- 15
filter(dd1, (won + lost) > 3 & 
           rank_pred_win_rate == 0.5 &
           (
               (round(mean_fish) == fc1 & play_island == "N" & win_rate >= 0.5 & win_rate <= pred_to_prob(filter(preds, mean_fish == fc1 & mulligan_rate == 0 & play_island == "N")$win_rate)) |
               (round(mean_fish) == fc1 & play_island == "N" & win_rate == pred_to_prob(filter(preds, mean_fish == 20 & mulligan_rate == 0 & play_island == "N")$win_rate)) |
               (round(mean_fish) == 19 & play_island == "N" & win_rate >= 0.5 & win_rate <= pred_to_prob(filter(preds, mean_fish == fc1 & mulligan_rate == 0 & play_island == "N")$win_rate)) |
               (round(mean_fish) == 20 & play_island == "N" & win_rate == pred_to_prob(filter(preds, mean_fish == 20 & mulligan_rate == 0 & play_island == "N")$win_rate))
           )) %>% 
    arrange(round(mean_fish), win_rate) %>% View
# f = 15, 15, 19, 20
# w = 0.5, 0.625, 0.5, 0.625
eg_decks <- c("283eb95733b063ffae3b8da06aa85f0e", "4f052be34af0d21580c80f9acb38529f", "cb37927ed4a418c9ab19f1a92b659853", "5176752dba7410851827f8e0139dfa72")
filter(dd1, deck_digest %in% eg_decks)

game_fish_eg <- filter(game_fish, deck_digest %in% eg_decks)
game_fish_eg1 <- group_by(game_fish_eg, deck_digest) %>% slice(1) %>% ungroup %>% 
    select(draft_id, deck_digest, mean_fish, q1_fish, median_fish, q3_fish, num_plains, num_island, num_swamp, num_mountain, num_forest, num_non_basic, rank_pred_win) %>% 
    left_join(y = game1, by = "draft_id") %>% 
    group_by(deck_digest) %>% slice(1) %>% ungroup

decks_eg <- vector(mode = "list", length = nrow(game_fish_eg1))

for (i in seq_along(decks_eg)) {
    game_i <- slice(game_fish_eg1, i)
    deck_i <- select(select(game_i, -deck_digest), contains("deck_")) %>% 
        select(where(is_ne_zero)) %>% 
        pivot_longer(everything(), names_to = "name", values_to = "number") %>% 
        mutate(name = str_remove(string = name, pattern = "^deck_(A-){0,1}"))
    if (sum(deck_i$number) < 40) warning("fewer than 40 cards in deck ", i)
    deck_i <- left_join(x = deck_i, 
        y = transmute(as.data.frame(db1, stringsAsFactors = FALSE), name, type, cost, 
            is_tapped = as.logical(is_tapped), 
            is_basic = as.logical(is_basic)), 
        by = "name")
    decks_eg[[i]] <- deck_i
}

select(game_fish_eg1, deck_digest, mean_fish, rank) %>% 
    left_join(y = select(filter(dd1, deck_digest %in% eg_decks), deck_digest, n_games, won, win_rate, pred_win_rate), 
        by = "deck_digest")

for (i in seq_along(decks_eg)) {
    write_csv(x = arrange(decks_eg[[i]], desc(type), cost_to_mana_value(cost)), path = paste0(game_fish_eg1$deck_digest[i], ".csv"))
}

gih1 <- read_csv("17_lands_gih.csv") %>% 
    mutate(Win_Rate = as.numeric(str_remove(Win_Rate, pattern = "%")), 
           IWD = as.numeric(str_remove(IWD, pattern = "pp")))

left_join(decks_eg[[i]], gih1, by = c("name" = "Name")) %>% arrange(desc(IWD)) %>% View

# mana curve by mana value count
dupe_row <- which(!duplicated(game1$draft_id))
tlast <- Sys.time()
res2 <- matrix(0, nrow = length(dupe_row), ncol = 5, 
    dimnames = list(NULL, c("num_spells", "num_mv_gte_5", "num_mv_gte_6", "win_rate", "num_games")))
for (i in seq_len(nrow(res2))) {
    ind <- dupe_row[i]
    if ((tlast + 5) < Sys.time()) {
        tlast <- Sys.time()
        message("deck ", i, " of ", length(dupe_row), " at ", Sys.time())
    }
    # pivot deck row to table
    game_i <- slice(game1, ind)
    deck_i <- select(game_i, contains("deck_")) %>% 
        select(where(is_ne_zero)) %>% 
        pivot_longer(everything(), names_to = "name", values_to = "number") %>% 
        mutate(name = str_remove(string = name, pattern = "^deck_(A-){0,1}"))
    deck_i <- left_join(x = deck_i, 
    y = transmute(as.data.frame(db1, stringsAsFactors = FALSE), 
                  name, type, cost, 
                  mana_value = cost_to_mana_value(cost)),
    by = "name")
    res2[i, ] <- c(sum(deck_i[deck_i$type == "spell",]$number),
                   sum(deck_i[deck_i$type == "spell" & deck_i$mana_value >= 5,]$number),
                   sum(deck_i[deck_i$type == "spell" & deck_i$mana_value >= 6,]$number),
                   game_i$user_game_win_rate_bucket, 
                   sum(game1$draft_id == game_i$draft_id))
}
q2 <- quantile(res2[, "win_rate"], probs = (0:10)/10, na.rm = TRUE)
t2 <- as.data.frame(res2) %>% 
    filter(num_games > 2 & !is.na(win_rate)) %>% 
    group_by(win_rate = cut(win_rate, breaks = q2, include.lowest = TRUE)) %>% 
    summarise(num_decks = n(),
        prop_gte_5 = sum(num_mv_gte_5) / sum(num_spells),
        prop_gte_6 = sum(num_mv_gte_6) / sum(num_spells))
separate(t2, col = win_rate, into = c("lo", "hi"), sep = ",") %>% 
    mutate(lo = as.numeric(str_remove(lo, pattern = "\\[|\\(")), 
           hi = as.numeric(str_remove(hi, pattern = "\\]"))) %>% 
    group_by(row = seq_len(n())) %>% 
    mutate(mid = sum(c(lo, hi)) / 2) %>% ungroup %>% 
    select(-row) %>% 
    pivot_longer(cols = prop_gte_5:prop_gte_6, names_to = "Mana Value", values_to = "Proportion") %>% 
    mutate(`Mana Value` = factor(`Mana Value`, levels = c("prop_gte_5", "prop_gte_6"), labels = c(">=5", ">=6"))) %>% 
    ggplot(mapping = aes(x = Proportion, y = mid, colour = `Mana Value`)) + 
    geom_point() + 
    geom_smooth(method = "lm") + facet_wrap(~`Mana Value`, scales = "free_x", labeller = label_both) +
    ylab("Win Rate Bucket") + 
    guides(colour = "none") +
    theme_bw()
ggsave("win_rate2.png", width = 5, height = 4)
