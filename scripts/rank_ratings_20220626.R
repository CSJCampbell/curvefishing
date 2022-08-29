
library(PlayerRatings)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# download from https://www.17lands.com/public_datasets
url_name <- "https://17lands-public.s3.amazonaws.com/analysis_data/game_data"
game_file_zipped_name <- "game_data_public.HBG.PremierDraft.csv.gz"
if (!file.exists(game_file_zipped_name)) {
    download.file(url = file.path(url_name, game_file_zipped_name),
                  destfile = game_file_zipped_name)
}
game1 <- read_csv(file = gzfile(game_file_zipped_name))

game1 <- game1 %>% mutate(
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

game1 %>%
    group_by(rank, opp_rank) %>%
    summarise(`N games` = n(),
              win_rate = sum(won) / n()) %>%
    arrange(desc(win_rate))

game1 %>%
    group_by(on_play) %>%
    summarise(`N games` = n(),
              win_rate = sum(won) / n()) %>%
    arrange(desc(win_rate))

# manually estimate gamma
est_gamma <- 22
m1 <- glicko(data.frame(d = 1, a = 1, b = 2, c = 1), status = data.frame(Player = 1:2, Rating = c(2047, 2000), Deviation = 10), gamma = est_gamma)
predict(m1, newdata = data.frame(d = 2, a = 1, b = 2), tng = 0, gamma = est_gamma)
predict(m1, newdata = data.frame(d = 2, a = 1, b = 2), tng = 0, gamma = -est_gamma)

# proportion pair up/down
summarise(group_by(game1, `Ranks Equal` = rank != opp_rank), N = n())

game2 <- transmute(filter(game1, rank != opp_rank),
    day = as.numeric(date - min(date)),
    rank,
    opp_rank,
    won = as.numeric(won),
    on_play = ((on_play) - 0.5) * 2 * est_gamma)

ranks <- c("none", "bronze", "silver", "gold", "platinum", "diamond", "mythic")
init <- data.frame(Player = ranks,
    Rating = c(1800, 2000, 2100, 2200, 2300, 2400, 2500),
    Deviation = 50
)

# estimate rank ratings
pr1 <- glicko(x = select(filter(game2, day <= 18), -5),
    status = init,
    gamma = filter(game2, day <= 18)$on_play,
    cval = 30)
pr1$ratings


save(pr1, file = "pr1.rda")

preds <- predict(pr1,
    newdata = select(filter(game2, day > 18), 1:3), gamma = filter(game2, day > 18)$on_play)

# confusion matrix
m1 <- filter(game2, day > 18) %>%
    cbind(`Pred Win Rate` = preds) %>%
    group_by(
        `Pred Win` = cut(`Pred Win Rate`, breaks = c(0, 0.5, 1), include = TRUE, labels = c("Lose", "Win")),
        `Obs Win` = factor(won, levels = 0:1, labels = c("Lost", "Won"))) %>%
    summarise(N = n(), .groups = "drop") %>%
    pivot_wider(`Obs Win`, names_from = `Pred Win`, values_from = N)
m1
# accuracy
(m1[1, 2] + m1[2, 3]) / sum(m1[-1])

# calibration chart
qu1 <- quantile(preds, probs = (0:10) / 10)
filter(game2, day > 18) %>%
    cbind(`Pred Win Rate` = preds) %>%
    group_by(`Pred Win Rate Grp` = cut(`Pred Win Rate`, breaks = qu1, include = TRUE)) %>%
    summarise(
        `Pred Win Rate` = mean(`Pred Win Rate`),
        `Obs Win Rate` = sum(won) / n(),
        N = n()) %>%
    ggplot(mapping = aes(x = `Pred Win Rate`, y = `Obs Win Rate`)) +
    geom_abline(colour = "grey") +
    geom_point(mapping = aes(alpha = N)) +
    geom_smooth(se = FALSE) +
    theme_bw()
ggsave("rank_ratings1.png")

combs <- expand.grid(day = 19,
    rank = pr1$ratings$Player,
    opp_rank = pr1$ratings$Player)
preds_rank <- predict(pr1,
    newdata = combs,
    gamma = est_gamma)

cbind(mutate(combs,
        rank = factor(rank, levels = ranks, labels = str_to_title(ranks)),
        opp_rank = factor(opp_rank, levels = ranks, labels = str_to_title(ranks))), `Prob Win (%)` = preds_rank * 100) %>%
    ggplot(mapping = aes(x = opp_rank, y = rank, fill = `Prob Win (%)`)) +
    geom_tile() +
    geom_text(mapping = aes(label = sprintf(fmt = "%1.0f", `Prob Win (%)`))) +
    xlab("Opponent's Rank") +
    ylab("Rank") +
    scale_fill_viridis_c() +
    theme_bw()
ggsave("rank_ratings2.png")
