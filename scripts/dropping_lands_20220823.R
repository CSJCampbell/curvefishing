
library(curvefishing)
library(dplyr)
library(forcats)

# modified version of go_fish
decklist = sligh
turns = 5L
play = TRUE
handsize = 7L
nsim = 5000L
fishing <- numeric(length = nsim)
plays <- matrix(nrow = nsim, ncol = turns)
deck <- shuffle_deck(decklist)
for (fsh in seq_along(fishing)) {
    deck_f <- goldfish(deck = shuffle_deck(deck),
                       turns = turns, play = play, handsize = handsize)
    fishing[fsh] <- sum(deck_f$opportunities, na.rm = TRUE)
    plays[fsh, ] <- seq_len(turns) %in% deck_f$turn
}

# summarise output
opps <- data.frame(
        profile = apply(plays, MARGIN = 1, FUN = function(x) paste0(as.numeric(x), collapse = "")),
        nplays = apply(plays, MARGIN = 1, FUN = sum),
        fish = fishing, stringsAsFactors = FALSE)

# visualise
summarise(group_by(opps, profile, nplays), fish = mean(fish, na.rm = TRUE), prob = n() / nsim, .groups = "drop") %>% 
    arrange(nplays * 1000 + fish) %>% 
    mutate(profile = factor(profile, levels = profile)) %>% 
    ggplot(mapping = aes(x = profile, y = fish)) + 
    geom_point() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("sligh_drops.png")
