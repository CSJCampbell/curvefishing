
library(dplyr)
library(curvefishing)
library(ggplot2)

rock <- tribble(
~number,                  ~name,   ~type, ~cost, ~is_tapped,
      4,    "Birds of Paradise", "spell",   "G", FALSE,
      1,       "Llanowar Elves", "spell",   "G", FALSE,
      4,         "Albino Troll", "spell",  "1G", FALSE,
      3,       "Yavimaya Elder", "spell", "1GG", FALSE,
      4,     "Yavimaya Granger", "spell",  "2G", FALSE,
      4,      "Deranged Hermit", "spell", "3GG", FALSE,
      1,           "Woodripper", "spell", "3GG", FALSE,
      4, "Phyrexian Plaguelord", "spell", "3BB", FALSE,
      4,               "Duress", "spell",   "B", FALSE,
      2,       "Vampiric Tutor", "spell",   "B", FALSE,
      1,       "Tranquil Grove", "spell",  "1G", FALSE,
      3,          "Rapid Decay", "spell",  "1B", FALSE,
      2,   "Diabolic Servitude", "spell",  "3B", FALSE,
      4,      "Treetop Village",  "land",   "G",  TRUE,
      2,            "Dust Bowl",  "land",   "1", FALSE,
      6,                "Swamp",  "land",   "B", FALSE,
     11,               "Forest",  "land",   "G", FALSE)

# moved tweak_lands into package 20220721

# validate behaviour by having zero swamps, but increasing the number of Forest
rock_r <- rock
rock_r[rock_r$name == "Swamp", c("name", "cost")] <- list("Mountain", "R")

combs <- cbind("R" = 0:17, "G" = 17:0)
res17r <- vector(mode = "list", length = nrow(combs))

for (cc in seq_along(res17r)) {
    rock_cc <- tweak_lands(decklist = rock_r, landnumber = combs[cc, , drop = TRUE])
    f_cc <- go_fish(decklist = rock_cc, nsim = 100)
    res17r[[cc]] <- f_cc
}
vapply(X = res17r, FUN = c, FUN.VALUE = numeric(1))
vapply(X = res17r, FUN = mean, FUN.VALUE = numeric(1))

res_ls <- res17r
for (i in seq_along(res_ls)) {
    fishing <- attr(x = res17r[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1)
}
fd2 <- bind_rows(res_ls)

ggplot(fd2,
       mapping = aes(x = factor(i), y, fill = factor(i))) +
    scale_fill_manual("R", values = colorRampPalette(c("#3c874a", "red"))(18)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    guides("fill" = "none") +
    xlab("Number of Mountain") +
    ylab("Opportunities") +
    theme_bw()

# validate behaviour by having zero swamps, and zero Forest, expect only opps from Treetop Village
rock_r[rock_r$name == "Forest", c("name", "cost")] <- list("Plains", "W")

combs <- cbind("R" = 0:17, "W" = 17:0)
res17r <- vector(mode = "list", length = nrow(combs))

for (cc in seq_along(res17r)) {
    rock_cc <- tweak_lands(decklist = rock_r, landnumber = combs[cc, , drop = TRUE])
    f_cc <- go_fish(decklist = rock_cc, nsim = 100)
    res17r[[cc]] <- f_cc
}
vapply(X = res17r, FUN = c, FUN.VALUE = numeric(1))
vapply(X = res17r, FUN = mean, FUN.VALUE = numeric(1))

res_ls <- res17r
for (i in seq_along(res_ls)) {
    fishing <- attr(x = res17r[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1)
}
fd2 <- bind_rows(res_ls)

ggplot(fd2,
       mapping = aes(x = factor(i), y, fill = factor(i))) +
    scale_fill_manual("R", values = colorRampPalette(c("yellow", "red"))(18)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    guides("fill" = "none") +
    xlab("Number of Mountain") +
    ylab("Opportunities") +
    theme_bw()

# validate behaviour by having zero swamps, and zero Forest, zero Treetop Village
rock_r[rock_r$name == "Treetop Village", c("name", "cost")] <- list("Mishra's Factory", "1")

combs <- cbind("R" = 0:17, "W" = 17:0)
res17r <- vector(mode = "list", length = nrow(combs))

for (cc in seq_along(res17r)) {
    rock_cc <- tweak_lands(decklist = rock_r, landnumber = combs[cc, , drop = TRUE])
    f_cc <- go_fish(decklist = rock_cc, nsim = 100)
    res17r[[cc]] <- f_cc
}
vapply(X = res17r, FUN = c, FUN.VALUE = numeric(1))
vapply(X = res17r, FUN = mean, FUN.VALUE = numeric(1))

res_ls <- res17r
for (i in seq_along(res_ls)) {
    fishing <- attr(x = res17r[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1)
}
fd2 <- bind_rows(res_ls)

ggplot(fd2,
       mapping = aes(x = factor(i), y, fill = factor(i))) +
    scale_fill_manual("R", values = colorRampPalette(c("yellow", "red"))(18)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    guides("fill" = "none") +
    xlab("Number of Mountain") +
    ylab("Opportunities") +
    theme_bw()

# maximise number of opportunities

combs <- cbind("B" = 0:17, "G" = 17:0)
res <- vector(mode = "list", length = nrow(combs))

for (cc in seq_along(res)) {
  rock_cc <- tweak_lands(decklist = rock, landnumber = combs[cc, , drop = TRUE])
  f_cc <- go_fish(decklist = rock_cc, nsim = 1000)
  res[[cc]] <- f_cc
}

vapply(X = res, FUN = c, FUN.VALUE = numeric(1))
vapply(X = res, FUN = mean, FUN.VALUE = numeric(1))

res_ls <- res
for (i in seq_along(res_ls)) {
    fishing <- attr(x = res[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1)
}
fd2 <- bind_rows(res_ls)

ggplot(fd2,
       mapping = aes(x = factor(i), y, fill = factor(i))) +
    scale_fill_manual("B", values = colorRampPalette(c("#3c874a", "#070504"))(18)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    guides("fill" = "none") +
    xlab("Number of Swamp") +
    ylab("Opportunities") +
    theme_bw()

ggplot(fd2,
       mapping = aes(x, y, colour = i + 3)) +
    scale_colour_gradient("B", low = "#3c874a", high = "#070504") +
    geom_step() +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw() +
    ylim(c(0, 33)) +
    facet_grid(. ~ I(i + 3))
ggsave("rock_basics.png")
