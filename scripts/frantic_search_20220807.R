
library(dplyr)
library(curvefishing)
library(ggplot2)
library(dplyr)
library(tidyr)

limited_1 <- read.csv("limited_1.csv", stringsAsFactors = FALSE)
limited_1

f1 <- go_fish(decklist = limited_1, nsim = 1000)

fishing1 <- attr(x = f1, which = "fishing")
fd1 <- data.frame(
    x = seq_along(fishing1) / length(fishing1),
    y = sort(fishing1))
ggplot(fd1,
       mapping = aes(x, y)) +
    geom_step() +
    geom_point(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red")) +
    geom_text(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red"),
              mapping = aes(label = round(y, digits = 1)),
              nudge_y = c(2, -2)) +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw()
ggsave("uwxxx1.png")

# maximise number of opportunities

combs <- expand.grid("W" = 0:15, "U" = 0:15, "B" = 0:15, "R" = 0:15, "G" = 0:15)
combs <- as.matrix(filter(combs, apply(combs, MARGIN = 1, FUN = function(x) sum(x) == 15L)))

res <- vector(mode = "list", length = nrow(combs))

for (cc in seq_along(res)) {
    message("combination ", cc, " of ", nrow(combs), " at ", Sys.time())
    limited_cc <- tweak_lands(decklist = limited_1, landnumber = combs[cc, , drop = TRUE])
    f_cc <- go_fish(decklist = limited_cc, nsim = 100)
    res[[cc]] <- f_cc
}

combs <- as.data.frame(combs)

combs$mean <- vapply(X = res, FUN = mean, FUN.VALUE = numeric(1))

quantile(combs$mean, probs = c(0.5, 0.9, 0.95, 0.99, 1))
sum(combs$mean > 15)
combs[combs$mean > 15, ]


ggplot(
    mutate(
        pivot_longer(
            mutate(
                combs,
                split = factor(case_when(
                    W > 8 ~ "W",
                    U > 8 ~ "U",
                    B > 8 ~ "B",
                    R > 8 ~ "R",
                    G > 8 ~ "G",
                    W + U > 9 ~ "WU",
                    U + B > 9 ~ "UB",
                    B + R > 9 ~ "BR",
                    R + G > 9 ~ "RG",
                    G + W > 9 ~ "GW",
                    W + B > 9 ~ "WB",
                    U + R > 9 ~ "UR",
                    B + G > 9 ~ "BG",
                    R + W > 9 ~ "RW",
                    G + U > 9 ~ "GU",
                    TRUE ~ "M"
                ), levels = c("W", "U", "B", "R", "G",
                              "WU", "UB", "BR", "RG", "GW",
                              "WB", "UR", "BG", "RW", "GU",
                              "M"))
            ), cols = -c(mean, split), names_to = "Basic", values_to = "Count"),
        Basic = factor(Basic, levels = c("W", "U", "B", "R", "G"))),
    mapping = aes(
        x = Basic,
        y = Count,
        group = mean,
        color = mean)) +
    geom_line(alpha = 0.6) +
    scale_colour_gradientn(colours = viridisLite::magma(50), limit = c(3, 15.5)) +
    xlab(NULL) +
    theme_bw() +
    facet_wrap(~split, ncol = 5) +
    theme(legend.position = c(0.7, 0.07),
          legend.direction = "horizontal")
ggsave("uwxxx2.png")


combs_top3 <- combs[combs$mean > 15, ]
res_top3 <- res_ls <- vector(mode = "list", length = nrow(combs_top3))
for (i in seq_along(res_ls)) {
    limited_i <- tweak_lands(decklist = limited_1, landnumber = unlist(combs_top3[i, 1:5, drop = TRUE]))
    res_top3[[i]] <- go_fish(decklist = limited_i, nsim = 1000)
    fishing <- attr(x = res_top3[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1)
}

vapply(X = res_top3, FUN = mean, FUN.VALUE = numeric(1))

fd2 <- left_join(
    x = bind_rows(res_ls),
    y = transmute(combs_top3, basics = paste0(W, U, B, R, G), i = 0:2),
    by = "i")

ggplot(fd2,
       mapping = aes(x = basics, y)) +
    geom_boxplot(fill = "lightblue") +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    guides("fill" = "none") +
    xlab("Number of Basics (WUBRG)") +
    ylab("Opportunities") +
    theme_bw()

ggplot(fd2,
       mapping = aes(x, y)) +
    geom_step() +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw() +
    facet_grid(. ~ I(basics))

fd3 <- bind_rows(fd2[-3], mutate(fd1, basics = "66111"))

ggplot(fd3,
       mapping = aes(x, y, colour = basics)) +
    geom_step() +
    # geom_point(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red")) +
    # geom_text(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red"),
    #           mapping = aes(label = round(y, digits = 1)),
    #           nudge_y = c(2, -2)) +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw()

ggsave("uwxxx3.png")
