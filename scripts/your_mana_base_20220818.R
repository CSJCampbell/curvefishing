
library(dplyr)
library(curvefishing)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

limited <- read.csv("limited_3.csv", stringsAsFactors = FALSE)
limited

# f1 <- go_fish(decklist = limited, nsim = 1000)
# fishing1 <- attr(x = f1, which = "fishing")
# fd1 <- data.frame(
#     x = seq_along(fishing1) / length(fishing1),
#     y = sort(fishing1))
# ggplot(fd1,
#        mapping = aes(x, y)) +
#     geom_step() +
#     geom_point(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red")) +
#     geom_text(data = data.frame(x = c(0.5, 0.5), y = c(f1, mean(fishing1))), colour = c("black", "red"),
#               mapping = aes(label = round(y, digits = 1)),
#               nudge_y = c(2, -2)) +
#     xlab("Probability") +
#     ylab("Opportunities") +
#     theme_bw()
# ggsave("rgw1.png")

# maximise number of opportunities

combs <- expand.grid("R" = 3:7, "G" = 1:6, "W" = 0:4)
combs <- as.matrix(filter(combs, apply(combs, MARGIN = 1, FUN = function(x) sum(x) == 13L)))
combs <- as.data.frame(combs)
res <- vector(mode = "list", length = nrow(combs))

# for (cc in seq_along(res)) {
#     message("combination ", cc, " of ", nrow(combs), " at ", Sys.time())
#     limited_cc <- tweak_lands(decklist = limited, landnumber = combs[cc, , drop = TRUE])
#     f_cc <- go_fish(decklist = limited_cc, nsim = 1000)
#     res[[cc]] <- f_cc
# }
#
# combs <- as.data.frame(combs)
#
# combs$mean <- vapply(X = res, FUN = function(x) mean(attr(x = x, which = "fishing")), FUN.VALUE = numeric(1))
#
# quantile(combs$mean, probs = c(0.5, 0.9, 0.95, 0.99, 1))
#
# ggplot(
#     mutate(
#         pivot_longer(
#             combs,
#             cols = -c(mean), names_to = "Basic", values_to = "Count"),
#         Basic = factor(Basic, levels = c("W", "U", "B", "R", "G"))),
#     mapping = aes(
#         x = Basic,
#         y = Count,
#         group = mean,
#         color = mean)) +
#     geom_line(alpha = 0.6) +
#     scale_colour_gradientn(colours = viridisLite::magma(50)) +
#     xlab(NULL) +
#     theme_bw()


res_ls <- vector(mode = "list", length = nrow(combs))

for (i in seq_along(res_ls)) {
    message("combination ", i, " of ", nrow(combs), " at ", Sys.time())
    combs_i <- unlist(combs[i, 1:3, drop = TRUE])
    limited_i <- tweak_lands(decklist = limited, landnumber = combs_i)
    res[[i]] <- go_fish(decklist = limited_i, nsim = 2000)
    fishing <- attr(x = res[[i]], which = "fishing")
    res_ls[[i]] <- data.frame(
        x = seq_along(fishing) / length(fishing),
        y = sort(fishing),
        i = i - 1,
        R = combs_i[["R"]],
        G = combs_i[["G"]],
        W = combs_i[["W"]],
        basics = paste0(combs_i, collapse = ""),
        mean = mean(fishing))
}

# vapply(X = res, FUN = function(x) mean(attr(x = x, which = "fishing")), FUN.VALUE = numeric(1))

# mean_742 <- combs[combs$R == 7 & combs$G == 4 & combs$W == 2, "mean", drop = TRUE]

fd2 <- bind_rows(res_ls)
mean_742 <- fd2[fd2$basics == "742", "mean", drop = TRUE][1]

# fd2 <- left_join(
#     x = bind_rows(res_ls),
#     y = mutate(combs, basics = paste0(R, G, W), i = seq_len(nrow(combs)) - 1),
#     by = "i")

ggplot(mutate(filter(fd2, mean >= mean_742), basics = fct_reorder(basics, -mean)),
       mapping = aes(x = basics, y, fill = basics)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", colour = "white") +
    geom_hline(yintercept = mean_742, colour = "grey") +
    guides("fill" = "none") +
    xlab("Number of Basics (RGW)") +
    ylab("Opportunities") +
    theme_bw()

ggsave("rgw3.png")

ggplot(mutate(filter(fd2, mean >= mean_742), basics = fct_reorder(basics, -mean)),
       mapping = aes(x = y, fill = basics)) +
    geom_density() +
    geom_vline(xintercept = mean_742, colour = "grey") +
    facet_grid(basics ~ .) +
    guides("fill" = "none") +
    xlab("Opportunities") +
    ylab("Density") +
    theme_bw()

ggsave("rgw4.png")

ggplot(filter(fd2, mean >= mean_742),
       mapping = aes(x, y)) +
    geom_step() +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw() +
    facet_grid(. ~ I(basics))

ggplot(filter(fd2, mean >= mean_742),
       mapping = aes(x, y, colour = basics)) +
    geom_step() +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw()

res_df <- vector(mode = "list", length = nrow(combs))
for (i in seq_along(res)) {
    fishing <- attr(x = res[[i]], which = "fishing")
    res_df[[i]] <- data.frame(
        opportunities = fishing,
        mean = fd2[fd2$i + 1 == i, "mean", drop = TRUE],
        basics = fd2[fd2$i + 1 == i, "basics", drop = TRUE])
}
od3 <- bind_rows(res_df)

quantile(od3$opportunities[od3$basics == "742"], probs = c(0.5, 0.9, 0.95, 0.99, 1))

od3 <- od3[od3$mean >= mean_742, ]
aov3 <- aov(opportunities ~ basics, data = od3)
TukeyHSD(aov3)
