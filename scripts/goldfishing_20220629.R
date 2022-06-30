
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

f1 <- go_fish(decklist = sligh, nsim = 500)
fishing1 <- attr(x = f1, which = "fishing")
fd1 <- data.frame(
    x = seq_along(fishing1) / length(fishing1),
    y = sort(fishing1))
ggplot(fd1,
    mapping = aes(x, y)) +
    geom_step() +
    xlab("Probability") +
    ylab("Opportunities") +
    theme_bw()
ggsave("sligh.png")

f2 <- go_fish(decklist = rock, nsim = 500)
fishing2 <- attr(x = f2, which = "fishing")
fd2 <- data.frame(
  x = seq_along(fishing2) / length(fishing2),
  y = sort(fishing2))
ggplot(fd2,
       mapping = aes(x, y)) +
  geom_step() +
  xlab("Probability") +
  ylab("Opportunities") +
  theme_bw()
ggsave("rock.png")
f2

