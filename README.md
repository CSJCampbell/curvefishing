# curvefishing

The game **Magic: the Gathering** consists of playing _lands_, which allow a player to cast _spells_ to play against another player.
This package uses a simplified simulation engine to goldfish for the mana curve. 

    # install devtools for devtools::install_github
    install.packages("devtools")
    library(devtools)
    # install curvefishing
    install_github("CSJCampbell/curvefishing/curvefishing")

The input to the package tools are deck data frames with column names:

* _type_: character column with values `"land"` and `"spell"`
* _cost_: character column with WUBRG mana costs, e.g. `"1WUB"`
* _number_: optional numeric column, number of cards to include in deck, e.g. `4`

To estimate the fish metric for a deck, the `go_fish` function will expand decklists with the number column, then shuffle and analyse the first 7 turns for opportunities to play.

    library(dplyr)
    library(curvefishing)
    rock <- tribble(
      ~number,            ~name,   ~type, ~cost, ~is_tapped,
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
      2,   "Diabolic Servitude", "spell", "3BB", FALSE,
      4,      "Treetop Village",  "land",   "G",  TRUE,
      2,            "Dust Bowl",  "land",   "1", FALSE,
      6,                "Swamp",  "land",   "B", FALSE,
     11,               "Forest",  "land",   "G", FALSE)
    f1 <- go_fish(decklist = rock)
    f1
    # ><>
    #  14
    plot(f1)

![Turn 7 curve fish for the rock](/fish_the_rock.png?raw=true)

Note that spells are not played, so no costs can be paid by spells, and no additional cards drawn.  
