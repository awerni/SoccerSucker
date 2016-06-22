loginText <- "Please log in with your account."

db <- list(dbname = "EuroCup2016", host = "myserver", port = 5432, user = "user", password = "pass")

helpGer <- paste("Für den richtigen Sieger (1, 2 oder X) gibt es 3 Punkte, die richtige Tordifferenz",
                 "einen Zusatzpunkt und für das exakt richtige Ergebnis einen weiteren Punkt.",
                 "Gewertet wird das Ergebnis nach der regulären Spielzeit (90 Minuten + Nachspielzeit).",
                 "Ab der KO –Runde gelten fast die gleichen Regeln: Gewertet wird das Ergebnis nach 120min",
                 "(falls erforderlich) und es gibt doppelte Punkte.",
                 "Da das Spiel ab der KO-Runde nach 120 Minuten oft noch nicht entschieden ist,",
                 "muss man, wenn man „Unentschieden“ getippt hat zusätzlich den Sieger wählen.",
                 "Für die richtige Wahl gibt es dann noch 2 Extra-Punkte. Diese beiden Extrapunkte",
                 "gibt es auch für alle, die sich für den richtigen Sieger nach der regulären Spielzeit ",
                 "oder Verlängerung entschieden haben.")

helpEng <- paste("For the correct winner (1, 2, or X) there are 3 points to earn. The right",
                 "goal difference leads to an additional point. If you guess the exact result, you gain another point.",
                 "The points are rewarded after the regular time (90 min. + additional time).",
                 "From the KO-phase on, almost the same rules apply. The result is taken after 120min (regular time and overtime)",
                 "and you get double points.",
                 "In the KO-phase the decision is sometimes not made after the regular time or overtime.",
                 "If you bet for a draw, you have to vote for a winner in addition. For the correct guess",
                 "you gain 2 extra points. These extra points are also added, if you choose the correct",
                 "winner after the regular time or overtime.")
