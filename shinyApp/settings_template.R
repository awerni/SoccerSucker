db <- list(dbname = "WorldCup2024", host = "myserver", port = 5432, user = "user", password = "pass")
lang <- "en"

logo_file <- "GoStrong.png"
time_zone <- "Europe/Paris"

checkAccount <- function(user, pass) {
  sql <- paste0("SELECT checklogin FROM checkLogin('", user, "', '", pass, "')")
  getPostgresql(sql)$checklogin > 0   
}
  
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
                 "From the KO-phase on, almost the same rules apply. The result is taken after 120min (regular time and",
                 "overtime, if required) and you get double points.",
                 "In the KO-phase the decision is sometimes not made after the regular time or overtime.",
                 "If you bet for a draw, you have to vote for a winner in addition. For the correct guess",
                 "you gain 2 extra points. These extra points are also added, if you choose the correct",
                 "winner after the regular time or overtime.")

helpPor <- paste("Para o vencedor correto (1, 2 ou X) há 3 pontos a ganhar. A diferença de gols correta",
                 "leva a um ponto adicional. Se você acertar o resultado exato, ganha mais um ponto.",
                 "Os pontos são recompensados após o tempo regular (90 min. + tempo adicional).",
                 "A partir da fase nocaute, quase as mesmas regras se aplicam. O resultado é considerado após 120 minutos (tempo regular e",
                 "prorrogação, se necessário) e você ganha pontos duplos.",
                 "Na fase nocaute, a decisão às vezes não é feita após o tempo regular ou prorrogação.",
                 "Se você apostar em um empate, deve votar em um vencedor além disso. Para o palpite correto",
                 "você ganha 2 pontos extras. Esses pontos extras também são adicionados se você escolher o",
                 "vencedor correto após o tempo regular ou prorrogação.")

helpFr <- paste("Pour le bon vainqueur (1, 2 ou X), il y a 3 points à gagner. La bonne différence de buts",
                "mène à un point supplémentaire. Si vous deviez le résultat exact, vous gagnez un autre point.",
                "Les points sont récompensés après le temps réglementaire (90 min. + temps supplémentaire).",
                "À partir de la phase éliminatoire, presque les mêmes règles s'appliquent. Le résultat est pris après 120 minutes (temps réglementaire et",
                "prolongation, si nécessaire) et vous obtenez des points doubles.",
                "En phase éliminatoire, la décision n'est parfois pas prise après le temps réglementaire ou la prolongation.",
                "Si vous pariez sur un match nul, vous devez également voter pour un vainqueur. Pour la bonne supposition",
                "vous gagnez 2 points supplémentaires. Ces points supplémentaires sont également ajoutés si vous choisissez le",
                "bon vainqueur après le temps réglementaire ou la prolongation.")

