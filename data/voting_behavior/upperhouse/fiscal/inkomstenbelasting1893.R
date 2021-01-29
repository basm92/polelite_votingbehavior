#inkomstenbelasting1893_ek <- data.frame()

voor <- c("Viruly", " Magnée", " Vlielander Hein", " van Lier", 
" de Jong", " E. Gremers", " Pijnappel", " Vening Meinesz", 
" Blijdenstein", " Fransen van de Putte", " Bultman", 
" Welt", " Kist", " Wertheim", " van Alphen", 
" Nebbens Sterling", " Muller", " Stork", " A. Prins",
" Merkelbach", " J. Prins", " Fokker", 
" Alberda van Ekenstein", 
" Coenen", " Breebaart", 
" van Roijen en van Gennep")

vote_1 <- rep(1, length(voor))


tegen <- c("Engelberts", " Rahusen", " Verheijen",
" L. van Nispen", " Melvil van Lynden", " Smitz",
" Godin de Beaufort", " Pyls", " W. Cremers", 
" de Savornin Lohman", " Prinzen", 
" van Nispen tot Pannerdeu", " Sassen", 
" Regout", " van Pallandt van Waardenburg",
"Neerijnen",  "van Naamen van Eemnes")

vote_2 <- rep(0, length(tegen))

inkomstenbelasting1893_ek <- data.frame(politician = c(voor, tegen), 
           vote = c(vote_1, vote_2), 
           law = "Inkomstenbelasting 1893", 
           house = "Eerste Kamer", 
           date = "1893-09-28"
           )

