#successiewet1916
#eerste kamer
# eerste poging: rejected

tegen <- c("van Wichen", " Bosch van Oud-Amelisweerd", 
" van Swaay", " van den Berg", 
" Smits", " Diepen",
" van Wassenaer van Catwijck", " Verheijen",
" de Vos van Steenwijk", " van Lamsweerde", 
" van der Maesen de Sombreff", " Gilissen", " van der Does de Willebois",
" Haffmans", " Michiels van Kessenich", " Franssen",
" van den Biesen", " van Lanschot", " van Basten Batenburg", 
" Reekers", "Aumale baron van Hardenbroek van Hardenbroek", "van Voorst tot Voorst")

vote_1 <- rep(0, length(tegen))

voor <- c("de Boer", 
" Staal", " van Nierop", 
" van Houten", " Kappeyne van de Coppello",
" Binnerts", " van Kol", " Bergsma",
" Laan", " Stork", " van der Feltz", 
" Dojes", " Kraus", " Drucker", 
" van Holthe tot Echten", " Polak", " Cremer",
"Tjarda van Starkenborgh Stachouwer")

vote_2 <- rep(1, length(voor))

successiewet1916_ek <- data.frame(politician = c(tegen, voor), vote = c(vote_1, vote_2), 
           law = "Successiewet 1916", house = "Eerste Kamer",
           date = "1916-11-29")

