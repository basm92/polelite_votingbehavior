#staatsschuldwet 1914
# unaniem aangenomen

voor <- c("de Boer", " Zijlma", 
" Colijn", " Drucker", " Reekers", 
" Franssen", " Cremer",
" van Waterschoot van der Gracht", 
" van Holthe tot Echten", 
" Bosch van Oud-Amelisweerd", " van Nierop", 
" van den Berg", " Kraus", 
" van Wassenaer van Catwiick", " Regout", 
" Gilissen", " Smits", " van Swaay", 
" van Houten", " van Welderen Rengers", 
" van der Hoeven", " Dojes", " de Vos van Steenwijk",
" van den Biesen", " de Gijselaar", 
" Tjarda van Starkenborgh Stachouwer", " van Kol", 
" Polak", " Michiels van Kessenich", 
" van Lamsweerde", " Bavinck", " Staal", 
" van der Does de Willebois", " van der Feltz", 
" Woltjer", " Bergsma", " van der Maesen de Sombreff", 
" van der Lande",
"van Basten Batenburg", "van Voorst tot Voorst")

vote_1 <- rep(1, length(voor))

staatsschuldwet1914_ek <- data.frame(politician = voor, vote = vote_1, 
           law = "Staatsschuldwet 1914", house = "Eerste Kamer",
           date = "1914-12-23")