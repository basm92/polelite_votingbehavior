# successiewet 1878

voor <- c("Teding van Berkhout", " Borsius ", 
" Pincoffs", " Hein", " Smit", 
" du Marchie van Voorthuysen ", " Viruly", 
" Stork", " Buchner", " Schot", " de Sitter", 
" Dumbar", " van Rhemen van Rhemenshuizen", 
" Duymaer van Twist", 
" Prins", " de Raadt", " van Eysinga", " van Volïenhoven ", "Pické")

vote_1 <- rep(1, length(voor))

tegen <- c("Smits", " Thooft", " van Sasse van Ysselt",
" Hengst", " Beerenbroek", " Geertsema", 
" Huydecoper van Maarsseveen", 
" de Villers de Pité", " Michiels van Kessenich", 
" van Akerlaken", " Blussé", " Coenen", 
" Aylva baron van Pallandt van Waardenburg en Neerijnen", " van Rijckevorsel", 
" Vos de Wael", "de Vos van Steenwijk")

vote_2 <- rep(0, length(tegen))

successiewet1878_ek <- data.frame(politician = c(voor, tegen), vote = c(vote_1, vote_2),
           law = "Successiewet 1878", house = "Eerste Kamer",
           date = "1878-06-04")
