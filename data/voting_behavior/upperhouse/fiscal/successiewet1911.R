# successiewet 1911 ek
# unaniem

voor <- c("Lucasse", " Eranssen", " de Jong", " van Nierop", " van den Berg", " 
    van der Maesen de Sombreff", " van den Biesen", " 
    van der Does de Willebois", " van Lamsweerde", " Bloembergen", " 
    van Waterschoot van der Gracht", " 
    van Voorst tot Voorst", " Prinzen", " van Leeuwen", "
    van Lanschot", " van Pallandt", " Verheijen", " Laan", " 
    Breebaart", " Havelaar", " Barge", " Staal", " Kist", " 
    Woltjer", " do Marez Oyens", "Aumale baron van Hardenbroek van Hardenbroek", " Welt", "
    Dojes", " van der Feltz", " Lely", 
    " van Löben Sels", " van der Kun", 
" Bosch van Drakestein", " Pelinck", 
" Sickenga", " van Weideren Rengers", 
"  Röell", " van Wassenaer van Rosande", "Schimmelpenninck van der Oye")

vote_1 <- rep(1, length(voor))

successiewet1911_ek <- data.frame(politician = voor, vote = vote_1, law = "Successiewet 1911",
           house = "Eerste Kamer", date = "1911-05-18")