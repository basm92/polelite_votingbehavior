# successiewet 1921
# eerste kamer

voor <- c("Cremer", " Kraus", " Binnerts", " Van der Maesen de Sombreff", 
" van der Hoeven", " Bergsma", " Idenburg", 
" Wittert van Hoogland",
" Pothuis-Smit", " van Embden", " Vliegen", 
" van der Lande", " Polak", 
" Mendels", " Dojes", " Diepenhorst", 
" van Houten", " Smeenge", " van Swaay", " Gilissen",
" Geertsema", " de Waal Malefijt", " van Nierop", 
" van der Feltz", " Lucasse", " van den Berg", 
" van Loon", " Slingenberg", " Bosch van Oud-Amelisweerd", 
" van Basten Batenburg", " van Lanschot", " van der Does de Willebois", 
" Haffmans", "van Voorst tot Voorst")

vote_1 <- rep(1, length(voor))

tegen <- c("Verheijen", "Reekers", "Fokker",
" van Wassenaer van Catwijck"," 't Hooft", "Smits",
"de Vos van Steenwijk")

vote_2 <- rep(0, length(tegen))

successiewet1921_ek <- data.frame(politician = c(voor, tegen), vote = c(vote_1, vote_2),
           law = "Successiewet 1921", 
           house = "Eerste Kamer", 
           date = "1921-06-09")