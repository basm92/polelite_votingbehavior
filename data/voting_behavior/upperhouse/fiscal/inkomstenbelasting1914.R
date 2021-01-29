#inkomstenbelasting 1914_ek
# unaniem aangenomen

voor <- c("van Kol"," Haffmans"," Franssen"," Reekers",
" Zijlma"," Woltjer"," de Vos van Steenwijk",
" van Weideren Reugers"," van Holthe tot Echten",
" van Houten"," de Boer"," Drucker"," Kraus",
" Dojes"," Bergsma"," Polak"," van Swaay",
" van Waterschoot van der Gracht"," de Gijselaar",
" van Nierop"," 't Hooft"," Fokker",
" van Wassenaar van Catwijck"," Gilissen",
" van Lamsweerde"," Staal",
" van den Biesen"," Bosch van Oud-Ainelisweerd")

vote_1 <- rep(1, length(voor))

inkomstenbelasting1914_ek <- data.frame(politician = voor, 
                                        vote = vote_1, 
                                        date = "1914-12-18", 
                                        law = "Inkomstenbelasting 1914", 
                                        house = "Eerste Kamer")
