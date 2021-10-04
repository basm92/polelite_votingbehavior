inkomstenbelasting1872 <- data.frame(politician = c("Nierstrasz"," Heydenrijck","
van Wassenaer van Catwijck "," Jonckbloet"," van Sypesteyn"," Tak ","Rombach",
                                                    "Rutgers van Rozenburg"," de BruynKops"," Westerhoff"," Taets van Amerongen"," van Foreest",
                                                    "van Reenen"," Pyls "," Hoffman"," Storm van 's Gravesande",
                                                    "van Loon"," Sandberg"," Kien"," de Bieberstein "," van Kerkwijk ","Borret",
                                                    "Saaymans Vader "," Mackay"," Godefroi"," van Nispen van Sevenaer ",
                                                    " Verheijen"," van Hardenbroek van Lockhorst","Heemskerk Az."," van den Heuvel",
                                                    " van Naamen van Eemnes","Begram"," Bots"," Smitz"," Kappeyne van de Coppello",
                                                    " van Lynden van Sandenburg"," Viruly Verbrugge"," van Eck","van Kuyk",
                                                    " Wintgens"," Arnoldts"," van Zinnicq Bergmann","van der Doos de Willebois",
                                                    " Haffmans"," Luyben"," de Brauw","C. van Nispen tot Sevenaer",
                                                    " van der Maesen de Sombreff","van Zuylen van Nyevelt"," Dumbar"," 's Jacob")) %>%
    mutate(politician = stringr::str_trim(politician), vote = rep(0, length(politician)))


politician <- c("Bredius "," Oldenhuis Gratama"," Idzerda"," Hingst"," van Houten"," Mirandolle"," Blom","
de Jong "," Cremers"," de Roo van Alderwerelt"," van Beyma thoe Kingma"," Gevers Deynoot"," de Ruiter Zylker"," Dam","
van Delden"," du Marchie van Voorthuysen"," Lenting"," Wybenga"," Fransen van de Putte "," Bergsma"," Smidt"," de Lange"," 
van der Linden"," Moens"," Heemskerk Bz."," Stieltjes"," Dullert") %>%
    stringr::str_trim()

vote <- rep(1, length(politician))

inkomstenbelasting1872 <- rbind(inkomstenbelasting1872, cbind(politician, vote)) %>%
    mutate(law = "Inkomstenbelasting 1872", date = "1872-05-02", house = "Tweede Kamer",
           vote = as.numeric(vote),
           date = lubridate::ymd(date))


#inkomstenbelasting1872 <- get_polid_tk(inkomstenbelasting1872)