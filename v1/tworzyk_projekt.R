macierze = read.table("macierze1.csv", header = TRUE, sep=";")

wektor = split(macierze, macierze$Numer.macierzy)

macierz = function(x){
  wk1 = wektor[x]
  wk1 = unlist(wk1)
  wk1 = wk1[2:17]
  wk1 = data.frame(wk1)
  wk1 = wk1[,1]
  matrix(wk1[1:16],4,4, byrow = TRUE)
}

liczba_wierszy <- (macierze[,1])

array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("$\\begin{bmatrix}", matrix_string, "\\end{bmatrix}$"))
}


###wyznaczniki

wyznacznik = function(x){
  det(macierz(x))
}

###odwracalność
odwracalnosc = function(x){
  if (round(det(macierz(x)) == 0)) {
    paste('Nie')
  }
  else paste('Tak')
}


###macierze odwrotne
macierze_odwrotne = function(x){
  if (round(det(macierz(x)) == 0 )) {
    paste('Brak macierzy odwrotnej')
  }
  else round(solve(macierz(x)), digits = 3)
}

###wektory wlasne

wektory_wlasne = function(x){
  Re(round(eigen(macierz(x))$vectors, digits = 3))
}

###wartosci wlasne

wartosci_wlasne = function(x){
  matrix(Re(round(eigen(macierz(x))$values, digits = 3)), 4,1)
}

suma_diagonali = function(x){
  sum(diag(macierz(x)))
}

srednia_wartosc = function(x){
  mean(macierz(x))
}

elementy_zerowe = function(x){
  sum(colSums(macierz(x) == 0))
}


file.copy("Podstawowa preambuła.tex","tworzyk_projekt.tex", overwrite = TRUE)
p = "tworzyk_projekt.tex"


write("\\title{Raport macierze}\\author{Tworzyk Damian} ", p, append = TRUE)
write("\\begin{document} ", p, append = TRUE)
write("\\maketitle  \\tableofcontents \\newpage", p, append = TRUE)
write("\\begin{center}\\vspace*{5pt}
\\begingroup \\section{Wprowadzenie}
Ponizszy raport zawiera wybrane wlasciwosci zestawu 733 macierzy, dla przejrzystosci zostal podzielony na trzy rozdzialy. Pierwszy z nich zawiera informacje na temat wyznacznikow, odwracalnosci i macierzy odwrotnych, w kolejnym znajduja sie wektory wlasne, wartosci wlasne oraz wybrane wlasciwosci elementow macierzy, w trzecim graficzna reprezentacja wybranych wlasciwosci. 
\\end{center}\\endgroup\\vspace*{5pt}
\\newpage", p, append = TRUE)
write("\\vspace*{\\fill}\\begingroup\\section{Rozdzial 1, Macierze odwrotne, wyznaczniki, odwracalnosc}\\endgroup
      \\vspace*{\\fill}\\newpage", p, append = TRUE)



for (n in 1:733) {
  if (n%%10 == 1 ) {
    write("\\bgroup \\def\\arraystretch{1.2} \\vspace{0.2in} \\begin{tabular}{c c c c c}", p, append=TRUE)   
    write("LP & Macierz & Macierz odwrotna & Wyznacznik & Odwracalnosc\\\\", p, append = TRUE)
  }
  write(liczba_wierszy[n], p, append = TRUE)
  write('&', p, append = TRUE)
  write(array_to_LaTeX(macierz(n)), p, append = TRUE)
  write('&', p, append = TRUE)
  if (class(macierze_odwrotne(n))[1] == "matrix") {
  write(array_to_LaTeX(macierze_odwrotne(n)), p, append = TRUE)
  }
  else{write("Brak", p, append = TRUE)}
  write('&', p, append = TRUE)
  write(wyznacznik(n), p, append = TRUE)
  write('&', p, append = TRUE)
  write(odwracalnosc(n), p, append = TRUE)
  write('\\\\', p, append = TRUE)  
  if (n%%10 == 0) {
    write("\\end{tabular} \\egroup \\newpage", p, append=TRUE)
  }
}
write("\\end{tabular} \\egroup", p, append=TRUE)



write("\\vspace*{\\fill}\\begingroup\\section{Rozdzial 2, wektory wlasne, wartosci wlasne, srednia elementow macierzy ,suma elementow na diagonali, ilosc elementow zerowych}\\endgroup
      \\vspace*{\\fill}\\newpage", p, append = TRUE)

for (n in 1:733) {
  if (n%%10 == 1 ) {
    write("\\bgroup \\def\\arraystretch{1.2} \\vspace{0.2in} \\begin{tabular}{c c c c c c}", p, append=TRUE)   
    write("LP &Wektory wlasne & wartosci wlasne & Srednia el & suma diagonali & ilosc. el 0\\\\", p, append = TRUE)
  }
  write(liczba_wierszy[n], p, append = TRUE)
  write('&', p, append = TRUE)
  write(array_to_LaTeX(wektory_wlasne(n)), p, append = TRUE)
  write('&', p, append = TRUE)
  write(array_to_LaTeX(wartosci_wlasne(n)), p, append = TRUE)
  write('&', p, append = TRUE)
  write(srednia_wartosc(n), p, append = TRUE)
  write('&', p, append = TRUE)
  write(suma_diagonali(n), p, append = TRUE)
  write('&', p, append = TRUE)
  write(elementy_zerowe(n), p, append = TRUE)
  write('\\\\', p, append = TRUE)  
  if (n%%10 == 0) {
    write("\\end{tabular} \\egroup \\newpage", p, append=TRUE)
  }
}
write("\\end{tabular} \\ \\newpage", p, append=TRUE)






wartosci_wlasne(1)

lista_wyznacznikow = c()

for (n in 1:733) {
  lista_wyznacznikow = append(lista_wyznacznikow, det(macierz(n)))  
}


jpeg("wyznaczniki.jpg")
plot(lista_wyznacznikow, main = "Rozklad wyznacznikow", xlab = "Numer macierzy", ylab = "Wyznacznik")
dev.off()


warunek1 = c()
for (n in 1:733) {
  if (wyznacznik(n) > 0 & srednia_wartosc(n) < 0) {
    warunek1 = append(warunek1, n)
  }
}

warunek1 = c("True" = length(warunek1), "False" = length(liczba_wierszy) - length(warunek1))
jpeg('warunek1.jpg', height = 540, width = 700)
barplot(warunek1,col=c("yellow","blue"), main = "dodatni wyznacznik, ujemna srednia elementow")
dev.off()



srednia_wartosc_ujemna = c()
srednia_wartosc_dodatnia = c()
srednia_wartosc_zerowa = c()
for (n in 1:733) {
  if (srednia_wartosc(n)>0) {
    srednia_wartosc_dodatnia = append(srednia_wartosc_dodatnia, srednia_wartosc(n))
  }
  if (srednia_wartosc(n)<0) {
    srednia_wartosc_ujemna = append(srednia_wartosc_ujemna, srednia_wartosc(n))
  }
  if(srednia_wartosc(n) == 0){
    srednia_wartosc_zerowa = append(srednia_wartosc_zerowa, srednia_wartosc(n))
  }
}
srednia_wartosc_elementow = c('dodatnia'=length(srednia_wartosc_dodatnia), 'ujemna'= length(srednia_wartosc_ujemna), 'zerowa' = length(srednia_wartosc_zerowa))

jpeg('srednia_wartosc_elementow.jpg')
pie(srednia_wartosc_elementow, col=c("blue","yellow","green"), main = "Srednia wartosc elementow")
dev.off()


dodatnie = c()
ujemne = c()
zerowe = c()
for (n in 1:733) {
  if (lista_wyznacznikow[n]>0) {
    dodatnie = append(dodatnie, lista_wyznacznikow[n])
  }
  if (lista_wyznacznikow[n]<0) {
    ujemne = append(ujemne, lista_wyznacznikow[n])
  }
  if (lista_wyznacznikow[n] == 0 ) {
    zerowe = append(zerowe, lista_wyznacznikow[n])
  }
}

znaki_wyznacznikow = c("dodatnie" = length(dodatnie), "ujemne" = length(ujemne), "zerowe" = length(zerowe))

jpeg("znaki_wyznacznika.jpg")
pie(znaki_wyznacznikow,main="znak wyznacznika",col=c("blue","green","yellow"))
dev.off()


odwracalnosc = c('odwracalne' = length(dodatnie) + length(ujemne), 'nieodwracalne'= length(zerowe))

jpeg("odwracalnosc.jpg")
barplot(odwracalnosc, col=c("green","blue"), main = "Odwracalnosc")
dev.off()


srednia_wartosci_wlasnych = function(x){
  mean(wartosci_wlasne(x))
}

warunek2 = c()
for (n in 1:length(lista_wyznacznikow)) {
  if (lista_wyznacznikow[n] > 0 & lista_wyznacznikow[n] < 2137 & srednia_wartosci_wlasnych(n)>0) {
    warunek2 = append(warunek2, liczba_wierszy[n])
  }
}

warunek2
wartosci_wlasne(1)>0

warunek2 = c('True' = length(warunek2), 'False' = length(lista_wyznacznikow) - length(warunek2) )

jpeg("warunek2.jpg",height = 540, width = 700)
barplot(warunek2, col=c("red","black"), main = "Macierz z wyznacznikiem w przedziale [0,2137] o dodatniej sredniej wartosci wlasnych")
dev.off()


suma_macierzy = function(x){
  sum(macierz(x))
}


warunek3 = c()
for (n in 1:length(liczba_wierszy)) {
  if (lista_wyznacznikow[n] > 0 & suma_diagonali(n) < 0 & srednia_wartosc(n) < 0 & suma_macierzy(n) < 0 ) {
    warunek3 = append(warunek3, liczba_wierszy[n])
  }  
}

warunek3 = c('True'= length(warunek3), 'False' = length(dodatnie) - length(warunek3))

jpeg("warunek3.jpg",height = 540, width = 800)
barplot(warunek3, col=c("blue","yellow"), main = "Macierze z dodatnim wyznacznikiem o ujemnej sumie diagonali, sredniej wartosci i sumie elementow", )
dev.off()


write("\\vspace*{\\fill}\\begingroup\\section{Rozdzial 3, Graficzna reprezentacja wybranych wlasciwosci}\\endgroup
\\vspace*{\\fill}\\newpage", p, append = TRUE)


write("\\includegraphics[scale=0.5]{odwracalnosc.jpg}", p,append=TRUE)
write("\\includegraphics[scale=0.5]{srednia_wartosc_elementow.jpg}\\\\", p,append=TRUE)
write("\\includegraphics[scale=0.5]{wyznaczniki.jpg}", p,append=TRUE)
write("\\includegraphics[scale=0.5]{znaki_wyznacznika.jpg}\\\\", p,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek1.jpg}\\\\", p,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek2.jpg}\\\\", p,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek3.jpg}", p,append=TRUE)


write("\\end{document} ", p, append = TRUE)




