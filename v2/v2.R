data = read.csv("macierze2.csv", header = TRUE, sep=";")

macierz <- function(x){
  m <- data[x,2:17]
  m <- unlist(m)
  matrix(m,4,4,byrow = T)
}

#numer macierzy

lp = data[,1]

#wyznacznik

wyznacznik <- function(x){
  round(det(macierz(x)))
}

#odwracalnosc

odwracalnosc <- function(x){
  if (wyznacznik(x) == 0) {
    paste("Nie")
  }
  else{
    paste('Tak')
  }
}

#macierz odwrotna

macierz_odwrotna <- function(x){
  if (wyznacznik(x) != 0) {
    round(solve(macierz(x)), digits = 3)
  }
  else paste("Brak")
}

#wartosci wlasne

wartosci_wlasne <- function(x){
  round(Re(eigen(macierz(x))$values), digits = 3)
}

#wektory wlasne

wektory_wlasne <- function(x){
  round(Re(eigen(macierz(x))$vectors), digits = 2)
}

#slad

slad <- function(x){
  sum(diag(macierz(x)))
}

#macierz transponowana

macierz_transponowana <- function(x){
  t(macierz(x))
}

#ilosc elementow dodatnich

el_dodatnie <- function(x){
  sum(colSums(macierz(x) > 0))
}

#srednia
srednia <- function(x){
  mean(macierz(x))
}

#suma elementow

suma <- function(x){
  sum(macierz(x))
}




#wykresy

#znaki wyznacznikow

d = 0
u = 0
z = 0

for (n in 1:length(lp)) {
  if (wyznacznik(n) > 0) {
    d = d + 1
  }
  if (wyznacznik(n) < 0 ) {
    u = u +1
  }
  if (wyznacznik(n) == 0) {
    z = z +1
  }
}

znaki_wyznacznikow = c("Dodatni" = d, "Ujemny" = u, "Zerowy" = z)

jpeg('znak_wyznacznika.jpg')
pie(znaki_wyznacznikow,col=c("red","green","blue"), main = "Znak wyznacznika")
dev.off()


#parzystosc wyznacznika

parzysty = 0
nieparzysty = 0

for (n in 1:length(lp)){
  if (wyznacznik(n)%%2 == 0) {
    parzysty = parzysty + 1
  }
  else{
    nieparzysty = nieparzysty +1
  }
}

parzystosc_wyznacznika = c("Parzysty" = parzysty, "Nieparzysty" = nieparzysty)

jpeg('parzystosc_wyznacznika.jpg')
barplot(parzystosc_wyznacznika,col=c("green","blue"), main = "Parzystosc wyznacznika")
dev.off()


#znak sladu

d1 = 0
u1 = 0
z1 = 0

for (n in 1:length(lp)){
  if (slad(n) > 0 ) {
    d1 = d1 + 1
  }
  if (slad(n) < 0 ) {
    u1 = u1 + 1
  }
  if (slad(n) == 0) {
    z1 = z1 + 1
  }
}

znak_sladu = c("Dodatni" = d1, "Ujemny" = u1, "Zerowy" = z1)

jpeg('znak_sladu.jpg')
pie(znak_sladu,col=c("yellow","green","blue"), main = "Znak sladu")
dev.off()

#odracalnosc

odwracalna = 0
nieodwracalna = 0

for (n in 1:length(lp)) {
  if (odwracalnosc(n) == "Tak") {
    odwracalna = odwracalna + 1
  }
  else nieodwracalna = nieodwracalna +1
}

odwracalnosc1 = c("Odwracalna" = odwracalna, "Nieodwracalna" = nieodwracalna)

jpeg('odwracalnosc.jpg')
barplot(odwracalnosc1,col=c("green","blue"), main = "Odwracalnosc")
dev.off()

#wyznaniki2

w1 = 0

for (n in 1:length(lp)) {
  if (wyznacznik(n) > 100 && macierz(n)[1] > 0) {
    w1 = w1 + 1
  }
}

w11 = length(lp) - (w1)

warunek1 = c("Tak" = w1, "Nie" = w11)

jpeg('warunek1.jpg', height = 540, width = 700)
barplot(warunek1,col=c("green","blue"), main = "Czy wyznacznik jest wiekszy od 100 i pierwszy element dodatni")
dev.off()


w2 = 0
for (n in 1:length(lp)) {
  if (wyznacznik(n)%%2 == 0  && slad(n)%%2 == 0) {
    w2 = w2 + 1
  }
}

w21 = length(lp) - w2


warunek2 = c("Tak" = w2, "Nie" = w21)

jpeg('warunek2.jpg')
pie(warunek2,col=c("green","blue"), main = "Czy wyznacznik i slad sa parzyste")
dev.off()



w3 = 0

for (n in 1:length(lp)) {
  if (wyznacznik(n) <  0  && sum(macierz(n)[1,]) > 0) {
    w3 = w3 + 1
  }
}

w31 = length(lp) - w3

warunek3 = c("Tak" = w3, "Nie" = w31)

jpeg('warunek3.jpg', height = 540, width = 700)
barplot(warunek3,col=c("green","blue"), main = "Czy wyznacznik jest ujemny, a suma pierwszego wiersza dodatnia")
dev.off()


macierztex <- function(macierz){
    macierztex = paste(macierz[,1], macierz[,2], macierz[,3], macierz[,4], sep = "&", collapse = "\\\\")
    paste("$\\begin{bmatrix}",macierztex, "\\end{bmatrix}$")
}


file.copy("Podstawowa preambu³a.tex","v2.tex", overwrite = TRUE)
f = "v2.tex"


write("\\title{Raport macierze}\\author{Tworzyk Damian} ", f, append = TRUE)
write("\\begin{document} ", f, append = TRUE)
write("\\maketitle \\newpage", f, append = TRUE)
write("\\section{Wprowadzenie} 
      Raport zawiera wlasciwosci i wykresy wlasciwosci macierzy, wlasciwoœci zostaly podzielone na 3 tabele, zawieraja informacje o wyznaczniku, odwracalnosci, macierzy odwrotnej, macierzy transponowanej, wektorach oraz wartosciach wlasnych oraz pewne wlasciwosci elementow macierzy.
      \\newpage", f,append=TRUE)


write("\\section{W³asciwosci}", f,append=TRUE)
for (n in 1:length(lp)) {
  if (n%%10 == 1 ) {
    write("\\bgroup \\def\\arraystretch{1.2} \\vspace{0.2in} \\begin{tabular}{c c c c c}", f, append=TRUE)   
    write("LP & Macierz & Wyznacznik & Odwracalnosc & Macierz odwrotna\\\\", f, append = TRUE)
  }
  write(lp[n], f, append = TRUE)
  write("&", f, append = TRUE)
  write(macierztex(macierz(n)), f, append = TRUE)
  write("&", f, append = TRUE)
  write(wyznacznik(n), f, append = TRUE)
  write("&", f, append = TRUE)
  write(odwracalnosc(n), f, append = TRUE)
  write("&", f, append = TRUE)
  if (class(macierz_odwrotna(n))[1] == "matrix") {
    write(macierztex(macierz_odwrotna(n)), f, append = TRUE)
  }
  else {write("Brak", f, append = TRUE)}
  write('\\\\', f, append = TRUE)
  if (n%%10 == 0) {
    write("\\end{tabular} \\egroup \\newpage", f, append=TRUE)
  }
}
write("\\end{tabular} \\egroup", f, append=TRUE)
write("\\newpage", f, append=TRUE)

for (n in 1:length(lp)) {
  if (n%%10 == 1 ) {
    write("\\bgroup \\def\\arraystretch{1.2} \\vspace{0.2in} \\begin{tabular}{c c c c}", f, append=TRUE)   
    write("LP & Transponowana & Wektor wlasne & Wartosci wlasne\\\\", f, append = TRUE)
  }
  write(lp[n], f, append = TRUE)
  write("&", f, append = TRUE)
  write(macierztex(macierz_transponowana(n)), f, append = TRUE)
  write("&", f, append = TRUE)
  write(macierztex(wektory_wlasne(n)), f, append = TRUE)
  write("&", f, append = TRUE)
  write(wartosci_wlasne(n), f, append = TRUE)
  write('\\\\', f, append = TRUE)
  if (n%%10 == 0) {
    write("\\end{tabular} \\egroup \\newpage", f, append=TRUE)
  }
}
write("\\end{tabular} \\egroup", f, append=TRUE)
write("\\newpage", f, append=TRUE)

for (n in 1:length(lp)) {
  if (n%%10 == 1 ) {
    write("\\bgroup \\def\\arraystretch{1.2} \\vspace{0.2in} \\begin{tabular}{c c c c c c}", f, append=TRUE)   
    write("LP & Macierz & el dodatnie & slad & srednia elementow & suma elementow\\\\", f, append = TRUE)
  }
  write(lp[n], f, append = TRUE)
  write("&", f, append = TRUE)
  write(macierztex(macierz(n)), f, append = TRUE)
  write("&", f, append = TRUE)
  write(el_dodatnie(n), f, append = TRUE)
  write("&", f, append = TRUE)
  write(slad(n), f, append = TRUE)
  write("&", f, append = TRUE)
  write(srednia(n), f, append = TRUE)
  write("&", f, append = TRUE)
  write(suma(n), f, append = TRUE)
  write('\\\\', f, append = TRUE)
  if (n%%10 == 0) {
    write("\\end{tabular} \\egroup \\newpage", f, append=TRUE)
  }
}
write("\\end{tabular} \\egroup", f, append=TRUE)
write("\\newpage", f, append=TRUE)

write("\\section{Wykresy}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{odwracalnosc.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{parzystosc_wyznacznika.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{znak_sladu.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{znak_wyznacznika.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek1.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek2.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{warunek3.jpg}", f,append=TRUE)

write("\\end{document}", f, append = TRUE)

























