data = read.csv("macierze3.csv", header = TRUE, sep=";")

macierz <- function(x){
  m <- data[x,2:17]
  matrix(unlist(m),4,4,byrow = TRUE)
}

nr = data[,1]

wyznacznik <- function(x){
  round(det(macierz(x)))
}


odwrotna <- function(x){
  if (wyznacznik(x) != 0) {
    round(solve(macierz(x)), digits = 2 )
  }
  else paste("Brak")
}

odwracalnosc <- function(x){
  if (wyznacznik(x) == 0 ) {
    paste("Nieodwracalna")
  }
  else paste("Odwracalna")
}

wektory_wl <- function(x){
  Re(round(eigen(macierz(x))$vectors, digits = 2))
}


wartosci_wl <- function(x){
  Re(round(eigen(macierz(x))$values, digits = 2))
}

slad <- function(x){
  sum(diag(macierz(x)))
} 

transponowana <- function(x){
  t(macierz(x))
}

macierz3 <- function(x){
  macierz(x)%*%macierz(x)%*%macierz(x)
}

suma <- function(x){
  sum(macierz(x))
}

latex <- function(macierz){
  latex = paste(macierz[,1], macierz[,2], macierz[,3], macierz[,4], sep = "&", collapse = "\\\\")
  paste("$\\begin{bmatrix}",latex, "\\end{bmatrix}$")
}

#wykresy

q = 0
w = 0

for (n in 1:length(nr)) {
  if (odwracalnosc(n) == "Odwracalna") {
    q = q + 1
  }
  else w = w + 1
}

odwracalnosc1 = c("Odwracalna" = q, "Nieodwracalna" = w)

png('odwracalnosc.png')
barplot(odwracalnosc1, col=c("green","blue"), main = 'Odwracalnosc')
dev.off()

q1 = 0
q11 = 0
w1 = 0

for (n in 1:length(nr)) {
  if (slad(n) > 0) {
    q11 = q11 + 1
    if (wyznacznik(n)<0) {
      q1 = q1 + 1
    }
    else w1 = w1 + 1
  }
}

wekt1 = c("Dodatni" = q1, "Ujemny" = w1)

png('wykres1.png')
barplot(wekt1, col=c("green","yellow"), main = 'Znak wyznacznika przy ujemnym œladzie')
dev.off()

q2 = 0 
w2 = 0

for (n in 1:length(nr)){
  if (sum(macierz(n)[1,]) < wyznacznik(n)) {
    q2 = q2 +1
  }
  else w2 = w2 +1
}

wekt2 = c("True" = w2, "False" = q2)

png('wykres2.png',height = 540, width = 700)
barplot(wekt2, col=c("green","blue"), main = 'Sume elementow pierwszego wiersza wiêksza od wyznacznika:')
dev.off()

q3 = 0
q31 = 0
w3 = 0

for (n in 1:length(nr)){
  if (slad(n) > 0 ) {
    q31 = q31 + 1
    if (macierz(n)[16] > 0 ) {
      q3 = q3 + 1
    }
    else w3 = w3 + 1 
  }
}

wekt3 = c("Dodatni" = q3, "Ujemny" = w3)

png('wykres3.png',height = 540, width = 700)
pie(wekt3, col=c("blue","green"), main = 'Znak osatniego elementu przy dodatnim œladzie:')
dev.off()


q4 = 0
w4 = 0

for (n in 1:length(nr)){
  if (det(macierz3(n)) > 50) {
    q4 = q4 +1
  }
  else w4 = w4 +1
}

wekt4 = c("Wiekszy od 50" = q4, "Mniejszy od 50" = w4)


png('wykres4.png',height = 540, width = 700)
pie(wekt4, col=c("green","yellow"), main = 'Wyznacznik macierz do 3 potegi')
dev.off()


q5 = 0
w5 = 0

for (n in 1:length(nr)){
  if (slad(n) <= 0 ) {
    q5 = q5 + 1
  }
  else w5 = w5 + 1 
}

wekt5 = c("Mniejszy od 0" = q5, "Silnie wiekszy od 0" = w5)

png('wykres5.png',height = 540, width = 700)
pie(wekt5, col=c("green","yellow"), main = 'Znak œladu')
dev.off()



d = 0
u = 0
z = 0

for (n in 1:length(nr)){
  if (wyznacznik(n)>0) {
    d = d + 1
  }
  if (wyznacznik(n)<0) {
   u = u +1 
  }
  if (wyznacznik(n)==0) {
    z = z +1
  }
}

znak_wyznacznika = c("Dodatni" = d, "Ujemny"= u, "Zerowy" = z)


png('wykres6.png',height = 540, width = 700)
barplot(znak_wyznacznika, col=c("gold"), main = 'znak wyznacznika')
dev.off()


#latex
file.copy("Podstawowa preambu³a.tex","v3.tex", overwrite = TRUE)
f = "v3.tex"

write("\\title{Raport macierze}\\author{Tworzyk Damian}\\date{1 Maja 2022} ", f, append = TRUE)
write("\\begin{document} ", f, append = TRUE)
write("\\maketitle \\newpage", f, append = TRUE)
write("\\section{Wprowadzenie} Raport przedstawia wlasnosci macierzy oraz niektore z nich przedstawione na wykresach. Zawieraja
one informacje o wyznaczniku, odwracalnosci, wektorach i wartosciach wlasnych oraz dodatkowo slad,
transponowanie, macierz do trzeciej potegi i suma wszystkich elementow \\newpage", f, append = TRUE)

write("\\section{Wlasciwosci} ", f, append = TRUE)
for (n in 1:length(nr)){
  write("Numer macierzy:", f, append = TRUE)
  write(nr[n], f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Macierz:\\\\", f, append = TRUE)
  write("", f, append = TRUE)
  write(latex(macierz(n)), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Macierz odwrotna:\\\\", f, append = TRUE)
  write("", f, append = TRUE)
  if (class(odwrotna(n))[1] == "matrix") {
    write(latex(odwrotna(n)), f, append = TRUE)
  }
  else {write("Brak", f, append = TRUE)}
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Macierz transponowana:\\\\", f, append = TRUE)
  write("", f, append = TRUE)
  write(latex(transponowana(n)), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Macierz do potegi 3:\\\\", f, append = TRUE)
  write("", f, append = TRUE)
  write(latex(macierz3(n)), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Wektory wlasne:\\\\", f, append = TRUE)
  write("", f, append = TRUE)
  write(latex(wektory_wl(n)), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Slad:\\\\", f, append = TRUE)
  write(slad(n), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Wyznacznik:\\\\", f, append = TRUE)
  write(wyznacznik(n), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Odwracalnosc:\\\\", f, append = TRUE)
  write(odwracalnosc(n), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Wartosci wlasne:\\\\", f, append = TRUE)
  write(wartosci_wl(n), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("Suma elementow:\\\\", f, append = TRUE)
  write(suma(n), f, append = TRUE)
  write("\\\\", f, append = TRUE)
  write("\\newpage", f, append = TRUE)
}


write("\\newpage", f, append=TRUE)

write("\\section{Wykresy}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{odwracalnosc.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres1.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres2.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres3.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres4.jpg}\\\\", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres5.jpg}", f,append=TRUE)
write("\\includegraphics[scale=0.5]{wykres6.jpg}", f,append=TRUE)

write("\\end{document}", f, append = TRUE)


