BV94isoch <- function(WtPctNaCl, Th) {
  #function to calculate the isochore for a H2O-NaCl fluid using the salinity in weight percent NaCl and the homogenization temperature in degrees celcius
  xrt = ((18.28+1.4413*WtPctNaCl+0.0047241*WtPctNaCl^2-0.0024213*WtPctNaCl^3+0.000038064*WtPctNaCl^4)+(0.019041-0.015268*WtPctNaCl+0.000566012*WtPctNaCl^2-0.0000042329*WtPctNaCl^3-0.000000030354*WtPctNaCl^4)*Th+(-0.00015988+0.000036892*WtPctNaCl-0.0000019473*WtPctNaCl^2+0.000000041674*WtPctNaCl^3-0.00000000033008*WtPctNaCl^4)*Th^2)
}

B93s <- function(Tm) {
  #function to calculate the salinity using Temperature on ice melting in equation 1 from Bodnar (1993)
  WtPctNaCl = 0.00 + 1.78 * abs(Tm) - 0.0442 * abs(Tm)^2 + 0.000557 * abs(Tm)^3
}

ZF87mol <- function(WtPctNaCl){
  #calculate the molality using the salinity in weight percent NaCl
  molality = 55.55 * (18.01534 * WtPctNaCl / (18.01534 * WtPctNaCl + 58.4428 * (100 - WtPctNaCl))) / (1 - (18.01534 * WtPctNaCl / (18.01534 * WtPctNaCl + 58.4428 * (100 - WtPctNaCl))))
}

ZF87rho <- function(molality, Th){
  #if M<5 calculate the density using molality (m) into equation 22 from Zhang and Frantz (1987), Chemical Geology, vol. 64, pg. 335-350
  #if M>5 calculate the density using Th and WtPctNaCl into equation A1 from Bodnar (1983)
  g1 <- 1.0014
  g2 <- -0.00022323
  g3 <- -0.0000013472
  g4 <- -0.0000000046597
  g5 <- 0.023547
  g6 <- 0.0045636
  g7 <- 0.00048805
  g8 <- -0.00006498
  g9 <- -0.000053074
  g10 <- 0.000001009
  as <- g1 + g5*molality + g6*molality^2 + g7*molality^3
  bs <- g2 + g8*molality + g9*molality^2
  cs <- g3 + g10*molality
  dens = as + bs * Th + cs * Th^2 + g4*Th^3 
}
  
B83rho <- function(Th, WtPctNaCl){
# calculate the density using Th and WtPctNaCl into equation A1 from Bodnar (1983)
  tr <- Th/100
  sr <- WtPctNaCl/10
  dens = 0.9923 - 0.030512 * tr^2 - 0.00021977 * tr^4 + 0.086241 * sr - 0.041768 * tr * sr + 0.014825 * tr^2 * sr + 0.0014460 * tr^3 * sr - 0.0000000030852 * tr^8 * sr + 0.013051 * tr * sr^2 - 0.0061402 * tr^2 * sr^2 - 0.0012843 * tr * sr^3 + 0.00037604 * tr^2 * sr^3 - 0.0000000099594 * tr^2 * sr^7
}


## function to calculate Pressure given Temperature and Salinity using equation 1 from Atkinson (2002)
## use in temperature range -21.2 to 300 C
## input Temperature(T) in celcius and salinity(S) in weight percent NaCl

A02e1 <- function(T, S){
  b100 <- -27.2444260945847
  b110 <- 19.6752698743832
  b120 <- -6.03987279418686
  b130 <- 1.05318996126294
  b140 <- -0.107523830798341
  b150 <- 0.0063216048751384
  b160 <- -0.000201391014431089
  b170 <- 0.0000029370853393422
  b101 <- -3.43823854919821
  b111 <- 2.94599944070388
  b121 <- -1.02712250242286
  b131 <- 0.132241696257544
  b151 <- -0.000792743415349166
  b112 <- -5.05958726370657
  b122 <- 5.3605248349362
  b132 <- -1.43460393380215
  b142 <- 0.115824432759682
  b103 <- 49.6470810423974
  b113 <- -78.057932502234
  b123 <- 18.9371135629599
  b133 <- -0.517812895404853
  b143 <- -0.147543794540513
  b104 <- 284.920532084465
  b124 <- -27.5731923399911
  b134 <- 4.28981312806579
  b105 <- -671.468924936888
  b115 <- 272.541674619991
  b125 <- -43.6261697756668
  b106 <- -306.420824097701
  b116 <- 191.281847960897
  b107 <- -476.3912831617690
  xr1 <- S/100
  tr1 <- (T+273.15)/100
  P = 10^(b100 + b101*xr1 + b103*xr1^3 + b104*xr1^4 + b105*xr1^5 + b106*xr1^6 + b107*xr1^7 + b110*tr1 + b120*tr1^2 + b130*tr1^3 + b140*tr1^4 + b150*tr1^5 + b160*tr1^6 + b170*tr1^7 + b111*tr1*xr1 + b112*tr1*xr1^2 + b113*tr1*xr1^3 + b115*tr1*xr1^5 + b116*tr1*xr1^6 +b121*tr1^2*xr1 + b122*tr1^2*xr1^2 + b123*tr1^2*xr1^3 + b124*tr1^2*xr1^4 + b125*tr1^2*xr1^5 + b131*tr1^3*xr1 + b132*tr1^3*xr1^2 + b133*tr1^3*xr1^3 + b134*tr1^3*xr1^4 + b142*tr1^4*xr1^2 + b143*tr1^4*xr1^3 + b151*tr1^5*xr1)
}

## function to calculate Pressure given Temperature and Salinity using equation 2 from Atkinson (2002)
## use in temperature range 300 to 484 C
## input Temperature(T) in celcius and salinity(S) in weight percent NaCl

## It is unclear if coefficient b60 in table 3 should be used for the final portion of the equation (ie, "b61*T6*X1") or if b61 is 0, 0 is used here

A02e2 <- function(Th, WtPctNaCl){
  b200 <- -7.25091694178014
  b201 <- 52.7356253268118
  b202 <- -95.139589091999
  b203 <- 237.001333358832
  b204 <- -270.853923550159
  b206 <- -645.843502315573
  b207 <- 411.125432162485
  b210 <- 2.91847163119225
  b211 <- -30.0742277771278
  b212 <- 41.0055099275695
  b213 <- -82.6121950337547
  b214 <- 86.8864476861164
  b215 <- 85.0303689746524
  b216 <- -24.1431916305905
  b220 <- -0.28727704099067
  b221 <- 5.98143068258883
  b222 <- -5.87588111777108
  b223 <- 7.89995198785004
  b224 <- -10.886118260654
  b230 <- 0.00898343968706551
  b231 <- -0.436696998416016
  b232 <- 0.27654361988714
  b243 <- -0.0074586977134027
  b251 <- 0.000828114542697018
  b260 <- 0.0000015228510009732
  b261 <- 0
  xr2 <- WtPctNaCl/100
  tr2 <- (Th+273.15)/100
  P = 10^(b200 + b201*xr2 + b202*xr2^2 + b203*xr2^3 + b204*xr2^4 + b206*xr2^6 + b207*xr2^7 + b210*tr2 + b220*tr2^2 + b230*tr2^3 + b260*tr2^6 + b211*tr2*xr2 + b212*tr2*xr2^2 + b213*tr2*xr2^3 + b214*tr2*xr2^4 + b215*tr2*xr2^5 + b216*tr2*xr2^6 + b221*tr2^2*xr2 + b222*tr2^2*xr2^2 + b223*tr2^2*xr2^3 + b224*tr2^2*xr2^4 + b231*tr2^3*xr2 + b232*tr2^3*xr2^2 + b243*tr2^4*xr2^3 + b251*tr2^5*xr2 + b261*tr2^6*xr2)
}

## function to calculate Pressure given Temperature and Salinity using equation 2 from Atkinson (2002)
## use in temperature range 300 to 484 C
## input Temperature(T) in celcius and salinity(S) in weight percent NaCl

## It is unclear if coefficient b16 in table 4 is 0, 0 is used here, same is true for b23

A02e3 <- function(Th, WtPctNaCl){
  b300 <- -10.1708289018151
  b302 <- 11.2757426837744
  b303 <- -36.872487022751
  b304 <- 194.960961125252
  b305 <- -412.422321803683
  b306 <- 386.510614818185
  b307 <- -139.824224670799
  b310 <- 4.7809083340614
  b311 <- -0.40967075242513
  b312 <- -4.54787095348135
  b314 <- 7.95552629364737
  b315 <- -6.37018268058654
  b320 <-  -0.739844995911952
  b321 <- 0.253297767997384
  b322 <- 0.47135407604605
  b324 <- 0.546929925768266
  b330 <- 0.06103819662666580000
  b331 <- -0.0345961482768031
  b332 <- 0.0366507031096144
  b333 <- -0.0221690359176769
  b340 <- -0.00273833143331212000
  b341 <- 0.0003090020876693
  b343 <- 0.000111317870749135
  b350 <- 0.00008040575345729920
  b351 <- -0.000005651762407103
  b360 <- -0.00000127759502052200
  b370 <- 0.00000000897506220730
  xr3 <- WtPctNaCl/100
  tr3 <- (Th+273.15)/100
  P = 10^(b300 + b302*xr3^2 + b303*xr3^3 + b304*xr3^4 + b305*xr3^5 + b306*xr3^6 + b307*xr3^7 + b310*tr3 + b320*tr3^2 + b330*tr3^3 + b340*tr3^4 + b350*tr3^5 + b360*tr3^6 + b370*tr3^7 + b311*tr3*xr3 + b312*tr3*xr3^2 + b314*tr3*xr3^4 + b315*tr3*xr3^5 + b321*tr3^2*xr3 + b322*tr3^2*xr3^2 + b324*tr3^2*xr3^4 + b331*tr3^3*xr3 + b332*tr3^3*xr3^2 + b333*tr3^3*xr3^3 + b341*tr3^4*xr3 + b343*tr3^4*xr3^3 + b351*tr3^5*xr3)
}

## Knight and Bodnar (1989) equation to calculate the critical temperature

KB89ct <- function(WtpctNaCl){
  Tcrit <- 374.1 + 8.8 * WtpctNaCl + 0.1771 * WtpctNaCl^2 - 0.02113 * WtpctNaCl^3 + 0.0007334 * WtpctNaCl^4
}

## Knight and Bodnar (1989) equation to calculate the critical pressure

KB89cp <- function(Tcrit){
  Pcrit <- 2094 - 20.56 * Tcrit + 0.06896 * Tcrit^2 - 0.00008903 * Tcrit^3 + 0.00000004214 * Tcrit^4
}

## Sterner, Hall and Bodnar equation to calculate NaCl solubility

SHB88Zs <- function(Tm){
  SHBpsi <- Tm/100
  WtPctNaCl <- 26.242 + 0.4928 * SHBpsi + 1.42 * SHBpsi^2 - 0.223 * SHBpsi^3 + 0.04129 * SHBpsi^4 + 0.006295 * SHBpsi^5 - 0.001967 * SHBpsi^6 + 0.0001112 * SHBpsi^7
}

SHB88Nas <- function(Tm){
  a <- 39.19843568
  b <- 13.59271070
  c <- -12.95646989
  d <- -22.39663458
  e <- -1.52357614
  f <- 9.29672450
  g <- 6.55485254
  h <- -0.66512956
  i <- -0.39464296
  j <- 0.83679331
  k <- -3.61131899
  l <- 0.00275314
  m <- 0.00822892
  n <- -0.00608193
  o <- 0.00344002
  p <- -0.00531261
  q <- 0.04128563
  r <- 0.00629464
  s <- 0.00488294
  tr <- Tm/100
  WtPctNaCl <- a + b * tr + c + d * tr + e * tr^2 + f * tr + g * tr^2 + h * tr^3 + i * tr^3 + j * tr^3 + k * tr^2 + l * tr^7 + m * tr^6 + n * tr^7 + o * tr^7 + p * tr^6 + q * tr^4 + r * tr^5 + s * tr^6
}

SHB88hhs <- function(Tm){
  a <- 40.36947594
  b <- 14.80771966
  c <- -14.08238722
  tr <- Tm/100
  WtPctNaCl <- a + b*tr + c
}

LSB12p <- function(Tm, Th){
  PatTh <- 87.5232693318019 + Th^2* -0.410049875259057 + Th^3 * 0.00115907340158665 + 1.77287229548973 * Tm + Tm^2 * -0.00953597270388461 + Tm^3 * 0.00037967073890189 + Th * Tm * 0.33525139919695 + Th * Tm^2 * -0.00164242453216317 + Th^2* Tm * 0.00118974098346504 + Th^2* Tm^4 * 0.00000000000282835751035787 + Th^3 * Tm * -0.0000066648110839168 + Th^3 * Tm^2 * 0.0000000255742997455 + Th^3 * Tm^3 * -0.0000000000435446772743859 + Th^3 * Tm^4 * 0.0000000000000202257752380179 + Th^4 * Tm * -0.0000000034212870046 + Th^4 * Tm^3 * 0.0000000000000187505885651732 + Th^4 * Tm^4 * -0.0000000000000000151982791793341
}

LSB12s <- function(Tm, Th){
  WtPctNaCl <- 26.4575-0.000361*Th^2+0.00000055302*Th^3+(0.010765+0.0003697*Th-0.0000001544*Th^2-0.000000000379*Th^3)*Tm
}

LSB12rho <- function(Tm, Th){
  dens <- 1.17409380847416 + Th * Th * 0.0000003419910544866 + Th * Th * Th * -0.0000000097510758897 + 0.000113203232231015 * Tm + Tm * Tm * 0.0000021472131887127 + Th * Tm * -0.0000039306105206257 + Th * Tm * Tm * -0.0000000034820260987 + Th * Th * Tm * 0.0000000085101215958 + Th * Th * Tm * Tm * 0.0000000000038536934497866 + Th * Th * Th * Tm * 0.0000000000202101552856566 + Th * Th * Th * Tm * Tm * -0.0000000000000197393383070816
}

LSB12isoch <- function(Tm, Th){
  dpdt <- -5.01872449367917 + 0.521117855127741 * Th + Th * Th * -0.00276532636651147 + Th * Th * Th * -0.0000056616797510133 + 0.167219283196215 * Tm + Tm * Tm * -0.00022855921978017 + Tm * Tm * Tm * 0.0000030812875257656 + Th * Tm * -0.00322688273803601 + Th * Tm * Tm * -0.0000046457607039912 + Th * Th * Tm * 0.0000337530246311533 + Th * Th * Tm * Tm * -0.0000000600890446176 + Th * Th * Tm * Tm * Tm * 0.0000000000421856907337845 + Th * Th * Th * Tm * Tm * 0.0000000000273435969059504 + Th * Th * Th * Tm * Tm * Tm * -0.0000000000000322220546243756
}