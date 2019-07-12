BV94isoch <- function(S, Th) {
  #function to calculate the isochore for a H2O-NaCl fluid using the salinity in weight percent NaCl and the homogenization temperature in degrees celcius
xrt = ((18.28+1.4413*S+0.0047241*S^2-0.0024213*S^3+0.000038064*S^4)+(0.019041-0.015268*S+0.000566012*S^2-0.0000042329*S^3-0.000000030354*S^4)*Th+(-0.00015988+0.000036892*S-0.0000019473*S^2+0.000000041674*S^3-0.00000000033008*S^4)*Th^2)
}