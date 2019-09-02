
# Magozzi et al. (2019) Oecologia

#-----------------------------------------------------------------------------#
#                 SS HO MODEL - First- & Second-order parameters              #
#-----------------------------------------------------------------------------#

#### (1) INPUTS ####
# Description of REQUIRED arguments with examples; including first- and second-order parameters -  

# First-order parameters - 

# 1- Waste type; can be "urea" or "uric_acid"
#waste_type = "urea"

# 2- Proportion of carbohydrate in the diet 
#Pcarb = 0.85

# 3- Proportion of protein in the diet 
#Pprot = 0.10

# 4- Proportion of lipid in the diet 
#Pfat = 1 - Pcarb - Pprot 

# 5- Proportion of food mass that is in liquid water form 
#Pw = 0.60

# Second-order parameters

# 6- Body mass [g]
#M = 50 

# 7- Body temperature [degrees C]
#bTc = 37

# 8- O isotope composition of environmental water [per mil]
#d18Oew = -10

# 9- Environmental temperature [degrees C]
#eTc = 20

# 10- Relative humidity 
#Rh = 0.50

# Description of OPTIONAL arguments

# Basal metabolic rate [W=J s^-1]
#BMR = NULL

# Basal metabolic rate [mol O2 d^-1]
#BMR_O2 = NULL

# Field metabolic rate [mol O2 d^-1]
#FMR = NULL

# Total water flux [mol H2O d^-1]
#TWF = NULL

# Flux of drinking water in [mol O2 d^-1]
#Fdw = NULL

# Flux of vapor water out [mol H2O d^-1]
#Fvw = NULL

# H isotope composition of environmental water [per mil]
#d2Hew = NULL

# H isotope composition of environmental leaf water [per mil]
#d2Hleafw = NULL

# H isotope composition of carbohydrate in the diet [per mil]
#d2Hcarb = NULL

# H isotope composition of protein in the diet [per mil]
#d2Hprot = NULL

# H isotope composition of lipid in the diet [per mil]
#d2Hfat = NULL

# H isotope composition of food water [per mil]
#d2Hfw = NULL

# H isotope composition of drinking water [per mil]
#d2Hdw = NULL

# O isotope composition of environmental leaf water [per mil]
#d18Oleafw = NULL

# O isotope composition of carbohydrate in the diet [per mil]
#d18Ocarb = NULL

# O isotope composition of protein in the diet [per mil]
#d18Oprot = NULL

# O isotope composition of lipid in the diet [per mil]
#d18Ofat = NULL

# O isotope composition of food water [per mil]
#d18Ofw = NULL

# O isotope composition of drinking water [per mil]
#d18Odw = NULL


HOmodel = function(waste_type,
				Pcarb,
				Pprot,
				Pfat,
				M,
				bTc,
				Pw, 
				d18Oew,
				eTc,
				Rh,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL) {
					
				
#### (2) CONSTANTS ####

# Functions to convert delta values into R values and vice versa
RstandardH = 0.00015575
RstandardO = 0.0020052

delta.to.R = function(delta, Rstandard) {
  R.value = ((1000 + delta)/1000) * Rstandard
  return(R.value)
}

R.to.delta = function(Rsample, Rstandard) {
  delta.value = (Rsample/Rstandard - 1) * 1000
  return(delta.value)
}	

# rH for each macronutrient: rH = mol H2 produced / mol O2 consumed [mol H2/mol O2]
# Note that H2 produced = H in macronutrient/2
rHcarb = 6/6
rHfat = 16/23
rHprot = if (waste_type == "urea") {7/6} else {28/21}

# Constants for the calculation of the basal metabolic rate [W=J s^-1]
# Normalization constant for endotherms [W (g^(3/4))^-1]
b0 = 2.94 * 10^8
# Elevation coefficient 
elc = 0.71
# Activation energy [eV]
E = 0.63
# Boltzmann's constant [eV K^-1]
k = 8.62 * 10^-5

# Constants for the conversion of the basal metabolic rate into units of [mol O2 d^-1]
# Respiratory quotient for each macronutrient: Rq = mol CO2 produced / mol O2 consumed [mol CO2/mol O2]
Rqcarb = 6/6
Rqfat = 16/23
Rqprot = if (waste_type == "urea") {5/6} else {14/21} 
# Enthalpy of combustion for each macronutrient [kJ/mol O2]
Entcarb = 467.1
Entfat = 436.5
Entprot = 432.0 # Assumes that enthalpy does not change for uric acid 

# Constants for the conversion of the basal metabolic rate into field metabolic rate [mol O2 d^-1]
# Mean ratio of field metabolic rate to basal metabolic rate
FMR_BMR = 2.91

# Ms for each macronutrient: Ms = molecular weight / mol O2 consumed [g]
Mscarb = 180/6
Msfat = 256/23
Msprot = if (waste_type == "urea") {178/6} else {178/21}

# Molecular weight of water [g/mol]
MwH2O = 18

# Constants for the calculation of the total water flux [mol H2O d^-1]
# Coefficients for the allometric relationship between body mass [g] and total water flux [mol H2O d^-1] - fit to Nagy & Peterson (1988) data + more (datafile: Data condensed.csv)
aTWF = 0.051
bTWF = 0.73

# Constants for the calculation of the flux of vapor water out [mol H2O d^-1]
# Coefficients for the allometric relationship between body mass [g] and evaporative water loss [mol H2O d^-1] - fit to Altman & Dittmer (1968) and Crawford & Lasiewski (1986) data (datafile: EWL_mammals_birds_Altman_Crawford.csv)
aEWL = 0.009
bEWL = 0.80

# rH for waste: rHwaste = mol H2 in waste / mol O2 consumed [mol H2/mol O2]
rHwaste = if (waste_type == "urea") {2/6} else {4/21}	

# rO for each macronutrient: ro = O produced / O2 consumed [mol O/mol O2]
# Note that O produced = O in macronutrient 
rOcarb = 6/6 
rOfat = 2/23
rOprot = if (waste_type =="urea") {4/6} else {16/21}

# rO for waste: rOwaste = mol O in waste / mol O2 consumed [mol O/mol O2]
rOwaste = if (waste_type == "urea") {1/6} else {6/21}	

# Constants for the calculation of the H isotope composition of environmental water [per mil]
# Coefficients for the global meteoric water line
aGMWL = 8
bGMWL = 10

# Constants for the calculation of the H isotope composition of leaf water [per mil] 
# Coefficients for the calculation of equilibrium H isotope fractionation between liquid water and vapor
aHleafw = 24.884
bHleafw = 76.248
cHleafw = 52.612
# Kinetic H isotope fractionation caused by diffusion of vapor in the air
epsHk = 0.03

# Constants for the calculation of the H isotope composition of leaf macronutrients [per mil]
# Coefficients for the calculation of H isotope composition of leaf carbohydrate [per mil]
aHcarb_ew = 0.36
bHcarb_ew = 158
aHcarb_leafw = 0.64
bHcarb_leafw = 171
# H isotope fractionation between environmental water and leaf lipid for multiple plant types and alkanes [per mil]
epsHew_fat = -120
# alpha value - 
alphaHew_fat = (epsHew_fat/1000) + 1
# Coefficient for the calculation of H isotope composition of leaf protein [per mil] 
cHprot_carb = 40

# H isotope fractionation associated with evaporative water loss
alphaHbw_vw = 0.937

# Constants for the calculation of the O isotope composition of leaf water [per mil]
# Coefficients for the calculation of equilibrium O isotope fractionation between liquid water and vapor
aOleafw = 1.137
bOleafw = 0.4156
cOleafw = 2.0667
# Kinetic oxygen isotope fractionation caused by diffusion of vapor in the air
epsOk = 0.03 

# Constants for the calculation of the O isotope composition of leaf macronutrients [per mil]
# Coefficients for the calculation of O isotope composition of leaf carbohydrate [per mil]
aOcarb_ew = 0.42
bOcarb_ew = 27
aOcarb_leafw = 0.58
bOcarb_leafw = 27
# Coefficients for the calculation of O isotope composition of leaf lipid [per mil]
aOfat_carb = 0.455
bOfat_carb = 1.428
# Coefficient for the calculation of O isotope composition of leaf protein [per mil]
cOprot_fat = 6

# O isotope composition of atmospheric O2 [per mil]
d18Oo2 = 23.5
# R value - 
Ro2 = delta.to.R(d18Oo2, RstandardO)

# O isotope fractionation associated with the absorption of O2 in the lungs
alphaOatm_abs = 0.992

# O isotope fractionation associated with evaporative water loss
alphaObw_vw = 0.988 

# O isotope fractionation associated with the exhalation of CO2
alphaObw_CO2 = 1.038

# Proportion of keratin H routed from dietary protein 
PfH = 0.60

# H isotope fractionation associated with the synthesis of keratin protein
alphaHprot = 1.002

# Proportion of keratin O routed from dietary protein 
PfO = 0.19

# Proportion of follicle water derived from body water
Pbw = 0.81

# O isotope fractionation associated with carbonyl O-water interaction [per mil]
epsOc_w = 10.8 # Tuned after accounting for O routing
# alpha value - 
alphaOc_w = (epsOc_w + 1000) / 1000

# Proportion of gut water derived from body water
g1 = 0.56 # Tuned after accounting for O routing

# Proportion of gut water derived from drinking water
g2 = 0.09 # Tuned after accounting for O routing


#### (3) CALCULATIONS from (1) and (2) ####

# Average rH for combined macronutrients [mol H2/mol O2]
rH = rHcarb*Pcarb + rHfat*Pfat + rHprot*Pprot
	  
# Body temperature [K]
bTk = bTc + 273.15
	  
# Conversion factor for each macronutrient: CF = Rq / Ent [mol CO2/kJ]
CFcarb = Rqcarb/Entcarb
CFfat = Rqfat/Entfat
CFprot = Rqprot/Entprot
	  
# Average conversion factor for combined macronutrients [mol CO2/kJ] - for the conversion of [kJ] to [mol O2]
CF = CFcarb*Pcarb + CFfat*Pfat + CFprot*Pprot
	  
# Average mass of solids for combined macronutrients [g]
Ms = Mscarb*Pcarb + Msfat*Pfat + Msprot*Pprot
	  
# Average rO for combined macronutrients [mol O/mol O2]
rO = rOcarb*Pcarb + rOfat*Pfat + rOprot*Pprot
	  
# Average respiratory quotient for combined macronutrients [mol CO2/mol O2]
Rq = Rqcarb*Pcarb + Rqfat*Pfat + Rqprot*Pprot

# Environmental temperature [K]
eTk = eTc + 273.15
	  
# Equilibrium H isotope fractionation between liquid water and vapor [per mil]
epsHplus = (exp((aHleafw/(eTk)^2)*10^3 - (bHleafw/eTk) + cHleafw*10^-3) -1 )*1000
	  
# H isotope enrichment at evaporative water site compared to source water
DeltaHe = ((epsHplus/1000)+epsHk)*(1-Rh)

# equilibrium O isotope fractionation between liquid water and vapor [per mil]
epsOplus = (exp((aOleafw/(eTk)^2)*10^3 - (bOleafw/eTk) - (cOleafw*10^-3)) -1 )*1000

# O isotope enrichment at evaporative water site compared to source water 
DeltaOe = ((epsOplus/1000)+epsOk)*(1-Rh)


#### (4) CHECKS + OTHER CALCULATIONS #### 

# Basal metabolic rate based as a function of body size and temperature [W=J/s]
BMR = if (is.null(BMR)) {b0 * M^elc * exp(-E/(k*bTk))} else {BMR}
	  
# Basal metabolic rate expressed as [mol O2 d^-1]
BMR_O2 = if (is.null(BMR_O2)) {BMR * (60*60*24/1000) * CF} else {BMR_O2}
	  
# Field metabolic rate [mol O2 d^-1]
FMR = if (is.null(FMR)) {FMR_BMR * BMR_O2} else {FMR}

# Flux of metabolically derived H in [mol H2 d^-1]
FfH = rH * FMR 

# Flux of food water in [mol H2O d^-1]
Ffw = (Ms/MwH2O) * (Pw/(1-Pw)) * FMR

# Total water flux [mol H2O d^-1]
TWF = if (is.null(TWF)) {aTWF * M^bTWF} else {TWF}

# Total water flux (adjusted) [mol H2O d^-1]
TWFadj = ifelse((FfH + Ffw) > TWF, (FfH + Ffw), TWF)

# Flux of drinking water in [mol H2O d^-1]
Fdw = if (is.null(Fdw)) {TWFadj - (FfH + Ffw)} else {Fdw}

# Total water flux (actual) [mol H2O d^-1]
TWFact = FfH + Ffw + Fdw

# Flux of vapor water out (evaporative water loss) [mol H2O d^-1]
Fvw = if (is.null(Fvw)) {aEWL * M^bEWL} else {Fvw}

# Flux of waste H out [mol H2 d^-1]
FwasteH = rHwaste * Pprot * FMR

# Flux of liquid water out [mol H2O d^-1]
Flw = TWFact - Fvw - FwasteH

# Flux of metabolically derived O in [mol O d^-1]
FfO = rO * FMR 

# Flux of inhaled O2 in [mol O d^-1]
FO2 =  2 * FMR

# Flux of waste O out [mol O d^-1]
FwasteO = rOwaste * Pprot * FMR

# Flux of exhaled CO2 [mol O d^-1]
FCO2 = 2 * Rq * FMR

# H isotope composition of environmental water [per mil]
d2Hew = if (is.null(d2Hew)) {aGMWL * d18Oew + bGMWL} else {d2Hew}
# R value - 
RHew = delta.to.R(d2Hew, RstandardH) 

# H isotope composition of leaf water
RHleafw = if (is.null(d2Hleafw)) {RHew * (DeltaHe + 1)} else {delta.to.R(d2Hleafw, RstandardH)}
# delta value - 
d2Hleafw = R.to.delta(RHleafw, RstandardH)

# H isotope composition of leaf carbohydrate [per mil]
d2Hcarb = if (is.null(d2Hcarb)) {aHcarb_ew * (d2Hew + bHcarb_ew) + aHcarb_leafw * (d2Hleafw - bHcarb_leafw)} else {d2Hcarb}
# R value - 
RHcarb = delta.to.R(d2Hcarb, RstandardH)
	  
# H isotope composition of leaf protein [per mil]
d2Hprot = if (is.null(d2Hprot)) {d2Hcarb - cHprot_carb} else {d2Hprot}
# R value - 
RHprot = delta.to.R(d2Hprot, RstandardH)

# H isotope composition of leaf lipid [per mil]
d2Hfat = if (is.null(d2Hfat)) {R.to.delta(RHew * alphaHew_fat, RstandardH)} else {d2Hfat}
# R value - 
RHfat = delta.to.R(d2Hfat, RstandardH) 

# H isotope composition of the whole diet 
RHf = RHcarb*Pcarb + RHprot*Pprot + RHfat*Pfat
# delta value - 
d2Hf = R.to.delta(RHf, RstandardH)

# H isotope composition of food water
RHfw = if (is.null(d2Hfw)) {RHleafw} else {delta.to.R(d2Hfw, RstandardH)}
# delta value - 
d2Hfw = R.to.delta(RHfw, RstandardH)

# H isotope composition of drinking water
RHdw = if (is.null(d2Hdw)) {RHew} else {delta.to.R(d2Hdw, RstandardH)}
# delta value - 
d2Hdw = R.to.delta(RHdw, RstandardH)

# O isotope composition of environmental water - R value 
ROew = delta.to.R(d18Oew, RstandardO) 

# O isotope composition of leaf water
ROleafw = if (is.null(d18Oleafw)) {ROew * (DeltaOe + 1)} else {delta.to.R(d18Oleafw, RstandardO)}
# delta velue - 
d18Oleafw = R.to.delta(ROleafw, RstandardO)

# O isotope composition of leaf carbohydrate [per mil]
d18Ocarb = if (is.null(d18Ocarb)) {aOcarb_ew * (d18Oew + bOcarb_ew) + aOcarb_leafw * (d18Oleafw + bOcarb_leafw)} else {d18Ocarb}
# R value -
ROcarb = delta.to.R(d18Ocarb, RstandardO)
	  
# O isotope composition of leaf lipid [per mil]
d18Ofat = if (is.null(d18Ofat)) {aOfat_carb * d18Ocarb + bOfat_carb} else {d18Ofat}
# R value - 
ROfat = delta.to.R(d18Ofat, RstandardO)
	  
# O isotope composition of leaf protein [per mil]
d18Oprot = if (is.null(d18Oprot)) {d18Ofat + cOprot_fat} else {d18Oprot}
# R value - 
ROprot = delta.to.R(d18Oprot, RstandardO)

# O isotope composition of the whole diet 
ROf = ROcarb*Pcarb + ROprot*Pprot + ROfat*Pfat
# delta value - 
d18Of = R.to.delta(ROf, RstandardO)

# O isotope composition of food water
ROfw = if (is.null(d18Ofw)) {ROleafw} else {delta.to.R(d18Ofw, RstandardO)}
# delta value - 
d18Ofw = R.to.delta(ROfw, RstandardO)

# O isotope composition of drinking water
ROdw = if (is.null(d18Odw)) {ROew} else {delta.to.R(d18Odw, RstandardO)}
# delta value -
d18Odw = R.to.delta(ROdw, RstandardO)


#### (5) OUTPUTS ####

# H isotope composition of body water
RHbw = (FfH*RHf + Ffw*RHfw + Fdw*RHdw) / (Fvw*alphaHbw_vw + FwasteH + Flw)
d2Hbw = R.to.delta(RHbw, RstandardH)
	  
# O isotope composition of body water
RObw = (FfO*ROf + Ffw*ROfw + Fdw*ROdw + FO2*alphaOatm_abs*Ro2) / (Fvw*alphaObw_vw + FwasteO + Flw + FCO2*alphaObw_CO2)
d18Obw = R.to.delta(RObw, RstandardO)

# H isotope composition of follicle water
RHfollw = Pbw*RHbw + (1 - Pbw)*RHf
	  
# O isotope composition of gut water
ROgw = g1*RObw + g2*ROdw + (1 - g1 - g2)*ROfw
	  
# H isotope composition of keratin
RHker = PfH*RHprot + (1 - PfH)*alphaHprot*RHfollw  
d2Hker = R.to.delta(RHker, RstandardH)
	  
# O isotope composition of keratin
ROker = PfO*ROprot + (1-PfO)*alphaOc_w*ROgw 
d18Oker = R.to.delta(ROker, RstandardO)


return(list("waste_type"=waste_type, "Pcarb"=Pcarb, "Pprot"=Pprot, "Pfat"=Pfat, "M"=M, "bTc"=bTc, "Pw"=Pw, "d18Oew"=d18Oew, "eTc"=eTc, "Rh"=Rh, 
"BMR"=BMR, "BMR_O2"=BMR_O2, "FMR"=FMR, "TWF"=TWF, "Fdw"=Fdw, "Fvw"=Fvw, "d2Hew"=d2Hew, "d2Hleafw"=d2Hleafw, "d2Hcarb"=d2Hcarb, "d2Hprot"=d2Hprot, "d2Hfat"=d2Hfat, "d2Hfw"=d2Hfw, "d2Hdw"=d2Hdw, "d18Oleafw"=d18Oleafw, "d18Ocarb"=d18Ocarb, "d18Oprot"=d18Oprot, "d18Ofat"=d18Ofat, "d18Ofw"=d18Ofw, "d18Odw"=d18Odw, 
"d18Oo2"=d18Oo2, 
"FfH"=FfH, "Ffw"=Ffw, "TWFadj"=TWFadj, "TWFact"=TWFact, "FwasteH"=FwasteH, "Flw"=Flw, "FO2"=FO2, "FwasteO"=FwasteO, "FCO2"=FCO2, "d2Hf"=d2Hf, "d18Of"=d18Of, 
"d2Hbw"=d2Hbw, "d18Obw"=d18Obw, "d2Hker"=d2Hker, "d18Oker"=d18Oker))
	
				}
		
				
test2 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				M=50,
				bTc=37,
				Pw=0.60, 
				d18Oew=-10,
				eTc=20,
				Rh=0.50,				
				BMR=NULL,
				BMR_O2=NULL,
				FMR=NULL,
				TWF=NULL,
				Fdw=NULL,
				Fvw=NULL,
				d2Hew=NULL,
				d2Hleafw=NULL,
				d2Hcarb=NULL,
				d2Hprot=NULL, 
				d2Hfat=NULL,
				d2Hfw=NULL,
				d2Hdw=NULL,
				d18Oleafw=NULL,
				d18Ocarb=NULL,
				d18Oprot=NULL,
				d18Ofat=NULL,
				d18Ofw=NULL,
				d18Odw=NULL)	

# Example of output values after running the model with the prescribed inputs: 
# Body water d2H value: -39.09 [per mil]
# Body water d18O value: -0.57 [per mil]
# Keratin d2H value: -96.17 [per mil]
# Keratin d18O value: 14.40 [per mil]				

				
