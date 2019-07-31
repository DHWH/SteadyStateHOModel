
# Magozzi et al. (2019) in review

#-----------------------------------------------------------------------------#
#                      SS HO MODEL - First-order parameters                   #
#-----------------------------------------------------------------------------#

#### (1) INPUTS #### 
# Description of arguments with examples; including only first-order parameters -  

# 1- Waste type; can be "urea" or "uric_acid"
#waste_type = "urea"

# 2- Proportion of carbohydrate in the diet 
#Pcarb = 0.85

# 3- Proportion of protein in the diet 
#Pprot = 0.10

# 4- Proportion of lipid in the diet 
#Pfat = 1 - Pcarb - Pprot 

# 5- Field metabolic rate [mol O2 d^-1]
#FMR = 0.15

# 6- Proportion of food mass that is in liquid water form 
#Pw = 0.60

# 7- Flux of drinking water in [mol H2O d^-1]
#Fdw = 0.39

# 8- Flux of vapor water out [mol H2O d^-1]
#Fvw = 0.21

# 9- H isotope composition of carbohydrate in the diet [per mil]
#d2Hcarb = -88.18

# 10- H isotope composition of protein in the diet [per mil]
#d2Hprot = -128.18

# 11- H isotope composition of lipid in the diet [per mil]
#d2Hfat = -181.60

# 12- H isotope composition of food water [per mil]
#d2Hfw = -16.28

# 13- H isotope composition of drinking water [per mil]
#d2Hdw = -70

# 14- O isotope composition of carbohydrate in the diet [per mil]
#d18Ocarb = 28.42

# 15- O isotope composition of protein in the diet [per mil]
#d18Oprot = 20.36

# 16- O isotope composition of lipid in the diet [per mil]
#d18Ofat = 14.36 

# 17- O isotope composition of food water [per mil]
#d18Ofw = 9.70

# 18- O isotope composition of drinking water [per mil]
#d18Odw = -10


HOmodel = function(waste_type,
				Pcarb,
				Pprot,
				Pfat,
				FMR, 
				Pw, 
				Fdw, 
				Fvw, 
				d2Hcarb,
				d2Hprot, 
				d2Hfat, 
				d2Hfw,
				d2Hdw,
				d18Ocarb,
				d18Oprot,
				d18Ofat,
				d18Ofw,
				d18Odw) {

					
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

# Respiratory quotient for each macronutrient: Rq = mol CO2 produced / mol O2 consumed [mol CO2/mol O2]
Rqcarb = 6/6
Rqfat = 16/23
Rqprot = if (waste_type == "urea") {5/6} else {14/21}

# Ms for each macronutrient: Ms = molecular weight / mol O2 consumed [g]
Mscarb = 180/6
Msfat = 256/23
Msprot = if (waste_type == "urea") {178/6} else {178/21}

# Molecular weight of water [g/mol]
MwH2O = 18

# rH for waste: rHwaste = mol H2 in waste / mol O2 consumed [mol H2/mol O2]
rHwaste = if (waste_type == "urea") {2/6} else {4/21}

# rO for each macronutrient: ro = O produced / O2 consumed [mol O/mol O2]
# Note that O produced = O in macronutrient 
rOcarb = 6/6 
rOfat = 2/23
rOprot = if (waste_type =="urea") {4/6} else {16/21}

# rO for waste: rOwaste = mol O in waste / mol O2 consumed [mol O/mol O2]
rOwaste = if (waste_type == "urea") {1/6} else {6/21}

# H isotope fractionation associated with evaporative water loss
alphaHbw_vw = 0.937

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

# Average mass of solids for combined macronutrients [g]
Ms = Mscarb*Pcarb + Msfat*Pfat + Msprot*Pprot
	  
# Average rO for combined macronutrients [mol O/mol O2]
rO = rOcarb*Pcarb + rOfat*Pfat + rOprot*Pprot
	  
# Average respiratory quotient for combined macronutrients [mol CO2/mol O2]
Rq = Rqcarb*Pcarb + Rqfat*Pfat + Rqprot*Pprot


#### (4) OTHER CALCULATIONS ####	

# Flux of metabolically derived H in [mol H2 d^-1]
FfH = rH * FMR

# Flux of food water in [mol H2O d^-1]
Ffw = (Ms/MwH2O) * (Pw/(1-Pw)) * FMR
  
# Total water flux (actual) [mol H2O d^-1]
TWFact = FfH + Ffw + Fdw	  

# Flux of waste H out [mol H2 d^-1]
FwasteH = rHwaste * Pprot * FMR

# Flux of liquid water out [mol H2O d^-1] 
Flw = TWFact - Fvw - FwasteH

# Flux of metabolically derived O out [mol O d^-1]
FfO = rO * FMR

# Flux of inhaled O2 [mol O d^-1]
FO2 = 2 * FMR

# Flux of waste O out [mol O d^-1]
FwasteO = rOwaste * Pprot * FMR

# Flux of exhaled CO2 out [mol O d^-1]
FCO2 = 2 * Rq * FMR

# H isotope compositions of macronutrients in the diet - R values
RHcarb = delta.to.R(d2Hcarb, RstandardH)
RHprot = delta.to.R(d2Hprot, RstandardH)
RHfat = delta.to.R(d2Hfat, RstandardH)

# H isotope composition of the whole diet 		
RHf = RHcarb*Pcarb + RHprot*Pprot + RHfat*Pfat
# delta value - 
d2Hf = R.to.delta(RHf, RstandardH)	

# H isotope composition of food water - R value
RHfw = delta.to.R(d2Hfw, RstandardH)

# H isotope composition of drinking water - R value
RHdw = delta.to.R(d2Hdw, RstandardH)

# O isotope compositions of macronutrients in the diet - R values
ROcarb = delta.to.R(d18Ocarb, RstandardO)
ROprot = delta.to.R(d18Oprot, RstandardO)
ROfat = delta.to.R(d18Ofat, RstandardO)		

# O isotope composition of the whole diet
ROf = ROcarb*Pcarb + ROprot*Pprot + ROfat*Pfat
# delta value - 
d18Of = R.to.delta(ROf, RstandardO)

# O isotope composition of food water - R value
ROfw = delta.to.R(d18Ofw, RstandardO)

# O isotope composition of drinking water - R value
ROdw = delta.to.R(d18Odw, RstandardO)	


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


return(list("waste_type"=waste_type, "Pcarb"=Pcarb, "Pprot"=Pprot, "Pfat"=Pfat, "FMR"=FMR, "Pw"=Pw, "Fdw"=Fdw, "Fvw"=Fvw, "d2Hcarb"=d2Hcarb, "d2Hprot"=d2Hprot, "d2Hfat"=d2Hfat, "d2Hfw"=d2Hfw, "d2Hdw"=d2Hdw, "d18Ocarb"=d18Ocarb, "d18Oprot"=d18Oprot, "d18Ofat"=d18Ofat, "d18Ofw"=d18Ofw, "d18Odw"=d18Odw, 
"d18Oo2"=d18Oo2, 
"FfH"=FfH, "Ffw"=Ffw, "TWFact"=TWFact, "FwasteH"=FwasteH, "Flw"=Flw, "FO2"=FO2, "FwasteO"=FwasteO, "FCO2"=FCO2, "d2Hf"=d2Hf, "d18Of"=d18Of, 
"d2Hbw"=d2Hbw, "d18Obw"=d18Obw, "d2Hker"=d2Hker, "d18Oker"=d18Oker))
	
				}
				
							
test1 = HOmodel(waste_type="urea",
				Pcarb=0.85,
				Pprot=0.10,
				Pfat=1 - 0.85 - 0.10,
				FMR=0.15, 
				Pw=0.60, 
				Fdw=0.39, 
				Fvw=0.21, 
				d2Hcarb=-88.18,
				d2Hprot=-128.18, 
				d2Hfat=-181.60, 
				d2Hfw=-16.18,
				d2Hdw=-70,
				d18Ocarb=28.42,
				d18Oprot=20.36,
				d18Ofat=14.36,
				d18Ofw=9.70,
				d18Odw=-10)	

# Example of output values after running the model with the prescribed inputs: 
# Body water d2H value: -38.76 [per mil]
# Body water d18O value: -0.49 [per mil]
# Keratin d2H value: -96.07 [per mil]
# Keratin d18O value: 14.44 [per mil]    	  					


