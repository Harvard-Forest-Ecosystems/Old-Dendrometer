"calc_kgc" = function(spp, dbh){
	# 1/06 kmk
	# revised 5/07
  # updated 5/2017 TGW for compatibility with dplyr syntax
	# a subroutine for calculating kgC of individual trees
	# using their dbhs & spp specific allometries
	# spp is the column name that contains the spp names
	# dbh is the column name that contain the dbhs in cm
	# allometries calculate biomass (kg) from diameters (cm)
	# many of the equations deal with units other than kg & cm, so conversions are done within the equations
	# "else if" statements allow there to be no members of a given spp

#Fraxinus americana (White ash)
	if(spp == "ash"){
		kgc <- 3.2031 - 0.2337 * (dbh*10) + 0.006061 * (dbh*10)^2
	} else if (spp == "bb"){
#Betula lenta (Black birch)
	  kgc <- (1.6542 * (dbh/2.54)^2.6606)/2.2
	} else	if(spp == "beech"){
#Fagus grandifolia (American beech)
	  kgc <- (10^(2.028 + 2.3981 * log10(dbh)) + 10^(1.4182 + 2.5509 * log10(dbh)))/1000
  } else	if(spp == "bo"){
#Quercus velutina (Black oak)
	  kgc <- (2.14567 * (dbh/2.54)^2.50304)/2.2
  } else	if(spp == "cherry"){
#Prunus serotina  (Black cherry)
	  kgc <- (1.8082 * (dbh/2.54)^2.6174)/2.2
  } else	if(spp == "gb"){
#Betula populifolia (Gray birch)
	  kgc <- (exp(0.6541776 + 2.529903 * log(dbh/2.54)) + exp(-0.3969096 + 1.628732 * log(dbh/2.54)))/2.2
  } else	if(spp == "hem"){
#Tsuga canadensis (Eastern hemlock)
    kgc <- (exp(0.2607714 + 2.366161 * log(dbh/2.54)) + exp(-1.775095 + 2.703338 * log(dbh/2.54)))/2.2
  } else	if(spp == "rm"){
#Acer rubrum (Red maple)
	  kgc <- (exp(5.02 + 2.33 * log(dbh)))/1000
  } else	if(spp == "ro" | spp == "chestnut"){
#Quercus rubra (Northern red oak)  & Castenea dentate (American chestnut)
    kgc <- (exp(5.03 + 2.39 * log(dbh)))/1000
  } else	if(spp == "rp"){
#Pinus resinosa (Red pine)
	  kgc <- (exp(0.3078881 + 2.448125 * log(dbh/2.54)) + exp(-1.508616 + 2.501085 * log(dbh/2.54)))/2.2
  } else	if(spp == "sm"){
#Acer pennsylvanicum (Striped maple)
	  kgc <- (exp(4.68 + 2.38 * log(dbh)))/1000
  } else	if(spp == "wb"){
#Betula papyrifera (Paper birch)
	  kgc <- (exp(-0.1463825 + 2.896804 * log(dbh/2.54)) + exp(-1.053626 + 2.249402 * log(dbh/2.54)))/2.2
  } else	if(spp == "wh" ){
#Hamamelis virginiania (witch hazel)
	  kgc <- (38.111 * (dbh^2.9))/1000
  } else	if(spp == "wo"){
#Quercus alba (White oak)
	  kgc <- exp(-0.82061 + 2.84694 * log(dbh/2.54))
  } else	if(spp == "wp"){
#Pinus strobus (white pine)
	  kgc <- (exp(-0.04659059 + 2.545864 * log(dbh/2.54)) + exp(-1.701115 + 2.485762 * log(dbh/2.54)))/2.2
  } else	if(spp == "ws"){
#Picea glauca (white spruce)
#general spruce equation for all spp.
	  kgc <- (exp(0.3960433 + 2.374645 * log(dbh/2.54)) + exp(-1.351318 + 2.338385 * log(dbh/2.54)))/2.2
  } else	if(spp == "yb"){
#Betula alleghaniensis (Yellow birch)
	  kgc <- (10^(2.1413 + 2.2683 * log10(dbh)) + 10^(1.0535 + 2.7995 * log10(dbh)))/1000
#----------------------------------------------
	# these next equations are for the sapling data
	# they have units cm & g, so we do the conversion to in and lbs
	# within the equations to be consistent will all the others
  } else	if(spp == "hbb" | spp == "SS"){
#Vaccinium spp. (high bush blueberry) & Rhus typhina (Staghorn sumac)
	  kgc <- (95.143 * (dbh)^3.706)/1000
  } else	if(spp == "nwr" ){
#Viburnum spp. (Northern wild raisin)
	  kgc <- (29.615 * (dbh)^3.243)/1000
  } else	if(spp == "SPI"){
#Spiraea spp.
	  kgc <- (36.648 * (dbh)^2.579)/1000
  } else	if(spp == "FPC" ){
#Prunus pennsylvanica (pin cherry)
	  kgc <- (49.916 * (dbh)^2.547)/1000
  } else	if(spp == "haw" | spp == "unk"){
#Crataegus spp. (Hawthorn)
  # for hawthorn spp use the general equation for mixed hardwood from Jenkins et al.
	# biomass is converted from kg to lbs in the equation
	   kgc <- exp(-2.48 + 2.4835*logb(dbh))
  } else {
    # general equation for mixed hardwood from Jenkins et al.
    kgc <- NA
  }
#------------------------------------------------
	# convert from biomass to C, assuming C is 50% of mass
	return(kgc * 0.5)

}

