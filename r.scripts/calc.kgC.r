"calc.kgC" = function(x.name, spp, dbh)
{
	# 1/06 kmk
	# revised 5/07
	# a subroutine for calculating kgC of individual trees
	# using their dbhs & spp specific allometries
	# x.name is the name of the object the fnctn is being applied to in ""
	# spp is the column number that contains the spp names
	# dbh is the column number(s) that contain the dbhs in cm
	x <- get(x.name)
	# allometries calculate biomass (kg) from diameters (cm)
	# many of the equations deal with units other than kg & cm, so conversions are done within the equations
	# "if" statements allow there to be no members of a given spp
	x[is.na(x[,spp]),spp]<-"na"
	
#Fraxinus americana (White ash) 	
	ash <- x[, spp] == "ash"
	if(sum(ash) > 0)
		x[ash, dbh] <- 3.2031 - 0.2337 * (x[ash, dbh]*10) + 0.006061 * (x[ash, dbh]*10)^2
		
#Betula lenta (Black birch) 
	bb <- x[, spp] == "bb"
	if(sum(bb) > 0)
		x[bb, dbh] <- (1.6542 * (x[bb, dbh]/2.54)^2.6606)/2.2	
    	 
#Fagus grandifolia (American beech)     
	beech <- x[, spp] == "beech"
	if(sum(beech) > 0)
		x[beech, dbh] <- (10^(2.028 + 2.3981 * log10(x[beech, dbh])) + 10^(1.4182 + 2.5509 * log10(x[beech, dbh])))/1000

#Quercus velutina (Black oak) 
bo <- x[, spp] == "bo"
	if(sum(bo) > 0)
		x[bo, dbh] <- (2.14567 * (x[bo, dbh]/2.54)^2.50304)/2.2 
    
#Prunus serotina  (Black cherry)                
	cherry <- x[, spp] == "cherry"
	if(sum(cherry) > 0)
		x[cherry, dbh] <- (1.8082 * (x[cherry, dbh]/2.54)^2.6174)/2.2 
    
#Betula populifolia (Gray birch)                   
	gb <- x[, spp] == "gb"
	if(sum(gb) > 0)
		x[gb, dbh] <- (exp(0.6541776 + 2.529903 * log(x[gb, dbh]/2.54)) + exp(-0.3969096 + 1.628732 * log(x[gb, dbh]/2.54)))/2.2
	
#Tsuga canadensis (Eastern hemlock) 
  hem <- x[, spp] == "hem"
	if(sum(hem) > 0)
		x[hem, dbh] <- (exp(0.2607714 + 2.366161 * log(x[hem, dbh]/2.54)) + exp(-1.775095 + 2.703338 * log(x[hem, dbh]/2.54)))/2.2
	
#Acer rubrum (Red maple) 
  rm <- x[, spp] == "rm"
	if(sum(rm) > 0)
		x[rm, dbh] <- (exp(5.02 + 2.33 * log(x[rm, dbh])))/1000
		
#Quercus rubra (Northern red oak)  & Castenea dentate (American chestnut)
		
	ro <- x[, spp] == "ro" | x[, spp] == "chestnut"
	if(sum(ro) > 0)
		x[ro, dbh] <- (exp(5.03 + 2.39 * log(x[ro, dbh])))/1000
		
#Pinus resinosa (Red pine) 
	rp <- x[, spp] == "rp"
	if(sum(rp) > 0)
		x[rp, dbh] <- (exp(0.3078881 + 2.448125 * log(x[rp, dbh]/2.54)) + exp(-1.508616 + 2.501085 * log(x[rp, dbh]/2.54)))/2.2
	
#Acer pennsylvanicum (Striped maple) 
  sm <- x[, spp] == "sm"
	if(sum(sm) > 0)
		x[sm, dbh] <- (exp(4.68 + 2.38 * log(x[sm, dbh])))/1000

#Betula papyrifera (Paper birch) 
	wb <- x[, spp] == "wb"
	if(sum(wb) > 0)
		x[wb, dbh] <- (exp(-0.1463825 + 2.896804 * log(x[wb, dbh]/2.54)) + exp(-1.053626 + 2.249402 * log(x[wb, dbh]/2.54)))/2.2

#Hamamelis virginiania (witch hazel)
	wh <- x[, spp] == "wh"
	if(sum(wh) > 0)
		x[wh, dbh] <- (38.111 * (x[wh, dbh]^2.9))/1000

#Quercus alba (White oak)
	wo <- x[, spp] == "wo"
	if(sum(wo) > 0)
		x[wo, dbh] <- exp(-0.82061 + 2.84694 * log(x[wo, dbh]/2.54))

#Pinus strobus (white pine) 
	wp <- x[, spp] == "wp"
	if(sum(wp) > 0)
		x[wp, dbh] <- (exp(-0.04659059 + 2.545864 * log(x[wp, dbh]/2.54)) + exp(-1.701115 + 2.485762 * log(x[wp, dbh]/2.54)))/2.2

#Picea glauca (white spruce) 
#general spruce equation for all spp.
	ws <- x[, spp] == "ws"
	if(sum(ws) > 0)
		x[ws, dbh] <- (exp(0.3960433 + 2.374645 * log(x[ws, dbh]/2.54)) + exp(-1.351318 + 2.338385 * log(x[ws, dbh]/2.54)))/2.2

#Betula alleghaniensis (Yellow birch) 
	yb <- x[, spp] == "yb"
	if(sum(yb) > 0)
		x[yb, dbh] <- (10^(2.1413 + 2.2683 * log10(x[yb, dbh])) + 10^(1.0535 + 2.7995 * log10(x[yb, dbh])))/1000
	#----------------------------------------------
	# these next equations are for the sapling data
	# they have units cm & g, so we do the conversion to in and lbs 
	# within the equations to be consistent will all the others

#Vaccinium spp. (high bush blueberry) & Rhus typhina (Staghorn sumac)
	hbb <- x[, spp] == "hbb" | x[, spp] == "SS"
	if (sum(hbb) > 0)
		x[hbb, dbh] <- (95.143 * (x[hbb, dbh])^3.706)/1000

#Viburnum spp. (Northern wild raisin)
	nwr <- x[, spp] == "nwr"
	if (sum(nwr) > 0)
		x[nwr, dbh] <- (29.615 * (x[nwr, dbh])^3.243)/1000

#Spiraea spp.
	SPI <- x[, spp] == "SPI"
	if (sum(SPI) > 0)
		x[SPI, dbh] <- (36.648 * (x[SPI, dbh])^2.579)/1000

#Prunus pennsylvanica (pin cherry)
	FPC <- x[, spp] == "FPC"
	if (sum(FPC) > 0)
		x[FPC, dbh] <- (49.916 * (x[FPC, dbh])^2.547)/1000
	
  #Crataegus spp. (Hawthorn)
  # for hawthorn spp use the general equation for mixed hardwood from Jenkins et al.
	# biomass is converted from kg to lbs in the equation
	haw <- x[,spp] == "haw"
	if (sum(haw) > 0)
		x[haw, dbh] <- exp(-2.48 + 2.4835*logb(x[haw, dbh]))
	#------------------------------------------------
	# convert from biomass to C
	x[, dbh] <- x[, dbh]/2
	x.out<-get(x.name)
	x.out[,dbh]<-x[,dbh]
	x.out
}
