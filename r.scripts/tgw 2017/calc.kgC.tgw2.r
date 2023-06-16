"calc_kgc" = function(spp, dbh)
{
	# 1/06 kmk
	# revised 5/07
	# a subroutine for calculating kgC of individual trees
	# using their dbhs & spp specific allometries
	# x.name is the name of the object the fnctn is being applied to in ""
	# spp is the column number that contains the spp names
	# dbh is the column number(s) that contain the dbhs in cm
		# allometries calculate biomass (kg) from diameters (cm)
	# many of the equations deal with units other than kg & cm, so conversions are done within the equations
	# "if" statements allow there to be no members of a given spp
	kgc <- dbh
	
#Fraxinus americana (White ash) 	
	ash <- which(spp == "ash")
	if(sum(ash) > 0)
		kgc[ash] <- 3.2031 - 0.2337 * (dbh[ash]*10) + 0.006061 * (dbh[ash]*10)^2
		
#Betula lenta (Black birch) 
	bb <- which(spp == "bb")
	if(sum(bb) > 0)
		kgc[bb] <- (1.6542 * (dbh[bb]/2.54)^2.6606)/2.2	
    	 
#Fagus grandifolia (American beech)     
	beech <- which(spp == "beech")
	if(sum(beech) > 0)
		kgc[beech] <- (10^(2.028 + 2.3981 * log10(dbh[beech])) + 10^(1.4182 + 2.5509 * log10(dbh[beech])))/1000

#Quercus velutina (Black oak) 
bo <- which(spp == "bo")
	if(sum(bo) > 0)
		kgc[bo] <- (2.14567 * (dbh[bo]/2.54)^2.50304)/2.2 
    
#Prunus serotina  (Black cherry)                
	cherry <- which(spp == "cherry")
	if(sum(cherry) > 0)
		kgc[cherry] <- (1.8082 * (dbh[cherry]/2.54)^2.6174)/2.2 
    
#Betula populifolia (Gray birch)                   
	gb <- which(spp == "gb")
	if(sum(gb) > 0)
		kgc[gb] <- (exp(0.6541776 + 2.529903 * log(dbh[gb]/2.54)) + exp(-0.3969096 + 1.628732 * log(dbh[gb]/2.54)))/2.2
	
#Tsuga canadensis (Eastern hemlock) 
  hem <- which(spp == "hem")
	if(sum(hem) > 0)
		kgc[hem] <- (exp(0.2607714 + 2.366161 * log(dbh[hem]/2.54)) + exp(-1.775095 + 2.703338 * log(dbh[hem]/2.54)))/2.2
	
#Acer rubrum (Red maple) 
  rm <- which(spp == "rm")
	if(sum(rm) > 0)
		kgc[rm] <- (exp(5.02 + 2.33 * log(dbh[rm])))/1000
		
#Quercus rubra (Northern red oak)  & Castenea dentate (American chestnut)
		
	ro <- which(spp == "ro" | spp == "chestnut")
	if(sum(ro) > 0)
		kgc[ro] <- (exp(5.03 + 2.39 * log(dbh[ro])))/1000
		
#Pinus resinosa (Red pine) 
	rp <- which(spp == "rp")
	if(sum(rp) > 0)
		kgc[rp] <- (exp(0.3078881 + 2.448125 * log(dbh[rp]/2.54)) + exp(-1.508616 + 2.501085 * log(dbh[rp]/2.54)))/2.2
	
#Acer pennsylvanicum (Striped maple) 
  sm <- which(spp == "sm")
	if(sum(sm) > 0)
		kgc[sm] <- (exp(4.68 + 2.38 * log(dbh[sm])))/1000

#Betula papyrifera (Paper birch) 
	wb <- which(spp == "wb")
	if(sum(wb) > 0)
		kgc[wb] <- (exp(-0.1463825 + 2.896804 * log(dbh[wb]/2.54)) + exp(-1.053626 + 2.249402 * log(dbh[wb]/2.54)))/2.2

#Hamamelis virginiania (witch hazel)
	wh <- which(spp == "wh")
	if(sum(wh) > 0)
		kgc[wh] <- (38.111 * (dbh[wh]^2.9))/1000

#Quercus alba (White oak)
	wo <- which(spp == "wo")
	if(sum(wo) > 0)
		kgc[wo] <- exp(-0.82061 + 2.84694 * log(dbh[wo]/2.54))

#Pinus strobus (white pine) 
	wp <- which(spp == "wp")
	if(sum(wp) > 0)
		kgc[wp] <- (exp(-0.04659059 + 2.545864 * log(dbh[wp]/2.54)) + exp(-1.701115 + 2.485762 * log(dbh[wp]/2.54)))/2.2

#Picea glauca (white spruce) 
#general spruce equation for all spp.
	ws <- which(spp == "ws")
	if(sum(ws) > 0)
		kgc[ws] <- (exp(0.3960433 + 2.374645 * log(dbh[ws]/2.54)) + exp(-1.351318 + 2.338385 * log(dbh[ws]/2.54)))/2.2

#Betula alleghaniensis (Yellow birch) 
	yb <- which(spp == "yb")
	if(sum(yb) > 0)
		kgc[yb] <- (10^(2.1413 + 2.2683 * log10(dbh[ws])) + 10^(1.0535 + 2.7995 * log10(dbh[ws])))/1000
	#----------------------------------------------
	# these next equations are for the sapling data
	# they have units cm & g, so we do the conversion to in and lbs 
	# within the equations to be consistent will all the others

#Vaccinium spp. (high bush blueberry) & Rhus typhina (Staghorn sumac)
	hbb <- which(spp == "hbb" | spp == "SS")
	if (sum(hbb) > 0)
		kgc[hbb] <- (95.143 * (dbh[SS])^3.706)/1000

#Viburnum spp. (Northern wild raisin)
	nwr <- which(spp == "nwr")
	if (sum(nwr) > 0)
		kgc[nwr] <- (29.615 * (dbh[nwr])^3.243)/1000

#Spiraea spp.
	SPI <- which(spp == "SPI")
	if (sum(SPI) > 0)
		kgc[SPI] <- (36.648 * (dbh[SPI])^2.579)/1000

#Prunus pennsylvanica (pin cherry)
	FPC <- which(spp == "FPC")
	if (sum(FPC) > 0)
		kgc[FPC] <- (49.916 * (dbh[FPC])^2.547)/1000
	
  #Crataegus spp. (Hawthorn)
  # for hawthorn spp use the general equation for mixed hardwood from Jenkins et al.
	# biomass is converted from kg to lbs in the equation
	haw <- which(spp == "haw")
	if (sum(haw) > 0)
		kgc[haw] <- exp(-2.48 + 2.4835*logb(dbh[haw]))
	#------------------------------------------------
	# convert from biomass to C
	kgc <- kgc/2
	return(kgc)
}
