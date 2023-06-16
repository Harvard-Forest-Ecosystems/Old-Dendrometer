"new.dbh.make" = function(calipers, size.ref, init.cal)
{
	# This function was first written by LUH on 1/19/01.
	# Updated by ELG on 1/24/13 to itterate, which eliminates a bias in estimate.
	# This function takes vectors of dendrometer/caliper readings and transforms them into DBHs.
	# It does this by transforming each chord (the dendro readings) into an angle and then into an arc.
	# "calipers" is the vector of dendrometer readings (mm).
	# "size.ref" is a vector of DBH's (cm) to use when calculating arcs from chords.
	# "init.cal" is a vector of caliper readings (mm) taken at the same time as size.ref.
	# These vectors have been created in excel, reflecting any changes that have been made to the band.
	# Hence, each measurement period has a different vector with references.
  
  DBH<-0
  new.DBH<-size.ref
  while(all(abs(DBH-new.DBH)>.0001,na.rm=T)){
    DBH<-new.DBH
    angle <- 2. * asin(calipers/(DBH * 10.))
    arc <- (angle * DBH * 10.)/2.
    angle2 <- 2. * asin(init.cal/(size.ref * 10.))
    arc2 <- (angle2 * size.ref * 10.)/2.
    new.DBH <- ((size.ref * pi) + ((arc - arc2)/10.))/pi
  }
   return(new.DBH) 
}
