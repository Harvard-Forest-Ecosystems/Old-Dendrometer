calc_dbh <- function(dend, refdend, refdbh){
  DBH <- 0
  calcdbh <- refdbh
  while(all(abs(DBH-calcdbh)>.0001,na.rm=T)){
    DBH <- calcdbh
    refcirc <- pi * refdbh * 10
    refangle <- 2 * asin(refdend/(refdbh * 10))
    refarc <- refangle * ((refdbh * 10) / 2)
    calcangle<- 2 * asin(dend/(DBH * 10))
    calcarc <- calcangle * ((DBH * 10) / 2)
    arcchg <- calcarc - refarc
    calcdbh <- (refcirc + arcchg) / pi / 10
  }
  return(calcdbh)
}
