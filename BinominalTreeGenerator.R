# Generiert alle möglichen XXX-Strategien in einer Matrix
BinomonalTreeGenerator<-function(E,N)
{
  # Bestimmen der Anzahl an möglichen Kostenwege
  W<-2^N
  
  # Erstellen der Wegmatrix
  binEs<-matrix(0,W,N)
  
  # Spaltenweises füllen mit "L" und "R", solange bis alle Felder in SPalte belegt sind
  # rep... Replicate Elements of Vectors and Lists
  for (n in 1:N)
    {binEs[,n]<-c(rep(E[1],2^(N-n)), # befüllen mit L
                  rep(E[2],2^(N-n))) # befüllen mit R
    
    #print(binEs[,n])
  }
  return(binEs)
}