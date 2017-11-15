TimeBasedControl<-function(CM)
{
  W<-dim(CM)[1] # Anzahl der möglichen "Kostenwege"
  S<-dim(CM)[1] # Anzahl der möglichen Strategien
  N<-dim(CM)[2] # ANzahl der Perioden
  
  SM<-BinomonalTreeGenerator(c("L","R"),N) # Strategie Matrix
  E_Cost<-c(0,S) # Erwartungswert von den Kosten der jeweiligen Strategie
  
  # Errechnen des Erwartungswertes aller Strategien
  for (s in 1:S){
    Prob<-matrix(0,S,N)
    
    # Erstellen der Wahrscheinlichkeitsmatrix
    for (w in 1:W)
    {
      # Zeilenweises einfügen der jew. Wahrscheinlichkeiten
      for (n in 1:N)
      {
        if (SM[s,n]==SM[w,n]) Prob[w,n]<-0.75
        else Prob[w,n]<-0.25
      }
    }
    ProbT<-apply(Prob,1,prod) # Vektor mit den Wahrscheinlichkeiten das der jew. Weg eintritt
    E_Cost[s]<-ProbT%*%apply(CM,1,sum) # Erwartungswert der jew. Strategie
  }
  
  # Anzeigen der Ergebnisse
  print("Papageorgiou Beispiel: Ermittlung der opt. zeitbasierten Strategie mit den geringsten zu erw. Kosten")
  #dimnames(SM)<-list(seq(1,S),c("Str","ate","gie"))
  print(cbind(SM,E_Cost))
  print("Optimale zeitbasierte Strategie")
  print(SM[which(E_Cost==min(E_Cost)),])
  print("Erwartungswert der minimalen Kosten")
  return(E_Cost[which(E_Cost==min(E_Cost))])
}