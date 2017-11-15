# Erstellt den Kostenbaum in Matrixform
CostMatrixGenerator<-function()
{
  CM<-matrix(0,8,3)
  CM[1:4,1]<-10
  CM[c(3,5),3]<-1200
  CM[7:8,3]<-12
  return(CM)
}  