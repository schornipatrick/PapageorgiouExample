MainTimeBased<-function()
{
# Erstellen des Kostenbaums in Matrixform
CM<-CostMatrixGenerator()
#print(CM)

# Abruf der TimeBasedControl-Funktion
minKost<-TimeBasedControl(CM)
print(minKost)
}