RMSE=function(predicted.gate,predicted.runway,actual.gate,actual.runway)
  {
  n=length(predicted.gate)
  RMSE.gate=sqrt(1/n*(predicted.gate-actual.gate)^2)
  RMSE.runway=sqrt(1/n*(predicted.runway-actual.runway)^2)
  final=0.75*RMSE.gate+0.25*RMSE.runway
  return(final)
    
}