NN_CV <- function(Data, DV_Name, Activation = 'tanh', Hidden = c(6, 4, 2), Algorithm = 'rprop+', Stepmax = 3e4, Threshold = 1, Rep = 5, Seed = 1, CV = 2, Trace = TRUE) {
  
  library('neuralnet')
  
  #############################################################################################################
  # PRELIMIARIES
  
  if(Seed >= 0) {
    set.seed(Seed)
  }
  DAW					<- Data[complete.cases(Data),]
  N					<- dim(DAW)[1]
  DAW					<- DAW[sample(1:N, N, replace = FALSE),]
  
  #############################################################################################################
  # CROSS-VALIDATION
  
  N_Test				<- floor(N / CV)
  CV_Boundaries			<- c(seq(0, (CV - 1) * N_Test, N_Test), N)
  Prediction				<- rep(NA,N)
  Truth					<- DAW[[ DV_Name ]]
  Model_objects			<- list()
  
  if(Trace) {
    cat('\nCROSS-VALIDATION:')
  }
  for(cvf in 1:CV) {
    test_ind 								<- (CV_Boundaries[cvf] + 1) : CV_Boundaries[cvf + 1]
    Data_Train 								<- DAW[-test_ind,]
    Data_Test 								<- DAW[test_ind, names(Data) != DV_Name]
    NN_Out								<- NN_Repeated(Data = Data_Train, DV_Name = DV_Name, Activation = Activation, Hidden = Hidden, Algorithm = Algorithm, 
                                 Stepmax = Stepmax, Threshold = Threshold, Rep = Rep, Seed = -1, Trace = Trace, Plot = FALSE)
    Prediction[test_ind]						<- compute(NN_Out$Model_obj, Data_Test)$net.result
    Model_objects[[ paste('fold ', cvf, sep = '') ]]	<- NN_Out$Model_obj
    
    if(Trace) {
      cat(paste('\n   Processed fold ', cvf, '.', sep = ''))
    }
  }
  if(Trace) {
    cat('\n\n')
  }
  
  list(
    CV_Error = as.numeric(sqrt(mean( (Truth - Prediction)^2 ))), Model_objects = Model_objects
  )
  
}