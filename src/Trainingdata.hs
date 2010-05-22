module Trainingdata where

data Trainingdata = Trainingdata {
	learnSteps :: Int, -- number of learn steps for backpropagation
	input :: TrainDataList, -- training for all learnsteps and whole input layer
	output :: TrainDataList -- trainng for all leransteps and whole output layer
	} deriving (Show, Eq)

-- Training for a Input/Output Layer
type TrainData = [Double] 

-- Training list for one learn step
-- index of the input data and output data must match
type TrainDataList = [TrainData] 



