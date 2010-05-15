Fragen
============

Monaden & Co
------------
	:: (Monad m) => [Neuron] -> [Neuron] -> [Neuron] -> m [Neuron]

Monade steht links, manchmal steht sie rechts, wie genau?
Unterschied von `=>` und `->`
Was ist was? `(Monad m) / m [Neuron]`

Variablen ändern?
-----------------

	network = [[n1_1, n1_2], [n2_1, n2_2], [n3_1]]

	inputLayer = network !! 0
	hiddenLayer = network !! 1
	outputLayer = network !! 2
	
	newHiddenLayer = calcLayer inputLayer hiddenLayer
	n2_1_new = Neuron 0 0.345 []
	
Muss man alle alten Referenzen neu generieren bis zur Obersturktur (network) ?