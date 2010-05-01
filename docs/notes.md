Fragen
======

1.Frage: Kann eine XOR Logik nicht doch mit nur 3 statt 4 Neuronen entwerfen
--------

`Info: ` auf der [Seite der Uni Münster][uni-muenster] ist erklärt, wie eine XOR Logik mit Neuronen aussieht.
Es steht dort, dass man auf jeden Fall ein Neuron in der Hidden Schicht braucht und somit 4 Neuronen benötigt.

Eigentlich könnte man sich das Hiden Unit sparen, wenn man die Gewichte so läßt und die Aktivierungsfunktion des Neurons an der Output Schicht (Neuron Nr.4 :N4) so festlegt: Wenn das Eingangssignal (Summe aller Signale in N4) == 1, dann gib 1 aus, sonst 0.
Eine weitere Frage wäre, ob es das gleiche wäre: wenn ein Neuron nicht über den Schwellenwert kommt UND ob ein Neuron über den Schwellenwert kommt und danach aber 0 ausgibt/feuert.

[uni-muenster]: http://cs.uni-muenster.de/Professoren/Lippe/lehre/skripte/wwwnnscript/prinzip.html#xor

2.Frage: Das Neuronale Netz arbeitet intern nur mit Zahlen (Gewichten und Signalen), wie soll man Text, Bilder, etc. auf Zahlen abbilden
--------
Genaue Beschreibung und mögliche Ideen folgen...

3.Frage: Wie funktioniert das Gradienten(abstiegs)verfahren beim Back-Propagation-Algorithmus genau.
--------
Eventuell werden wir es bis Dienstag es mehrmals durchgelesen und auch verstanden haben.