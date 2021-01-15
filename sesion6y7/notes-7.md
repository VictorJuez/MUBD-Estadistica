p.acierto = 0.60
Parece bajo pero hay que considerar que tenemos hay 9 clases.
Si lo hicieramos full random obtendriamos un p.acierto de 0.11, y en cambio ahora tenemos 0.6, asi que aunque parezca poco no lo es.

plot de features: me interesa en el plot que haya una clase que discrimine mucho.
Gini cuantifica esta discriminacion de una clase sobre el resto. Inidce GINI cuanto mayor mejor.

Quitar variables porque si, no es buena idea, por minimo relevante que sean las utilizaremos, nos dan informacion
Laplace solo aplica para predictores categoricos y no continuos

POdemos utilizar VIF tambien para elminar variables correlacionadas

-------- 

Arboles condicioales

Random forest: hacer muchos arboles condicionales haciendo versiones aleatorias (sobre filas y columnas)
  - Bootstrap de las observaciones
  
  
---

SVM - Support Vector Machines

Permitimos menos error, por lo que hay menos puntos que intervienen en la solucion
