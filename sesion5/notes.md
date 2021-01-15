distancias
- Euclidea: la mas utilizada
- Mahalanobis: en comparacion a la euclidea tiene en cuenta la correlacion entre caracteristicas
- Gower: para variables categoricas

Distancias entre clusters
- Criterio para juntar clusters mas utilizado: Criterio de word

- Inercia total: 
  - Inercia Entre-grupo: los centros de gravedad de cada cluster y veer la distancia al centro de gravedad total. 
  - Inercia Intra-grupo: la suma de las distancias de cada observacion al centro de gravedad del cluster
- NOs interesa que la inercia entre-grupo sea muy grande y poca de Intra-grupo. 

Variabilidad explicada: La proporcion de la variabilidad total que viene explicada por la particioon que he hecho
Si augmentamos el numero de clusters tendremos una variabilidad explicada mayor. Si cada observacion fuera un cluster explicamos el 100% (aunque no tendria sentido hacerlo)

Metodos de agrupacion
- El algomerativo suele ser mas rapido

Criterio de ward
- Unir los que el cambio en la distancia intra sea el menor
- Unire aquellos clusters que sean mas pequenos

- correlacion cofenetica: similitud que hay entre las distancias originales y las distancias del dendograma son elevadas, significa que el arbol esta bien construido.

Regla del codo
Cuando se pasa del codo, la inercia intra disminuye por el azar y no porque a cuantos mas clusters menos inercia.

Estadistico de Hopkins: Realmente los datos se pueden dividir en Clusters? Realmente los necesitamos?


No hay que escalar los datos si estan tomados en la misma escala