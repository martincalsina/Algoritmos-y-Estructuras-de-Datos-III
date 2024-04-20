import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List
import Test.HUnit (Test(TestLabel))

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}
{-
Constructores: 
  Constructor base:
  |Personaje Posición String  :: Posicion -> String -> Personaje = (Float, Float) -> String -> Personaje = :: Posicion -> String -> b = ::  
  Constructores recursivos:
  | Mueve Personaje Dirección :: Personaje -> Dirección -> Personaje = :: b -> Dirección -> b               
  | Muere Personaje :: Personaje -> Personaje = :: b -> b
  Llamamos Personaje = b
 -}
foldPersonaje :: (Posición -> String -> b) -> (b -> Dirección -> b) -> (b -> b) -> Personaje -> b
foldPersonaje casoPersonaje casoMueve casoMuere personaje = case personaje of 
                                                          Personaje pos name -> casoPersonaje pos name
                                                          Mueve pers dir -> casoMueve (rec pers) dir
                                                          Muere pers -> casoMuere (rec pers)
                                                          where rec = foldPersonaje casoPersonaje casoMueve casoMuere
--En los constructores recursivos, aplico foldPersonaje sobre el parámetro de tipo Personaje
{-
Constructores: 
  Constructor base:
  | Objeto Posición String  :: Posicion -> String -> Objeto = :: Posicion -> String -> b
  Constructores recursivos:
  | Tomado Objeto Personaje :: Objeto -> Personaje -> Objeto = :: b -> Personaje -> b
  | EsDestruido Objeto :: Objeto -> Objeto = :: b -> b
  Renombre Objeto = b
-}
foldObjeto :: (Posición -> String -> b) -> (b -> Personaje -> b) -> (b -> b) -> Objeto -> b
foldObjeto casoObjeto casoTomado casoDestruido objeto = case objeto of 
                                                        Objeto pos name -> casoObjeto pos name
                                                        Tomado obj pers -> casoTomado (rec obj) pers
                                                        EsDestruido obj -> casoDestruido (rec obj)
                                                        where rec = foldObjeto casoObjeto casoTomado casoDestruido
--En los constructores recursivos, aplico foldObjeto sobre el parámetro de tipo Objeto

-- 
{-Ejercicio 2-}
--Decisión: Permitimos consultar posicion de un personaje muerto
posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje (const) (\recursionSobrePers dir -> siguiente_posición recursionSobrePers dir) (id)
--Idea: Hacemos recursion sobre el personaje, una vez llegamos al constructor base devolvemos la posición. En el caso de un personaje en movimiento la actualizamos en base a la dirección en que se movió. Si está muerto devolvemos su posición tal cual

--Decisión: Permitimos conocer el nombre de objetos destruidos
nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (\x y -> y) (const) (id)
--Idea: Hacemos recursion sobre el objeto, una ves llegamos al constructor base retornamos el 2do parámetro de este (su nombre). Si el objeto está tomado por un personaje hacemos recursión sobre el objeto.

{-Ejercicio 3-}
--Decisión: Si los objetos fueron destruidos ya no pertenecen al universo
objetos_en :: Universo -> [Objeto]
objetos_en  = foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) []
--Idea: Hacemos recursión sobre la lista Universo, si el elemento de la lista es un objeto que no fue destruido lo agregamos a la lista que devolveremos como respuesta

--Decisión: Los personajes muertos ya no pertenecen al universo
personajes_en :: Universo -> [Personaje]
personajes_en = foldr (\x rec -> if es_un_personaje x && está_vivo(personaje_de x) then personaje_de x : rec else rec) []
--Idea: Hacemos recursión sobre la lista Universo, si el elemento es un personaje y está vivo lo agregamos a la lista que devolveremos como respuesta
{-

Queremos probar que

∀ u :: Universo . ∀ o :: Objeto . elem o (objetos_en u) ⇒ elem (Right o) u

Básicamente nos están diciendo que si un objeto (de tipo Objeto) pertence a la lista
de Objeto de un universo u, entonces pertenece a la lista de todos los elementos de ese universo
(una lista de Either Personaje Objeto, por eso el Right o)

por definición de Universo, esto es equiavalente a probar que

∀ u :: [Either Personaje Objeto] . ∀ o :: Objeto . elem o (objetos_en u) ⇒ elem (Right o) u

Entonces, usemos el principio de inducción sobre listas para probar esta implicación.

Sea nuestro predicado unario:

P(xs): ∀ o :: Objeto . elem o (objetos_en xs) ⇒ elem (Right o) xs, para xs :: [Either Personaje Objeto]

Debido a la estructura de xs, basta ver que vale para

- caso base P([])
- paso inductivo P(xs) => P(x:xs) para algún x :: Either Personaje Objeto, xs :: [Either Personaje Objeto]

----- caso base P([]) -----

Queremos ver que

elem o (objetos_en []) ⇒ elem (Right o) []

pero por definición de objetos_en

elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] []) => elem (Right o) []

pero como foldr se define

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

matchea con el caso base, por lo que se tiene

elem o [] => elem (Right o) []

pero tomando la definición usal de elem como

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) = (e == x) || elem e xs

por el caso base se tiene

False => elem (Right o) []

implicación que es verdadera cualquiera sea el valor de verdad de elem (Right o) [], pues
False => p es siempre True para todo p :: Bool


----- paso inductivo P(xs) => P(x:xs)-----


HI: ∀ o :: Objeto . elem o (objetos_en xs) ⇒ elem (Right o) xs Para algún xs :: [Either Personaje Objeto]

Queremos ver que

elem o (objetos_en (x:xs)) ⇒ elem (Right o) x:xs para todo x :: Either Personaje Objeto

por definición de objetos_en

elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] (x:xs)) => elem (Right o) (x:xs)

por definición de elem en el lado derecho

elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] (x:xs)) => (Right o) == x || elem (Right o) xs

Vamos a tener que usar extensionalidad sobre x, ya que es_un_objeto hace pattern matching con Right o Left,
constructores de Either Personaje Objeto, del que es x.

O sea, hay que probar que la implicación vale para:

x = Left y, para y :: Personaje
x = Right y, para y :: Objeto

--- Sea x :: Left y, con y :: Personaje ---

Por propiedad β (slide 7, práctica clase 9/4), podemos reemplazar los valores que toma la función
anónima del foldr con lo que le está siendo pasado, o sea que

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] (x:xs)

pasa a ser (por definición de foldr para el caso de lista no vacía)

(\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) x recFoldr

donde por definición de foldr, recFoldr (alias para que no quede tan largo) es

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs

Ahora aplicando la propiedad β se tiene

if es_un_objeto Left y && not(fue_destruido (Left y)) then objeto_de (Left y) : recFoldr else recFoldr

Pero por definición de es_un_objeto

es_un_objeto Left y = False

Así, la guarda del if evalúa False (da igual el valor de not (fue destruido (Left y))), por lo que se tiene

if False then objeto_de (Left y) : recFoldr else recFoldr 

que nos lleva al else,

recFoldr

que habíamos dicho es

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs

Por lo que la expresión

elem o (foldr (\x rec -> if es_un_objeto x then objeto_de x : rec else rec) (x:xs) ) []) => elem (Right o) (x:xs)

es equivalente, en este caso, a

elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs) => (Right o) == x || elem (Right o) xs

pero como x matchea con Left y, del lado derecho de la implicación se tiene 

(Right o) == (Left y) || elem (Right o) xs

Que al ser distintos constructores se evalúa

False || elem (Right o) xs

Donde el valor de verdad de la disyunción queda determinado por el término de la derecha

elem (Right o) xs

o sea que, en definitiva, la implicación resultante es

elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs) => elem (Right o) xs

pero el foldr interno es equivalente a objetos_en xs por definición

elem o (objetos_en xs) => elem (Right o) xs

Implicación que es verdadera por hipótesis inductiva.

--- Sea x :: Right y . para y :: Objeto

Nuevamente, por propiedad β, reemplazamos los valores que toma la función
anónima del foldr con lo que le está siendo pasado por argumento, por lo que

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] (x:xs)

desarrollando foldr para el caso recursivo

(\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) x recFoldr

para recFoldr alias de

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs

por cuestiones de espacio.

Entonces, evaluando los argumentos en la función anónima que tenemos:


if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : recFoldr else recFoldr

Pero sabemos con qué matchea x

if es_un_objeto (Right y) && not(fue_destruido (objeto_de (Right y))) then objeto_de (Right y) : recFoldr else recFoldr

donde por definición

es_un_objeto (Right y) = True

lo que hace que la guarda del if, al ser una conjunción, quede determinada por el valor de verdad
del segundo termino

Pero además, como

objeto_de (Right y) = y

por definición, se tiene que la expresión es equivalente a

if not(fue_destruido y) then y : recFoldr else recFoldr

Tenemos que analizar el caso en que la guarda es verdadera y aquel en que es falsa por separado.

--- Caso Falso

if False then y : recFoldr else recFoldr

es equivalente a 

recFoldr

o sea 

foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs

por lo que reemplazando en la implicación nos queda


elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs) => (Right o) == x || elem (Right o) xs

Pero ese foldr es, por definición igual a objetos_en xs, osea que se tiene

elem o (objetos_en xs) => (Right o) == x || elem (Right o) xs

Reemplazando x por lo que matchea


elem o (objetos_en xs) => (Right o) == (Right y) || elem (Right o) xs

Es casi la HI, pero hay que ver el valor de la expresión (Right o) == (Right y)

- Si es verdadera, nos queda

elem o (objetos_en xs) => True || elem (Right o) xs

Donde el lado derecho es verdadero da igual el valor de ese elem

elem o (objetos_en xs) => True

Y esta implicación es verdadera sin importar el valor del antecedente, pues V => V of F => V es verdadero

- Si es falsa, nos queda

elem o (objetos_en xs) => False || elem (Right o) xs

El valor de verdad del lado derecho queda determinado por su elem

elem o (objetos_en xs) => elem (Right o) xs

Pero esto es verdadero por HI.

Nos queda ver el otro caso de la función anónima

---

if True then y : recFoldr else recFoldr

es equivalente a

y : recFoldr

o sea que expandiendo el recFoldr

y : (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs)

reemplazando en la implicación original

elem o (y : (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs)) => (Right o) == x || elem (Right o) xs

que es equivalente, por definición de elem, a

y == o || elem o (foldr (\x rec -> if es_un_objeto x && not(fue_destruido (objeto_de x)) then objeto_de x : rec else rec) [] xs) => (Right o) == x || elem (Right o) xs

pero ese foldr es equivalente, por definición, a

y == o || elem o (objetos_en xs) => (Right o) == x || elem (Right o) xs

pero como sabemos el constructor con el que matchea x

y == o || elem o (objetos_en xs) => (Right o) == (Right y) || elem (Right o) xs

Ahora bien y == o <==> (Right o) == (Right y) toman el mismo valor de verdad, pues es el mismo dato
"suelto" o con el constructor Right. Hay que analizar los casos que se generan a partir del valor
de verdad de esas expresiones.

--- y == o es verdadero

También Right o == Right y es verdadero, luego reemplazando por los valores de verdad

True || elem o (objetos_en xs) => True || elem (Right o) xs

True => True

La implicación es verdadera.

--- y == o es falso

También Right o == Right y es falso, así

False || elem o (objetos_en xs) => False || elem (Right o) xs

Entonces los valores de verdad del lado izquierdo y derecho de la implicación dependen de las dos funciones
que usan elem, o sea

elem o (objetos_en xs) => elem (Right o) xs

pero esto es válido por hipótesis inductiva.

----------------------------------------------------------------

Demostrados el caso base y el caso inductivo, concluimos que 

P(xs) para todo xs :: Universo

-}

{-Ejercicio 4-}
{-Decisión: Si se consulta por un personaje que no está en el universo devolveremos [], ya que trivialmente no tendrá objetos en el universo.
            Los objetos en posesión de un personaje muerto siguen en su posesión.
-}
objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de nombre = foldr (\u rec -> if  es_un_objeto u && not(fue_destruido(objeto_de u)) && en_posesión_de nombre (objeto_de u) then (objeto_de u) : rec else rec) [] 
--Idea: Para cada objeto en posesion_de String que no está destruido agrego a lista


{-Ejercicio 5-}

-- Asume que hay al menos un objeto
--Decisión: En el caso en que haya más de un objeto a la misma distancia retornamos el que aparece último en la lista de los objetos del universo
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano personaje universo = foldr1 (\obj rec -> objeto_distancia_minima personaje obj rec) (objetos_libres_en universo)

objeto_distancia_minima :: Personaje -> Objeto -> Objeto -> Objeto
objeto_distancia_minima p o1 o2 = if (distancia (Left p) (Right o1)) < (distancia (Left p) (Right o2)) then o1 else o2

{-
Idea: como sólo nos importan los objetos libres, los obtenemos de una vez con la función que hace eso. Ya con estos,
y asumiendo que hay por lo menos hay 1, vamos a ir quedándonos con el que menor distancia tenga a nuestro personaje.
Como el caso base no es la lista vacía, usamos foldr1 en vez de foldr.
-}

{-Ejercicio 6-}
--Decisión: Las gemas destruidas no cuentan, sino Thanos podría ganar con todas las gemas destruidas
--Asumimos que Thanos está en el universo
tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = (foldr (\obj rec -> if es_una_gema obj then 1 + rec else rec) 0 (objetos_en_posesión_de "Thanos" u)) == 6
{-
Idea: a partir de la lista de todos los objetos del personaje "Thanos" (vacía si no está en el universo),
contamos aquellas que son gemas, que deberían sumar 6 para cumplir con el ejercicio.

-}

{-Ejercicio 7-}
--Decisión: Si una de los personajes cuya presencia es parte de las condiciones para ganarle a Thanos está muerto entonces no podemos ganarle
podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = (está_el_personaje "Thanos" u && está_vivo (personaje_de_nombre "Thanos" u)) && ((not (tiene_thanos_todas_las_gemas u) && está_el_personaje "Thor" u && está_el_objeto "Stormbreaker" u) || (está_el_personaje "Wanda" u && está_el_personaje "Visión" u && está_el_objeto "Gema de la Mente" u && elem (objeto_de_nombre "Gema de la Mente" u) (objetos_en_posesión_de  "Visión" u)))

{-
Idea: es traducir las dos posibilidades que nos indicaron a una función.
-}
{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  --Definimos funciones para testear el funcionamiento de los esquemas fold
  ,
  foldPersonaje (\pos name -> 2 * fst pos ) (const) (id) ironMan ~=? 4.0    -- Duplicamos la primer coord de la posición de un personaje
  ,
  foldPersonaje (\pos name -> Personaje (0,0) name ) (const) (id) ironMan ~=? Personaje (0,0) "IronMan" --Reestablecemos a posición cero
  ,
  foldPersonaje (\pos name -> Personaje (0,0) name ) (const) (id) spiderManMuerto ~=? Personaje (0,0) "SpiderMan" --Si esta muerto lo reseteamos a posicion cero
  ,
  foldPersonaje (\pos name -> Personaje (0,0) name ) (const) (id) spiderManMovido2 ~=? Personaje (0,0) "SpiderMan" --Si esta en moviemiento a posicion cero
  ,
  foldPersonaje (const(id)) (const) (\r -> r ++ " Muerto") spiderManMuerto ~=? "SpiderMan Muerto" --Definimos una funcion muerto que nos retorna el nombre del personaje y si está muerto
  ,
  foldPersonaje (const(id)) (const) (\r -> r ++ " Muerto") ironMan ~=? "IronMan"
  ,
  foldPersonaje (const(id)) (const) (\r -> r ++ " Muerto") spiderManMovido ~=? "SpiderMan"
  ,
  --Definimos una función que nos retorna si un objeto es una Gema de Mente y no está en posesión de Thanos
  foldObjeto (\pos name -> name == "Gema de Mente") (\obj pers -> obj && ((nombre_personaje pers) /= "Thanos")) (id) gemaMente ~=? True
  ,
  foldObjeto (\pos name -> name == "Gema de Mente") (\obj pers -> obj && ((nombre_personaje pers) /= "Thanos")) (id) gemaAlmaEnPoderDeThanos ~=? False
  ,
  foldObjeto (\pos name -> name == "Gema de Mente") (\obj pers -> obj && ((nombre_personaje pers) /= "Thanos")) (id) gemaMenteEnPoderDeThanos ~=? False
  ,
  foldObjeto (\pos name -> name == "Gema de Mente") (\obj pers -> obj && ((nombre_personaje pers) /= "Thanos")) (id) gemaMenteEnPoderVision ~=? True
  ,  
  foldObjeto (\pos name -> 0) (\rec pers -> 1+rec) (id) objeto1TomadoTomado ~=? 2 --contar la cantidad de veces que fue tomado
  ,
  foldObjeto (\pos name -> []) (\rec pers -> pers:rec) (id) objeto1TomadoTomado ~=? [ironMan, spiderMan] --historial de los personajes que lo tomaron (mas reciente a menos)
  ,
  foldObjeto (\pos name -> True) (\rec pers -> False) (id) objeto1TomadoTomado ~=? False --Decimos si nunca fue tomado el objeto
  ,
  foldObjeto (\pos name -> True) (\rec pers -> False) (id) objeto1 ~=? True --idem
  ,
  foldObjeto (\pos name -> fst pos > 0 && snd pos > 0) (const) (id) crocsDeHulk ~=? True--Decidimos si el objeto fue inicializado en el primer cuadrante  
  

  
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  TestLabel "Personaje que no se movió" $ test [
    posición_personaje phil     -- Caso de test 1 - expresión a testear
      ~=? (0,0)                 -- Caso de test 1 - resultado esperado
    ], 
  TestLabel "Personaje que se movió" $ test [
    posición_personaje spiderManMovido4
      ~=? (5,2)
    ],
  TestLabel "Personaje que se movió y se murió" $ test [
    posición_personaje spiderManMuerto
      ~=? (5,2)
    ],
  TestLabel "Nombre de Objeto quieto" $ test [
    nombre_objeto computadoraCuantica
      ~=? "ComputadoraCuantica"
    ],
  TestLabel "Nombre de Objeto que fue tomado por alguien" $ test [
    nombre_objeto escudoCapitanAmerica
      ~=? "escudoCapitanAmerica"
    ],
  TestLabel "Nombre de Objeto que fue destruido" $ test [
    nombre_objeto escudoCapitanAmericaDestruido
      ~=? "escudoCapitanAmerica"
    ]
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  TestLabel "Objetos de Universo vacío" $ test [
    objetos_en []            -- Caso de test 1 - expresión a testear
      ~=? []                 -- Caso de test 1 - resultado esperado
    ],
  TestLabel "Objetos de Universo con un único objeto" $ test [
    objetos_en universoUnObjeto          
      ~=? [computadoraCuantica]       
    ],
  TestLabel "Objetos de Universo con varios Objetos" $ test [
    objetos_en universoVariosObjetos       
      ~=? [computadoraCuantica, escudoCapitanAmerica]       
    ],
  TestLabel "Objetos de Universo con varios Objetos y Personajes" $ test [
    objetos_en universo1     
      ~=? [computadoraCuantica, tablaDePicada, escudoCapitanAmerica]    
    ],
  --DUDA: creo que este no hace falta, habría que preguntar si podemos asumir que no hay objetos repetidos
  TestLabel "Objetos de Universo con Objetos repetidos" $ test [
    objetos_en universoConObjetosRepetidos    
      ~=? [computadoraCuantica, escudoCapitanAmerica, computadoraCuantica]
    ],
  TestLabel "Objetos de universo con todos los objetos destruidos" $ test [
    objetos_en todosObjetosDestruidos 
      ~=? []
    ],
  TestLabel "Personajes de Universo vacío " $ test [
    personajes_en []
      ~=? []
    ],
  TestLabel "Personajes de Universo con un único Personaje" $ test [
    personajes_en universoUnPersonaje
      ~=? [spiderMan]
    ],
  TestLabel "Personajes de Universo con varios Personajes" $ test [
    personajes_en universoVariosPersonajes
      ~=? [spiderMan, capitanAmerica, phil]
    ],
  TestLabel "Personajes de Universo con varios Personajes y Objetos" $ test [
    personajes_en universo1
      ~=? [superMan, ironMan, spiderMan, capitanAmerica]
    ],
  TestLabel "Personajes de Universo con personaje muerto" $ test [
    personajes_en universoConPersonajeMuerto
      ~=? [ironMan]
    ]
  ]


testsEj4 = test [ -- Casos de test para el ejercicio 4
  TestLabel "Objetos de un Personaje y un Universo vacío" $ test [
    objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
      ~=? []                             -- Caso de test 1 - resultado esperado
    ],
  TestLabel "Objetos de un Personaje en Universo sin Dueño " $ test [
    objetos_en_posesión_de "CapitanAmerica" universoObjetosSinDueño
      ~=? []
    ],
  TestLabel "Objetos de un Personaje en Universo con Dueño " $ test [
    objetos_en_posesión_de "CapitanAmerica" universoUnObjetoConDueño
      ~=? [escudoCapitanAmerica]
    ],
  TestLabel "Objetos de un Personaje en Universo con Dueño pero que fue destruido" $ test [
    objetos_en_posesión_de "CapitanAmerica" universoUnObjetoPeroDestruido
      ~=? []
    ],
  TestLabel "Objetos de un Personaje en Universo con varios objetos que son suyos" $ test [
    objetos_en_posesión_de "SpiderMan" universoVariosObjetosConDueño
      ~=? [objeto1Tomado, objeto2Tomado]
    ],
  TestLabel "Objetos de un Personaje en Universo con varios objetos que son suyos pero hay destruidos" $ test [
    objetos_en_posesión_de "SpiderMan" universoVariosObjetosConDueñoYDestruidos
      ~=? [objeto1Tomado, objeto2Tomado]
    ],
  TestLabel "Objetos de un Personaje en Universo que tiene objetos destruidos y en buen estado" $ test [
    objetos_en_posesión_de "IronMan" universonConObjetosDePersonajeDestruidosYNo
      ~=? [rayoLaserTomadoPorIronMan, lanzallamasTomadoPorIronMan]
    ],
  TestLabel "Objetos de un Personaje muerto en Universo" $ test [
    objetos_en_posesión_de "Hulk" universoConPersonajeMuertoConObjetos
      ~=? [crocsDeHulkTomadosPorHulk, lanzallamasTomadoPorHulk]
    ]
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  TestLabel "Objeto libre mas cercano en universo con un solo objeto" $ test [
    objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
      ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
    ],
  TestLabel "Objeto libre mas cercano a personaje que tiene objetos a la misma distancia" $ test [
    objeto_libre_mas_cercano ironMan universoConObjetosAMismaDistancia
      ~=? rayoLaser
    ],
  TestLabel "Objeto libre mas cercano a personaje que tiene objetos a la misma distancia pero uno destruido, retorna el 3ro" $ test [
    objeto_libre_mas_cercano ironMan universoConObjetosAMismaDistanciaUnoDestruidoOtroTomadoTomaEl3ro
      ~=? armaduraDeIronMan
    ],
    TestLabel "Objeto libre mas cercano a personaje que tiene objetos a la misma distancia pero uno destruido" $ test [
    objeto_libre_mas_cercano ironMan universoConObjetosAMismaDistanciaPeroUnoDestruido
      ~=? rayoLaser
    ],  
  TestLabel "Objeto libre mas cercano a personaje que tiene varios objetos a distintas distancias" $ test [
    objeto_libre_mas_cercano ironMan universoConObjetosADistintasDistancias
      ~=? paloDeAmasar
    ]  
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  TestLabel "Tiene Thanos todas las gemas en un universo sin Thanos" $ test [
    tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
      ~=? False                                            -- Caso de test 1 - resultado esperado
    ],
  TestLabel "Tiene Thanos todas las gemas en un universo pero tiene Thanos todas las gemas en un universo tiene menos de 6 gemas y no están todas en el universo" $ test [
    tiene_thanos_todas_las_gemas universoThanosTieneMenosDe6GemasYnoEstanLas6
      ~=? False
    ],  
  TestLabel "Tiene Thanos todas las gemas en un universo en que Thanos tiene menos de 6 pero si están todas las gemas" $ test [
    tiene_thanos_todas_las_gemas universoThanosTieneMenosDe6GemasPeroSiEstanTodas
      ~=? False 
    ],  
  TestLabel "Tiene Thanos todas las gemas en un universo en que Thanos tiene menos de 6 y una la tiene otro personaje" $ test [
    tiene_thanos_todas_las_gemas universoThanosTieneMenosDe6GemasYUnaLaTieneVision
      ~=? False
    ],  
  TestLabel "Tiene Thanos todas las gemas en un universo en que Thanos tiene las 6 gemas pero unas destruidas" $ test [
    tiene_thanos_todas_las_gemas universoThanosTiene6GemasPeroUnasDestruidas
      ~=? False
    ],
  TestLabel "Tiene Thanos todas las gemas en un universo en que Thanos tiene las 6 gemas en buen estado" $ test [
    tiene_thanos_todas_las_gemas universoThanosTiene6GemasEnBuenEstado
      ~=? True
    ]
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  TestLabel "Podemos ganarle a Thanos en un universo sin Thanos" $ test [ 
    podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
      ~=? False                                          -- Caso de test 1 - resultado esperado
  ],
  TestLabel "Podemos ganarle a Thanos si tiene todas las gemas" $ test [ 
    podemos_ganarle_a_thanos casoThanosTieneTodasLasGemas          
      ~=? False                                          
  ],
  TestLabel "Podemos ganarle a Thanos si esta Thor, tambien el StormBreaker pero no en su posesion" $ test [ 
    podemos_ganarle_a_thanos casoEstaThorYStormBreakerPeroNoEnPosesión          
      ~=? True                                          
  ],
  TestLabel "Podemos ganarle a Thanos si está Thor con el StormBreaker en su posesion" $ test [ 
    podemos_ganarle_a_thanos casoEstaThorYStormBreakerEnPosesionThor
      ~=? True                                          
  ],
  TestLabel "Podemos ganarle a Thanos si está Thor pero no el StormBreaker" $ test [ 
    podemos_ganarle_a_thanos casoEstaThorPeroNoStormBeaker          
      ~=? False
  ],
  TestLabel "Podemos ganarle a Thanos si se cumplen las condiciones pero no vino Thanos" $ test [ 
    podemos_ganarle_a_thanos casoPodemosGanarlePeroNoVinoThanos
      ~=? False                                          
  ],
  TestLabel "Podemos ganarle a Thanos si esta Wanda pero no Vision" $ test [ 
    podemos_ganarle_a_thanos casoEstaWandaPeroNoVision
      ~=? False                                           
  ],
  TestLabel "Podemos ganarle a Thanos si Vision tiene gema de Mente pero no esta Wanda" $ test [ 
    podemos_ganarle_a_thanos casoVisionTieneGemaPeroNoEstaWanda        
      ~=? False
  ],
  TestLabel "Podemos ganarle a Thanos si estan Wanda y Vision pero no la gema" $ test [ 
    podemos_ganarle_a_thanos casoEstaWandaEstaVisionPeroNoGema          
      ~=? False
  ],
  TestLabel "Podemos ganarle a Thanos si esta Wanda y está Vision pero la gema de Mente la tiene Thanos" $ test [ 
    podemos_ganarle_a_thanos casoEstaWandaEstaVisionPeroGemaMenteLaTieneThanos          
      ~=? False                                           
  ],
  TestLabel "Podemos ganarle a Thanos si estan Wanda y Vision pero la gema de Mente se rompio" $ test [ 
    podemos_ganarle_a_thanos casoEstaWandaEstaVisionPeroGemaMenteSeRompio          
      ~=? False
  ],
  TestLabel "Podemos ganarle a Thanos si esta el StormBreaker pero Thor esta muerto" $ test [ 
    podemos_ganarle_a_thanos casoStormBreakerPeroThorMuerto          
      ~=? False
  ],
  TestLabel "Podemos ganarle a Thanos si se cumplen todas las condiciones en simultaneo" $ test [ 
    podemos_ganarle_a_thanos casoSeDaTodoParaGanar
      ~=? True
    ],
  TestLabel "Podemos ganarle a Thanos si se cumplen todas las condiciones en simultaneo pero Thanos está muerto" $ test [ 
    podemos_ganarle_a_thanos casoSeDaTodoParaGanarPeroThanosEstaMuerto
      ~=? False
    ]  
  ]
{-Inputs de Tests-}

  {-Personajes utilizados-}
superMan :: Personaje
superMan = Personaje (0,0) "SuperMan"

ironMan :: Personaje
ironMan = Personaje (2,3) "IronMan"

capitanAmerica :: Personaje
capitanAmerica = Personaje (5,3) "CapitanAmerica"

spiderMan :: Personaje
spiderMan = Personaje (4,1) "SpiderMan"

muereSpiderMan :: Personaje
muereSpiderMan = Muere spiderMan

spiderManMovido :: Personaje
spiderManMovido = Mueve spiderMan Norte
spiderManMovido2 = Mueve spiderManMovido Norte -- (6,1)
spiderManMovido3 = Mueve spiderManMovido2 Sur -- (5,1)
spiderManMovido4 = Mueve spiderManMovido3 Este -- (5,2)
spiderManMuerto = Muere spiderManMovido4 --(5,2)

hulk :: Personaje 
hulk = Personaje (1,1) "Hulk"

hulkMuerto :: Personaje
hulkMuerto = Muere hulk

thanos :: Personaje
thanos = Personaje (2,12) "Thanos"

thanosMuerto :: Personaje
thanosMuerto = Muere thanos

vision :: Personaje 
vision = Personaje (1,9) "Vision"

thor :: Personaje
thor = Personaje (4,5) "Thor"

thorMuerto :: Personaje
thorMuerto = Muere thor

wanda :: Personaje 
wanda = Personaje (2,11) "Wanda"


  {-Objetos utilizados-}
computadoraCuantica :: Objeto
computadoraCuantica = Objeto (0,1) "ComputadoraCuantica"

tablaDePicada :: Objeto
tablaDePicada = Objeto (7,2) "TablaDePicada"

escudoCapitanAmerica :: Objeto
escudoCapitanAmerica = Tomado (Objeto (4,2) "escudoCapitanAmerica") capitanAmerica

escudoCapitanAmericaDestruido :: Objeto
escudoCapitanAmericaDestruido = EsDestruido escudoCapitanAmerica

armaduraDeIronMan :: Objeto
armaduraDeIronMan = Objeto (3,3) "Armadura de IronMan"

armaduraDeIronManDestruida :: Objeto
armaduraDeIronManDestruida = EsDestruido armaduraDeIronMan

crocsDeHulk :: Objeto
crocsDeHulk = Objeto (21,2) "Crocs de Hulk"

crocsDeHulkTomadosPorHulk :: Objeto
crocsDeHulkTomadosPorHulk = Tomado crocsDeHulk hulk

crocsDeHulkDestruidos :: Objeto
crocsDeHulkDestruidos = EsDestruido crocsDeHulk

objeto1 :: Objeto
objeto1 = Objeto (0,0) "Objeto 1"

objeto1TomadoTomado :: Objeto
objeto1TomadoTomado = Tomado objeto1Tomado ironMan


objeto1Tomado :: Objeto
objeto1Tomado = Tomado objeto1 spiderMan

objeto2 :: Objeto
objeto2 = Objeto (0,0) "Objeto 2"

objeto2Tomado :: Objeto
objeto2Tomado = Tomado objeto2 spiderMan

objeto3 :: Objeto
objeto3 = Objeto (0,0) "Objeto 3"
objeto3Tomado = Tomado objeto3 spiderMan

objeto3Destruido :: Objeto
objeto3Destruido = EsDestruido objeto3Tomado

rayoLaser :: Objeto
rayoLaser = Objeto (3,2) "Rayo laser"

paloDeAmasar :: Objeto
paloDeAmasar = Objeto (4,1) "Palo de amasar"

lanzallamas :: Objeto
lanzallamas = Objeto (5,5) "Lanzallamas"

rayoLaserTomadoPorIronMan :: Objeto
rayoLaserTomadoPorIronMan = Tomado rayoLaser ironMan

paloDeAmasarDestruido :: Objeto
paloDeAmasarDestruido =  EsDestruido paloDeAmasar

paloDeAmasarDestruidoTomadoPorIronMan :: Objeto
paloDeAmasarDestruidoTomadoPorIronMan = Tomado paloDeAmasarDestruido ironMan

lanzallamasTomadoPorIronMan :: Objeto
lanzallamasTomadoPorIronMan = Tomado lanzallamas ironMan

mascaraDeSpiderMan :: Objeto
mascaraDeSpiderMan = Objeto (3,4) "Mascara de SpiderMan"

mascaraDeSpiderManTomadaPorSpiderMan :: Objeto
mascaraDeSpiderManTomadaPorSpiderMan = Tomado mascaraDeSpiderMan spiderMan

crocsDeHulkDestruidosTomadoPorHulk :: Objeto
crocsDeHulkDestruidosTomadoPorHulk = EsDestruido crocsDeHulkTomadosPorHulk

lanzallamasTomadoPorHulk :: Objeto
lanzallamasTomadoPorHulk = Tomado lanzallamas hulk

gemaMente :: Objeto
gemaMente = Objeto (32,7) "Gema de Mente"

gemaMenteEnPoderDeThanos :: Objeto
gemaMenteEnPoderDeThanos = Tomado gemaMente thanos

gemaMenteDestruida :: Objeto
gemaMenteDestruida = EsDestruido gemaMente

gemaMenteDestruidaEnPoderDeThanos :: Objeto
gemaMenteDestruidaEnPoderDeThanos = Tomado gemaMenteDestruida thanos

gemaAlma :: Objeto
gemaAlma = Objeto (34,3) "Gema de Alma"

gemaAlmaDestruida :: Objeto
gemaAlmaDestruida = EsDestruido gemaAlma

gemaAlmaDestruidaEnPoderDeThanos :: Objeto
gemaAlmaDestruidaEnPoderDeThanos =  Tomado gemaAlmaDestruida thanos

gemaAlmaEnPoderDeThanos :: Objeto
gemaAlmaEnPoderDeThanos = Tomado gemaAlma thanos

gemaEspacio :: Objeto
gemaEspacio = Objeto (2,7) "Gema de Espacio"

gemaEspacioEnPoderDeThanos :: Objeto
gemaEspacioEnPoderDeThanos = Tomado gemaEspacio thanos

gemaPoder :: Objeto
gemaPoder = Objeto (15,7) "Gema de Poder"

gemaPoderEnPoderDeThanos :: Objeto
gemaPoderEnPoderDeThanos = Tomado gemaPoder thanos

gemaTiempo :: Objeto
gemaTiempo = Objeto (1,7) "Gema de Tiempo"

gemaTiempoEnPoderDeThanos :: Objeto
gemaTiempoEnPoderDeThanos = Tomado gemaTiempo thanos

gemaRealidad :: Objeto
gemaRealidad = Objeto (3,7) "Gema de Realidad"

gemaRealidadEnPoderDeThanos :: Objeto
gemaRealidadEnPoderDeThanos = Tomado gemaRealidad thanos

escudoCapitanAmericaTomadoPorSpiderMan :: Objeto
escudoCapitanAmericaTomadoPorSpiderMan = Tomado escudoCapitanAmerica spiderMan

armaduraDeIronManDestruidaTomadaPorHulk :: Objeto
armaduraDeIronManDestruidaTomadaPorHulk = Tomado armaduraDeIronManDestruida hulk

rayoLaserTomadoPorHulk :: Objeto
rayoLaserTomadoPorHulk = Tomado rayoLaser hulk

stormbreaker :: Objeto
stormbreaker = Objeto (7,1) "Stormbreaker"

gemaMenteEnPoderVision :: Objeto
gemaMenteEnPoderVision = Tomado gemaMente vision

stormbreakerEnPoderDeThor :: Objeto
stormbreakerEnPoderDeThor = Tomado stormbreaker thor

  {-Universos utilizados-}

universo1 = universo_con [superMan, ironMan, spiderMan, capitanAmerica] [computadoraCuantica, tablaDePicada, escudoCapitanAmerica]

universo2 = universo_con [spiderManMovido4, ironMan] []

universoUnObjeto = universo_con [spiderManMovido4, ironMan] [computadoraCuantica]

universoVariosObjetos = universo_con [spiderManMovido4, ironMan] [computadoraCuantica, escudoCapitanAmerica, escudoCapitanAmericaDestruido]

universoConObjetosRepetidos = universo_con [spiderManMovido4, ironMan] [computadoraCuantica, escudoCapitanAmerica, computadoraCuantica, escudoCapitanAmericaDestruido]

universoUnPersonaje = universo_con [spiderMan] []

universoVariosPersonajes = universo_con [spiderMan, capitanAmerica, phil] []

todosObjetosDestruidos = universo_con [ironMan] [crocsDeHulkDestruidos, armaduraDeIronManDestruida]

universoConPersonajeMuerto = universo_con [spiderManMuerto, ironMan] []

universoObjetosSinDueño = universo_con [capitanAmerica] [tablaDePicada]

universoUnObjetoConDueño = universo_con [capitanAmerica] [escudoCapitanAmerica]

universoUnObjetoPeroDestruido = universo_con [capitanAmerica] [escudoCapitanAmericaDestruido]

universoConPersonajeMuertoConObjetos = universo_con [hulkMuerto, ironMan] [crocsDeHulkTomadosPorHulk, lanzallamasTomadoPorHulk]

universonConObjetosDePersonajeDestruidosYNo = universo_con [spiderMan, ironMan, hulk] [mascaraDeSpiderManTomadaPorSpiderMan, crocsDeHulkDestruidosTomadoPorHulk, armaduraDeIronManDestruidaTomadaPorHulk, rayoLaserTomadoPorIronMan, paloDeAmasarDestruidoTomadoPorIronMan, lanzallamasTomadoPorIronMan]

universoVariosObjetosConDueño = universo_con [capitanAmerica, spiderMan] [escudoCapitanAmerica, objeto1Tomado, tablaDePicada, objeto2Tomado]

universoVariosObjetosConDueñoYDestruidos = universo_con [capitanAmerica, spiderMan] [escudoCapitanAmerica, objeto1Tomado, objeto3Destruido, tablaDePicada, objeto2Tomado]

universoConObjetosAMismaDistancia = universo_con [ironMan] [paloDeAmasar, rayoLaser]

universoConObjetosAMismaDistanciaPeroUnoDestruido = universo_con [ironMan] [paloDeAmasarDestruido, rayoLaser]

universoConObjetosAMismaDistanciaUnoDestruidoOtroTomadoTomaEl3ro = universo_con [ironMan] [paloDeAmasarDestruido, rayoLaserTomadoPorHulk, armaduraDeIronMan]

universoSinObjetosPeroConPersonajes = universo_con [ironMan, hulk, spiderMan] []

universoPeroNoSirveNingunObjetoCercano = universo_con [ironMan, spiderMan] [armaduraDeIronManDestruidaTomadaPorHulk, crocsDeHulkDestruidos, paloDeAmasarDestruido, escudoCapitanAmericaTomadoPorSpiderMan]

universoConObjetosADistintasDistancias = universo_con [ironMan, hulk] [paloDeAmasar, escudoCapitanAmerica, crocsDeHulk, lanzallamas]

universoThanosTieneMenosDe6GemasYnoEstanLas6 = universo_con [thanos, thor] [gemaMenteEnPoderDeThanos, gemaAlmaEnPoderDeThanos, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, gemaRealidadEnPoderDeThanos]

universoThanosTieneMenosDe6GemasYUnaLaTieneVision = universo_con [thanos, thor] [gemaAlmaEnPoderDeThanos, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, gemaRealidadEnPoderDeThanos, gemaTiempoEnPoderDeThanos, gemaMenteEnPoderVision]

universoThanosTiene6GemasPeroUnasDestruidas = universo_con [thanos, thor] [gemaMenteDestruidaEnPoderDeThanos, gemaAlmaDestruidaEnPoderDeThanos, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, gemaRealidadEnPoderDeThanos, gemaTiempoEnPoderDeThanos]

universoThanosTiene6GemasEnBuenEstado = universo_con [thanos, thor] [gemaMenteEnPoderDeThanos, gemaAlmaEnPoderDeThanos, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, gemaRealidadEnPoderDeThanos, gemaTiempoEnPoderDeThanos]

universoThanosTieneMenosDe6GemasPeroSiEstanTodas = universo_con [thanos, thor] [gemaMenteEnPoderDeThanos, gemaAlmaEnPoderDeThanos, gemaEspacio, gemaPoder, gemaRealidad, gemaTiempo]
 
casoThanosTieneTodasLasGemas = universo_con [thanos, wanda, thor, spiderMan, vision, ironMan] [gemaMenteEnPoderDeThanos, gemaAlmaEnPoderDeThanos, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, gemaTiempoEnPoderDeThanos, gemaRealidadEnPoderDeThanos]

casoEstaThorYStormBreakerPeroNoEnPosesión = universo_con [thanos, wanda, spiderMan, thor] [stormbreaker, gemaMente, gemaAlma, gemaPoder, gemaEspacio, gemaRealidad, gemaTiempo]

casoEstaThorYStormBreakerEnPosesionThor = universo_con [thanos, wanda, spiderMan, thor] [stormbreakerEnPoderDeThor, gemaMente, gemaAlma, gemaPoder, gemaEspacio, gemaRealidad, gemaTiempo]

casoEstaThorPeroNoStormBeaker = universo_con [ironMan, wanda, vision, thor, thanos] [gemaAlmaDestruida, gemaMente, gemaPoder]

casoPodemosGanarlePeroNoVinoThanos = universo_con [wanda, spiderMan, thor] [stormbreakerEnPoderDeThor, gemaMente, gemaAlma, gemaPoder, gemaEspacio, gemaRealidad, gemaTiempo]

casoEstaWandaPeroNoVision = universo_con [wanda, spiderMan, thor, thanos] [gemaAlmaEnPoderDeThanos, armaduraDeIronMan]

casoVisionTieneGemaPeroNoEstaWanda = universo_con [vision, thor, thanos, hulk] [gemaMenteEnPoderVision, gemaEspacioEnPoderDeThanos, gemaPoderEnPoderDeThanos, armaduraDeIronMan]

casoEstaWandaEstaVisionPeroNoGema = universo_con [vision, wanda, thanos, spiderManMuerto] [gemaTiempoEnPoderDeThanos, gemaRealidadEnPoderDeThanos, gemaAlmaEnPoderDeThanos]

casoEstaWandaEstaVisionPeroGemaMenteLaTieneThanos = universo_con [wanda, vision, hulk, thanos] [gemaMenteEnPoderDeThanos, gemaAlmaDestruidaEnPoderDeThanos, mascaraDeSpiderMan]

casoEstaWandaEstaVisionPeroGemaMenteSeRompio = universo_con [wanda, vision, hulk, thanos] [gemaMenteDestruida, gemaAlmaDestruidaEnPoderDeThanos, mascaraDeSpiderMan]

casoStormBreakerPeroThorMuerto = universo_con [wanda, vision, thanos, thorMuerto] [stormbreaker]

casoSeDaTodoParaGanar = universo_con [wanda, vision, thor, hulk, thanos] [gemaMenteEnPoderVision, stormbreakerEnPoderDeThor, gemaPoderEnPoderDeThanos]

casoSeDaTodoParaGanarPeroThanosEstaMuerto = universo_con [wanda, vision, thor, hulk, thanosMuerto] [gemaMenteEnPoderVision, stormbreakerEnPoderDeThor, gemaPoderEnPoderDeThanos]
