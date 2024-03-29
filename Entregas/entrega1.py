#defino las clases jugador y formacion por comodidad, para que no quede horrible la función

class Jugador:
  
  def __init__(self, nombre, ataque, defensa):
    self.nombre = nombre
    self.ataque = ataque
    self.defensa = defensa

  def get_nombre(self):
    return self.nombre

  def get_ataque(self):
    return self.ataque

  def get_defensa(self):
    return self.defensa
  
class Formacion:

  def __init__(self):
    self.jugadores = []
    self.nombres = []
    self.ataque = -1
    self.defensa = -1
    self.longitud = 0 #cant de jugadores que tiene esta formación elegidos, siempre coincide con k, podría incluso quitarlo

  def full_update(self, otra_form):
    self.jugadores = otra_form.get_jugadores().copy() #es un arreglo, tengo que copiarlo para que no hayan problemas de referencia con otras llamadas recursivas
    self.nombres = otra_form.get_nombres().copy()
    self.ataque = otra_form.get_ataque()
    self.defensa = otra_form.get_defensa()
    self.longitud = len(self.jugadores) #será igual a 10, pero lo dejo así nomás

  def get_jugadores(self):
    return self.jugadores

  def get_nombres(self):
    return self.nombres

  def get_ataque(self):
    return self.ataque

  def get_defensa(self):
    return self.defensa

  def add_jugador(self, jugador):
    self.jugadores.append(jugador)
    self.nombres.append(jugador.get_nombre())
    self.longitud += 1

    if self.longitud <= 5: #si esto pasa, es que este jugador es de ataque
       self.ataque += jugador.get_ataque()
    else: #es que es defensa
       self.defensa += jugador.get_defensa()

  def pop_jugador(self):
    self.nombres.pop()
    jugador_quitado = self.jugadores.pop() #tomo el jugador que quiero quitar
    
    if self.longitud <= 5: #estoy quitando a un atacante
       self.ataque -= jugador_quitado.get_ataque()
    else: #estoy quitando a un defensa
       self.defensa -= jugador_quitado.get_defensa()

    self.longitud -= 1 #quitamos a uno 


#consideremos 5 parámetos
#J = un arreglo base con todos los jugadores que nos dieron
#C = un bitmask que nos dice si ya hemos elegido o no al jugador en la iésima posición de J para nuestra formación
#k = la cantidad de jugadores de nuestra formacióna actual
#actual_form = la formacióna actual (tendrá una lista permutación de J)
#best_form = la mejor formación encontrada hasta ahora

def mejor_formacion(J, C, k, actual_form, best_form):

  n = len(J) #será 10, pero para seguir la onda de las guías lo pongo como una variable genérica
  if k == n: #si nuestra formación actual ya tiene a los 10 jugadores, tendremos que ver si es mejor que la best_form o no
     if mejor(actual_form, best_form):
        best_form.full_update(actual_form)
  elif k == 5 and actual_form.get_ataque() < best_form.get_ataque(): #si ya elegimos a los atacantes y perdimos, da igual lo que siga
    return #poda por optimalidad
  elif k == 5 and actual_form.get_ataque() >= best_form.get_ataque(): #si ya elegí a todos los atacantes y no perimos, quizá gane por defensa
       for j in range(0, n):
        if C[j] == 0: #todos los que no fueron elegidos deben ser defensa de a fuerzas
          actual_form.add_jugador(J[j])
       mejor_formacion(J, C, n, actual_form, best_form)
       
       for i in range(0, 5): #quito a los que añadí
           actual_form.pop_jugador()
  else:
    
    #en este caso nuestra lista está incompleta, tenemos que meter más jugadores todavía
    for j in range(0, n): #vemos todos los que hay
      if C[j] == 0: #para cada uno de aquellos que SI está disponible, tenemos que probar esa combinación
         C[j] = 1 #le digo al bitmask que lo he elegido
         actual_form.add_jugador(J[j]) #lo agrego a la permutación actual
         mejor_formacion(J, C, k+1, actual_form, best_form) #sigo agregando los jugadores que faltan, me actualizará best_form
         C[j] = 0 #luego de esta permutación, debe quedar libre
         actual_form.pop_jugador() #lo quito de la formación, y que la próxima iteración del for tenga disponibles todos los que se supone que le corresponden en este nivel
   
#ESTA (la 2) ES LA SOLUCION POSTA, la que pasó el Vjudge (la otra tardaba más de 1s, esta solo 150ms)
#me di cuenta tarde que no hace falta generar las n! permutaciones de los 10 jugadores y meterle una poda, si no meramente
#los subconjuntos de 5 jugadores de los 10, pues si los asignamos a esos como atacantes inmediatamente
#el resto son defensores, acá la cantidad de hojas es el combinatorio 10 con 5 = 252, mientras que allá es
#10x9x8x7x6 = 30240 debido a la poda que hacen la segunda y tercer guarda. Es enorme la diferencia


#jesimo_jugador es el indice de J (o C, según se vea) del jugador en el que estoy parado y decido
#si va a ser delantero o no, el resto es como con mejor_formacion a secas
def mejor_formacion2(J, C, k, jesimo_jugador, actual_form, best_form):
  
  n = len(C) #cant de jugadores en total
  if jesimo_jugador == n and k == n: #si nuestra formación actual ya tiene a los 10 jugadores, tendremos que ver si es mejor que la best_form o no
     if mejor(actual_form, best_form):
        best_form.full_update(actual_form)
  elif k == 5 and actual_form.get_ataque() < best_form.get_ataque(): #si ya elegimos los atacantes y no le ganamos a la mejor f, no vale la pena seguir
    return
  elif k == 5 and actual_form.get_ataque() >= best_form.get_ataque(): #si ya elegí a todos los atacantes y no perdí, quizá gane por defensa, se asignan automáticamente los que faltan
       
       for j in range(0, n):
        if C[j] == 0: #todos los que no fueron elegidos deben ser defensa de a fuerzas
          actual_form.add_jugador(J[j])

       mejor_formacion2(J, C, n, n, actual_form, best_form) #va a caer en la primer guarda y chequear si es posible ganarle al mejor
       
       for i in range(0, 5): #quito a los que añadí
           actual_form.pop_jugador()

  elif jesimo_jugador == n and k != n: #si caigo acá, es porque k != 5 pero vi todo, o sea que elegí a 4 o menos, no se analiza ese caso
    return
  else:
    #en este caso nuestra lista está incompleta, tenemos que meter más jugadores todavía
    #en un caso no agrego el j-ésimo elemento, paso al siguiente
    mejor_formacion2(J, C, k, jesimo_jugador+1, actual_form, best_form)
    #en otro sí lo agrego
    C[jesimo_jugador] = 1 #estás en uso
    actual_form.add_jugador(J[jesimo_jugador])
    mejor_formacion2(J, C, k+1, jesimo_jugador+1, actual_form, best_form)
    C[jesimo_jugador] = 0 #estás libre
    actual_form.pop_jugador() #y luego lo quito para que no haya problema cuando se retroceda entre nodos
    

def mejor(actual_f, best_f):

  ataque_act = actual_f.get_ataque()
  ataque_bst = best_f.get_ataque()
  defensa_act = actual_f.get_defensa()
  defensa_bst = best_f.get_defensa()

  if ataque_act > ataque_bst: #caso en el que le gana por tener mejor ataque
    return True
  elif ataque_act == ataque_bst and defensa_act > defensa_bst : #empata en ataque pero le gana en defensa
    return True
  elif ataque_act == ataque_bst and defensa_act == defensa_bst and gana_por_nombres(actual_f, best_f): #empata en todo pero le gana por orden lexicografico
    return True
  else: #no gana
    return False

def gana_por_nombres(actual_f, best_f):
  nombres_actual = actual_f.get_nombres()
  nombres_mejor = best_f.get_nombres()

  for i in range(5): #nos dice que sólo vale para los atacantes, por lo que los defensores debería dar igual
    
    i_nombre_actual = nombres_actual[i]
    i_nombre_mejor = nombres_mejor[i]

    if i_nombre_actual < i_nombre_mejor: #el de la form actual está antes lexicograficamente que el de la mejor
      return True #le gana
    elif i_nombre_actual > i_nombre_mejor: #el de la form actual está después 
      return False

  
  return False #si llego acá, es porque todos lo nombres eran iguales, devuelvo False para que no se actualice best_f
    

def printear_formacion(formacion):

  nombres_atacantes = formacion.get_nombres()[0:5]
  nombres_defensores = formacion.get_nombres()[5:10]

  nombres_atacantes.sort()
  nombres_defensores.sort()

  text = "("

  for i in range(0, 4):
    text += nombres_atacantes[i] +", "
  
  text += nombres_atacantes[4] + ")\n"
  

  text += "("

  for i in range(0, 4):
    text += nombres_defensores[i]+", "

  text += nombres_defensores[4] + ")"

  print(text)

if __name__ == "__main__":
    
    T = int(input()) #test cases
    jugadores_global = []

    for i in range(0, T):
      
      jugadores_por_caso = []

      for j in range(0, 10): #los jugadores a agarrar
        nombre, ataque, defensa = input().split()
        ataque = int(ataque)
        defensa = int(defensa)
        jugador = Jugador(nombre, ataque, defensa)
        jugadores_por_caso.append(jugador)
      
      jugadores_global.append(jugadores_por_caso)

      #actual_form = Formacion()
      #best_form = Formacion()
      #mejor_formacion(jugadores_por_caso, C, 0, actual_form, best_form)
      #print(f"Case {i+1}:")
      #printear_formacion(best_form)

      
    
    for i in range(0, T):
      J = jugadores_global[i] 
      C = [0 for _ in range(10)]
      actual_form = Formacion()
      best_form = Formacion()
      mejor_formacion2(J, C, 0, 0, actual_form, best_form)
      print(f"Case {i+1}:")
      printear_formacion(best_form)

