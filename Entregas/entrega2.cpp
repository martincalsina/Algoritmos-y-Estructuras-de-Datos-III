#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

/*
IDEA: si tenemos un vector E de edificios E_1, E_2, ..., E_N y llamamos
E-1 a E_1, E_1, ..., E_N-1 (el vector sin el último elemento),
=> la subsecuencia ascendente/descendente de E será la subsecuencia ascendente/descendente más larga de E-1
+ [E_N] contemplando tres casos:
1. Si E_N > E_N-1 de la subsecuencia ascendente más larga de E_N-1, entonces lo debemos agregar
2. Si E_N < E_N-1 de la subsecuencia descendente más larga de E_N-1, entonces lo debemos agregar
3. Si no ocurre ninguna de esas, es que E_N = E_N-1 en altura. Nos quedamos con el de mayor ancho (
    al menos así parece que se está haciendo en los outputs de ejemplo:
    80 80 80
    15 25 20
    Devuelve 25 tanto para ascendente como descendente
    30 20 20 10
    20 30 40 50
    Devuelve 110 para la descendente, o sea que el width del primer 20 lo ignora y se queda con el segundo
    Tiene sentido, pues es la MAS LARGA (basado en ancho)
)
*/

class Edificio {

    private: 
        int width;
        int height;
    
    public:

        Edificio(int w, int h) : width(w), height(h) {

        }

        int get_width() const { 
            return this->width;
        }

        int get_height() const {
            return this->height;
        }


};

class Subsecuencia {

    private:
        vector<Edificio> edificios;
        int total_width;
    public:

        Subsecuencia() {
            this->total_width = 0;
        }
        
        Subsecuencia(vector<Edificio>& ed) : edificios(ed) {
            this->total_width = 0;
        }

        void actualizar(Subsecuencia& actual_ls) {
            vector<Edificio> nuevos_edificios = actual_ls.get_edificios();
            this->edificios.assign(nuevos_edificios.begin(), nuevos_edificios.end());
            this->total_width = actual_ls.get_total_width();
        }

        vector<Edificio> get_edificios() { 
            return this->edificios; 
        }

        int get_total_width() { 
            return this->total_width;
        }

        void push_edificio(Edificio& ed) {
            this->total_width += ed.get_width();
            this->edificios.push_back(ed);
        }

        void pop_edificio() {
            Edificio ultimo_ed = this->edificios[this->edificios.size() - 1];
            this->total_width -= ultimo_ed.get_width();
            this->edificios.pop_back();
        }

};


//checkeo que sea estrictamente ascendente en altura
bool es_ascendente(Subsecuencia& actual_ls) {

    vector<Edificio> edificios = actual_ls.get_edificios();
    int n = edificios.size();

    if (n < 2) {
        return true;
    }
    else if (n == 2) {
        return edificios[n-1].get_height() > edificios[n-2].get_height();
    } else {
        return 
        edificios[n-1].get_height() > edificios[n-2].get_height() && 
        edificios[n-2].get_height() > edificios[n-3].get_height();
    }

    /*
    for (int i = 1; i < n; i++) {
        if (edificios[i].get_height() <= edificios[i-1].get_height()) {
            return false;
        }
    }

    return true;
    */
}

//checkeo que sea estrictamente descendente en altura
bool es_descendente(Subsecuencia& actual_ls) {

    vector<Edificio> edificios = actual_ls.get_edificios();
    int n = edificios.size();

    if (n < 2) {
        return true;
    }
    else if (n == 2) {
        return edificios[n-1].get_height() < edificios[n-2].get_height();
    } else {
        return 
        edificios[n-1].get_height() < edificios[n-2].get_height() && 
        edificios[n-2].get_height() < edificios[n-3].get_height();
    }
    
    /*
    for (int i = 1; i < n; i++) {
        if (edificios[i].get_height() >= edificios[i-1].get_height()) {
            return false;
        }
    }
    

    return true;
    */
}

bool es_mas_grande(Subsecuencia& actual_ls, Subsecuencia& otra_ls) {
    return actual_ls.get_total_width() > otra_ls.get_total_width();
}

/*
Vamos a armar todas las subsecuencias ascendentes y descendentes, para luego quedarnos con las más largas
E: vector con los objetos Edificio iniciales
j: índice del iésimo edificio
actual_ls: la subsecuencia con la que estamos trabajando ahora
asc_ls: vector con la subsecuencia ascendente más larga
desc_ls: vector con la subsecuencia descedente más larga
*/

/*
OJO AL PIOJO
asc_ls y desc_ls se están construyendo "al revés"
por ejemplo, con el input
10 100 50 30 80 10
50 10 10 15 20 10
1era llamada, j = 6
tenemos que hacer los dos casos, tomar en cuenta o no al (h, w) = (10, 10)
LO TOMO EN CUENTA
actual_ls = [(10,10)]
2da llamada, j=5
veo al [(80, 20)]
tras el push_back()
eso me da [(10,10), (80, 20)]
¿Qué significa esto? Que estamos viendo de "izquierda a derecha" en vez de
"derecha a izquierda" como dice la consigna.
Entonces, algo creciente de "izquierda a derecha" es algo decreciente de 
"izquierda a derecha". No afecta al algoritmo como tal, si no al printeo del output
Pues esto significa que vamos a tener que tomar al revés los resultados.
Es decir, temp = asc_ls;
          asc_ls = desc_ls;
          desc_ls = temp;
por así decirlo (en verdad no vamos a cambiar las variables, pero es para tenerlo presente)
*/

void ls_bt(vector<Edificio>& E, int j, Subsecuencia& actual_ls, Subsecuencia& asc_ls, Subsecuencia& desc_ls) {

    //ya vimos todos los edificios
    if (j == -1) {
        return;
    }
    //si no cumple al menos una de estas condiciones, no vale la pena seguir
    else if (!es_ascendente(actual_ls) && !es_descendente(actual_ls)) {
        return; //poda
    }
    else {

        /*CASO EN EL QUE TOMAMOS EN CUENTA ESTE EDIFICIO*/
        //lo agregamos al final
        actual_ls.push_edificio(E[j]);
        ls_bt(E, j-1, actual_ls, asc_ls, desc_ls); //vemos si se actualiza actual_ls
        
        //actualizamos asc_ls y desc_ls si corresponde
        if (es_ascendente(actual_ls) && es_mas_grande(actual_ls, asc_ls)) {
            asc_ls.actualizar(actual_ls);
        }
        if (es_descendente(actual_ls) && es_mas_grande(actual_ls, desc_ls)) {
            desc_ls.actualizar(actual_ls);
        }

        actual_ls.pop_edificio(); //lo quiteamos

        /*CASO EN EL QUE NO TENEMOS EN CUENTA A ESTE EDIFICIO*/
        ls_bt(E, j-1, actual_ls, asc_ls, desc_ls);

        if (es_ascendente(actual_ls) && es_mas_grande(actual_ls, asc_ls)) {
            asc_ls.actualizar(actual_ls);
        }
        if (es_descendente(actual_ls) && es_mas_grande(actual_ls, desc_ls)) {
            desc_ls.actualizar(actual_ls);
        }

    }

    return;

}

/*
Ahora bien, lo de arriba funciona, pero no me piden devolver las subsecuencias correspondientes,
por lo que debería poder ser capaz de prescindir de ellas.
Pero además, no es necesario buscar a la vez la secuencia decreciente y creciente más
grandes en la misma función.
Como vimos antes, ver de izquierda a derecha contra ver de derecha a izquierda
nos van a dar resultados válidos, sólo que opuestos.
IDEA: si sabemos resolver el problema para el caso ascendente, sabemos resolverlo
para el descendente, pues no es más que voltear inicialmene a nuestro vector E.
Es muy similar a lo anterior, pero sin las Subsecuencia y ahora viendo sólo un caso,
buscar la secuencia ascendente de máxima longitud.
Ahora se ve más claro que es algo así como un problema de MaxiSubconjuntos, 
en cada caso debemos tener en cuenta o no a un edificio, pero nuestro criterio
para saber si una decisión es válida (la de agregar, el ignorar siempre lo es) es
si la altura del que estamos viendo es estrictamente mayor que la del anterior.
En ese caso, nuestro subproblema va a tener la longitud de aquella subsecuencia
más la de este edificio.

E: sigue siendo lo mismo que antes
i: es el iésimo edificio que debemos decidir si incluir o no
j: es la altura del último elemento que vimos
memo: una matriz de iesimo-elemento por altura del último elemento visto,
se guarda las width totales
*/

int ls_top_down(vector<Edificio>& E, int i, int j, vector<vector<int>>& memo) {

    int res = 0;
    //ya vimos todos los edificios
    if (i == -1) {
        return 0;
    } else {
        if (memo[i][j] == -1) { //no calculamos este resultado, debemos hacerlo

           if (j < E[i].get_height()) { //si este es menor, podemos buscar la asc_ls
              res = ls_top_down(E, i-1, E[i].get_height(), memo) + E[i].get_width(); //lo tuvimos en cuenta, sumamos su ancho;
           }

           //pero también debemos checkear el caso en que no tenemos en cuenta al iésimo edificio
           res = max(res, ls_top_down(E, i-1, j, memo));

           memo[i][j] = res;
            
        }
    }
    
    return memo[i][j];
    
 
}


void printear_res(int caseNum, Subsecuencia& asc_ls, Subsecuencia& desc_ls) {

    cout << "Case " << caseNum << ". "; //número de caso de prueba
    
    int asc_width = asc_ls.get_total_width();
    int desc_width = desc_ls.get_total_width();

    if (asc_width >= desc_width) {
        cout << "Increasing (" << asc_width << "). ";
        cout << "Decreasing (" << desc_width << ").";
    } else {
        cout << "Decreasing (" << desc_width << "). ";
        cout << "Increasing (" << asc_width << ").";
    }

    cout << endl; // salto de línea al final de cada caso de prueba

}

void printear_res_top_down(int caseNum, int asc_width, int desc_width) {

    cout << "Case " << caseNum << ". "; //número de caso de prueba

    if (asc_width >= desc_width) {
        cout << "Increasing (" << asc_width << "). ";
        cout << "Decreasing (" << desc_width << ").";
    } else {
        cout << "Decreasing (" << desc_width << "). ";
        cout << "Increasing (" << asc_width << ").";
    }

    cout << endl; // salto de línea al final de cada caso de prueba

}


int main() {

    int T; //variable para los casos de prueba
    cin >> T;//

    for (int caseNum = 1; caseNum <= T; ++caseNum) {

        int N; //número de edificios
        cin >> N;

        vector<int> alturas(N);
        vector<int> anchos(N);

        for (int i = 0; i < N; ++i) {
            cin >> alturas[i];
        }

        for (int i = 0; i < N; ++i) {
            cin >> anchos[i];
        }

        vector<Edificio> edificios;
        for (int i = 0; i < N; ++i) {
            edificios.push_back(Edificio(anchos[i], alturas[i]));
        }
        
        /*
        acá ni idea cómo funciona este max_element de C++ que devuelve un iterador,
        pero me permite acceder a la altura máxima de los edificios*/
        auto iterador_max = max_element(alturas.begin(), alturas.end());

        int altura_max = *iterador_max;

        vector<vector<int>> memo(N, vector<int>(altura_max+1, -1));

        int asc_width = ls_top_down(edificios, N-1, 0, memo);
        
        vector<vector<int>> memo_2(N, vector<int>(altura_max+1, -1));
            
        reverse(edificios.begin(), edificios.end());

        int desc_width = ls_top_down(edificios, N-1, 0, memo_2);
        
        /*como antes, debo pasarlo al revés*/
        printear_res_top_down(caseNum, desc_width, asc_width);

    }

    return 0;
}



/*
int main() {

    int T; //variable para los casos de prueba
    cin >> T;//
    vector<vector<Edificio>> cases_inputs;

    for (int k = 1; k <= T; k++) {

        int N; //número de edificios
        cin >> N;

        vector<int> alturas(N);
        vector<int> anchos(N);

        for (int i = 0; i < N; ++i) {
            cin >> alturas[i];
        }

        for (int i = 0; i < N; ++i) {
            cin >> anchos[i];
        }

        vector<Edificio> edificios;
        for (int i = 0; i < N; ++i) {
            edificios.push_back(Edificio(anchos[i], alturas[i]));
        }

        cases_inputs.push_back(edificios);

    }

    //printeo todo junto
    for (int caseNum = 0; caseNum < T; caseNum++) {
        
        Subsecuencia actual_ls = Subsecuencia();
        Subsecuencia asc_ls = Subsecuencia();
        Subsecuencia desc_ls = Subsecuencia();

        vector<Edificio> edificios = cases_inputs[caseNum];
        int N = edificios.size();

        ls_bt(edificios, N-1, actual_ls, asc_ls, desc_ls);

        //las pasams al revés por lo que dijimos de que estamos viendo de
        //"izquierda a derecha" en vez de "derecha a izquierda"
        printear_res(caseNum+1, desc_ls, asc_ls);

    }

    return 0;

}
*/


/*
int main() {

    int T; //variable para los casos de prueba
    cin >> T;//

    for (int caseNum = 1; caseNum <= T; ++caseNum) {

        int N; //número de edificios
        cin >> N;

        vector<int> alturas(N);
        vector<int> anchos(N);

        for (int i = 0; i < N; ++i) {
            cin >> alturas[i];
        }

        for (int i = 0; i < N; ++i) {
            cin >> anchos[i];
        }

        vector<Edificio> edificios;
        for (int i = 0; i < N; ++i) {
            edificios.push_back(Edificio(anchos[i], alturas[i]));
        }

        Subsecuencia actual_ls = Subsecuencia();
        Subsecuencia asc_ls = Subsecuencia();
        Subsecuencia desc_ls = Subsecuencia();

        ls_bt(edificios, N-1, actual_ls, asc_ls, desc_ls);

        //las pasams al revés por lo que dijimos de que estamos viendo de
        //"izquierda a derecha" en vez de "derecha a izquierda"
        printear_res(caseNum, desc_ls, asc_ls);

    }

    return 0;
}
*/