#include <vector>
#include <iostream>

using namespace std;

/*
Básicamente nos están diciendo que vamos a tener una matriz de ceros y enteros positivos,
y que comenzando en una de las celdas de arriba (o abajo, según armemos la matriz),
vamos a poder movernos abajo, derecha abajo o izquierda abajo (siempre que no nos caigamos) a
un ritmo que ellos nos digan. Queremos saber cuántas "bellotas" nos permite obtener el camino
que más tiene.
IDEA Backtracking: básicamente generamos todos los posibles caminos, es un for con llamadas
recursivas. Arriba de todo tenemos t arboles a los que elegir, cada uno de altura h.
Para cada uno, calculamos recursivamente cuánto es la máxima suma que nos van a dar
hacer todos los posibles movimientos que se nos permiten

Nota: debería dar lo mismo ir desde arriba hasta abajo que desde abajo hasta arriba para encontrar
el mejor camino

Llamemos

M: la matiz del arbol, la fila 0 es el nivel 0 y las columnas son cada uno de los árboles
son las columnas. El arbol 1 es M[i][0], el 2 es M[i][1]...
h: el nivel de los árboles en el que estoy, la fila
t: el árbol en el que estoy, las columnas.
f: el ratio con el que voy cayendo, cuántas filas bajo por movimiento
k: las bellotas acumuladas que tengo
*/

int bw_bt(vector<vector<int>>& M, int h, int t, int f, int k) {

    int m = M[0].size(); //cant de arboles
    int res = k;

    if (h <= 0) { //voy a tener n+1 filas para no confundirme con los índices nomás
        return res; //si estoy en el subsuelo no hay chance de agarrar más bellotas
    }
    else {

        int k_act = k + M[h][t]; //me sumo las bellotas que hayan acá

        //voy abajo nomá, acá siempre se baja 1
        res = bw_bt(M, h-1, t, f, k_act); //si me cayera daría 0
        
        //veo todas las posibilidades a la izquierda
        int i = 1;
        while (t-i >= 0) {
            res = max(res, bw_bt(M, h-f, t-i, f, k_act));
            i++;
        }
        
        //veo todas las posibilidades a la derecha
        i = 1;
        while (t+i < m) {
            res = max(res, bw_bt(M, h-f, t+i, f, k_act));
            i++;
        }

    }

    return res;

};

void imprimirMatriz(const vector<vector<int>>& matriz) {
    for (const vector<int>& fila : matriz) {
        for (int elemento : fila) {
            cout << elemento << " ";
        }
        cout << endl;
    }
}


/*
Acá un top down está regalado, dada una matriz memo = altura * tree en el que estoy parado,
devolver la máxima cantidad de bellotas que podría conseguir. 
*/

/*
Creo que ya sé por qué no funca, y es que mis estados dependen NO solo de h y t donde estoy
parado, si no tambieén del k, la cantidad de bellotas con las que llegué ahí
UPDATE: SI, ERA ESO, MIS ESTADOS ESTABAN SIENDO PERJUDICADOS POR ESE K
OTRA UPDATE: me da limit excedeed aunque pase los debug, voy a tener que sacarme un bottom up de la galera*/
int bw_top_down(vector<vector<int>>& M, int h, int t, int f, vector<vector<int>>& memo) {

    int m = M[0].size(); //cant de arboles
    int res = 0;

    if (h <= 0) { //voy a tener n+1 filas para no confundirme con los índices nomás
        return 0; //si estoy en el subsuelo no hay chance de agarrar más bellotas
    }
    else {

        if (memo[h][t] == -1) {

            res = M[h][t]; // me sumo las bellotas que hayan acá

            // voy abajo nomá, acá siempre se baja 1
            int camino_posible = bw_top_down(M, h - 1, t, f, memo); // si me cayera daría 0
            int mejor_res = camino_posible + res;

            // veo todas las posibilidades a la izquierda
            int i = 1;
            while (t - i >= 0)
            {
                mejor_res = max(mejor_res, res+bw_top_down(M, h - f, t - i, f, memo));
                i++;
            }

            // veo todas las posibilidades a la derecha
            i = 1;
            while (t + i < m)
            {
                mejor_res = max(mejor_res, res+bw_top_down(M, h - f, t + i, f, memo));
                i++;
            }

            memo[h][t] = mejor_res;

        }


    }

    return memo[h][t];

};

/*
IDEA: la última fila válida, la 1, sólo tienen como potencial ganar la cantidad
de bellotas que ellos tienen. Luego, puedo ir subiendo hasta arriba recto,
arriba a la izquierda o arriba a la derecha. Debo tener en cuenta f para eso.
Si en algún momento veo que con f me paso de largo, tengo que de fuerzas subir 
de a 1.
*/

//este sólo modifica un memo, de la primera fila tenemos que quedarnos con la que más
//garpa
int bw_bottom_up(vector<vector<int>>& M, int f, vector<vector<int>>& memo) {

    int n = memo.size();
    int m = memo[0].size();

    if (n > 1) { //sólo en caso de que me pasen unos árboles de altura 0
       for (int i = 0; i < m; i++) {
          memo[1][i] = M[1][i]; //en el nivel 1, lo mejor es quedarse con las bellotas y chau
       }
    }

    //ahora llenamos desde los 2 pies para arriba
    for (int i = 2; i < n; i++) {

        for (int j = 0; j < m; j++) {

            int estas_bellotas = M[i][j];
            
            //ir abajo
            int mejor_res = memo[i-1][j] + estas_bellotas;

            //ir a la derecha;
            if (i - f >= 1) {
                int k = 1;
                while (j+k < m) { //si no me caigo
                    mejor_res = max(mejor_res, estas_bellotas + memo[i-f][j+k]);
                    k++;
                }
            }

            //ir a la izquierda
            if (i - f >= 1) {
                int k = 1;
                while (j-k >= 0) { //si no me caigo
                    mejor_res = max(mejor_res, estas_bellotas + memo[i-f][j-k]);
                    k++;
                }
            }

            memo[i][j] = mejor_res;
            

        }

    }

    int res = 0; //si no tuvieramos nada, no tendríamos bellotas que recoger
    
    //nos quedamos con el camino que da más bellotas en general
    for (int i = 0; i < m; i++) {
        res = max(res, memo[n-1][i]);
    }

    return res;

}


int main() {

    int T; //num test cases

    cin >> T;

    for (int i = 0; i < T; i++) {

        int num_trees;
        cin >> num_trees;
        int height_trees;
        cin >> height_trees;
        int f;
        cin >> f;

        //hago la matriz de este test case, inicialmente puros ceros
        vector<vector<int>> M(height_trees+1, vector<int>(num_trees, 0));
        
        //cada línea es un árbol, de izquierda a derecha según el gráfico que nos dieron
        for (int j = 0; j < num_trees; j++) {

            int cant_bellotas;
            cin >> cant_bellotas;

            for (int k = 0; k < cant_bellotas; k++) {
                //nos dicen en qué fila está cada bellota
                int fila_bellota;
                cin >> fila_bellota;
                M[fila_bellota][j] += 1;
            }

        }

        //hacemos el memo
        vector<vector<int>> memo(height_trees + 1, vector<int>(num_trees, -1));

        //ya tenemos la matriz, a sacar el mejor camino
        int res = bw_bottom_up(M, f, memo);

        //imprimirMatriz(memo);

        cout << res << endl;

    }

    return 0;

}


/*
int main() {

    int T; //num test cases

    cin >> T;

    vector<int> resultados;

    for (int i = 0; i < T; i++) {

        int num_trees;
        cin >> num_trees;
        int height_trees;
        cin >> height_trees;
        int f;
        cin >> f;

        //hago la matriz de este test case, inicialmente puros ceros
        vector<vector<int>> M(height_trees+1, vector<int>(num_trees, 0));
        
        //cada línea es un árbol, de izquierda a derecha según el gráfico que nos dieron
        for (int j = 0; j < num_trees; j++) {

            int cant_bellotas;
            cin >> cant_bellotas;

            for (int k = 0; k < cant_bellotas; k++) {
                //nos dicen en qué fila está cada bellota
                int fila_bellota;
                cin >> fila_bellota;
                M[fila_bellota][j] += 1;
            }

        }

        //hacemos el memo
        vector<vector<int>> memo(height_trees + 1, vector<int>(num_trees, -1));

        //ya tenemos la matriz, a sacar el mejor camino
        int res = bw_bottom_up(M, f, memo);

        resultados.push_back(res);

    }

    for (int i=0; i < resultados.size(); i++) {
        cout << resultados[i] << endl;
    }

    return 0;

}
*/
/*int main() {

    int T; //num test cases

    cin >> T;

    vector<int> resultados;

    for (int i = 0; i < T; i++) {

        int num_trees;
        cin >> num_trees;
        int height_trees;
        cin >> height_trees;
        int f;
        cin >> f;

        //hago la matriz de este test case, inicialmente puros ceros
        vector<vector<int>> M(height_trees+1, vector<int>(num_trees, 0));
        
        //cada línea es un árbol, de izquierda a derecha según el gráfico que nos dieron
        for (int j = 0; j < num_trees; j++) {

            int cant_bellotas;
            cin >> cant_bellotas;

            for (int k = 0; k < cant_bellotas; k++) {
                //nos dicen en qué fila está cada bellota
                int fila_bellota;
                cin >> fila_bellota;
                M[fila_bellota][j] += 1;
            }

        }

        //hacemos el memo
        vector<vector<int>> memo(height_trees + 1, vector<int>(num_trees, -1));

        //ya tenemos la matriz, a sacar el mejor camino
        int res = 0;

        //el primer arbol a tocar puede ser cualquiera, darán distintos res
        for (int tree = 0; tree < num_trees; tree++) {
            res = max(res, bw_top_down(M, height_trees, tree, f, memo));
        }

        //imprimirMatriz(memo);

        //cout << res << endl;

        resultados.push_back(res);

    }

    for (int i=0; i < resultados.size(); i++) {
        cout << resultados[i] << endl;
    }

    return 0;
}
*/
/*
int main() {

    int T; //num test cases

    cin >> T;

    for (int i = 0; i < T; i++) {

        int num_trees;
        cin >> num_trees;
        int height_trees;
        cin >> height_trees;
        int f;
        cin >> f;

        //hago la matriz de este test case, inicialmente puros ceros
        vector<vector<int>> M(height_trees+1, vector<int>(num_trees, 0));
        
        //cada línea es un árbol, de izquierda a derecha según el gráfico que nos dieron
        for (int j = 0; j < num_trees; j++) {

            int cant_bellotas;
            cin >> cant_bellotas;

            for (int k = 0; k < cant_bellotas; k++) {
                //nos dicen en qué fila está cada bellota
                int fila_bellota;
                cin >> fila_bellota;
                M[fila_bellota][j] += 1;
            }

        }

        //hacemos el memo
        vector<vector<int>> memo(height_trees + 1, vector<int>(num_trees, -1));

        //ya tenemos la matriz, a sacar el mejor camino
        int res = 0;

        //el primer arbol a tocar puede ser cualquiera, darán distintos res
        for (int tree = 0; tree < num_trees; tree++) {
            res = max(res, bw_top_down(M, height_trees, tree, f, memo));
        }

        //imprimirMatriz(memo);

        cout << res << endl;

    }

    return 0;
}
*/

/*
int main() {

    int T; //num test cases

    cin >> T;

    for (int i = 0; i < T; i++) {

        int num_trees;
        cin >> num_trees;
        int height_trees;
        cin >> height_trees;
        int f;
        cin >> f;

        //hago la matriz de este test case, inicialmente puros ceros
        vector<vector<int>> M(height_trees+1, vector<int>(num_trees, 0));
        
        //cada línea es un árbol, de izquierda a derecha según el gráfico que nos dieron
        for (int j = 0; j < num_trees; j++) {

            int cant_bellotas;
            cin >> cant_bellotas;

            for (int k = 0; k < cant_bellotas; k++) {
                //nos dicen en qué fila está cada bellota
                int fila_bellota;
                cin >> fila_bellota;
                M[fila_bellota][j] += 1;
            }

        }

        //ya tenemos la matriz, a sacar el mejor camino
        int res = 0;

        //el primer arbol a tocar puede ser cualquiera, darán distintos res
        for (int tree = 0; tree < num_trees; tree++) {
            res = max(res, bw_bt(M, height_trees, tree, f, 0));
        }


        cout << res << endl;

    }

    int zero;

    cin >> zero; //por algún motivo se ingresa un cero, ni idea

    return 0;
}*/
