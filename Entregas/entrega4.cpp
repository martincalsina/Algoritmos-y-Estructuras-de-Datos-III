#include <iostream>

using namespace std;

/*
Prácticamente nos están dando el algoritmo servido, sólo hay que agregar que para el caso de longitudes impares

"If strings a and b have odd lengths, they are equivalent if and only if they are equal. "

Agrega codeforces.

Entonces, el algoritmo sería:
- Si son de longitud impar, los strings son equivalentes si y sólo si son iguales.
- Si son de longitud par, los string son equivalente si vale alguna de las siguientes situaciones:
   Divididos en A_1, A_2, B_1 y B_2 de igual longitud, son equivalentes si y sólo si
   - A_1 es equivalente a B_1 y A_2 a B_2
   - o A_1 es equivalente a B_2 y A_2 a B_1 (como el cruzado del anterior)

nos aseguramos de usar índices para trabajr con los substrings y que no se nos vaya la complejidad
por andar haciendo slices. Como se supone que los tamaños son los mismos, ambos strings usarán los mismos
índices.

El caso base está cubierto para strings de longitud 1, pues es impar, sabemos cómo se evalúa eso.
*/

/*
En verdad no hace falta j_end, pero sí i_init, j_init y i_end. El último me va a marcar
cuántas letras voy a tener que recorrer desde i_init y j_init, que no son iguales en el caso en que comparo
A_1 con B_2 y A_2 con B_1, por eso los necesito
*/
bool substrings_iguales(string& sub_A, string& sub_B, int i_init, int i_end, int j_init, int j_end) {
    
    int i = i_init;
    int j = j_init;

    while (i <= i_end) {
        if (sub_A[i] != sub_B[j]) {
            return false;
        }
        i++;
        j++;
    }

    return true;
}

bool eq_str_dq(string& A, string&B, int i_init, int i_end, int j_init, int j_end) {

    int n = i_end - i_init + 1; //length de los substrings, se supone la misma

    if (n % 2 == 1) { //si son de longitud impar tengo sólo hay una manera de chequear la equivalencia
        return substrings_iguales(A, B, i_init, i_end, j_init, j_end); //acá los i deberían ser iguales a los j porque no se hizo división
    }
    
    //si no es impar, hay que dividir
    int i_middle = (n / 2) + i_init;
    int j_middle = (n / 2) + j_init;
    
    //left con left y right con right
    bool caso1 = eq_str_dq(A, B, i_init, i_middle-1, j_init, j_middle-1) && eq_str_dq(A, B, i_middle, i_end, j_middle, j_end);
    
    //me dio time limit excedeed calculando caso1 y caso2 de primeras, pruebo así
    if (caso1) {
        return true;
    }

    //efectivy wonder, funcó así, no calcula caso2 si no hace falta

    //left con right y right con left
    bool caso2 = eq_str_dq(A, B, i_init, i_middle-1, j_middle, j_end) && eq_str_dq(A, B, i_middle, i_end, j_init, j_middle-1);

    if (caso2) {
        return true;
    }

    return false;
}

int main() {


    string A;
    cin >> A;
    string B;
    cin >> B;
    bool res = eq_str_dq(A, B, 0, A.length()-1, 0, B.length()-1);

    if (res) {
        cout << "YES" << endl;
    } else {
        cout << "NO" << endl;
    }
    return 0;
}