#include <iostream>
#include <vector>
#include <numeric> // Necesario para accumulate

using namespace std;

//dado un conjunto de elementos C, queremos ver si hay algún posible subconjunto cuya suma sea igual a k
//C = el conjunto, implementado como vector
//j = el elemento que estamos viendo ahora mismo y decidiremos si elegir o no incluir en el subconjunto
//suma_potencial = suponiendo que elijamos SI en todos los elementos que no hemos visto, cuánto obtendríamos
bool maxi_subconjunto(vector<int>& C, int j, int k, int suma_potencial) {

    if (k == 0) {
        return true;
    } else if (j < 0) { //ya vimos todos los elementos, si la suma fuera buena, hubieramos caído en la primer guarda
        return false;
    } else if (k < 0) {
        return false;
        //el menor < es una poda, pues si me paso de largo, está claro que seguir sumando elementos
        //no me hará llegar a la solución. Poda por factibilidad.
    } else if (k - suma_potencial > 0) {
        //si ni con la suma potencial llegamos a obtener una solución, hay que hacer backtracking
        return false; //nos quedamos cortos
    } else {
        //estamos parados en el jésimo elemento del conjunto, debemos decidir si tener en considerción o no
        //CASO 1, NO lo tenemos en consideración: entonces la suma k permanece igual, pero pasamos a ver
        //al j-1ésimo elemento. Como no lo elegimos, la suma potencial se reduce
        //CASO 2, SI lo tenemos en consideración: entonces la suma k a alcanzar se reduce,
        //igualmente nos movemos al j-1ésimo elemento. Otra vez, la suma potencial se reduce
        bool noAgregarJ = maxi_subconjunto(C, j-1, k, suma_potencial-C[j]);
        bool siAgregarJ = maxi_subconjunto(C, j-1, k-C[j], suma_potencial-C[j]);
        return noAgregarJ || siAgregarJ;
    }

}

int main() {

    // Caso 1
    vector<int> C1 = {2, 5, 7, 3, 11};
    int suma1 = 2+5+7+3+11;
    int len1 = C1.size(); 
    int k1 = 15;

    cout << "Caso 1: " << endl;
    bool res1 = maxi_subconjunto(C1, len1 - 1, k1, suma1); //debería ser true
    cout << res1 << endl;

    // Caso 2
    vector<int> C2 = {1, 3, 6, 10};
    int suma2 = 1+3+6+10;
    int len2 = C2.size(); 
    int k2 = 7;

    cout << "\nCaso 2: " << endl;
    bool res2 = maxi_subconjunto(C2, len2 - 1, k2, suma2); //debería ser true
    cout << res2 << endl;

    // Caso 3
    vector<int> C3 = {4, 8, 12};
    int suma3 = 4+8+12;
    int len3 = C3.size(); 
    int k3 = 7;

    cout << "\nCaso 3: " << endl;
    bool res3 = maxi_subconjunto(C3, len3 - 1, k3, suma3); //debería ser false
    cout << res3 << endl;

    return 0;
}