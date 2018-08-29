#include <nlohmann/json.hpp>
#include <vector>
#include <map>
#include <iostream>

using namespace std;
using json = nlohmann::json;
using realMatrix = vector<vector<double>>;

struct restriction {
    map<char, double> function;
    string type;
    double value;
};

map<char, double> createFunction(json fn) {
    map<char, double> objectiveFunction;
    for (json::iterator item = fn.begin(); item != fn.end(); ++item)
        objectiveFunction[item.key()[0]] = item.value();

    return objectiveFunction;
}

restriction createRestriction(json jrestriction) {
    restriction r;

    r.type = jrestriction["type"];
    r.value = jrestriction["val"];

    auto fn = jrestriction["fn"];

    map<char, double> objectiveFunction;
    for (json::iterator item = fn.begin(); item != fn.end(); ++item)
        objectiveFunction[item.key()[0]] = item.value();

    r.function = objectiveFunction;
    return r;
}

void showInfo(realMatrix data) {
    for (auto row : data) {
        for (auto element : row) printf("%+10.4f ", element);
        printf("\n");
    }
}


realMatrix simplex(realMatrix Data) {

    for (size_t i = 0; i < Data.size(); i++) {
        for (size_t j = 0; j < Data[i].size(); j++) {
            Data[i][j] = 3;
        } 
    }
    

    return Data;
}


int main () {

    json simplexData;
    cin >> simplexData;

    vector<char> names;
    for (string name : simplexData["variables"]["names"]) names.push_back(name[0]);
    
    bool isMaxFunction = simplexData["objectiveFunction"]["type"] == "max";
    auto objectiveFunction = createFunction(simplexData["objectiveFunction"]["fn"]);

    vector<restriction> restrictions(simplexData["restrictions"].size());
    
    for (size_t i = 0; i < restrictions.size(); i++) {
        restrictions[i] = createRestriction(simplexData["restrictions"][i]);
    }

    for (auto x : names) cout << x << " ";
    cout << "\n";


    realMatrix tableu;

    realMatrix data = {{1, 2, 3}, {4, 5, 6}};
    auto x = simplex(data);

    x[0][0] = 130.343;
    showInfo(x);

    return 0;
}