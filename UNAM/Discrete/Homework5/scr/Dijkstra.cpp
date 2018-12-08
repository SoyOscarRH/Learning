#include <vector>
#include <set>
#include <iostream>

using namespace std;
#define INF 0x3f3f3f3f

struct DijkstraResult { vector<int> minDistanceFromSourceTo; vector<int> previousVertex;};

DijkstraResult Dijkstra(const vector < vector< pair<int,int> > >& graph, int source, int numberOfVertexs) {
    vector<int> minDistanceFromSourceTo(numberOfVertexs, INF);
    vector<int> previousVertex(numberOfVertexs, -1);

    minDistanceFromSourceTo[source] = 0;
    previousVertex[source] = source;
    
    set< pair<int,int> > distancesFromSource;
    distancesFromSource.insert({0, source});

    while (distancesFromSource.empty() == false) {
        auto top = distancesFromSource.begin();
        distancesFromSource.erase(top);
        int u = top->second;

        for (auto next: graph[u])    {
            int v = next.first, weight = next.second;

            if (minDistanceFromSourceTo[v] > minDistanceFromSourceTo[u] + weight) {
                auto previous = distancesFromSource.find( {minDistanceFromSourceTo[v], v});
                if (previous != distancesFromSource.end()) distancesFromSource.erase(previous);
                
                minDistanceFromSourceTo[v] = minDistanceFromSourceTo[u] + weight;
                distancesFromSource.insert({minDistanceFromSourceTo[v], v});
                previousVertex[v] = u;
            }
        }
    }

    return {minDistanceFromSourceTo, previousVertex};
}

int main() {
    int numberOfVertexs, numberOfEdges, source;
    cin >> numberOfVertexs >> numberOfEdges >> source; 


    int u, v, weight;
    vector < vector< pair<int,int> > > graph(numberOfVertexs);

    for (int i = 0; i < numberOfEdges; i++) {
        //Input the starting vertex of the edge, the ending vertex and the cost of the edge.
        cin >> u >> v >> weight; 
        graph[u].push_back({v, weight});
        graph[v].push_back({u, weight});
    }

    auto result = Dijkstra(graph, source, numberOfVertexs); 
    auto minDistanceFromSourceTo = result.minDistanceFromSourceTo;
    auto previousVertex = result.previousVertex;
 
    for (int vertex = 0; vertex < numberOfVertexs; vertex++) {
        cout << "min distance to " << char('a' + vertex) << ": " << minDistanceFromSourceTo[vertex] << endl;

        int currentVertex = vertex;
        while (currentVertex != source) {
            cout << char('a' + currentVertex) << " <- ";
            currentVertex = previousVertex[currentVertex];
        }

        cout << char('a' + source) << endl;
    }
    
}