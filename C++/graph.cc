#include <iostream>
#include <cassert>

using namespace std;

/*
   Graph interface
    unsigned vertex_count() const = 0;
    unsigned edge(unsigned v1, unsigned v2) = 0;
    void set_edge(unsigned v1, unsigned v2, int value) = 0;
    void set_undirected_edge(unsigned v1, unsigned v2, int value) = 0;
    */

class AdjacencyMatrix {
private:
    int* _m;
    unsigned _vertex_count;

    inline int & edge_internal(unsigned v1, unsigned v2) {
        assert(v1 < _vertex_count);
        assert(v2 < _vertex_count);
        return _m[v1 * _vertex_count + v2];
    }

    AdjacencyMatrix();
    AdjacencyMatrix& operator=(AdjacencyMatrix& rhs);
    AdjacencyMatrix(AdjacencyMatrix const & rhs);

public:
    AdjacencyMatrix(unsigned vertex_count) {
        _vertex_count = vertex_count;
        _m = new int[vertex_count*vertex_count];
        if (_m == nullptr) {
            cout << "Failed to allocate memory for adjacency matrix.\n";
            abort();
        }
    }
    ~AdjacencyMatrix() {
        cout << "deleting AM\n";
        delete[] _m;
    }
    unsigned vertex_count() const { return _vertex_count; }
    unsigned edge(unsigned v1, unsigned v2) {
        return edge_internal(v1,v2);
    }
    void set_edge(unsigned v1, unsigned v2, int value) {
        edge_internal(v1,v2) = value;
    }
    void set_undirected_edge(unsigned v1, unsigned v2, int value) {
        edge_internal(v1, v2) = value;
        edge_internal(v2, v1) = value;
    }

};

template <typename Graph>
void basic_graph_test(Graph & g) {
    auto count = g.vertex_count();
    for(decltype(count) i = 0; i < count; ++i) {
        if (i % 2 == 0) {
            g.set_undirected_edge(i, 0, i*i);
        }
    }
    for(decltype(count) i = 0; i < count; ++i) {
        if (i % 2 == 0) {
            if (g.edge(i, 0) != g.edge(0, i)) {
                cout << "Edges not equal" << endl;
                abort();
            }
            if (g.edge(i, 0) != i*i) {
                cout << "Edge not equal to expected value." << endl;
                abort();
            }
        }
    }
}

void basic_incidence_matrix() {
}

void basic_adjacency_matrix() {
    AdjacencyMatrix g(10000);
    basic_graph_test(g);
    cout << "ok\n";
}

void basic_adjacency_list() {
}

int main() {
    basic_incidence_matrix();
    basic_adjacency_matrix();
    basic_adjacency_list();
}

