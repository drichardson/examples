#include <iostream>
#include <cassert>

using namespace std;

void assert_equal(int v1, int v2) {
    if (v1 != v2) {
        cout << "assert_equal failed. " << v1 << " != " << v2 << endl;
        abort();
    }
}

/*
   Graph interface
    unsigned vertex_count() const = 0;
    unsigned edge(unsigned v1, unsigned v2) = 0;
    void set_edge(unsigned v1, unsigned v2, int weight) = 0;
    void set_undirected_edge(unsigned v1, unsigned v2, int weight) = 0;
    */

class AdjacencyMatrix {
private:
    int* _m;
    unsigned _vertex_count;

    inline int & edge_weight_internal(unsigned v1, unsigned v2) {
        assert(v1 < _vertex_count);
        assert(v2 < _vertex_count);
        // v1 is the row, v2 is the column.
        // The row shows all verticies v1 is connected to.
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
        delete[] _m;
    }
    unsigned vertex_count() const { return _vertex_count; }
    unsigned edge_weight(unsigned v1, unsigned v2) {
        return edge_weight_internal(v1,v2);
    }
    void set_edge_weight(unsigned v1, unsigned v2, int weight) {
        edge_weight_internal(v1,v2) = weight;
    }
    void set_undirected_edge_weight(unsigned v1, unsigned v2, int weight) {
        edge_weight_internal(v1, v2) = weight;
        edge_weight_internal(v2, v1) = weight;
    }

    // 
    unsigned out_degrees(unsigned v) const {
        assert(v < _vertex_count);
        unsigned row_base = v * _vertex_count;
        unsigned result = 0;
        for(unsigned i = 0; i < _vertex_count; ++i) {
            if (_m[row_base+i] > 0) ++result;
        }
        return result;
    }
};

class AdjacencyList {
    // array of VertexLists.
    struct VertexList {
        struct VertexList* next;
        unsigned vertex_index;
        int weight;
    };
    unsigned _vertex_count;
    VertexList** _verticies;

    VertexList* list_for_edge(unsigned v1, unsigned v2) {
        for(auto p = _verticies[v1]; p; p = p->next) {
            if (p->vertex_index == v2) {
                return p;
            }
        }
        return nullptr;
    }

public:
    AdjacencyList(unsigned vertex_count) {
        _vertex_count = vertex_count;
        _verticies = reinterpret_cast<VertexList**>(calloc(1, sizeof(_verticies[0])*vertex_count));
        if (_verticies == nullptr) {
            cout << "Failed to allocate _verticies" << endl;
            abort();
        }
    }
    ~AdjacencyList() {
        for(unsigned i = 0; i < _vertex_count; ++i) {
            for(auto p = _verticies[i]; p;) {
                auto tmp = p;
                p = p->next;
                delete tmp;
            }
        }
        free(_verticies);
    }
    unsigned vertex_count() const { return _vertex_count; }

    unsigned edge_weight(unsigned v1, unsigned v2) {
        auto p = list_for_edge(v1, v2);
        return p ? p->weight : 0;
    }

    void set_edge_weight(unsigned v1, unsigned v2, int weight) {
        auto p = list_for_edge(v1, v2);
        if (p) {
            p->weight = weight;
        } else {
            VertexList* vl = new VertexList;
            vl->weight = weight;
            vl->vertex_index = v2;
            vl->next = _verticies[v1];
            _verticies[v1] = vl;
        }
    }

    void set_undirected_edge_weight(unsigned v1, unsigned v2, int weight) {
        set_edge_weight(v1, v2, weight);
        set_edge_weight(v2, v1, weight);
    }

    unsigned out_degrees(unsigned v) const {
        assert(v < _vertex_count);
        unsigned result = 0;
        for(auto p = _verticies[v]; p; p = p->next) {
            ++result;
        }
        return result;
    }
};

template <typename Graph>
void basic_graph_test() {
    Graph g(5);
    g.set_undirected_edge_weight(0, 1, 5);
    g.set_undirected_edge_weight(1, 2, 4);
    g.set_undirected_edge_weight(1, 3, 10);
    g.set_undirected_edge_weight(2, 3, 7);
    // don't connect anything to 4

    for(unsigned i = 0; i < g.vertex_count(); ++i) {
        for(unsigned j = 0; j < g.vertex_count(); ++j) {
            unsigned weight = g.edge_weight(i,j);
            cout << "weight[" << i << "," << j << "]=" << weight << "\n";

            if ((i == 0 && j == 1) || (i == 1 && j == 0)) assert_equal(weight, 5);
            else if ((i == 1 && j == 2) || (i == 2 && j == 1)) assert_equal(weight, 4);
            else if ((i == 1 && j == 3) || (i == 3 && j == 1)) assert_equal(weight, 10);
            else if ((i == 2 && j == 3) || (i == 3 && j == 2)) assert_equal(weight, 7);
            else assert_equal(weight, 0);
        }
    }

    for(unsigned i = 0; i < g.vertex_count(); ++i) {
        auto d = g.out_degrees(i);
        cout << "out degrees[" << i << "]=" << d << "\n";

        switch(i) {
        case 0:
            assert_equal(d, 1);
            break;
        case 1:
            assert_equal(d, 3);
            break;
        case 2:
            assert_equal(d, 2);
            break;
        case 3:
            assert_equal(d, 2);
            break;
        case 4:
            assert_equal(d, 0);
            break;
        default:
            abort();
        }
    }
}

void basic_incidence_matrix() {
}

void basic_adjacency_matrix() {
    cout  << "basic_adjacency_matrix: start\n";
    basic_graph_test<AdjacencyMatrix>();
    cout  << "basic_adjacency_matrix: ok\n";
}

void basic_adjacency_list() {
    cout  << "basic_adjacency_list: start\n";
    basic_graph_test<AdjacencyList>();
    cout  << "basic_adjacency_list: ok\n";
}

int main() {
    basic_incidence_matrix();
    basic_adjacency_matrix();
    basic_adjacency_list();
    // TODO: TopCoder style graphs consisting of set A, B, and L, where (A[i], B[i]) is the
    // edge between verticies A[i] and B[i] and the weight of that edge is L[i].
}

