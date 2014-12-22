#include <cassert>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

void assert_equal(int v1, int v2) {
    if (v1 != v2) {
        cout << "assert_equal failed. " << v1 << " != " << v2 << endl;
        abort();
    }
}

class AdjacencyMatrix {
private:
    vector<int> _m;
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
    AdjacencyMatrix(unsigned vertex_count)
        : _m(vertex_count*vertex_count, 0),
        _vertex_count(vertex_count) {}

    unsigned size() const { return _vertex_count; }

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

    void neighbors(unsigned v, vector<int>* out) const {
        assert(v < _vertex_count);
        assert(out);
        unsigned row_base = v * _vertex_count;
        for(unsigned i = 0; i < _vertex_count; ++i) {
            if (_m[row_base+i] > 0) {
                (*out).push_back(i);
            }
        }
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
        _verticies = static_cast<VertexList**>(::calloc(vertex_count, sizeof(VertexList*)));
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
        ::free(_verticies);
    }
    unsigned size() const { return _vertex_count; }

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

    void neighbors(unsigned v, vector<int>* out) const {
        assert(v < _vertex_count);
        assert(out);
        for(VertexList* p = _verticies[v]; p; p = p->next) {
            (*out).push_back(p->vertex_index);
        }
    }
};

template <typename Graph>
void bfs(Graph const & graph, unsigned start_v, vector<int>* distance_out, vector<int>* parent_out) {
    assert(start_v < graph.size());
    assert(distance_out);
    assert(parent_out);

    // Inspired by Intro to Algorithms psuedo code

    // initialize output parameters
    vector<int> & parent = *parent_out;
    vector<int> & distance = *distance_out;

    parent.assign(graph.size(), -1);
    distance.assign(graph.size(), -1);

    // bookkeeping        
    vector<bool> discovered(graph.size(), false);

    discovered[start_v] = true;
    distance[start_v] = 0;
    parent[start_v] = -1;

    queue<int> q;
    q.push(start_v);
    vector<int> neigh;

    while(q.size() != 0) {
        unsigned u = q.front();
        q.pop();

        neigh.clear();
        graph.neighbors(u, &neigh);
        //cout << "n: " << neigh.size() << endl;
        for(unsigned i = 0; i < neigh.size(); ++i) {
            unsigned v = neigh[i];
            if (!discovered[v]) {
                discovered[v] = true;
                distance[v] = distance[u] + 1;
                parent[v] = u;
                q.push(v);
            }
        }
    }
}

template <typename Graph>
void max_node(Graph const & graph, unsigned from_v, unsigned *v_out, int *distance_out) {
    assert(v_out);
    assert(distance_out);
    assert(graph.size() > 0);

    vector<int> distance;
    vector<int> parent;
    bfs(graph, from_v, &distance, &parent);

    int max_distance = 0;
    unsigned max_distance_v = from_v;
    for(unsigned i = 0; i < graph.size(); ++i) {
        //cout << "  " << i << ": dist=" << distance[i] << "\n";
        if (distance[i] > max_distance) {
            max_distance = distance[i];
            max_distance_v = i;
        }
    }

    *v_out = max_distance_v;
    *distance_out = max_distance;
}

template <typename Graph>
unsigned diameter(Graph const & graph) {

    // From comment in http://tech-queries.blogspot.com/2010/09/diameter-of-tree-in-on.html

    if (graph.size() == 0) return 0;

    // 1. pick random node n
    unsigned n = 0;

    // 2. search for node with max distance to n, call it m
    unsigned m;
    int n_to_m_distance;
    max_node(graph, n, &m, &n_to_m_distance);
    //cout << "n_to_m: " << n_to_m_distance << "\n";
    //cout << "m is: " << m <<  "\n";

    // 3. search for node with max distance to m, call it o.
    unsigned o;
    int m_to_o_distance;
    max_node(graph, m, &o, &m_to_o_distance);
    //cout << "m_to_o: " << m_to_o_distance << "\n";

    return m_to_o_distance;
}

template <typename Graph>
void basic_graph_test() {
    Graph g(6);
    g.set_undirected_edge_weight(0, 1, 5);
    g.set_undirected_edge_weight(1, 2, 4);
    g.set_undirected_edge_weight(1, 3, 10);
    g.set_undirected_edge_weight(2, 3, 7);
    g.set_undirected_edge_weight(3, 4, 100);
    // don't connect anything to 5

    for(unsigned i = 0; i < g.size(); ++i) {
        for(unsigned j = 0; j < g.size(); ++j) {
            unsigned weight = g.edge_weight(i,j);
            //cout << "weight[" << i << "," << j << "]=" << weight << "\n";

            if ((i == 0 && j == 1) || (i == 1 && j == 0)) assert_equal(weight, 5);
            else if ((i == 1 && j == 2) || (i == 2 && j == 1)) assert_equal(weight, 4);
            else if ((i == 1 && j == 3) || (i == 3 && j == 1)) assert_equal(weight, 10);
            else if ((i == 2 && j == 3) || (i == 3 && j == 2)) assert_equal(weight, 7);
            else if ((i == 3 && j == 4) || (i == 4 && j == 3)) assert_equal(weight, 100);
            else assert_equal(weight, 0);
        }
    }

    for(unsigned i = 0; i < g.size(); ++i) {
        auto d = g.out_degrees(i);
        vector<int> neigh;
        g.neighbors(i, &neigh);
        //cout << "out degrees[" << i << "]=" << d << "\n";

        switch(i) {
        case 0:
            assert_equal(d, 1);
            assert_equal(neigh.size(), 1);
            break;
        case 1:
            assert_equal(d, 3);
            assert_equal(neigh.size(), 3);
            break;
        case 2:
            assert_equal(d, 2);
            assert_equal(neigh.size(), 2);
            break;
        case 3:
            assert_equal(d, 3);
            assert_equal(neigh.size(), 3);
            break;
        case 4:
            assert_equal(d, 1);
            assert_equal(neigh.size(), 1);
            break;
        case 5:
            assert_equal(d, 0);
            assert_equal(neigh.size(), 0);
            break;
        default:
            abort();
        }
    }

    vector<int> distance;
    vector<int> parent;
    bfs(g, 0, &distance, &parent);
    assert_equal(distance.size(), parent.size());
    assert_equal(distance.size(), g.size());
    assert_equal(distance[0], 0);
    assert_equal(distance[1], 1);
    assert_equal(distance[2], 2);
    assert_equal(distance[3], 2);
    assert_equal(distance[4], 3);
#if 0
    for(unsigned v = 0; v < distance.size(); ++v) {
        cout << v << ": dist=" << distance[v] << " parent=" << parent[v] << '\n';
    }
#endif

    Graph tree1(1);
    assert_equal(diameter(tree1), 0);

    Graph tree2(2);
    tree2.set_undirected_edge_weight(0,1,1);
    assert_equal(diameter(tree2), 1);

    Graph tree3(3);
    tree3.set_undirected_edge_weight(0,1,1);
    tree3.set_undirected_edge_weight(1,2,1);

    unsigned v = 10000;
    int dist = -1;
    max_node(tree3, 0, &v, &dist);
    assert_equal(diameter(tree3), 2);
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
    //basic_incidence_matrix();
    basic_adjacency_matrix();
    basic_adjacency_list();
    // TODO: TopCoder style graphs consisting of set A, B, and L, where (A[i], B[i]) is the
    // edge between verticies A[i] and B[i] and the weight of that edge is L[i].
}

