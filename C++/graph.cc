#include <cassert>
#include <iostream>
#include <limits>
#include <list>
#include <queue>
#include <stack>
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

    void neighbors(unsigned v, vector<unsigned> & out) const {
        assert(v < _vertex_count);
        unsigned row_base = v * _vertex_count;
        for(unsigned i = 0; i < _vertex_count; ++i) {
            if (_m[row_base+i] > 0) {
                out.push_back(i);
            }
        }
    }
};

class AdjacencyList {
    struct Entry {
        unsigned vertex_index;
        int weight;
    };
    vector<list<Entry>> _verticies;

    Entry* list_for_edge(unsigned v1, unsigned v2) {
        for(Entry & entry : _verticies[v1]) {
            if (entry.vertex_index == v2) {
                return &entry;
            }
        }
        return nullptr;
    }

public:
    AdjacencyList(unsigned vertex_count) : _verticies(vertex_count) {}

    unsigned size() const { return _verticies.size(); }

    unsigned edge_weight(unsigned v1, unsigned v2) {
        auto p = list_for_edge(v1, v2);
        return p ? p->weight : 0;
    }

    void set_edge_weight(unsigned v1, unsigned v2, int weight) {
        auto p = list_for_edge(v1, v2);
        if (p) {
            p->weight = weight;
        } else {
            _verticies[v1].push_front(Entry());
            Entry & e = _verticies[v1].front();
            e.vertex_index = v2;
            e.weight = weight;
        }
    }

    void set_undirected_edge_weight(unsigned v1, unsigned v2, int weight) {
        set_edge_weight(v1, v2, weight);
        set_edge_weight(v2, v1, weight);
    }

    unsigned out_degrees(unsigned v) const {
        assert(v < _verticies.size());
        return _verticies[v].size();
    }

    void neighbors(unsigned v, vector<unsigned> & out) const {
        assert(v < _verticies.size());
        for(Entry const & e : _verticies[v]) {
            out.push_back(e.vertex_index);
        }
    }
};

struct bfs_result {
    vector<bool> discovered;
    vector<int> distance;
    vector<int> parent;
};

template <typename Graph>
void bfs(Graph const & graph, unsigned start_v, bfs_result & result) {
    assert(start_v < graph.size());

    // Inspired by Intro to Algorithms psuedo code

    vector<int> & parent = result.parent;
    vector<int> & distance = result.distance;
    vector<bool> & discovered = result.discovered;

    // initialize output parameters
    parent.assign(graph.size(), -1);
    distance.assign(graph.size(), -1);
    discovered.assign(graph.size(), false);


    discovered[start_v] = true;
    distance[start_v] = 0;
    parent[start_v] = -1;

    queue<int> q;
    q.push(start_v);
    vector<unsigned> neigh;

    while(q.size() != 0) {
        unsigned u = q.front();
        q.pop();

        neigh.clear();
        graph.neighbors(u, neigh);
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

unsigned const dfs_no_parent = std::numeric_limits<unsigned>::max();

struct dfs_result {
    vector<unsigned> discovered_time;
    vector<unsigned> finished_time;
    vector<unsigned> parent; // dfs_no_parent if node has no parent.
};

template <typename Graph>
void dfs_visit(Graph const & graph, unsigned v, unsigned & time_counter, dfs_result & r) {
    ++time_counter;
    r.discovered_time[v] = time_counter;
    cout << "visiting " << v << " with time " << time_counter << endl;

    vector<unsigned> neighbors;
    graph.neighbors(v, neighbors);
    for(unsigned neigh : neighbors) {
        if (r.discovered_time[neigh] == 0) {
            r.parent[neigh] = v;
            dfs_visit(graph, neigh, time_counter, r);
        }
    }

    ++time_counter;
    r.finished_time[v] = time_counter;
    cout << "finishing " << v << " with time " << time_counter << endl;
}

template <typename Graph>
void dfs(Graph const & graph, dfs_result & r) {
    // Inspired by Introduction to Algorithms psuedo code
    r.discovered_time.assign(graph.size(), 0);
    r.finished_time.assign(graph.size(), 0);
    r.parent.assign(graph.size(), dfs_no_parent);

    // time 0 represents WHITE label from Intro to Algorithms. dfs_visit will increment
    // to 1 before assigning to first discovered vertex.
    unsigned time_counter = 0;

    for(unsigned v = 0; v < graph.size(); ++v) {
        if (r.discovered_time[v] == 0) {
            dfs_visit(graph, v, time_counter, r);
        }
    }
}

template <typename Graph>
void dfs_visit_no_recursion(Graph const & graph, unsigned root_v, unsigned & time_counter, dfs_result & r)
{
    struct stack_entry {
        unsigned vertex;
        vector<unsigned> neighbors;
    };

    // root_v is white, or else we wouldn't get here.
    stack<stack_entry> stack;
    stack.push({root_v});
    graph.neighbors(root_v, stack.top().neighbors);
    ++time_counter;
    cout << "visiting " << stack.top().vertex << " with time " << time_counter << endl;
    r.discovered_time[root_v] = time_counter;
    r.parent[root_v] = dfs_no_parent;

    // Loop invariant: item on stack if it has been discovered but
    // not finished. The neighbors, however, may be discovered or not.
    while(stack.size() > 0) {
        stack_entry & top = stack.top();
        if (top.neighbors.size() > 0) {
#if 1
            // this way is more efficient but doesn't mimick
            // dfs (recursive) exactly.
            unsigned v = top.neighbors.back();
            top.neighbors.pop_back();
#else
            // this way sucks efficiency wise (erase of first item is linear
            // w.r.t. size of vector) but exactly duplicates dfs (recursive)
            // behavior. This is here to help debug differences between
            // dfs (non-recursive) and dfs_recursive.
            unsigned v = top.neighbors.front();
            top.neighbors.erase(top.neighbors.begin());
#endif

            if (r.discovered_time[v] == 0) {
                ++time_counter;
                cout << "visiting " << v << " with time " << time_counter << endl;
                r.discovered_time[v] = time_counter;
                r.parent[v] = top.vertex;
                stack.push({v});
                graph.neighbors(v, stack.top().neighbors);
            }
        } else {
            ++time_counter;
            r.finished_time[top.vertex] = time_counter;
            stack.pop();
            cout << "finishing " << top.vertex << " with time " << time_counter << endl;
        }
    }
}

template <typename Graph>
void dfs_no_recursion(Graph const & graph, dfs_result & r) {
    // Inspired by Introduction to Algorithms psuedo code
    r.discovered_time.assign(graph.size(), 0);
    r.finished_time.assign(graph.size(), 0);
    r.parent.assign(graph.size(), dfs_no_parent);

    // time 0 represents WHITE label from Intro to Algorithms. dfs_visit will increment
    // to 1 before assigning to first discovered vertex.
    unsigned time_counter = 0;

    for(unsigned v = 0; v < graph.size(); ++v) {
        if (r.discovered_time[v] == 0) {
            dfs_visit_no_recursion(graph, v, time_counter, r);
        }
    }
}

template <typename Graph>
void max_node(Graph const & graph, unsigned from_v, unsigned *v_out, int *distance_out) {
    assert(v_out);
    assert(distance_out);
    assert(graph.size() > 0);

    bfs_result bres;
    bfs(graph, from_v, bres);

    int max_distance = 0;
    unsigned max_distance_v = from_v;
    for(unsigned i = 0; i < graph.size(); ++i) {
        //cout << "  " << i << ": dist=" << distance[i] << "\n";
        if (bres.distance[i] > max_distance) {
            max_distance = bres.distance[i];
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
        vector<unsigned> neigh;
        g.neighbors(i, neigh);
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

    {
        bfs_result r;
        vector<int> & d = r.distance;
        bfs(g, 0, r);
        assert_equal(d.size(), r.parent.size());
        assert_equal(d.size(), g.size());
        assert_equal(d[0], 0);
        assert_equal(d[1], 1);
        assert_equal(d[2], 2);
        assert_equal(d[3], 2);
        assert_equal(d[4], 3);
    }

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

    {
        dfs_result r;
        dfs(g, r);
        cout << "DFS:\n";
        for(unsigned v = 0; v < g.size(); ++v) {
            cout << "\t" << v
                << " discovered=" << r.discovered_time[v]
                << " finished=" << r.finished_time[v];

            if (r.parent[v] == dfs_no_parent) {
                cout << " root";
            } else {
                cout << " parent=" << r.parent[v];
            }
            cout << '\n';
        }
    }

    {
        dfs_result r;
        dfs_no_recursion(g, r);
        cout << "DFS (no_recursion):\n";
        for(unsigned v = 0; v < g.size(); ++v) {
            cout << "\t" << v
                << " discovered=" << r.discovered_time[v]
                << " finished=" << r.finished_time[v];

            if (r.parent[v] == dfs_no_parent) {
                cout << " root";
            } else {
                cout << " parent=" << r.parent[v];
            }
            cout << '\n';
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
    //basic_incidence_matrix();
    basic_adjacency_matrix();
    basic_adjacency_list();
    // TODO: TopCoder style graphs consisting of set A, B, and L, where (A[i], B[i]) is the
    // edge between verticies A[i] and B[i] and the weight of that edge is L[i].
}

