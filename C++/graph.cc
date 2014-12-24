#include <algorithm>
#include <cassert>
#include <iostream>
#include <limits>
#include <list>
#include <queue>
#include <stack>
#include <vector>
#include <map>

using namespace std;

void assert_equal(int v1, int v2) {
    if (v1 != v2) {
        cout << "assert_equal failed. " << v1 << " != " << v2 << endl;
        abort();
    }
}

void assert_true(bool condition) {
    if (!condition) {
        cout << "assert_true failed" << endl;
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

    void transpose(AdjacencyMatrix & out) const {
        out._m.resize(_m.size());
        for(unsigned i = 0; i < _vertex_count; ++i) {
            //unsigned row_base = i*_vertex_count;
            for(unsigned j = 0; j < _vertex_count; ++j) {
                // Gt(i,j) = G(j,i);
                out._m[i * _vertex_count + j] = _m[j * _vertex_count + i];
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

    void transpose(AdjacencyList & out) const {
        out._verticies.clear();
        out._verticies.resize(_verticies.size());
        for(unsigned u = 0; u < _verticies.size(); ++u) {
            for(Entry const & entry : _verticies[u]) {
                out.set_edge_weight(entry.vertex_index, u, entry.weight);
            }
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

unsigned const dfs_root = std::numeric_limits<unsigned>::max();

struct dfs_result {
    vector<unsigned> discovered_time;
    vector<unsigned> finished_time;
    vector<unsigned> parent; // dfs_root if node has no parent.
    bool dag = true;
};

struct dfs_options {
    void* context = 0;
    void (*on_discovered)(unsigned vertex, void* context) = 0;
    void (*on_finished)(unsigned vertex, void* context) = 0;
};

dfs_options dfs_default_options;

template <typename Graph>
void dfs_visit(Graph const & graph, unsigned v, unsigned & time_counter,
        dfs_result & r, dfs_options const & options)
{
    ++time_counter;
    r.discovered_time[v] = time_counter;
    if (options.on_discovered) options.on_discovered(v, options.context);
    //cout << "visiting " << v << " with time " << time_counter << endl;

    vector<unsigned> neighbors;
    graph.neighbors(v, neighbors);
    for(unsigned neigh : neighbors) {
        if (r.discovered_time[neigh] == 0) {
            r.parent[neigh] = v;
            dfs_visit(graph, neigh, time_counter, r, options);
        } else {
            // item has been discovered. If it has not been finished
            // that means it's being processed and if we're looking
            // at it now that means we have an edge to an in
            // process vertex, which means we have a cycle, which means
            // this is not a dag.
            if (r.finished_time[neigh] == 0) {
                r.dag = false;
            }
        }
    }

    ++time_counter;
    r.finished_time[v] = time_counter;
    if (options.on_finished) options.on_finished(v, options.context);
    //cout << "finishing " << v << " with time " << time_counter << endl;
}

template <typename Graph>
void dfs(Graph const & graph, dfs_result & r, dfs_options const & options = dfs_default_options) {
    // Inspired by Introduction to Algorithms psuedo code
    r.discovered_time.assign(graph.size(), 0);
    r.finished_time.assign(graph.size(), 0);
    r.parent.assign(graph.size(), dfs_root);

    // time 0 represents WHITE label from Intro to Algorithms. dfs_visit will increment
    // to 1 before assigning to first discovered vertex.
    unsigned time_counter = 0;

    for(unsigned v = 0; v < graph.size(); ++v) {
        if (r.discovered_time[v] == 0) {
            assert(r.finished_time[v] == 0);
            dfs_visit(graph, v, time_counter, r, options);
        }
    }
}

template <typename Graph>
void dfs_visit_no_recursion(Graph const & graph, unsigned root_v, unsigned &
        time_counter, dfs_result & r, dfs_options const & options) {
    struct stack_entry {
        unsigned vertex;
        vector<unsigned> neighbors;
    };

    // root_v is white, or else we wouldn't get here.
    stack<stack_entry> stack;
    stack.push({root_v});
    graph.neighbors(root_v, stack.top().neighbors);
    ++time_counter;
    //cout << "visiting " << stack.top().vertex << " with time " << time_counter << endl;
    r.discovered_time[root_v] = time_counter;
    if (options.on_discovered) options.on_discovered(root_v, options.context);
    r.parent[root_v] = dfs_root;

    // Loop invariant: item on stack if it has been discovered but
    // not finished. The neighbors, however, may be discovered or not.
    while(stack.size() > 0) {
        stack_entry & top = stack.top();
        if (top.neighbors.size() > 0) {
#if 1
            // this way is more efficient but doesn't mimick
            // dfs (recursive) exactly.
            unsigned neigh = top.neighbors.back();
            top.neighbors.pop_back();
#else
            // this way sucks efficiency wise (erase of first item is linear
            // w.r.t. size of vector) but exactly duplicates dfs (recursive)
            // behavior. This is here to help debug differences between
            // dfs (non-recursive) and dfs_recursive.
            unsigned neigh = top.neighbors.front();
            top.neighbors.erase(top.neighbors.begin());
#endif

            if (r.discovered_time[neigh] == 0) {
                ++time_counter;
                //cout << "visiting " << v << " with time " << time_counter << endl;
                r.discovered_time[neigh] = time_counter;
                r.parent[neigh] = top.vertex;
                if (options.on_discovered) options.on_discovered(neigh, options.context);
                stack.push({neigh});
                graph.neighbors(neigh, stack.top().neighbors);
            } else {
                if (r.finished_time[neigh] == 0) {
                    r.dag = false;
                }
            }
        } else {
            ++time_counter;
            r.finished_time[top.vertex] = time_counter;
            if (options.on_finished) options.on_finished(top.vertex, options.context);
            stack.pop();
            //cout << "finishing " << top.vertex << " with time " << time_counter << endl;
        }
    }
}

template <typename Graph>
void dfs_no_recursion(Graph const & graph, dfs_result & r,
        dfs_options const & options = dfs_default_options)
{
    // Inspired by Introduction to Algorithms psuedo code
    r.discovered_time.assign(graph.size(), 0);
    r.finished_time.assign(graph.size(), 0);
    r.parent.assign(graph.size(), dfs_root);

    // time 0 represents WHITE label from Intro to Algorithms. dfs_visit will increment
    // to 1 before assigning to first discovered vertex.
    unsigned time_counter = 0;

    for(unsigned v = 0; v < graph.size(); ++v) {
        if (r.discovered_time[v] == 0) {
            assert(r.finished_time[v] == 0);
            dfs_visit_no_recursion(graph, v, time_counter, r, options);
        }
    }
}

template <typename SequenceContainer>
void insert_front_unsigned(unsigned u, void* container)
{ 
    SequenceContainer* c = static_cast<SequenceContainer*>(container);
    c->insert(c->begin(), u);
}

template <typename Graph, typename SequenceContainer>
void topological_sort(Graph const & graph, SequenceContainer & result) {
    dfs_options o;
    o.context = &result;
    o.on_finished = insert_front_unsigned<SequenceContainer>;

    dfs_result res;
    //dfs_no_recursion(graph, res, o);
    dfs(graph, res, o);
    assert_true(res.dag); // topological sort impossible if not a dag
}

template <typename Graph>
void strongly_connected_components(Graph const & graph, dfs_result & r) {
    dfs_result res;
    dfs(graph, res);

    // compute transpose of graph
    Graph graphTranspose(graph.size());
    graph.transpose(graphTranspose);

    // get list of verticies ordered by finished time. Do so by mapping
    // time to vertex.
    map<unsigned, unsigned> finished_time_to_vertex;
    for(unsigned v = 0; v < graph.size(); ++v) {
        finished_time_to_vertex[res.finished_time[v]] = v;
    }

    // modified dfs() that takes into account the finish order of the first dfs() call.
    r.discovered_time.assign(graph.size(), 0);
    r.finished_time.assign(graph.size(), 0);
    r.parent.assign(graph.size(), dfs_root);
    unsigned time_counter = 0;
    for(auto itr = finished_time_to_vertex.crbegin(); itr != finished_time_to_vertex.crend(); ++itr) {
        cout << "f time: " << itr->first << ", v=" << itr->second << endl;
        unsigned v = itr->second;
        if (r.discovered_time[v] == 0) {
            assert(r.finished_time[v] == 0);
            dfs_visit(graphTranspose, v, time_counter, r, dfs_default_options);
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
void print_neighbors(Graph const & graph) {
    cout << "Graph Neighbors. Graph size " << graph.size() << '\n';
    vector<unsigned> neighbors;
    for(unsigned v = 0; v < graph.size(); ++v) {
        neighbors.clear();
        graph.neighbors(v, neighbors);
        cout << '\t' << v << ": ";
        for(unsigned neigh : neighbors) {
            cout << neigh << ' ';
        }
        cout << '\n';
    }
}

void print_dfs_tree(dfs_result const & r) {
    unsigned vertex_count = r.parent.size();
    for(unsigned v = 0; v < vertex_count; ++v) {
        cout << "\t" << v
            << " discovered=" << r.discovered_time[v]
            << " finished=" << r.finished_time[v];

        if (r.parent[v] == dfs_root) {
            cout << " root";
        } else {
            cout << " parent=" << r.parent[v];
        }
        cout << '\n';
    }
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
        cout << "Transpose:\n";
        Graph g3(3);
        g3.set_edge_weight(0,1,1);
        g3.set_edge_weight(0,2,1);
        cout << "pre-transpose:\n";
        print_neighbors(g3);
        Graph g3T(g3.size());
        g3.transpose(g3T);
        cout << "transpose:\n";
        print_neighbors(g3T);
    }

    {
        dfs_result r;
        dfs(g, r);
        cout << "DFS:\n";
        print_dfs_tree(r);
    }

    {
        dfs_result r;
        dfs_no_recursion(g, r);
        cout << "DFS (no_recursion):\n";
        print_dfs_tree(r);
    }

    {
        cout << "notdag\n";
        Graph notdag(2);
        notdag.set_edge_weight(0,1,1);
        notdag.set_edge_weight(1,0,1);
        dfs_result r;
        dfs(notdag, r);
        print_dfs_tree(r);
        assert_true(!r.dag);
    }

    {
        cout << "dag\n";
        Graph dag(3);
        dag.set_edge_weight(0,1,1);
        dag.set_edge_weight(0,2,1);
        dag.set_edge_weight(1,2,1);
        dfs_result r;
        dfs(dag, r);
        print_dfs_tree(r);
        assert_true(r.dag);
    }

    {
        cout << "Topological sort:\n";
        list<unsigned> sorted_list;
        Graph dag(4);
        dag.set_edge_weight(0, 1, 1);
        dag.set_edge_weight(0, 2, 1);
        dag.set_edge_weight(2, 1, 1);
        dag.set_edge_weight(3, 1, 1);
        topological_sort(dag, sorted_list);
        for(auto v : sorted_list) {
            cout << '\t' << v << '\n';
        }
        assert_equal(sorted_list.size(), dag.size());
    }

    {
        cout << "Strongly connected components (should have 3 components):\n";
        Graph g(6);
        // component 1
        g.set_edge_weight(0,1,1);
        g.set_edge_weight(1,0,1);

        // component 2
        g.set_edge_weight(2,3,1);
        g.set_edge_weight(3,2,1);

        // component 3
        g.set_edge_weight(4,5,1);
        g.set_edge_weight(5,4,1);

        dfs_result res;
        strongly_connected_components(g, res);

        print_dfs_tree(res);

        auto num_components = count(res.parent.begin(), res.parent.end(), dfs_root);
        assert_equal(num_components, 3);
    }

    {
        cout << "Strongly connected components (should have 3 components):\n";
        Graph g(6);
        // component 1
        g.set_edge_weight(0,1,1);
        g.set_edge_weight(1,0,1);

        // component 2
        g.set_edge_weight(2,3,1);
        g.set_edge_weight(3,2,1);

        // component 3
        g.set_edge_weight(4,5,1);
        g.set_edge_weight(5,4,1);
        g.set_edge_weight(5,0,1);

        dfs_result res;
        strongly_connected_components(g, res);

        print_dfs_tree(res);
        auto num_components = count(res.parent.begin(), res.parent.end(), dfs_root);
        assert_equal(num_components, 3);
    }
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
    basic_adjacency_matrix();
    basic_adjacency_list();
}

