#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef char b_node_key[40]; // our keys can be 40 bytes.
typedef int b_node_value;

// minimum degree is "t" in "Introduction to Algorithms"
#define B_TREE_MINIMUM_DEGREE 2
#define B_TREE_MAX_SUBTREES (B_TREE_MINIMUM_DEGREE * 2)
#define B_TREE_MAX_KEYS (B_TREE_MAX_SUBTREES - 1)

typedef struct b_tree_node {
    int count; // number of keys stored in current node
    bool is_leaf;
    b_node_key keys[B_TREE_MAX_KEYS];
    b_node_value values[B_TREE_MAX_KEYS];
    struct b_tree_node* subtrees[B_TREE_MAX_SUBTREES];
} b_tree_node;

typedef struct {
    b_tree_node* root;
} b_tree;

static void b_tree_key_copy(b_node_key dest, b_node_key const src) {
    strlcpy(dest, src, sizeof(b_node_key));
}

static b_tree_node* b_tree_new_node(void) {
    return (b_tree_node*)calloc(1, sizeof(b_tree_node));
}

static b_tree* b_tree_new(void) {
    b_tree* result = (b_tree*)malloc(sizeof(b_tree));
    result->root = b_tree_new_node();
    return result;
}

typedef struct {
    b_tree_node* x;
    int i;
} b_tree_search_result;

static b_tree_search_result b_tree_search_result_null; 

static inline bool b_tree_search_result_is_null(b_tree_search_result result) {
    return result.x == NULL;
}

b_tree_search_result b_tree_search(b_tree_node* x, char const* k) {
    // 1. i = 1
    int i = 0;

    // 2. while i <= x.n and k > x.key[i]
    while(i < x->count && strcmp(k, x->keys[i]) > 0) {
        // i = i + 1
        ++i;
    }
    // 4. if i <= x.n and key == x.key[i]
    if (i < x->count && strcmp(k, x->keys[i]) == 0) {
        // 5. return (x,i)
        return (b_tree_search_result){x, i};
    }
    // 6. elseif x.leaf
    else if (x->is_leaf) {
        // 7. return NIL
        return b_tree_search_result_null;
    }
    else {
        // 8. else Disk-Read(x.c[i])
        // 9. return B-Tree-Search(x.c[i],k)
        return b_tree_search(x->subtrees[i], k);
    }
}

// split the full tree at x.subtrees[i]
static void b_tree_split_child(b_tree_node* x, int const i) {
    // 1. z = Allocate-Node()
    b_tree_node* z = b_tree_new_node();

    // 2. y = x.c[i]
    b_tree_node* y = x->subtrees[i];

    // 3. z.leaf = y.leaf
    z->is_leaf = y->is_leaf;

    // 4. z.n = t - 1
    z->count = B_TREE_MINIMUM_DEGREE - 1;

    // 5. for j = 1 to t - 1
    for(int j = 0; j < B_TREE_MINIMUM_DEGREE - 1; ++j) {
        // 6. z.key[j] = y.key[j+t]
        b_tree_key_copy(z->keys[j], y->keys[j + B_TREE_MINIMUM_DEGREE]);
        z->values[j] = y->values[j+B_TREE_MINIMUM_DEGREE];
    }
    // 7. if not y.leaf
    if (!y->is_leaf) {
        // 8. for j = 1 to t
        for(int j = 0; j < B_TREE_MINIMUM_DEGREE; ++j) {
            // 9. z.c[j] = y.c[j+t]
            z->subtrees[j] = y->subtrees[j+B_TREE_MINIMUM_DEGREE];
        }
    }
    // 10. y.n = t - 1
    y->count = B_TREE_MINIMUM_DEGREE - 1;
    // 11. for j = x.n + 1 downto i + 1
    for(int j = x->count; j >= i + 1; --j) {
        // 12. x.c[j+1] = x.c[j]
        x->subtrees[j+1] = x->subtrees[j];
    }

    // 13. x.c[i+1] = z
    x->subtrees[i+1] = z;

    // 14. for j = x.n downto i
    for(int j = x->count; j >= i; --j) {
        // 15. x.key[j+1] = x.key[j]
        b_tree_key_copy(x->keys[j+1], x->keys[j]);
    }

    // 16. x.key[i] = y.key[t]
    b_tree_key_copy(x->keys[i], y->keys[B_TREE_MINIMUM_DEGREE]);

    // 17. x.n = x.n + 1
    x->count++;

    // 18. Disk-Write(y)
    // 19. Disk-Write(z)
    // 20. Disk-Write(x)
}

static void b_tree_insert_nonfull(b_tree_node* x, char const* k, b_node_value v) {
    // 1. i = x.n
    int i = x->count - 1;

    // 2. if x.leaf
    if (x->is_leaf) {
        // 3. while i >= 1 and k < x.key[i]
        while(i >= 0 && strcmp(k, x->keys[i]) < 0) {
            // 4. x.key[i+1] = x.key[i]
            b_tree_key_copy(x->keys[i+1], x->keys[i]);
            // 5. i = i - 1
            --i;
        }
        // 6. x.key[i+1] = k
        b_tree_key_copy(x->keys[i+1], k);
        // 7. x.n = x.n + 1
        x->count++;
        // 8. Disk-Write(x)
    } else {
        // 9. else while i >= 1 and k < x.key[i]
        while (i >= 0 && strcmp(k, x->keys[i]) < 0) {
            // 10. i = i - 1
            --i;
        }
        // 11. i = i + 1
        ++i;
        // 12. Disk-Read(x.c[i])
        // 13. if x.c[i].n == 2t-1
        if (x->subtrees[i]->count == B_TREE_MAX_KEYS) {
            // 14. B-Tree-Split-Child(x,i)
            b_tree_split_child(x, i);
            // 15. if k > x.key[i]
            if (strcmp(k, x->keys[i]) > 0) {
                // 16. i = i + 1
                ++i;
            }
        }
        // 17. B-Tree-Insert-Nonfull(x.c[i],k)
        b_tree_insert_nonfull(x->subtrees[i], k, v);
    }
}

void b_tree_insert(b_tree* T, char const* k, b_node_value v) {

    // 1. r = T.root
    b_tree_node* r = T->root;

    // 2. if r.n == 2t - 1
    if (r->count == B_TREE_MAX_KEYS) {
        // 3. s = Allocate-Node()
        b_tree_node* s = b_tree_new_node();

        // 4. T.root = s
        T->root = s;

        // 5. s.leaf = FALSE
        s->is_leaf = false;

        // 6. s.n = 0
        s->count = 0;

        // 7. s.c[1] = r
        s->subtrees[0] = r;

        // 8. B-Tree-Split-Child(s, 1)
        b_tree_split_child(s, 0);

        // 9. B-Tree-Insert-Nonfull(s,k)
        b_tree_insert_nonfull(s,k,v);

    } else {
        //10. else B-Tree-Insert-Nonfull(r,k)
        b_tree_insert_nonfull(r,k,v);
    }
}

void b_tree_delete(b_tree* t, char const* key) {
}

void pv(b_tree* t, char const *key) {
    b_node_value v;
    b_tree_search_result result = b_tree_search(t->root, key);
    if (result.x == NULL) printf("key '%s' not found\n", key);
    else printf("key (%d) '%s' has value %d\n", result.i, result.x->keys[result.i], v);
}

int main(int argc, char const** argv) {
    b_tree* t = b_tree_new();

    pv(t, "a");

    b_tree_insert(t, "g", 1);
    b_tree_insert(t, "a", 2);
    b_tree_insert(t, "h", 3);
    b_tree_insert(t, "z", 4);
    b_tree_insert(t, "f", 5);
    b_tree_insert(t, "m", 6);
    b_tree_insert(t, "b", 7);
    b_tree_insert(t, "o", 8);

    pv(t, "a");
    pv(t, "o");
    pv(t, "NOT THERE");

    return 0;
}

