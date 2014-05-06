#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define MAXN 1000
#define MAX_HASH 1021
typedef struct Edge Edge;
typedef struct Set Set;
typedef Set *Set_t;
typedef struct SetNode {
        int val;
        struct SetNode *next;        
} SetNode;
struct Set {
    SetNode *head[MAX_HASH];
};

Set_t set_create() {
    Set_t set = (Set_t)malloc(sizeof(Set));
    memset(set->head, 0, sizeof set->head);
    return set;
}

int set_belongs(Set_t set, int val) {
    SetNode *p;
    for (p = set->head[val % MAX_HASH]; p; p = p->next)
        if (p->val == val) return 1;
    return 0;
}

void set_push(Set_t set, int val) {
    SetNode *p = (SetNode*)malloc(sizeof(SetNode));
    int hv = val % MAX_HASH;
    p->next = set->head[hv];
    p->val = val;
    set->head[hv] = p;
}

struct Edge {
    int to;
    Edge *next;
} *head[MAXN], *rhead[MAXN];

typedef struct DFNode DFNode;
struct DFNode {
    int id;
    DFNode *next;
};

void add_edge(int a, int b, Edge **head) {
    Edge *p = (Edge*)malloc(sizeof(Edge));
    p->to = b;
    p->next = head[a];
    head[a] = p;
}
int dom[MAXN], ord[MAXN], vis[MAXN], par[MAXN];
int n, n0, m;
DFNode *df[MAXN];
Set_t dfset[MAXN];

void dfs(int u, int v) {
    static int ocnt = 0;
    Edge *e;
    if (vis[u] != -1) return;
    par[u] = v;
    vis[u] = 0;
    for (e = head[u]; e; e = e->next)
        dfs(e->to, u);
    vis[u] = ocnt;
    ord[ocnt++] = u;
}

int intersect(int b1, int b2) {
    while (b1 != b2)
        if (b1 < b2) b1 = dom[b1];
        else b2 = dom[b2];
    return b1;
}

int main() {
    int i;
    int ch = 1; 
    scanf("%d %d %d", &n, &m, &n0);
    for (i = 0; i < m; i++)
    {
        int a, b;
        scanf("%d %d", &a, &b);
        add_edge(a, b, head);
        add_edge(b, a, rhead);
    }
    for (i = 0; i < n; i++)
        vis[i] = dom[i] = -1;
    dfs(n0, -1);
    for (i = 0; i < n; i++)
        printf("%d ", ord[i]); puts("");
    dom[vis[n0]] = vis[n0];
    while (ch)
    {
        int i;
        ch = 0;
        for (i = n - 2; i >= 0; i--)
        {
            int id = ord[i];
            Edge *e = rhead[id];
            int new_idom = vis[par[id]];
            for (; e; e = e->next)
            {
                int p = e->to;
                if (vis[p] == new_idom) continue;
                if (dom[vis[p]] != -1)
                    new_idom = intersect(vis[p], new_idom);
            }
            if (dom[i] != new_idom)
            {
                ch = 1;
                dom[i] = new_idom;
            }
        }
        for (i = 0; i < n; i++)
            printf("%d ", ord[dom[vis[i]]]); puts("");
    }
    for (i = 0; i < n; i++)
    {
        dfset[i] = set_create();
        df[i] = NULL;
    }
    for (i = 0; i < n; i++)
        if (rhead[i] && rhead[i]->next)
        {
            Edge *p = rhead[i];
            for (; p; p = p->next)
            {
                int runner = p->to;
                while (vis[runner] != dom[vis[i]])
                {
                    if (!set_belongs(dfset[runner], i))
                    {
                        DFNode *np = (DFNode*)malloc(sizeof(DFNode));
                        np->id = i;
                        np->next = df[runner];
                        set_push(dfset[runner], i);
                        df[runner] = np;
                    }
                    runner = ord[dom[vis[runner]]];
                }
            }
        }
    for (i = 0; i < n; i++)
    {
        DFNode *p = df[i];
        printf("%d: ", i);
        for (; p; p = p->next)
            printf("%d ", p->id); puts("");
    }
    return 0;
}
