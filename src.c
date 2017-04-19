#define vec_binop(_name, _op) \
int _name ## _slow (sil_State *S, int t1, int t2) { \
    int dx=1, dy=1; \
    int n = 1; \
    const double *x, *y; \
    double xval, yval; \
    if(t1 == 5 || t1 == 6 || t1 == 7) { \
        dx = 0; \
        xval = sil_todouble(S, 1); \
        x = &xval; \
    } else if(t1 == 16) { \
        const vector *a = (vector *)sil_topointer(S, 1); \
        n = a->n; \
        x = a->x; \
    } else { \
        return sil_err(S, "Invalid 1st arg"); \
    } \
    if(t2 == 5 || t2 == 6 || t2 == 7) { \
        dy = 0; \
        yval = sil_todouble(S, 2); \
        y = &yval; \
    } else if(t2 == 16) { \
        const vector *b = (vector *)sil_topointer(S, 2); \
        n = b->n; /* we know both are not vectors */ \
        y = b->x; \
    } else { \
        return sil_err(S, "Invalid 2nd arg"); \
    } \
    vector *c = (vector *)malloc(sizeof(vector)+8*n); \
    c->n = n; \
    int i; \
    for(i=0; i<n; i++) { \
        c->x[i] = (*x) _op (*y); \
        x += dx; y += dy; \
    } \
    sil_settop(S, 0); \
    sil_pushvector(S, c); \
    return 0; \
} \
int _name (sil_State *S) { \
    int t1 = sil_type(S, 1); \
    int t2 = sil_type(S, 2); \
    if(t1 != 16 || t2 != 16) { /* slow version */ \
        return _name ## _slow(S, t1, t2); \
    } \
    const vector *a = (vector *)sil_topointer(S, 1); \
    const vector *b = (vector *)sil_topointer(S, 2); \
    if(a == NULL || b == NULL) \
        return sil_err(S, "Invalid arguments"); \
    if(a->n != b->n) \
        return sil_err(S, "Sizes of vectors don't match: %d vs %d", a->n,b->n);\
    vector *c = (vector *)malloc(sizeof(vector)+8*a->n); \
    c->n = a->n; \
    int i; \
    for(i=0; i<a->n; i++) { \
        c->x[i] = a->x[i] _op b->x[i]; \
    } \
    sil_settop(S, 0); \
    sil_pushvector(S, c); \
    return 0; \
}

// create add, mul, and sub functions
vec_binop(add, +);
vec_binop(mul, *);
vec_binop(sub, -);
vec_binop(divide, /);

// simple vector creation routines
int zeros(sil_State *S) {
    if(sil_type(S, 1) != 6) return sil_err(S, "zeros requires int argument");
    int i, n = sil_tointeger(S, 1);
    vector *v = (vector *)calloc((sizeof(vector) + 8*n)/4, 4);
    v->n = n;
    sil_settop(S, 0);
    sil_pushvector(S, v);
    return 0;
}
int range(sil_State *S) {
    if(sil_type(S, 1) != 6 || sil_type(S, 2) != 6)
        return sil_err(S, "range requires 2 int args");
    int i = sil_tointeger(S, 1);
    int j = sil_tointeger(S, 2);
    int n = 0, m = i <= j ? j - i : i - j;
    if(m > 10000000) {
        return sil_err(S, "range exceeds 10M elements");
    }
    vector *v = (vector *)malloc(sizeof(vector)+8*m);
    v->n = m;
    if(i <= j) {
        for(; i<j; i++,n++) {
            v->x[n] = i;
        }
    } else {
        for(; i>j; i--,n++) {
            v->x[n] = i;
        }
    }
    sil_settop(S, 0);
    sil_pushvector(S, v);
    return 0;
}

// [Float] -> Vector
int fromList(sil_State *S) {
    int i, n = sil_llen(S, 1);
    vector *v = (vector *)malloc(sizeof(vector)+8*n);
    v->n = n;
    for(i=0; i<n; i++) {
        sil_behead(S, 1);
        v->x[i] = sil_todouble(S, 2);
        sil_remove(S, 2);
    }
    sil_settop(S, 0);
    sil_pushvector(S, v);
    return 0;
}

// Vector -> [Float]
int toList(sil_State *S) {
    int i;
    const vector *v = (vector *)sil_topointer(S, 1);
    if(v == NULL) {
        return sil_err(S, "Invalid argument");
    }
    for(i=0; i<v->n; i++) {
        sil_pushdouble(S, v->x[i]);
    }
    sil_setcons(S, v->n);
    sil_remove(S, 1); // clear vector
    return 0;
}

// Vector -> Int -> Float
int elem(sil_State *S) {
    int i;
    const vector *v = (vector *)sil_topointer(S, 1);
    if(v == NULL) {
        return sil_err(S, "Invalid argument");
    }
    i = sil_tointeger(S, 2);
    sil_remove(S, 2);
    if(i < 0) i += v->n;
    if(i < 0 || i >= v->n) {
        return sil_err(S, "Invalid index");
    }
    sil_pushdouble(S, v->x[i]);
    sil_remove(S, 1);
    return 0;
}

// Int -> Float -> ST(Vector, Nil)
int setElem(sil_State *S) {
    size_t len; // Always assume len is wrong!
    vector *v = (vector *)sil_getST(S, &len);
    if(v == NULL) {
        return sil_err(S, "Can't update - no vector present");
    }
    int i = sil_tointeger(S, 1);
    if(i < 0) i += v->n;
    if(i < 0 || i >= v->n) {
        return sil_err(S, "Invalid index");
    }
    v->x[i] = sil_todouble(S, 2);
    sil_settop(S, 0);
    sil_pushnil(S);
    return 0;
}

// (Int -> Float) -> Int -> Vector
int fromFunction(sil_State *S) {
    int i, n = sil_tointeger(S, 2);
    if(n < 0 || n > 10000000) {
        return sil_err(S, "invalid length");
    }
    vector *v = (vector *)malloc(sizeof(vector)+8*n);
    v->n = n;

    sil_remove(S, 2);
    for(i=0; i<n; i++) {
        sil_pushvalue(S, 1); // copy function
        sil_pushinteger(S, i); // push arg
        sil_call(S, 2);
        v->x[i] = sil_todouble(S, 2);
        sil_remove(S, 2); // clear result
    }
    sil_remove(S, 1); // remove function
    sil_pushvector(S, v);
    return 0;
}

