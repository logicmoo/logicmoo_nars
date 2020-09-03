
%%%Examples for truncation ops


member(X,[Y|R]):- member(X,R),member(X,[X]).
member(X,[X|_]).

app([x,a],[b,c],[x,a,b,c]).
app([a],[b,c],[a,b,c]).
app([],[b,c],[b,c]).

ex(app([1,2],[3],[1,2,3]),'+').
ex(app([x,a],[b,c],[x,a,b,c]),'+').
ex(app([a],[b,c],[a,b,c]),'+').
ex(app([],[],[]),'+').
ex(app([p],[],[p]),'+').
ex(app([],[u],[u]),'+').
ex(app([],[x,y],[x,y]),'+').
ex(app([r,s],[],[r,s]),'+').
ex(app([g],[d],[g,d]),'+').
ex(app([9,8,7],[],[9,8,7]),'+').
ex(app([],[6,5,4],[6,5,4]),'+').
ex(app([4,3,5],[8],[4,3,5,8]),'+').
ex(app([r,w],[q,t,s,f,i],[r,w,q,t,s,f,i]),'+').
ex(app([j,k,l,m],[n,o,p,q,r],[j,k,l,m,n,o,p,q,r]),'+').
ex(app([r,s,t],[q,u,v],[t,s,r,q,u,v]),'-').
ex(app([s,t],[q,u,v],[s,r,q,u,v]),'-').


min(A,[A|B]):- min(C,B), ge(E,F).
p(X):- q(X,V1),r(V1,V2),q(V3),s(V3,V1).


column(X):- brick(X), standing(X), is_on(X,Y), ground(Y).

column(X):- brick(X), standing(X), is_on(X,Y), column(Y).

same_height(X,Y):- ground(X), ground(Y).

same_height(X,Y):- brick(X), standing(X), brick(Y), standing(Y), is_on(X,X1), is_on(Y,Y1), 
                   same_height(X1,Y1).


arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C).



ex(p(a),+).
ex(p(b),+).
ex(p(c),-).

q(a,qa).
q(b,qb).
q(c,qc).
r(qa,x).
r(qb,x).
r(qc,x).
s(sa,qa).
s(sb,qb).
s(sc,qc).
q(sa).
q(sb).