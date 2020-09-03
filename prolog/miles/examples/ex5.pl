% MENDEL

% parent generation
ex(colour(a,red),+).
ex(colour(b,yellow),+).

% f1
ex(colour(k(a,a),red),+).
ex(colour(k(a,b),red),+).
ex(colour(k(b,b),yellow),+).

% parent with f1
ex(colour(k(a,k(a,a)),red),+).
ex(colour(k(a,k(a,b)),red),+).
ex(colour(k(a,k(b,b)),red),+).
ex(colour(k(b,k(a,a)),red),+).
ex(colour(k(b,k(a,b)),red),+).
ex(colour(k(b,k(a,b)),yellow),+).
ex(colour(k(b,k(b,b)),yellow),+).

% f2
ex(colour(k(k(a,a),k(a,a)),red),+).
ex(colour(k(k(a,a),k(a,b)),red),+).
ex(colour(k(k(a,a),k(b,b)),red),+).
ex(colour(k(k(a,b),k(a,b)),red),+).
ex(colour(k(k(a,b),k(a,b)),yellow),+).
ex(colour(k(k(a,b),k(b,b)),red),+).
ex(colour(k(k(a,b),k(b,b)),yellow),+).
ex(colour(k(k(b,b),k(b,b)),yellow),+).





