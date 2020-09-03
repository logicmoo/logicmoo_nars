


has_wings(X):- bird(X).
has_beak(X):- bird(X).
bird(X):- vulture(X).
carnivore(X):- vulture(X).

ex(has_wings(tweety),+).
ex(has_beak(tweety),+).