****  Compilation ******

Pour compiler le simulateur de netlists, utilisez la commande suivante:
  > ocamlbuild zSimulator.byte

**** Utilisation *********

Pour lancer le simulateur, il faut lui donner un fichier .net :
  > ./zSimulator.byte test/nadder.net
Cela exécute la netlist nadder.net à l'infini, jusqu'à ce que l'utilisateur coupe l'exécution avec C-C

Pour exécuter un nombre fini k de cycles d'horloge, il faut le préciser avec l'option -n k :
  > ./zSimulator.byte -n 10 test/clock_div.net
Cela exécute la netlist clock_div sur 10 cycles d'horloge.

Il est possible de ne pas exécuter la netlist, mais seulement de produire la netlist ordonnée par le scheduler, avec l'option -print :
  > ./zSimulator.byte -print test/#nom_de_la_netlist#.net
Cela produit un fichier de la forme #nom_de_la_netlist#_built.net dont les équations sont bien ordonnées.


**** Spécifications ****

Pour les entrées,
 - les 0 à gauche ne sont pas significatifs
 - les entrées trop petites (2 bits au lieu des 5 attendus par exemple) sont acceptées et complétées par des 0 à gauche.
 - les entrées trop grandes (5 bits au lieu des 2 attendus par exemple) ne sont pas acceptées.

