# Syntax analyser
For given finite grammar creates LR automata. Using that automata it checks if given word is accepted by particular grammar. When for given grammar, building LR automata isn't possible, program gives kind of conflict that occurs in grammar (SHIFT-REDUCE, REDUCE-REDUCE).

**Code created for "Programming languages ​​and paradigms" on 3rd year of my CS studies at University of Warsaw.**

Main predicates definied:
* *createLR(+Gramamar, -Automata, -Info)*
For given grammar creates LR(0) automata. In case of success parameter Info is set to yes, otherwise it contains cause of conflict.

* *accept(+Automata, +Word)*
Success when Word is accepted by Automata.

Format of grammar defining:

*gramatyka(’E’, [prod(’E’, [[nt(’E’),+,nt(’T’)], [nt(’T’)]]),
prod(’T’, [[id], [’(’, nt(’E’), ’)’]]) ]).*

what is equal to:

*E -> E + T | T

T -> id | ( E )*


