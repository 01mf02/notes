5.8.2016
========


More interesting projects
-------------------------

From an old list that I found floating around:

* Verified k-NN / Naive Bayes
* LeanCoP @ Idris
* Simpsets
* Deep Learning
* Latent Semantics in RF


4.8.2016
========


Sequent calculus
----------------

Chad is convinced that in order to be able to translate proofs from
nanoCoP to Isabelle, it is necessary to understand the transformation of
proofs from sequent calculus to natural deduction, as these are the
calculi that underlie the two systems above.
So we started with a simple exercise: proving $p \lor \lnot p$ in
natural deduction using proof by contradiction.
Then, he gave me a classical sequent calculus and a classical
natural deduction calculus, both with four rules:

* Sequent calculus: Init, $\bot_l$, $\to_l$, $\to_r$
* Natural deduction: Init, "Contra", $\to_e$, $\to_i$

The task was to show the so-called "translation theorem":
If $\Gamma \Rightarrow \phi$ in sequent calculus, then $\Gamma \vdash \phi$
in natural deduction.

To prove this by induction, I tried to prove the following conjecture:
If $\Gamma \Rightarrow \Delta$, then
there exists $\phi \in \Delta$ such that $\Gamma \vdash \phi$.

Unfortunately, this conjecture turned out to be false.
Consider the sequent:

* Premise: $p \Rightarrow p \to \bot, p, \bot$
* Conclusion: $\Rightarrow p \to \bot, p$

This can be proven in sequent calculus, but it is neither possible
to prove only $\Rightarrow p \to \bot$ nor is it possible to prove only $p$.

I then proposed a new version of the translation theorem:
If $\Gamma \Rightarrow \Delta$, then
there exists a subset $\{\phi_1, \dots, \phi_n\} \subseteq \Delta$ such that
$\Gamma \vdash \lnot \phi_1 \to \dots \to \lnot \phi_n \to \bot$.

Chad pointed out that the $\lnot \phi_i$ could be moved to the left side
of the sequent, which would simplify matters.
After this change, we stopped the exercise, as Chad was convinced that I had
understood the idea of the translation.

Still, the calculi that we looked at did not have any quantifiers in them.
Chad also proposed to do the same exercise as above with calculi that have
basically *only* quantifiers and no logical connectives such as $\lnot$, $\to$.

However, we discussed the matter a bit more, and he mentioned that the
translation from expansion tree proofs to natural deduction/sequent calculus
had already been implemented in *TPS*, among others by Frank Pfenning and
Chad himself. TPS also features something called `PFENNING-TAC`,
which is apparently a method to reconstruct expansion tree proofs in a
"pretty" way, because otherwise even proofs of something as simple as
$p \to q \to p$ can explode quite a bit when done in a simple fashion.
After this, I asked Chad what was the point in me actually redoing work
that has already been mostly done before. He told me that it would actually be
unlikely that an article describing this work of mine would be accepted.
So I'm currently asking myself whether I should really continue on this road.
I love this project (reconstructing nanoCoP proofs in Isabelle),
but if it does not convey a new point, I see no scientific reason in doing it.
There is only the practical reason of having stronger automation in an ITP,
but is that enough to justify all the work?


Useful information
------------------

Chad has implemented the function to output the formulae that were used
in a proof, as well as which were the formulae that led to the processing
of formulae. This should make it possible to enable more directed ML methods.
However, I expect better results if the information is not only available for
formulae that were actually useful during the proving process, but also for
formulae that did *not* contribute, as my previous experiments showed that
discouraging the use of such formulae is often helpful.
If we could deduce more precisely in *which* situations such formulae
do not help, this could further improve the performance.



3.8.2016
========


Explanation-based guidance
--------------------------

Explanation-based guidance extracts information from proof steps
(or final proofs) that can be reused in case similar situations arise.
For example, $\forall x y. P a b x y$ and $\exists z. \lnot P a b z z$
creates a contradiction, but EBG will conclude something like
$\forall Q. \forall x y. Q x y \to \exists z. \lnot Q z z \to \bot$.
Such information can be generated for any intermediate step or final proof,
but the problem is that a large amount of such information may obstruct the
proof search.
My concern was that you might lose information that could help you guide the
proof search when you have a set of consistent symbols, such as succ.
However, this might be a minor detail. As Chad quite rightly points out,
"names should not matter".

* More specific pattern clauses (having fewer variables) should be preferred.
* Smaller pattern clauses (having fewer literals) should be preferred. 
* Subterms as features probably do not work. For example, would you prefer
  the pattern clause with $\forall x y. Q x y$ if you have
  $\forall x y. P a b x y$? There is not really a similar subterm ...
* It is probably best to wait with work on this until we have more concrete
  pattern clause data for several problems.


randoCoP
--------

I read the article about *randoCoP* by Thomas Raths and Jens Otten.
They make two important points:

* Shuffling the axioms instead of the clauses at the beginning
  of the proof search gives better results.
* Only the incomplete variants of leanCoP profit from shuffling.

Raths & Otten did not try randomizing clauses *during* the proof search.
However, Cezary told me he has done that by accident. What were the results?
In the leanCoP article from 2003 in JSC (Journal of Symbolic Computation),
Otten describes a variant of leanCoP, leanCoP_i, which also takes into account
the size of the clauses selected in extension steps, and adds this to the
path length.
Here, I see a possible improvement with ML: Depending on the "usefulness"
of a clause in the current situation, increase the path limit for the
current branch. Again, has Cezary done experiments with this?

Furthermore: The ML version of leanCoP should have an option to start
with positive clauses, instead of only starting with "conjecture clauses"!


Expansion trees
---------------

I discussed with Chad for several hours (!) today.
At the base of our discussion was one formula:
$((\exists x. P x) \lor (\exists y. Q y)) \land \forall x. \lnot P x \land \forall y. \lnot Q y$.
My concern was that nanoCoP would start the proof by assuming $\lnot P a$,
and only then would it use the clause $P a \lor Q b$, followed by $\lnot Q b$.
The problem with this is that the witness $a$ has to be retrieved first,
and to do this, one has to process the disjunction.
That means that proof reconstruction of nanoCoP proofs without $epsilon$
is not as straightforward as the reconstruction of leanCoP proofs,
because in the leanCoP case, at least the proof step order can be kept.
However, it should not matter because the order of proof steps is flexible.
It is helpful to see the proof more as a "refutation graph" more than a list,
or to use Chad's terminology, to consider the "matings".
Chad pointed out that the nanoCoP proofs can be represented as expansion tree
proofs, where leafs of the tree can be connected to other leafs, meaning that
the two leafs are mated. This allows us to elegantly treat the both sides
of the disjunction, obtaining the witness, and expanding the rest of the tree
until we get the complementary literal, at which point we have proven that the
branch implies $\bot$.
The most pressing practical problem for me at the moment is:
How to elegantly represent the matings inside the tree, and how to
extract this information from the nanoCoP proof?

It might also be necessary to treat certain pathological cases, such as a
disjunction where on the left side there is a selection node ($\exists$)
with an eigenvariable $a$ and on the right side there is a "universal" node
($\forall$, forgot the name right now) where this node is instantiated with $a$.
In this case, however, the universal node is not "blocked"
by the selection node, because the $a$ on the different sides of the disjunction
can be considered independent. I guess that Miller's or Pfenning's work would be
the best source to look up such cases. But, as Chad put it,
"one understands these things best if one implements it".

Furthermore, the "deskolemization" should not create Isabelle terms yet,
but just replace Skolem terms with eigenvariables. This should be relatively
straightforward, just creating new eigenvariables for every two Skolem terms
that are not unifiable w.r.t. $\sigma$.
(To remain "backward compatible", one can even add entries to the substitution
from every eigenvariable to one corresponding Skolem term.)
The dependency relation should also be adapted to consider the eigenvariables.
I should look up the exact definition ...

The workflow should probably be something like this:

1. nanoCoP proof
2. Skolem expansion tree proof (incl. mating)
3. Expansion tree proof
4. Reconstruction, respecting the dependency relation

Apparently, this has already been implemented in *TPS*.
However, according to Chad, usually expansion tree proofs are translated to
sequent calculus (Miller, Pfenning), which one can then translate to
natural deduction (Gentzen).


Prolog
------

I was interested in the "lower bound" example given in the article
"On the complexity of proof deskolemization" (Baaz, Hetzl, Weller)
<http://web.logic.at/staff/weller/deskol.pdf>.
To this end, I first tried to make a small Prolog program that should output
the lower bound example in TPTP syntax. The program looks like this:

~~~ prolog
% definitions of logical connectives and quantifiers
:- [nanocop_tptp2].

r(0, g0 => g0).

r(N, F) :-
  N > 0,
  M is N-1,
  r(M, S),
  F = ((? [X]: p(N, X) | g(N)) => ? [Y]: (p(N, Y) | g(N)) & S).
~~~

However, the problem here is that I cannot write output like "p1(X)",
because I cannot concatenate p with 1. As a workaround, I write
"p(1, X)", but this is not what I want in the final output.
What can I do here?


Recapitulation of potential projects
------------------------------------

Quoting an email to Cezary (8 Jun 2016):

* Monte-Carlo theorem proving (in E?)
* Reinforcement learning
* Shuffling of clauses during proof search in leanCoP
  (unlike randoCoP doing it not only at the beginning)
* Learning of decision procedures:
  For example, if you need to prove s(s(s(0))) > 0 and you have that s(x) > x,
  learn that continuous application of that premise will help you eliminate
  that goal easily. I thought this was quite similar to what Hazel Duncan did,
  but we will yet have to see.


1.7.2016
========


Here is a little résumé of discussions I had with people at IJCAR:


Christoph Wernhard
------------------

* Henkin quantifiers: <https://en.wikipedia.org/wiki/Branching_quantifier>
* Patrick Doherty, Witold Lukaszewicz, and Andrzej Sza las. Computing circumscription revisited: A reduction algorithm.

* leanCoP: Do not only use a maximum depth, but allow higher depths in case that the branch does not explode too much.
  See: <http://cs.christophwernhard.com/pie/cmprover/index.html>


Vampire talk (selecting the selection)
--------------------------------------

* Literal selection in Vampire:
  Give preference to literals that are least likely to unify with some term.
  The idea is that if we are in such a position where we can actually unify
  with such an unlikely unifiable term, then we should definitely do it! :)
  That means: Prefer literals that are large (contain many constants),
  and prefer literals that have more frequent occurrence of the same variable.


GAPT
----

* Deskolemisierung ist in Arbeit

Workflow:

* Expansion proof erhalten
* expansionSequent.antecedent gibt die Prämissen zurück
* Jede Prämisse ist selbst wieder ein Expansionsbaum
* extractInstances gibt die vollständig instaniierten tiefen Formeln zurück
* Vermutlich bester Einstiegspunkt: ExpansionToResolutionProof
* Gibt die einzelnen Transformationsschritte (allE, impI, ...) zurück,
  die zu einem Widerspruch führen
* Konvention: auf der linken Seite Konjunktion von negativen Sachen,
  rechts Disjunktion von positiven Sachen
* Isabelle muss gar nicht klausifizieren
* Problem: Skolem-Funktionen (werden von GAPT eingeführt)
* Vielleicht als erster Schritt Skolemisierung mittels Epsilon-Termen
  in HOL selbst machen, dann ist Deskolemisierung trivial


Jens
----

* "Be ignorant" of what other people do
* What am I doing differently?

Ideen für leanCoP/nanoCoP:

* positive Startklauseln statt Konjekturklauseln (laut Jens besser)
* PathLim nicht nur von PathLim, sondern auch von Klauselgröße
  schon im Óriginal-leanCoP-Paper experimentell getestet
* vielleicht auch mit Machine Learning das Maximal-Limit beeinflussen?


Jasmine
-------

* Möchte an neuem Beweiser für HOL arbeiten, der auf gut funktionierenden
  Techniken für FOL basiert und der first-order-Probleme möglichst
  gleich gut wie ein FOL-Beweiser lösen sollte
* Instanziierungs-Suche für HO-Formeln: simple Heuristik,
  und wenn diese nicht funktioniert, "shot in the dark".
  Das verbessert die Performanz deutlich -- welches Paper??
* GAPT in Isabelle nicht unbedingt eine gute Idee, weil
  Beweis-Suche jedesmal auf dem entsprechenden Beweiser beruht
* ProofMarket: schlechte Idee, weil nicht an Benutzern orientiert
* "Have at least one user"
* Beweise mit Löchern -- Löcher zulassen, wenn möglicherweise Prämisse vergessen
* Type encodings?

* Nunchaku: nette Pipeline-Architektur


Hans de Nivelle
---------------

* Skolemisation: Buch von Jean Gallier <http://www.cis.upenn.edu/~jean/home.html>
* Geo: ATP ohne Skolemisation!


Christoph Benzmüller
--------------------

* <https://page.mi.fu-berlin.de/cbenzmueller/>
* Serge Autexier, Andreas Meier: haben zusammengearbeitet und
  scheinbar ein Problem gefunden, dessen Deskolemisierung
  exponentielles Verhalten aufwies


Premise selection
-----------------

* Use SInE?
