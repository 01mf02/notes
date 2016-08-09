8.8.2016
========


Monday meeting
--------------

We had our Monday discussion today, and Jiří talked about his usage of
"parse trees with holes" to parse ambiguous sentences.
In a nutshell, he learns the parse trees of training examples, where
he actually learns several versions of the tree, which differ in the
maximal depth. Then, from the frequency of the parse trees, he
calculates a probability for each parse tree.
When parsing new examples, he chooses the parse tree with the
highest probability. Problems seem to arise when multiple parse trees
overlap, but I did not completely understand what he meant.

In any case, Jiří also suggested me to compare my machine learning methods
to a standard method such as SInE, which is a good idea.
Now how to run SInE on a set of THF problems? I should revisit the
premise selection parts of HOL(y)Hammer or ask Cezary ...


Prolog, pt. 2
-------------

I revisited the problem from 3.8.2016 to generate formulae with Prolog.
With Jirka's help, I created the following (lowerbound.pl):

~~~ prolog
% definitions of logical connectives and quantifiers
:- [nanocop_tptp2].

r(0, g0 => g0).

r(N, F) :-
  N > 0,
  M is N-1,
  r(M, S),

  atom_codes('p',PC), atom_codes('g',GC),
  number_codes(N,NC),
  append(PC,NC,PNC), append(GC,NC,GNC),
  atom_codes(PN,PNC), atom_codes(GN,GNC),

  PNX =.. [PN, X],
  PNY =.. [PN, Y],

  F = ((? [X]: PNX | GN) => (? [Y]: (PNY | GN) & S)).

formula(N, fof(1, conjecture, F)) :- r(N, F).
~~~

It can be run as follows (here for size 2):

    swipl -q -t "[lowerbound], formula(2,F), print(F), nl."

In comparison, a similar Haskell version looks much less readable:

~~~ haskell
import System.Environment

r 0 = "g0 => g0"
r n = concat ["\n  ((? [X", n', "]: p", n', "(X", n', ") | g", n', ") => (? [Y", n', "]: (p", n', "(y", n', ") | g", n', ") & ", r (n-1), "))"]
  where n' = show n

main = do
  [n] <- getArgs
  putStrLn ("fof(1, conjecture, (" ++ r (read n) ++ "\n  )).")
~~~

Next, I measured the sizes of the corresponding nanoCoP proofs.
A little `bash` is all that we need:

~~~ bash
#!/bin/bash
for i in {1..10}
do
  swipl -q -t "[lowerbound], formula($i,F), print(F), nl." > lowerbound$i.p
  ./nanocop.sh lowerbound$i.p | wc -w
done
~~~

This gives as output the number of words per proof for the formulae
from n = 1 to 10:

~~~
129
216
303
390
477
564
651
738
825
912
~~~

Looks quite linear, with k = 87.
This can be expected from reading the article of Baaz, Hetzl & Weller,
where the authors show that the Skolemised version of the problem
has a proof of maximally linear size (p. 15).
However, they also proved that all cut-free proofs of the
nonskolemised version are of at least exponential size.
When talking about this with Jiří, he meant that because in Isabelle,
one actually *has* cut, it might be possible to get the proof size down.
In particular, he mentioned Martin Giese <https://heim.ifi.uio.no/martingi/>,
who apparently did work on some tableaux calculi with respect to skolemisation.
According to Jiří, also Jens Otten might be working on a CoP version
that does not rely on skolemisation, i.e. it directly treats quantifiers.
This would of course be the laziest alternative of all. :)
But it would probably also create exponential-size proofs for problems
like this one, because the calculus is cut-free.



5.8.2016
========


Satallax formula graph
----------------------

Chad implemented recording all formulae that were created during proof search
and which formulae caused the creation of which other formulae,
together with the information whether any of these formulae was
used in the proof.

Based on this information, I wrote a small tool that visualises this (graph.ml):

~~~ ocaml
open Syntax
open Printf

let filter_useless = false

let print_node chan i (u, t) =
  if filter_useless
  then (if u then fprintf chan "  %d [tooltip=\"%s\"];\n" i (trm_str t) else ())
  else fprintf chan "  %d [style=filled tooltip=\"%s\" fillcolor=%s];\n" i (trm_str t) (if u then "green" else "red")


let print_edge chan is_used (target, sources) =
  let tooltip = String.concat ", " (List.map string_of_int sources) in
  List.iter (fun source -> if is_used target && is_used source then fprintf chan "  %d -> %d [tooltip=\"%s\"];\n" source target tooltip else ()) sources

let print_graph chan (nodes, edges) =
  let is_used i = if filter_useless then fst (Hashtbl.find nodes i) else true in
  fprintf chan "digraph {\n";
  Hashtbl.iter (print_node chan) nodes;
  List.iter (print_edge chan is_used) edges;
  fprintf chan "}\n"

let load_formdeps fp =
  let f = open_in_bin fp in
  let forms : (int, bool * trm) Hashtbl.t = input_value f in
  let deps : (int * int list) list = input_value f in
  close_in f;
  (forms, deps)

let _ =
  let formdeps = load_formdeps Sys.argv.(1) in
  print_graph stdout formdeps
~~~

It can be called as follows (needs syntax.ml and utils.ml from Satallax):

    ocamlbuild graph.native
    ./graph.native PUZ/PUZ081^1.p.formdeps | xdot /dev/stdin

While `xdot` is great, especially compared to the graphical user interface of
`dot` (which is extremely horrible), it unfortunately does not display the
tooltips that give the actual terms and for the edges the producing nodes.
So a different calling variant is:

    ./graph.native PUZ/PUZ081^1.p.formdeps | dot -Tsvg -o test.svg

Firefox can render the tooltips just fine, but it is a bit clumsy.
(You need to position the mouse cursor perfectly to get the tooltip.)
The best would be if xdot could just display the tooltips …

One can choose (via setting `filter_useless`) to only show those formulae
that contributed to the final proof. Sometimes, the unnecessary formulae
take up so much space that processing takes too long, in which case
this comes handy.

We also analysed the problem PUZ081^1.p, coming to the conclusion that
Zoey is a knight and Mel is a knave, as well as that knowing "Doctor Who"
might give clues to the solution of such puzzles.


Satallax idea
-------------

The more props were created from a prop that did not lead to a proof, the worse the prop is.


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


4.8.2015
========


FastXML pt. 2
-------------

I made an error in the previous evaluation of FastXML, namely that I
wrongly interpreted the prediction output of FastXML. The corrected version
now gives the following figures:

                    Samples      Cov     Prec      Rec      AUC     Rank
------------------ -------- -------- -------- -------- -------- --------
fastxml-eval/final     1529    0.796    3.273   117.18   0.7287    54.34
hl-knn-noidf-dw1       1529    0.824    3.504   186.12   0.9158    71.32


~~~
cp $FASTXML/{train,predict} ./
cabal update
cabal configure
cabal build
mkdir fastxml-eval
dist/build/libsvm/libsvm hl/{symsn,deps.a,seq} > fastxml-eval/train
dist/build/libsvm/libsvm hl/{symsn,deps.a,seq,eval} > fastxml-eval/eval
runhaskell scripts/libsvm-iter.hs fastxml-eval/{eval,train}
cd fastxml-eval
mkdir results
for i in eval-*
do
  echo $i
  ../train train-${i##eval-}
  ../predict $i
  mv scores.txt results/${i##eval-}
done
cd results
# make sure to delete ../final at this point if it already exists!
for i in `ls * | sort -n`; do cat $i >> ../final; done
cd ../..
./statisticshl.sh --fastxml hl/eval fastxml-eval/final
~~~


28.7.2015
=========


FastXML
-------

I evaluated FastXML on our premise selection datasets. It involved the
following steps:

1. Convert predict format to numeric LibSVM format
   (labels & features in one file)
2. For each evaluation sample, create a new problem, by making the training
   data include all data up to the evaluation sample, and making the
   testing data just the features of the evaluation sample
3. Run FastXML on all evaluation problems, concatenating the results.
4. Evaluate the result.

So far, the results are quite weak (on the HOL Light dataset):

 Samples      Cov     Prec      Rec      AUC     Rank
-------- -------- -------- -------- -------- --------
    1394    0.375    1.684   188.71   0.3398   138.84


Sparse matrices
---------------

* Idris: A matrix representation exists in Data.Matrix, but it is very
  lacking in features. Implementing sparse matrices would require a
  huge amount of work, considering that even maps are in a very early
  stage (for example, they do not implement union yet).
* Coq: TODO!



18.5.2015
=========


Making fonts nice
-----------------

I found out that installing cm-super via

    sudo apt-get install cm-super

fixes fonts being pixelated on Ubuntu.
Source: <http://tex.stackexchange.com/questions/1291/why-are-bitmap-fonts-used-automatically>



13.5.2015
=========


Installing GHC on colo12
------------------------

I noticed that I could not use recent Stackage LTS version on colo12 because
the versions of the Haskell packages installed were too old and therefore
incompatible with the versions required by Stackage. Therefore, I read
<http://www.stackage.org/install> and decided to install GHC and Cabal from
scratch. Surprisingly, the binaries from the GHC website worked, even though
Benni had originally claimed that they had made lots of problems.
I used ghc-7.8.4-x86_64-unknown-linux-centos65.tar.bz2, which is compatible
with the libgmp.so.3 installed on colo12.



8.5.2015
========


A Journey to the Past
---------------------

I revived my old attempts to implement a Saffari-based online forests.

    git checkout 56f8c8f2f3d6befc5101beb61a853401c287dd4d
    runhaskell OnlineForest.hs 10 isabelle/syms isabelle/deps isabelle/seq isabelle/eval10

What strikes the eye is the similarity of all predictions: The top 3 predicted
premises are basically always the same. Upon further inspection of the code, I noticed
that the version does not implement any code to "split" a leaf node when it has
grown beyond a certain size. Therefore, I'm anxious to claim having correctly
implemented a Saffari-based predictor.
Oza and Russell (2001) presented a way to online boosting/bagging algorithms.
This is behind the Poisson idea of Saffari. However, the real difficulty is
in updating a base model (in Oza's terminology), i.e., a decision tree.
For this, the solution proposed by Saffari is to discard trees with high OOBE
to ensure adaptivity to the current classification situation, i.e. to account
for changes in the underlying distribution of samples. This ensures that the 
classifier is biased towards the last encountered samples. However, this is
undesired behaviour for a premise selector, as the theorems learned last are
not necessary those most relevant for a theorem one wishes to prove.
For the very reason that this online forest approach performs well for tracking,
we estimate it will perform poorly for premise selection.
However, its Poisson-based sampling strategy has influenced our method to
online bagging algorithms, which is particularly efficient when frequently
querying after having learnt few additional samples.

Unfortunately, after the version given above, I did abandon Poisson-based online
forests completely; the next version has only binomial updating of trees left.



7.5.2015
========


Simulating Agrawal RF
---------------------

The results in the paper for Agrawal RF stem from:

    ./predictmizar.sh -e mizar/eval -p rforest:randFeats=16:giniFeats=16:minDepthWeight=0:predAlg=Naive



18.4.2015
=========


ATP evaluation
--------------

To make ATP evaluation easier, I collected the sources to various tools
(stat, latexstat, write_problems) in predict/eval and moved the evaluation
Makefile to predict/atp. There is also a new script, eval_mizar.sh, that takes
a prediction file and will automatically write the problems, run the evaluation,
and generate statistics.


Isabelle evaluation
-------------------

To evaluate the Isabelle probability dataset, it is advisable to generate a heap
image of it first:

    isabelle jedit -l HOL-Probability

Then, one should replace its file contrib/e-1.8/x86_64-linux/eprover with

    echo $* >> ~/eproverlog

to prevent the execution of eprover.


16.4.2015
=========


k-NN / RF break-even point
--------------------------

I measured the break-even point where k-NN and Eprover with a certain timeout
would match the performance of RF with Eprover and timeout = 10sec.
I always used 128 premises, and ran Eprover on 48 cores.

Timeout        Time   Theorems
-------- ----------- ---------
  15sec   7min07sec        969
  13sec   6min20sec        964
  10sec   5min15sec        938

Random Forests achieve 969 theorems with Eprover timeout = 10secs, taking 22min
for the premise selection.
For k-NN + Eprover to match this, we need to let Eprover run longer.
In particular, the difference is (6min20sec - 5min15sec) * 48 cores = ca. 50min.
So >50min of Eprover runtime can be compensated by just 22min of RF.



31.3.2015
=========


Naive Bayes
-----------

I rewrote the Naive Bayes predictor in Haskell to better understand it.
The resulting predictor is unfortunately slower than the C++ version, taking
about 45sec for the Mizar dataset vs 15sec of the C++ version. However, during
the translation of the code to Haskell, I was able to speed up the C++ predictor
a bit, from 18sec to 15sec.
During the master seminar, I had the idea to verify some properties of
Naive Bayes in Isabelle. For doing so, a functional representation of the
algorithm is surely a benefit.



23.3.2015
=========


Meeting with Cezary
-------------------

We first discussed the assignment for the ITP course, where I had complained
about the fact that it was not possible for two people in the course to select
the same programming languages for the Easter homework. Cezary stated that he
found that I was often trying to get an extra treatment, such as in the case of
Georg's course, where I got an extra exam, which afterwards turned out to have
been unnecessary, thus creating anger about me in the group.
I am digesting this and think how to avoid such events in the future.
To solve the current conflict, I will consider using C++ or perhaps a different
language such as Idris, with which I wanted to play with for some time already.
My only concern is whether I can learn Idris in such a short time and solve
the task with it.

Next, we discussed our interaction. Cezary told me that during his PhD, his
supervisor suggested a formalisation, which led him to some new ideas of his
own, ending up to be substantial parts of his PhD thesis. Quite surprisingly,
he said that he did not know what to work on for about the first half of a year
of his PhD.
Cezary expects that I should be doing more research on my own, and in general,
students usually read about a problem in more detail or play with it before
discussing it with him.

We went through the Dissertationsvereinbarung, and Cezary meant that the
introduction should be expanded to one and the content to about three pages.

We then compiled a list of potential PhD sub-projects, where --- according to
Cezary --- each of them should be sufficient for a whole PhD. Voilà :

- Premise selection
- Proof translation
- Rewriting
- Tactics:
  * Markov chains
  * Swarm algorithms
  * Reinforcement learning
- Theory exploration (generating new conjectures for theories):
  http://dblp.uni-trier.de/pers/hd/j/Johansson:Moa and what it cites
  Look for the Hipster!
- More on data for learning: learning pairs, blacklists, ...
- More of features
- Guiding Meson / Guiding superposition:
  Cezary mentioned that he was working on a superposition prover with guiding,
  which sounds interesting to me; however, he warned me that this would be a
  big amount of work.

We have agreed on a three-week research period where I should read more about
the mentioned topics and try to find out which of them suit me, or if I have
different ideas that inspire me.

Own idea: Haskell has a nice property verifier, QuickCheck. Might it be possible
to use this to quickly check the validity of auto-generated theorems?


Notes
-----

As these notes diverge more and more from the original purpose of
premise selection-related topics, I should find a better place for them.


Naive Bayes
-----------

To get a better understanding of how Naive Bayes works, I decided to make as a
little side-project a nicer, more commented version of it and port it to the
Haskell predictor.

Already now, the results for the refactored C++ version are nice (Mizar):

* Before: 3min40sec
*  After: 3min11sec



20.2.2015
=========


ATP evaluation
--------------

To evaluate the ATP quality of predictions, we first need to compile
the program 'write_problems' by Cezary:

    ocamlopt str.cmxa unix.cmxa utils.ml format.ml write_problems.ml -o write_problems

The order of files given to ocamlopt matters; you have to give all dependencies
of a file before the file itself!

Then, we use 'write_problems':

    ./write_problems knn eval seq statements 128

With the problems written to "eval-knn-128", we copy the Makefile to this
directory and run 'make -j48' there. This will take some time, and if we want
to obtain intermediate statistics, we can do it with the following command
(in "o/e18"):

    grep "SZS status Theorem" * | wc -l
    grep "SZS status ResourceOut" * | wc -l

This returns the number of proven/unproven theorems. To get fancier statistics,
one can copy the 'stat' tool to the "eval-knn-128" directory and run:

    ./stat , y,p , , false

This should create an HTML file.


Cabal installation
------------------

On colo12, the Cabal installation is broken such that it is impossible to
upgrade Cabal to a newer version due to errors related to Path and catch.
The solution is to use:

    /home/software/tct3/.cabal/bin/cabal install cabal-install

Unfortunately, the newly installed cabal is not getting picked up by bash,
which is why I have to call it using:

    ~/.cabal/bin/cabal


SSH-fu
------

To be able to type 'ssh c12' instead of 'ssh colo12-c703', one can add the
following lines to ~/.ssh/config:

    Host c12
    Hostname colo12-c703

Furthermore, to disable the password prompting at every login, one can run
on the account that wants to login to SSH 'ssh-keygen', not specifying any
password. The resulting key in ~/.ssh/id_rsa.pub can then be appended to the
~/.ssh/authorized_keys on the home account of the SSH server.

Finally, Benni told me that when you are running an SSH server (sshd), you
should set

    PermitRootLogin no

in /etc/ssh/sshd_config. Otherwise, bots might compromise your machine, as
happened to Georg!

Anyway, the IP address of my work laptop is 138.232.66.149. I have to be
logged into VPN to access the laptop from outside, and the default time limit
is 80 hours VPN per month. Usage statistics are available at:
<https://web-mail.uibk.ac.at/modem-mgmt/>

Time limit can be increased, see:
<http://www.uibk.ac.at/zid/netz-komm/vpn/vpneinf.html>



19.2.2015
=========


Quick reminder about Gnuplot
----------------------------

To plot in Gnuplot the inverse path score, one uses a command like:

    plot [0:10] [0:1] 1 - (1 - 0.7) / ((x + 1) ** 0.8)



10.2.2015
=========


newtype
-------

I've discovered the newtype language construct in Haskell, a really useful way
to increase type-safety without giving up performance.
However, somewhat annoying about it is that when wrapping an Int value in a
newtype, I cannot use IntMap anymore, which I so far extensively used.
So I thought about using HashMap, but unfortunately, it will probably be a bit
slower than IntMap, as can be read in:

"The performance of Haskell CONTAINERS package", Milan Straka:
<http://research.microsoft.com/en-us/um/people/simonpj/papers/containers/containers.pdf>

The other option would be to use a package like `enummapset`, which wraps
Data.IntMap for usage with Enum types.



3.2.2015
========


Power
-----

I forgot to take the power of 6 of TF-IDFs when calculating feature weights.
With this change, we further approach C++ k-NN:

~~~
               Cov    Prec     Rec     AUC    Rank
knnHaskell   0.871  13.562  246.65  0.9312   55.23
knnC++       0.874  13.619  249.75  0.9319   54.41
~~~


Recreating old k-NN in Haskell
------------------------------

I tried now to faithfully recreate the function of the C++ k-NN in Haskell,
but so far I failed to achieve its original prediction quality. In particular,
using the original "age" approach in Haskell gives:

~~~
                Cov    Prec     Rec     AUC    Rank
knnTfidfAge   0.632   8.545  475.14  0.7527  174.66
~~~

This is much worse than the C++ version, yet I do not see the functional
difference between the original and my version.


Finally!
--------

~~~
             Cov    Prec     Rec     AUC    Rank
knnTfidf   0.875  13.821  250.32  0.9336   53.57
~~~

I achieved this by using logarithms for the k as well as for the
number of labels.



2.2.2015
========


TF-IDF in k-NN
--------------

I integrated TF-IDF into Haskell k-NN. Results:

~~~
               Cov    Prec     Rec     AUC    Rank
knnHaskell   0.863  13.439  263.07  0.9283   59.19
knnC++       0.874  13.619  249.75  0.9319   54.41
~~~

Let's see if we can finally do some real improvement here!


k-NN Paper
----------

Stronger Automation for Flyspeck by Feature Weighting and Strategy Evolution:
<http://cl-informatik.uibk.ac.at/workspace/publications/13pxtp2.pdf>



28.1.2015
=========


Improving k-NN
--------------

Following the theme of my last log entry, I still did not understand some parts
of my Haskell k-NN code, as I believe it tried to reflect too closely the
original C++ version. So I undertook a major rewrite.
As a result of this, we now have a finer way to calculate the score of a label,
based on by which k neighbours it was proposed. Using this additional
information, I tried several score calculation functions. The best one brought
the following results:

~~~
                             Cov    Prec     Rec     AUC    Rank
mizar/logs/knnC++NoTfidf   0.816  12.918  315.91  0.9067   77.34
mizar/logs/knnHaskellNew   0.830  13.007  305.56  0.9123   73.50
mizar/logs/knnHaskellOld   0.803  12.752  320.73  0.8995   81.84
~~~

So the new version is now finally better than the C++ version without TF-IDF.


Need for Speed
--------------

I was able to increase Haskell's k-NN speed by using Data.IntMap.Strict instead
of its lazy counterpart, which brought down runtime from about 1min20sec to
44sec. Enabling -O2 in `cabal configure` and disabling profiling brought
runtime further down to 22sec. The C++ version takes 11sec.
(All tests performed on Mizar evaluation, home laptop.)



24.1.2015
=========


I was having a hard time reading my k-NN code, and in trying to understand what
I wrote, I made a new version of a function to more closely reflect what I
thought it should do. The result was a speed-up and a slight improvement in
prediction quality. :) The reason for the latter is that the previous k-NN
algorithm disregarded samples that had no features at all in common with the
query features, while the new version considers these, but puts them at the end
of the predictions.

* Old runtime: 1min20
* New runtime: 1min06

Prediction results:

~~~
              Cov    Prec     Rec     AUC    Rank
logknnNew   0.803  12.754  320.99  0.9001   81.85
logknnOld   0.803  12.752  320.73  0.8995   81.84
~~~



24.12.2014 (Christmas Special Edition)
======================================


Hello and welcome to this special Christmas edition! Santa Claus is still busy
delivering packets, and so am I making this lovely note.


Sorting statistics
------------------

To sort my (sometimes relatively messy-looking) statistics, I often have to do
a selection of some parameters, such as:

    grep "Null.*gF16" experiments04.txt

However, this results in relatively confusing output:

~~~
mizar/experiments04/rfbSNullmTD0gF16              0.793  12.645  341.69  0.8964   86.45
mizar/experiments04/rfbSNullmTD1gF16              0.796  12.668  340.88  0.8973   86.36
mizar/experiments04/rfbSNullmTD10000gF16          0.819  13.003  322.17  0.9081   78.28
mizar/experiments04/rfbSNullmTD16gF16             0.819  13.006  322.10  0.9080   78.24
mizar/experiments04/rfbSNullmTD2gF16              0.800  12.729  337.28  0.8995   84.85
mizar/experiments04/rfbSNullmTD32gF16             0.819  13.002  322.15  0.9081   78.28
mizar/experiments04/rfbSNullmTD4gF16              0.807  12.832  330.81  0.9034   82.06
mizar/experiments04/rfbSNullmTD8gF16              0.817  12.979  324.24  0.9072   78.93
~~~

The problem here is that the parameter mTD (maximal tree depth) does not get
sorted properly. The solution is to use the awesome `sort -V` command, which
gets its `-V` from version number sorting.

~~~
$ grep "Null.*gF16" experiments04.txt | sort -V
mizar/experiments04/rfbSNullmTD0gF16              0.793  12.645  341.69  0.8964   86.45
mizar/experiments04/rfbSNullmTD1gF16              0.796  12.668  340.88  0.8973   86.36
mizar/experiments04/rfbSNullmTD2gF16              0.800  12.729  337.28  0.8995   84.85
mizar/experiments04/rfbSNullmTD4gF16              0.807  12.832  330.81  0.9034   82.06
mizar/experiments04/rfbSNullmTD8gF16              0.817  12.979  324.24  0.9072   78.93
mizar/experiments04/rfbSNullmTD16gF16             0.819  13.006  322.10  0.9080   78.24
mizar/experiments04/rfbSNullmTD32gF16             0.819  13.002  322.15  0.9081   78.28
mizar/experiments04/rfbSNullmTD10000gF16          0.819  13.003  322.17  0.9081   78.28
~~~

That's the way I like it! Well, almost, because for plotting, I am only
interested in the parameter `mTD`. So I would like to cut away everything else.
The solution is non solum, sed etiam `sed`:

~~~
$ grep "Null.*gF16" experiments04.txt | sort -V | sed "s:mizar/experiments04/rfbSNullmTD::" | sed "s/gF16//"
0              0.793  12.645  341.69  0.8964   86.45
1              0.796  12.668  340.88  0.8973   86.36
2              0.800  12.729  337.28  0.8995   84.85
4              0.807  12.832  330.81  0.9034   82.06
8              0.817  12.979  324.24  0.9072   78.93
16             0.819  13.006  322.10  0.9080   78.24
32             0.819  13.002  322.15  0.9081   78.28
10000          0.819  13.003  322.17  0.9081   78.28
~~~



11.12.2014
==========


Flatten function
----------------

I tried to improve the linear flatten function, with success. The idea is still
that errors on higher nodes should be punished more severly, but the punishment
is now much "softer"
To see the punishment visualised, type the following in Gnuplot, which will
plot the error weight as function of the error node depth:

~~~
set xrange [0:10]
plot 0.75 + (1-0.75)*(x / 10)
plot 1    - (1-0.75)*(x / 10)
~~~

The new options are now called "ascending" and "descending". In the ascending
case, we punish errors at the top nodes more than errors at the bottom nodes,
where descending works the other way around.
In experiments, the ascending case seems to work better.



Error weight
------------

The error weight seems to have a greater potential for improvement than I have
initially expected. Results below for RF with 200 eval values, KNN as internal
predictor, one tree with all samples (sample freq = 1), maximum tree depth 1
and changing error weight:

~~~
                                        Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200KNNnT1sF1mTD1eW0.0    0.951  15.193   79.42  0.8374   26.51
mizar/logs/rf200KNNnT1sF1mTD1eW0.1    0.952  15.203   76.52  0.8431   26.01
mizar/logs/rf200KNNnT1sF1mTD1eW0.2    0.952  15.203   76.51  0.8435   25.97
mizar/logs/rf200KNNnT1sF1mTD1eW0.3    0.962  15.380   74.49  0.8532   24.20
mizar/logs/rf200KNNnT1sF1mTD1eW0.4    0.965  15.428   72.71  0.8550   23.88
mizar/logs/rf200KNNnT1sF1mTD1eW0.5    0.967  15.460   70.45  0.8571   23.49
mizar/logs/rf200KNNnT1sF1mTD1eW0.6    0.967  15.471   69.49 *0.8597  *23.46
mizar/logs/rf200KNNnT1sF1mTD1eW0.7    0.968  15.492   69.57  0.8595   23.67
mizar/logs/rf200KNNnT1sF1mTD1eW0.8    0.968  15.497   69.56  0.8590   23.87
mizar/logs/rf200KNNnT1sF1mTD1eW0.9    0.969  15.529   69.89  0.8572   24.37
mizar/logs/rf200KNNnT1sF1mTD1eW1.0   *0.974 *15.561  *68.53  0.8555   24.88

mizar/logs/knn200dW0.85               0.975  15.620   69.21  0.8497   26.44
~~~

The strange thing about these results is that AUC and Rank seem to be best when
the error weight (EW) is 0.6, but all other values are best when EW is 1.0.
However, an EW of 1.0 means that we do not punish errors at all, so the
improvement over regular k-NN seems to come only from the confidence biasing.
And that makes a quite big difference, as you can see below in the AUC column:

~~~
                                         Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200KNNnT1sF1mTD1eW1.0cC   0.975  15.620   69.22  0.8498   26.43
mizar/logs/rf200KNNnT1sF1mTD1eW1.0nC   0.974  15.561   68.53  0.8555   24.88
~~~


However, actually that confidence biasing in this case does not say much,
because it is equivalent to just adjusting the error weight to a different
value, because our maximal tree depth is one, so our confidence is constant.




6.12.2014
=========


Exploring confidence
--------------------

We have several ways to calculate confidence:

* Constant: Every tree split has the same confidence, e.g. 1. That is boring.
* Naive confidence: The confidence is derived on how well the split splits
  the set of samples in two, based on feature frequency.
* Gini confidence: The confidence is just the Gini index of the splitting
  feature.

The results for the different confidence calculations are as follows:

~~~
                                           Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200gF32constantConfidence   0.917  14.786  103.51  0.7855   34.65
mizar/logs/rf200gF32giniConfidence       0.918  14.791  103.37  0.7867   34.62
mizar/logs/rf200gF32naiveConfidence      0.922  14.877  102.18  0.7917   34.09
~~~

So it seems that even if we consider the Gini index for feature calculation,
it is better to resort to the naive confidence for confidence calculation.
By the way: The confidence calculation algorithm is configurable, using the
`confidenceAlg' flag in the RF predictor.

Note however that Gini features at the moment seem to make the results mostly
worse, except for AUC. Compare the results above with RF without Gini features:

~~~
                     Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200   0.927  14.797   96.20  0.7825   33.92
~~~


Saffari index = Gini index
--------------------------

After a thorough code analysis, I came to the conclusion that the Saffari index
is in fact equivalent to the Gini index. Because I made some optimisations to
make Saffari index calculation a bit faster than I previously implemented the
Gini index, I completely removed my old Gini index implementation and renamed
the Saffari index to Gini index.


Different k-NN weighting
------------------------

Differently from the previous approach, where I ran k-NN separately on each
tree branch and combined the results afterwards, I followed Cezary's advice
and made k-NN take weights for each individual label. That way, we run k-NN
on all labels from a tree at once, where we just need to flatten the tree
to hand a list of label-weight pairs to k-NN.
This approach is much faster than the combination in the aftermath, and it also
gives better results:

~~~
                                  Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200KNN             0.946  15.037   84.49  0.8211   28.47
mizar/logs/rf200KNNNewCombine   0.956  15.246   81.25  0.8430   25.61
mizar/logs/knn200               0.978  15.668   66.03  0.8691   23.27
~~~

k-NN, we're coming! ;)

It remains to study the effect of the trees on the k-NN prediction such that
the k-NN prediction + trees becomes better than plain k-NN. For this, let us
see the effect of k-NN after only a single tree split.
Furthermore, I noted that switching on Gini features made the prediction
considerably worse. I need to find out whether this is due to a bug in the code
(possibly due to the Saffari index migration?), or due to a bad configuration.



4.12.2014
=========


k-NN + RF
---------

I implemented k-NN in Random Forests. This is done in such a way that k-NN is
run on each tree leaf separately, then afterwards, we combine the individual
k-NN results using a combination algorithm like harmonic mean. For that, we
use as weights the tree punishment factor. That means that a tree leaf which
has been reached with many errors will have a lower impact on the overall
prediction result of the tree, because its k-NN result will have a lower
weight.

The results so far are promising:

~~~
                        Cov    Prec     Rec     AUC    Rank
mizar/logs/rf200      0.927  14.797   96.20  0.7825   33.92
mizar/logs/rf200KNN   0.946  15.037   84.49  0.8211   28.47
mizar/logs/knn200     0.978  15.668   66.03  0.8691   23.27
~~~

However, compared with plain k-NN, we are still behind.


Different tree build stop criteria
----------------------------------

So far, we built trees until the best splitting feature would produce an
empty subtree. We now made it configurable whether we would break on the
first empty subtree, or whether our minimal number of labels on both subtrees
of a split would be sub-logarithmic in the number of labels at the root.
This is especially important when running with the k-NN prediction (see above).



2.12.2014
=========


Haskell k-NN
------------

I implemented k-NN in Haskell, to make use of it in RF, and to better
understand the algorithm.
The C++ version is different in mainly two aspects:
* It uses an "age" variable to ensure that most of the times, values
  predicted with a certain k are not preceded by values which have been
  predicted with a higher k. However, in case that the values with the
  higher k are just numerous enough, they can still precede a value with
  lower k.
  The Haskell version is more strict here; a label predicted with a low k
  can never be preceded by a label with higher k.
* The C++ version uses TF-IDF information, which the Haskell version does not
  do at the moment.

Here are the current results:

~~~
                             Cov    Prec     Rec     AUC    Rank
mizar/logs/knnC++NoTfidf   0.816  12.918  315.91  0.9067   77.34
mizar/logs/knnC++          0.874  13.619  249.75  0.9319   54.41
mizar/logs/knnHaskell      0.803  12.754  320.72  0.8995   81.85
~~~

Because the Haskell version still performs a bit worse than the C++ version
without TF-IDF, I conclude that this is due to the "age" approach mentioned
above.



29.11.2014
==========


Confidence
----------

I implemented some kind of confidence measure, which should indicate us for
each individual splitting feature how bad an error at that point should be
punished. I implemented it for both feature frequency and Saffari index.
Furthermore, we can visualise the results in the tree graph.
At the moment, I just multiply the dependency weight with the confidence and
treat the rest as before. The results show a slight improvement, but not a
substantial one.


k-NN in RF
----------

I now additionally punish in the querying based on how many queried features
a label is lacking. I just count the number of query features and divide that
by the number of query features lacking from the label, let's call that $\delta$.
Then I multiply the final query weight with $\delta$, $\sqrt \delta$, $\delta^2$,
$\delta^3$, $\delta^{1/3}$, $\delta^{1/4}$ etc. It seems that the best results come out
with an exponent very close to 0, e.g. $\delta^{1/10}$.
Still, as before, this makes results a bit better, but not substantially.
An algorithm more closely resembling the actual k-NN might work better.


Increasing nTrees
-----------------

I tried to increase the number of trees in the forest. My results show that
increasing the tree count *together* with sample frequency gives best results,
but again, is this ever frustrating, we still can not compete with k-NN at all.
Man, is this breaking my nuts.



26.11.2014
==========


Saffari index
-------------

I tried to make the Gini index faster. While doing that, I researched which
method is used in the programs written by Amir Saffari:
His first version of Online Random Forests (ORF ^^) seems to use entropy,
while his revamped and newer code uses something like the Gini index. I'm
still not completely sure whether it is exactly the Gini index, but some
calculation parts like p (1 - p) closely resemble Gini.
So I just took his Gini-like implementation, which I called "Saffari index",
and implemented it in our Random Forests program.


Continuous saving of statistics
-------------------------------

The Saffari implementation saves statistics to faster calculate the
Saffari index, most notably which features are associated with which
dependencies, and how often.
I also implemented that in our RF, but happened to hit a pitfall:
Because the Saffari RF has basically a fixed set of features that it evaluates
for each tree node, we can principally take any feature from the samples at
that node. That implies that statistics only make sense if we save statistics
for all features! My hopes were that memory consumption would be lower than
in previous similar experiments where we stored lists of dependencies and where
we would now use maps, but alas, my hopes were not fulfilled: Even after only
evaluating the first 200 theorems of the mizar dataset, I was nearly running
out of memory. The implications and possible solutions are:

* We store statistics, but only for a limited set of features.
  If we need statistics for different features, we have to generate those
  on-the-fly.
* We do not store statistics at all and calculate everything anew.

The second option seems to be the easiest and most practicable one.



21.11.2014
==========


Statistics
----------

To make future additions to the mlstats program easier, I took that program
and rewrote it in Haskell, under the name of `Statistics'. This supports
export of CSV tables and processing of several prediction files at the same
time, which allows for more compact and readable statistics.
What is left to do for the program is to output the positions of predictions
at which a certain dependency has been predicted, and which dependencies were
not predicted at all, on a per-theorem base.


Impromptu meeting with Cezary
-----------------------------

We discussed RF a bit, and we agreed on that until next Friday, I would make
a description of the experiments we performed so far, without giving the
results yet. Furthermore, I should look at why the first predictions of RF
are generally quite bad. Also, whether (and if yes, why) does increasing
the number of trees make the final results worse?



18.11.2014
==========


Surprise, surprise!
-------------------

Replacing Seq by IntMap in the prediction code made the runtime go down from
about 60min to 6min! Additionally, the prediction quality difference between
this version and the unfiltered version is quite small.

~~~
mizar/logs/knn0.8+rf	 Cov: 0.881	 Prec: 13.720	 Rec: 246.44	 Auc: 0.9370	 Rank: 52.14	 Avg: 16.82
mizar/logs/rf		 Cov: 0.711	 Prec: 11.255	 Rec: 444.90	 Auc: 0.8499	 Rank: 124.26	 Avg: 16.82
~~~



17.11.2014
==========


Meeting with Cezary
-------------------

We discussed the current results, and in fact, AUC will increase if we predict
garbage, i.e. dependencies from the future. However, Cov should not. Therefore,
in my current RF filtering of predictions there must be a bug. Probably it is
because my check whether a prediction score is > 0 fill fail if the score is
really small (high punishment!), but should still be higher than 0.
Anyway, this is probably good news, because after fixing the bug, we should
have a quite good prediction quality again! ;)


Combination woes
----------------

It turns out that combining results using the Haskell combinator can give quite
substantial differences!

~~~
mizar/logs/knn0.8+rf_ocaml	 Cov: 0.884	 Prec: 13.819	 Rec: 247.01	 Auc: 0.9371	 Rank: 51.63	Avg: 16.82
mizar/logs/knn0.8+rf_haskell	 Cov: 0.884	 Prec: 13.819	 Rec: 247.07	 Auc: 0.9563	 Rank: 51.64 	Avg: 16.82
~~~

As we can see, the AUC is quite different. Perhaps because combf (OCaml) does
some filtering away of impossible predictions, which is undermined by the fact
that the results for rfBasic (below) combined with knn give the same results
as rf combined with knn using combf.


Logarithmic dependency weighting
--------------------------------

It turned out that considering the number of dependencies for splits would
give relatively bad performance, which is however ameliorated by
weighting dependencies by the logarithmic number of dependencies:

~~~
mizar/logs/rfBasic	 	Cov: 0.701	 Prec: 11.366	 Rec: 471.42	 Auc: 0.8385	 Rank: 133.34	 Avg: 16.82
mizar/logs/rfLogDeplen		Cov: 0.711	 Prec: 11.255	 Rec: 444.90	 Auc: 0.8499	 Rank: 124.26	 Avg: 16.82
mizar/logs/rfNewDeps		Cov: 0.427	 Prec: 8.286	 Rec: 694.06	 Auc: 0.6546	 Rank: 299.77	 Avg: 16.82
mizar/logs/rfNewDeps+logDeplen	Cov: 0.463	 Prec: 8.503	 Rec: 647.07	 Auc: 0.7042	 Rank: 256.24	 Avg: 16.82
~~~

So we see that rfLogDeplen brings the best results.
What do we get when we combine with k-NN?

~~~
mizar/logs/knn0.8+rfBasic	 Cov: 0.884	 Prec: 13.819	 Rec: 247.08	 Auc: 0.9371	 Rank: 51.64	Avg: 16.82
mizar/logs/knn0.8+rfLogDeplen	 Cov: 0.881	 Prec: 13.720	 Rec: 246.44	 Auc: 0.9370	 Rank: 52.14 	Avg: 16.82
~~~

In combination, we get a small decrease for most performance measures
(except for Rec).


Profiling Haskell code
----------------------

To find out more about the mysterious behaviour of the addAtIndices function,
I made a little test program:


~~~ haskell
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq


addAtIndices s = flip $ foldr (Seq.adjust (+s))

bm =
  let len = 30000
      seqs = replicate 32 (Seq.replicate len 0)
      indices = repeat $ [0, 1 .. len]
  in zipWith (\s i -> addAtIndices 1 i s) seqs indices

sum' = foldl' (+) 0

main = print $ sum' $ map (sum' . toList) bm
~~~

Then, I made another one in C++, which should do roughly the same task:

~~~ cpp
#include <iostream>
#include <vector>

using namespace std;

int main() {
  vector<vector<int> > bm(32, vector<int>(30000));
  for (auto& x : bm)
    for (int y = 0; y < x.size(); y++)
      x[y]++;
  
  int sum = 0;
  for (const auto& x : bm)
    for (const auto& y : x)
      sum++;

  cout << sum << endl;
  return 0;
}
~~~



How do they perform?


~~~
mfaerber@mfaerber-laptop:~$ /usr/bin/time ./testc++
960000
0.02user 0.00system 0:00.03elapsed 97%CPU (0avgtext+0avgdata 5044maxresident)k
0inputs+0outputs (0major+1327minor)pagefaults 0swaps
mfaerber@mfaerber-laptop:~$ /usr/bin/time ./testhaskellsequence
960000
2.70user 0.06system 0:02.77elapsed 99%CPU (0avgtext+0avgdata 26328maxresident)k
0inputs+0outputs (0major+49679minor)pagefaults 0swaps
~~~

So as we can see, the Haskell version is several orders of magnitude slower
than the C++ version! Almost all time is spent at addAtIndices ...


So I read about Haskell's arrays, which I previously deemed unnecessary due to
<http://stackoverflow.com/questions/9611904/haskell-lists-arrays-vectors-sequences>.
But I decided to give them a shot anyway. Here is the test program:


~~~ haskell
import Data.Array
import Data.List (foldl')

test = accumArray (+) 0 (1,30000) [(i, 1) | i <- [1 .. 30000]]
sum' = foldl' (+) 0

main =
  let arrays = replicate 30 (assocs test)
      summed = sum' $ map (sum' . map snd) arrays
  in  print summed
~~~


Results:

mfaerber@mfaerber-laptop:~$ /usr/bin/time ./testhaskellarray 
900000
0.06user 0.00system 0:00.07elapsed 98%CPU (0avgtext+0avgdata 10688maxresident)k
0inputs+0outputs (0major+2817minor)pagefaults 0swaps


So this is actually comparable to the C++ version! And the accumArray function
seems to do exactly what we need (the example in the documentation even
mentions the creation of a histogram!), therefore eliminating the need for ugly
array manipulation.




13.11.2014
==========


Progress
--------

I implemented the changes proposed by Cezary. Furthermore, I fixed the
prediction code such that we no longer propose premises we have not seen yet.
The profiling information is still a bit controversial: It says that the
function

    addAtIndices s = flip $ foldr (Seq.adjust (+s))

consumes about 70% of the time, as well as over 80% of memory allocated.
This is surprising, considering that this function should actually not
allocate any memory at all. As soon as the current experiment terminates,
I should try to run this with multiple cores, such that I can be sure that
all forest data is properly deepseq'd. If that does not give me any hints,
I should really make a small benchmark to see if addAtIndices itself is
the culprit of the problem or not.
Finally, I could also ask the master of masters, Mr. Felgenhauer ... ;)



12.11.2014
==========


Presentation's over!
--------------------

Finally --- whew.
Comments from Cezary after the presentation:

* We should try weighting dependencies by the logarithm of the number of
  dependencies.
* For feature frequency and Gini index, we should also consider the
  dependencies of labels.

This is work in progress.



7.11.2014
=========


RF with prediction combinator
-----------------------------

I finally integrated the new prediction combination to RF.
Tests on the isabelle-probability dataset showed harmonic mean to yield
slightly better results than traditional adding of weights, but not
considerably better. What gives clearly worse results is arithmetic mean.
Another thing which seems to clearly increase is the runtime needed when using
the new combinator.


"New" dataset -> cool results
-----------------------------

I used the mizar8-mptp2k dataset to make a full run of the RF predictor,
comparing it with existing k-NN and Naive Bayes. Running RF took about 50mins
to complete (with other tasks running in the background, though). Results:

~~~
rfsmall		 Cov: 0.699	 Prec: 11.328	 Rec: 502.17	 Auc: 0.8692	 Rank: 136.65	 Avg: 16.82
knnlogsmall	 Cov: 0.874	 Prec: 13.619	 Rec: 249.75	 Auc: 0.9319	 Rank: 54.41	 Avg: 16.82
nbayeslogsmall	 Cov: 0.865	 Prec: 13.660	 Rec: 251.77	 Auc: 0.9310	 Rank: 56.99	 Avg: 16.82
~~~

So as we can see, the performance of RF is still inferior to that of k-NN
or Naive Bayes. However, it can still yield good results, when we combine
it with k-NN or Naive Bayes (harmonic mean, new Haskell combinator):

~~~
knnrf0.8	 Cov: 0.884	 Prec: 13.819	 Rec: 247.07	 Auc: 0.9563	 Rank: 51.64	 Avg: 16.82
nbayesrf0.8	 Cov: 0.867	 Prec: 13.723	 Rec: 256.92	 Auc: 0.9513	 Rank: 56.68	 Avg: 16.82
knnnbayes0.8	 Cov: 0.881	 Prec: 13.771	 Rec: 244.54	 Auc: 0.9363	 Rank: 52.31	 Avg: 16.82
~~~

Here, we see that the best combination is k-NN with RF with a weight of 0.8
for the k-NN predictions. This is clearly better than the combination of k-NN
with Naive Bayes. :)




6.11.2014
=========


Prediction combinator
---------------------

I finished the rewrite of Cezary's combf program, which is now just called
`combine`. It is written in such a way that we can use the combination
routines also outside of the `combine' program, which is important for the RF.
Furthermore, it allows for an arbitrary number of prediction files, and it is
hopefully much more readable than the original OCaml program. :)
As a nice side-effect, we get superior performance from this rewrite
(tested by combining knnlog with knnlog, weights of 0.5 and maxRank=1024):

OCaml              : 28sec
Haskell (1 thread ): 28sec
Haskell (2 threads): 19sec


Preparation of RF for prediction combinator
-------------------------------------------

To prepare the migration of a simple addition-based RF combination code
to the more sophisticated one above, I thought about adapting the currently
computed prediction weights to be used in the prediction combinator, instead
of only its order in a list. For this, I considered an adaption of the RF code
such that the best prediction has the lowest weight, instead of the highest
weight ATM. However, I missed dependencies: Currently, each dependency adds a
certain score to a prediction. However, what to do when we have to improve the
score by subtracting from it? In the end, I thought it was probably too much of
a hassle, so I changed it back. We are therefore going to lose some information
by not considering the exact weight of a prediction, but only the order of
labels. Or, alternatively, we could consider the inverse of the score, however,
that would have the disadvantage of producing very small numbers.



3.11.2014
=========


Meeting with Cezary
-------------------

We discussed mainly three TODOs for me:

* At the moment, the query weight is independent of the depth of the tree
  where an error was committed. An intuition is that errors committed at a
  higher tree position should weigh more than an error committed at a low
  position. Therefore, we should play with adapting the query weight as a
  function of tree depth.
* Adding the weights of all identical predictions might not be the best
  method for combining predictions. We should try combining predictions
  more like in the `combf` program, e.g. taking another mean like the
  harmonic one.
* Cezary said: Just go forward and generate the graphs for differing Cov
  parameters, it takes 15min. Let's see about the 15min.

In the light of these items, a timely submission for the FLAIRS conference is
unlikely. Therefore, we might consider submitting it at another conference
(ITP, according to <http://www.inf.kcl.ac.uk/staff/urbanc/itp-2015/> in Nanjing)
in December --- if and only if we get good results.

Furthermore, my most recent attempts to recreate the combf program in Haskell
might not be the wisest thing to do, as the OCaml version already works nicely,
but still, I might study it a bit more to see how the combination of different
predictions exactly works. If, as a side effect, a new Haskell version emerges,
I would not be sad. :)



2.11.2014
=========


Memory consumption reduction
----------------------------

Excessive memory consumption by the Haskell predictor had recently become a
really urging issue, as I could hardly increase the sample frequency above 5,
because even with 5, >3GB of memory were needed, often resulting in
massive swapping and system stalling.
The reason for this behaviour was simple: Saving of feature frequency
statistics in every tree node took massive amounts of memory, and removing
this now results in a memory consumption of <200MB.
What I feared did not come true, namely that the absence of FF statistics
would result in too slow a program. For I noticed that even the label frequency
calculations were carried out relatively fast, so this indicated that the
same might actually hold true for feature frequency. This confirms the famous
statement by Knuth: "Premature optimization is the root of all evil."

Time for 32 trees, sample frequency of 5:

* 1 core : 4min21sec
* 2 cores: 4min05sec

So Haskell parallelisation gives us an advantage again, even if it is not
enormous.



1.11.2014 (All Hallow's Day Special Edition)
============================================


Gini index speedup
------------------

Due to much improved usage of data structures, the Gini index calculation
is now *much* faster, yielding acceptable (especially when compared to before)
prediction times.
The first thing that I noticed was the higher diversity in the trees:
Without Gini index, the top feature of the tree was nearly always tSet.set;
with Gini index, the top feature differs much more.
Furthermore, without any adaption of parameters, Gini index gives us better
results than trees purely built on feature frequency. At the moment, the
improvement is only marginal (AUC from 0.7138 to 0.7265 at nT=32:sF=1),
but perhaps variation of other parameters (and increase of sampling frequency!)
might yield better improvements still.
A very useful feature is the easy variability of number of features for which
the Gini index is calculated, because this greatly influences prediction speed.

At the moment, I'm not particularly motivated to transfer the new Gini index
calculation scheme to C++, because it would take quite some code writing
effort, but I'm even happier that the Haskell version shows good performance
now. :)



31.10.2014
==========


Even more alternative query weighting
-------------------------------------

After a discussion with Thibault, I came to the conclusion that it might
be interesting to study whether a longer path in the tree needs to be
encouraged or not. To do that, I multiplied that weight of a prediction
with (hit + miss) and powers respectively square roots of that expression.
However, it seems that discriminating path lengths did not improve the
overall prediction quality.


Visualisation idea
------------------

Something really cool would be the visualization of the tree querying by
showing "hot" and "cool" zones, namely nodes which have been passed with
higher and nodes which have been passed with lower weight.
However, this would probably take some time to implement.



30.10.2014
==========


Parallelisation observations
----------------------------

I noted that running the Haskell predictor with a different number of cores
gives quite different results whether you run it in evaluation or in 
interaction mode: In evaluation mode (100 samples), the results are:

1 core : 3min30sec
2 cores: 3min50sec

So running with more cores makes performance actually worse!
On the other hand, in interaction mode:

1 core : 56sec
2 cores: 35sec

So here performance is better with more cores.
I suspect the rnf function to be the main culprit here, but I do not
actually completely understand how parMap works. Even after instating
a flag in my data structures to avoid unnecessary evaluation, this does not
significantly reduce the amount of time spent on rnf as shown in the profiler.


Alternative query weighting
---------------------------

To explore other possibilities of querying, I tried to count the times we
took the path as indicated by the splitting feature (hit), as well as
the times we took the opposite path (miss). Then, I calculate the resulting
score for the prediction by:

	(hit / (hit + miss)) ^ c,

where c is a constant (currently called queryWeight).
(In case hit + miss is 0, we just return 1, but knowing that our trees
have depths greater than one, this is a case which should not appear.)

In my first tests, though, this scheme performed not very well. The best
results I got were with relatively high values of c, namely about 50, and
even there, AUC topped at 0.70, whereas the traditional querying would
attain AUCs of about 0.80.
Furthermore, changing other parameters, such as dependencyWeight, did not
seem to have a positive influence on AUC.


Integrating Gini index
----------------------

I reintegrated the Gini index into the decision tree finding. In doing so,
I used Josef's suggestion from Praha to determine naively a candidate set
of best features (exploiting feature frequency), then on this (small) set
calculate the Gini index.
However, the calculation is painstakingly slow: With nTrees = 32,
sampleFreq = 1 (!) and evaluation of the five best naively determined features
with the Gini index, after 15min we still do not have a single suggestion!
The profiling tells us that samplesWithLabel is the culprit of the slowness.
I do not believe that this can be substantially sped up, other than perhaps
by a switching to C++ (again!).



29.10.2014
==========


Searching parameter space
-------------------------

I ran some tests to find good RF parameters. My conclusion so far is that it
is very hard to top the current parameters, which give an AUC of about 0.77.
Interestingly, increasing the number of trees give worse results. My guess
would be that it is necessary to adapt the sample frequency to the higher
tree size, but this has proven to be difficult, because the program
currently does eat up a lot of RAM (with sample freq = 5 about 3GB!),
therefore an increase in sample frequency results in swapping and subsequent
computer freeze.
Running some experiments on colo12 with more RAM could be an option, but atm,
Haskell is not installed there, so I cannot test that immediately.
Running experiments with the C++ version would also be possible, but there,
I would need a recompile when I want to change parameters, so that is
cumbersome.
What I will do for now is to try and see whether I can improve prediction
quality by other means, namely
a) using the Gini index in conjunction with feature frequency, and
b) changing the query weighting scheme.

To justify my claims above, here are my experimental results:

* nT means number of trees
* sF is sampling frequency (default: 5)
* dW is dependency weight (default: 0.8)
* eW is error weight (default: 0.1)

~~~
lognT32sF5		 Cov: 0.653	 Prec: 5.212	 Rec: 656.38	 Auc: 0.7813	 Rank: 220.79	 Avg: 7.86
lognT48sF6		 Cov: 0.631	 Prec: 5.121	 Rec: 664.42	 Auc: 0.7741	 Rank: 227.92	 Avg: 7.86
lognT48sF6dW0.7		 Cov: 0.630	 Prec: 5.101	 Rec: 663.01	 Auc: 0.7743	 Rank: 227.71	 Avg: 7.86
lognT48sF6dW0.9		 Cov: 0.633	 Prec: 5.131	 Rec: 666.88	 Auc: 0.7730	 Rank: 229.07	 Avg: 7.86
lognT512sF10		 Cov: 0.250	 Prec: 0.500	 Rec: 975.00	 Auc: 0.5112	 Rank: 488.58	 Avg: 2.50
lognT512sF5		 Cov: 0.492	 Prec: 4.697	 Rec: 750.71	 Auc: 0.7131	 Rank: 288.73	 Avg: 7.86
lognT512sF5dW0.2	 Cov: 0.542	 Prec: 4.556	 Rec: 730.68	 Auc: 0.7092	 Rank: 292.50	 Avg: 7.86
lognT512sF5dW0.5	 Cov: 0.505	 Prec: 4.727	 Rec: 740.26	 Auc: 0.7174	 Rank: 284.40	 Avg: 7.86
lognT512sF5dW0.7	 Cov: 0.492	 Prec: 4.707	 Rec: 747.19	 Auc: 0.7151	 Rank: 286.72	 Avg: 7.86
lognT512sF5dW0.9	 Cov: 0.471	 Prec: 4.677	 Rec: 755.33	 Auc: 0.7098	 Rank: 292.00	 Avg: 7.86
lognT512sF5dW1.5	 Cov: 0.468	 Prec: 4.657	 Rec: 774.08	 Auc: 0.6941	 Rank: 307.69	 Avg: 7.86
lognT512sF5eW0.09	 Cov: 0.491	 Prec: 4.697	 Rec: 748.63	 Auc: 0.7142	 Rank: 287.60	 Avg: 7.86
lognT512sF5eW0.11	 Cov: 0.472	 Prec: 4.667	 Rec: 753.60	 Auc: 0.7114	 Rank: 290.39	 Avg: 7.86
lognT512sF5eW0.12	 Cov: 0.468	 Prec: 4.657	 Rec: 756.94	 Auc: 0.7097	 Rank: 292.09	 Avg: 7.86
~~~



28.10.2014
==========


Meeting with Cezary
-------------------

After reporting progress on the Flairs article, we agreed that now I should
concentrate on experiments, to get results.
Furthermore, it would be cool to integrate the Gini index into the current
framework, to make it comparable. That should probably happen in the C++
version ...
TF-IDF is also a hot topic.
Also, Cezary suggested to integrate some graphics similar to those in his
unpublished article (received by mail).


Making it faster
----------------

I implemented a check which would cancel tree querying as soon as a certain
number of errors is attained. For example, when we set this maximal error
to 3, we can cut down runtime to about half of the previous runtime, while
not changing the final result too much. A maximal error of 5 does not have
a big effect on runtime, though.
This solution has the downside that when changing the error (previously
punish) weight, the maximal error has to be increased, because otherwise,
the maximal error would counter the effect of the changed error weight.
Therefore, I will just leave this commented out for now.


Multi-threading
---------------

Our programm parallelizes nicely when building trees; here are the numbers
for building 32 trees with sample frequency 5 of isabelle-probability:

* 1 thread : 33.96sec
* 2 threads: 28.11sec
* 3 threads: 20.61sec
* 4 threads: 20.40sec


25.10.2014
==========


Small change, big change
------------------------

Being sick has its advantages. Fever can make you creative.
This morning, while still half asleep, I asked myself whether on a tree update,
I actually check whether there have been any new samples available?
If there are no new samples for the tree to be updated on, then I save myself
a lengthy calculation of the best feature, because nothing will change anyway.
It turns out that I implemented the check neither in C++ nor in Haskell!
In both languages, the change is essentially a one-liner. With huge outcome.

Testing data on 64 trees with sample frequency of 1 on first 100 evaluation
points of isabelle-probability data set:

Haskell (before): 2min19sec
Haskell (after):  1min24sec
C++ (before):         51sec
C++ (after):          14sec (!!)

That makes C++ the clear winner again. :/
However, with this change, the profiling information for Haskell has diverged
from previous experience quite a bit, so there might be a chance that the
Haskell performance can still be quite improved.
Most interestingly, now the Haskell parallelisation seems to pay off much
less, with the one thread version running at 1min32sec, which is only less
than 10% slower than the two-thread version.
Investigation of the phenomenon to come.


Haskell feature parity
----------------------

As of now, the Haskell version of RF supports all the features of the C++
version, plus an amazing command line interface for quick testing of
parameters.
The performance could still be improved, however, I'm not sure whether my
time would not be better spent now on some writing for the article.



22.10.2014
==========


Haskell progress
----------------

In the meanwhile, I finished the interaction part of the Haskell predictor.
It turns out that the parallelisation works quite nicely, as we are able
to almost cut in half the user time when doubling the number of threads
used. That means that the RF algorithm we use scales really well with
increasing processor count.


Optimisation attempt
--------------------

The slowest part of the Haskell implementation by far seems to be the
updating of the feature frequency. Because the feature frequency is
implemented using a Map from FeatureID to Int, I tried replacing it with
a Sequence. However, that had a quite bad impact on the performance, as
the program was now not even terminating in an acceptable time, taking
more than 10 times as long as the Map implementation. The timewaster no°1
seemed to be the finding of the minimal element of a Sequence, followed
by an innocent map over the whole sequence. The performance loss might
be due to the fact that the sequence is very sparsely populated, and
therefore we do many unnecessary operations on it.
Anyway, I changed the code back to Map, and it is fast again.


Comparison of C++ with Haskell speed
------------------------------------

The battle looms upon the horizon. Who will vanquish at this uneven-looking
challenge?
As a matter of fact, using 8 trees with a sampling frequency of 10, we get the
following numbers:

* C++:                 2min06sec
* Haskell (1 thread ): 1min47sec
* Haskell (2 threads): 1min08sec

That makes Haskell the clear winner performance-wise! :)
However, it seems to me that the Haskell predictions are still somewhat strange
when compared to the C++ ones. Perhaps there is still a bug in the query code.
As a next step, I will integrate tree saving into the Haskell RF.



20.10.2014
==========


Redoing things in Haskell
-------------------------

Because introducing new parameters in the C++ version of RF, I envisaged
transferring the RF in the new incarnation to Haskell again, to ease the
introduction of new parameters.
This has proven to take a lot of effort, but at present, the RF implementation
is mostly complete, most of the work left is in the interface, namely the
evaluation and interaction mode, and auxiliary functions like tree loading/
saving, where however we will probably be able to reuse many things from the
old Haskell implementation (at least for tree saving).


Meeting with Cezary
-------------------

We discussed the possibility of submitting the RF results to the FLAIRS-28
conference: <http://www.flairs-28.info/>
The deadline for submitting a paper is on Nov 17.
Until that date:

* Writing an outline for the paper, sending it to Cezary
* Selecting interesting experiments to write about
* Running experiments
* Completing the Haskell implementation

For the paper content:

* Introduction to the problem
* Presentation of exiting methods, like k-NN, Naive Bayes
* ATP community would like to use better methods than those above
* Short (!) introduction to RF
* Overview of existing work -> Agrawal, Saffari
* Why do these methods not work well for ATP?
* What are exactly the requirements of Premise Selection for AI methods?
* Which modifications did we try for RF?
* What are our results?
* Graphs, visualisation?



14.10.2014
==========


Let's start with a cool quote from `fortune` cookies:

> When we write programs that "learn", it turns out we do and they don't.


13.10.2014
==========


Faster tree generation algorithm
--------------------------------

After the new querying method, I thought about a new tree generation method.
In this method, I wanted to reduce the "randomness" of Random Forests a bit,
because up to now, it could have been that a sample was actually never learnt
by any tree, just because it was not drawn, and some samples were learnt more
often than others without any reason.
For this reason, I wanted to make sure that each sample is getting learnt as
often as any other sample.
(A random idea that just came up: Couldn't I learn a sample a certain number
of times depending on how often it has been used as dependency? But, on the
other hand, I would have to know before how often it will be used as
dependency, which I cannot, in general. Hmmm, I have to think about this ...)
Anyway, in the new scheme, I do not have to completely rebuild a tree if it
has to integrate a new sample, but I just update its statistics, and if it
seems that the new samples will require a rebuilding of the tree (because the
previous best splitting feature has been exceeded by a new splitting feature),
only then do we rebuild the tree.
This allows for a much faster tree building and, as it seems, a higher tree
quality too, though it seems that we still need to tweak some tree building
parameters.
Newest results with following parameters:

* Sampling frequency = 10
* Dependency weight = 0.25
* Punish weight = 0.05
* Cheating

~~~
rforestlogw0.25	 Cov: 0.647	 Prec: 4.664	 Rec: 515.98	 Auc: 0.7945	 Rank: 207.39	 Avg: 7.49
~~~

Because many of these parameters are not yet configurable with the command
line and I'm slowly running out of command line parameters anyway, I want to
implement a new command line interface which allows interpretation of
predictor-specific options directly in the predictor and independent from the
main file, similar to MEncoder's codec parameter interface.
Also, the new tree generation code is a bit messy --- cleaning it up is
difficult, though, and I am tempted to give ol'Haskell another go to see if
I can't match the C++ performance after all with more efficient
data structures ... most notably also considering parallel programming!


Small change, big change
------------------------

In the prediction code, there was the line:

    ans[label.first].second = label.second;

This assigned the answer weight of the current label to be the weight of the
current label. However, couldn't it happen that this might overwrite a previous
answer weight of the current label? It could be that it this is never the case
(because intuitively, labels should be chronically ordered, so a label should
never appear before this line), but Probieren geht über Studieren, so I
replaced the line by:

    ans[label.first].second += label.second;

And, at least in cheating mode, we get an increase from AUC 0.7411 to 0.7945.
Wohoo! It remains the question whether we get similar increases also in
non-cheating mode --- to be examined.



8.10.2014
=========


No cheating
-----------

Without cheating, a run takes 1h3min. The results are fortunately only slightly
worse than with cheating:

~~~
rforestlogw0.5_nocheat	 Cov: 0.540	 Prec: 3.808	 Rec: 695.12	 Auc: 0.6989	 Rank: 302.46	 Avg: 7.49
~~~



7.10.2014
=========


Results
-------

I made some tests with 256 trees, 512 samples, punish weight of 0.05 and
differing dependency weights.

~~~
rforestlogw0.125	 Cov: 0.468	 Prec: 3.009	 Rec: 701.02	 Auc: 0.6822	 Rank: 319.07	 Avg: 7.49
rforestlogw0.25		 Cov: 0.523	 Prec: 3.541	 Rec: 689.93	 Auc: 0.6991	 Rank: 302.20	 Avg: 7.49
rforestlogw0.5		 Cov: 0.549	 Prec: 3.864	 Rec: 687.56	 Auc: 0.7095	 Rank: 291.94	 Avg: 7.49
rforestlogw0.75		 Cov: 0.551	 Prec: 3.939	 Rec: 693.45	 Auc: 0.7092	 Rank: 292.27	 Avg: 7.49
~~~

As we can see, AUC is better than with any RF implementation I tested so far.
However, keep in mind that this result was achieved with "cheating", meaning
that for performance reasons, I train the trees on all data only once, then
query them for each theorem I would like to have a suggestion for.
Prediction without cheating turned out to be quite slow on my home computer,
taking about 30min for 30MB of predictions.


New parsers
-----------

Having realised that regular expressions in C++ do not seem to be suited to
parse more complex expressions efficiently, I removed the regular expression
parts, but rewrote the symbol parsing code completely with iterators, with
much more error checking.
So ends this little excursion into the wonderful, but slow world of regexps.
Using the new parser, we only need about 2sec from starting the program to
loading 256 trees and getting a suggestion. Very handy for testing. ;)


New tree querying
-----------------

The new trees, which very frequently exhibit sets of labels at the leafs of
cardinality 1, should be queried differently from the way we queried until now,
to avoid getting too few labels from the trees.
To do that, we now do not only consider the positive path along the tree, but
also all negative paths, and each time we take a negative decision, we weight
the labels on that path with a small factor, which I called "punish weight".
First experiments showed that with a punish weight above 0.1 (!), suggested
labels will predominantly contain irrelevant labels, such as "Subset.setI" if
we query for only "yHOL". On the other hand, a punish weight of 0.05 seems to
provide a good compromise between general suggestions and specific ones,
as the first ten suggestions give mostly labels from HOL, but also some not
from HOL.



6.10.2014
=========


New insights
------------

I got a new insight when I tried the interaction mode, and noted that upon
just typing "yHOL" as only feature, I did not get any HOL-related suggestions
among the first suggestions. Why was that the case? Looking at the trees,
I quickly realised that "grep yHOL forest/*.dot" did not bring up any results,
meaning that the trees didn't know anything about yHOL. That is clearly bad,
because if we know about a theorem only that it might have something to do
with HOL, we cannot suggest anything of relevance.
The reason why we did not know about yHOL in the trees is that I filtered away
features which would not appear very often and would therefore not provide any
good tree split. However, if we ignore the notion of a "good" tree split, we
might just continue building a tree until all features of all the samples used
in tree building have appeared. That means of course some kind of overgrowing
of trees, which has the disadvantage that when we query, we risk getting only
very few suggestions per tree. We can however remedy this by remounting the
best suggestion, and considering alternative paths, weighted with the error
we introduce that way. That way, we get more (and hopefully better)
suggestions.

At least some initial experiments confirmed my theory: With naive feature
selection and no minimal label count (min_labels = 1), we get very unbalanced
trees, but the suggestions for "yHOL" actually yield many HOL-related theorems
among the first suggestions. Furthermore, the building of trees is now
blazingly fast again, building 256 trees with 512 features in only about 42sec.
That leads me to believe that we might get better results with naive feature
selection, just because it is more fun to test new results that way. :)
With Gini feature selection, it is hard to even build so many trees with
so many samples in acceptable time.


Tree loading performance
------------------------

Now that building trees can be considered fast, the loading of
already-built trees is becoming a bottleneck, and considering my results for
symbol loading with <regex>, it is probable that writing a custom parser
would drastically decrease speed. ATM, loading a forest that took 42sec to
build takes 28sec to load, which is IMO inacceptable. So I will look again
into speeding up the <regex> parser, and if that fails, will write a custom
parser after all. :(



5.10.2014
=========


Tree loading and <regex> performance
------------------------------------

In my experiments, I noted I often had to change parameters in the RF code
which would impact only querying, but not creation of trees. Because every
code change would require a complete rebuild of the RF for nothing, I decided
to implement tree loading.
This is done by parsing the DOT files already generated for visualization.
For parsing, I used C++11's <regex>, which is however only available starting
from G++ 4.9. Due to good experiences with the parsing approach (simple code),
I also converted input_sym_line() to use <regex> functions, but when I tried
to convert also its bigger brother, read_syms(), I quickly found the
performance of the <regex> functions to be insufficient for loading big data.
My experiments showed that parsing a file which takes <1sec for the traditional
hand-rolled parser takes about 50sec for the <regex> version.
A possible remedy for this could be not using regex_match(), but something like
regex_token_iterator (because less data will be copied in that approach), but
I was not able to use that due to confusing arguments, which made extraction of
several instances of the same capturing group not only difficult,
but impossible. Something like: `(\w)+`

If a solution for that problem can be found with regex_token_iterator,
it is possible to adapt the existing code from the repository, which can be
found in revision 6ca0a87a35e4e4ff2c1440693cfff3e016cb349b. I will now delete
it from the main code with the very same commit as for this note.

Note: <http://www.regexr.com/> has been very useful in creating the regexps used
for parsing.


3.10.2014
=========


TF-IDFs for labels
------------------

We now record TF-IDF for each label. It seems, however, that TF-IDF information
does not make a big difference in the quality of the predictions. I tried
combining TF-IDF in several ways with the querying, for example dividing each
dependency's weight by the dependency's TF-IDF, or its exponential, also
doing the same for each label itself, also with exponents, sometimes also
exponents different from the dependency's exponent. All that did not
significantly improve the prediction.
At that point, I was led to believe that this might be because the label
suggestions without the dependencies were too weak and did not work correctly.
I am now going to investigate whether lower trees might help with that problem.


Optimizing n_of_samples_with_label
----------------------------------

As profiling showed, about 80% of runtime is spent in the function
n_of_samples_with_label, mostly to search through the dependencies of a theorem
to see if it contains a label. As the dependencies are usually stored as
vector, I tried to replace it with set and unordered_set.
The results, however, are a bit disappointing, as both set and unordered_set
do not seem to substantially change runtime. That might be due to the fact that
most theorems do not have a sufficiently large number of dependencies for which
sets would have a big pay-off.

Caveat: When you do profiling with gprof, recompiling the binary may change
your profiling output in retrospective without any warning!



1.10.2014
=========


Meeting with Cezary
-------------------

In our meeting, Cezary told me that he had not used TF-IDF for labels yet,
mostly because there was no reason for it, as k-NN and Naive Bayes would not
always propose very similar labels. For that reason, I decided to try TF-IDF
for labels.

Furthermore, Cezary told me that one could make a graph for Cover values of
different parameter values, with the possibility that RF might have some point
where they might outperform other predictors.

Also, Cezary proposed to send me more (and smaller) test data, stemming from
Mizar.

Overall, Cezary mentioned that I might want to read more literature and make
less experiments, as it seems to be usual for first-year PhD students. I could
not agree more. But first, let's do some more experiments. :)



30.9.2014
=========


Results
-------

Results won with the Gini index method also seem to suffer from a certain
abundance of Set.subsetI and Set.mem_Collect_eq. We should really consider
using TF-IDF of theorems along TF-IDF of features.
Speaking of which, we currently do not use TF-IDF of features at all. Is
that a thing to change perhaps?
Anyway, with 32 trees, 256 samples and 256 features, the results are:

rforestlog	 Cov: 0.302	 Prec: 2.702	 Rec: 948.34	 Auc: 0.4257	 Rank: 575.18	 Avg: 7.49

The needed time is 25min40sec. We should do some more profiling to see where
the new bottlenecks are.


Valgrind
--------

I made some tests with Valgrind/Callgrind to profile the C++ program.

valgrind --tool=callgrind ./predict [...]

This creates a file callgrind.out.$PID, where $PID is the process ID.
While there exists a nice tool to display the debugging information generated,
the debugging process is a rather lengthy one, as Valgrind seems to run the
program inside some kind of VM, which, according to the author, makes execution
about 50 times (!) slower.


Feature subsampling
-------------------

After implementing feature subsampling, I noted that using it seemed to often
yield slower runtime than without! It turned out that the feature subsampling
routine is relatively slow, which is why I currently only subsample features
at the beginning of building a tree and not at every tree node.
We basically have two options:
Either we do feature subsampling at every tree node (while reducing number of
features to keep the computation time down), or we filter out the less frequent
features, perhaps by using a probability-weighted feature subsampling scheme.


Data structure comparison
-------------------------

I suspected the labels_of_samples() function to be the main performance
bottleneck, so I tried different datatypes.

~~~ cpp
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <set>

int main()
{
    std::unordered_multiset <int> s;
    //std::multiset <int> s;

    for (int i=0; i < 10000000; i++) s.insert(i);
    for (int i=0; i < 10000000; i++) s.insert(i);

    std::unordered_map <int, int> m;
    //std::map <int, int> m;

    for (int i=0; i < 10000000; i++) m[i]++;
    for (int i=0; i < 10000000; i++) m[i]++;
}
~~~


The results were as follows:

Container          |     Time
:------------------|--------:
unordered_map      |  7,96sec
unordered_multiset | 11,39sec
map                | 26,56sec
multiset           | 26,79sec

So the winner is: unordered_map!
Using unordered_map in labels_of_samples() did however not yield such
impressive speed-ups (data measured by training 4 trees, each on 128 samples):

* multiset: 12,36sec
* unordered_map: 11,03sec

Still, it is a respectable performance gain of ~10%.
Out of curiousity, I also tried plain map, but that increased runtime to
13,77sec. unordered_multiset was not an option, as it does not have the
function upper_bound(), which is important to iterate only through unique
keys of the multiset.



29.9.2014
=========


Gini index
----------

I got the Gini index working; the bug was that I considered only unique labels
of samples, while the Gini index considers how often a label appears in a
sample.
Furthermore, I tried several optimisations (all tried without feature
subsampling yet):

* First, without optimizations, the runtime for building 10 trees with 512
  samples was at 9min32sec.
* Then before calculating the Gini index of a feature, I first check whether
  this feature will have a chance of providing a valid split, by checking
  whether the number of samples with/without the feature is below the
  min_labels threshold.
  This brings down the runtime to 4min45sec.
* Finally, I noted that when a label appears several times in a sample set,
  the Gini index calculation calculates the same thing thing for each
  occurrence of the label. Thus I converted labels to a multiset, which
  allows for checking how often a label appears overall and adapting the
  calculation appropriately.
  This brings down the runtime to 4min26sec.

I also converted the feature frequency vector to a multiset, as most features
will never appear, therefore reducing memory and time consumption.

During the tests, I also enabled profiling (via GCC flag "-pg"). It turned
out that unlike the Haskell profiler, profiling C++ code massively increased
runtime.



28.9.2014
=========


Fixing a bug
------------

I found out why the performance of the C++ Random Forest implementation was
so inferior to the Haskell version: In the C++ version, labels are encoded
differently (namely dependencies of theorems are not stored with the label
itself in one array), so when determining when to stop building the tree,
we probably stopped building too early, because we did not take the dependencies
into account.

Whew, that bug was hard to find! Still, it was not the last one --- it rests to
find out why the Gini index does not get calculated correctly in the
C++ version.



25.9.2014
=========


Dependency weighting
--------------------

I noted that in k-NN, dependencies of theorems are weighted according to the
number of dependencies of a theorem. So I tried to implement the same scheme
in Random Forests.
However, it seems that this does not make a big difference in the end.
I got the best results by weighting each dependency with the constant factor
of 1.7:

rforestwc1.7	 Cov: 0.307	 Prec: 2.845	 Rec: 915.77	 Auc: 0.4780	 Rank: 523.00	 Avg: 7.49

When using non-constant weights à la
  dependency_weight = constant / |dependencies|,
my best results were:

rforestwd7	 Cov: 0.287	 Prec: 2.364	 Rec: 916.50	 Auc: 0.4612	 Rank: 539.52	 Avg: 7.49
rforestwd8	 Cov: 0.288	 Prec: 2.367	 Rec: 916.43	 Auc: 0.4616	 Rank: 539.10	 Avg: 7.49
rforestwd9	 Cov: 0.287	 Prec: 2.367	 Rec: 916.96	 Auc: 0.4614	 Rank: 539.27	 Avg: 7.49

(Everything was measured with 128 trees / 128 samples per tree.)

So here a factor of 8 seems to do the trick, but still, the results
are a bit worse than the constant factor solution above.

In both variants above, we have the problem that the best solutions very
frequently contain the same first suggestions, most notably "Set.subsetI" and
"Set.mem_Collect_eq".
This is probably because "Set.subsetI" appears very often in the dependencies
of theorems. One way to remedy this could be to consider not only the TF-IDF
of features, but also of theorems.



24.9.2014
=========


New command-line interface
--------------------------

To make testing different configurations easier, I rewrote the command-line
interface of the C++ predictor.


Tree visualisation
------------------

It is now also possible to visualise trees from the C++ predictor.


TF-IDF integration
------------------

I also tried to consider TF-IDF data by looking at which features have been
traversed in a tree when finding labels, and multiplying respectively adding
the corresponding TF-IDF values for the traversed features. However, for
many trees the resulting TF-IDF values are very similar, which could indicate
the similitude of many trees.
To research this, I will inspect some trees visualised.


23.9.2014
=========


C++ implementation
------------------

I reimplemented Random Forests in C++, for which I modified the predictor
interface to support efficient learning of multiple datapoints.
The speed increase is considerable: With 512 trees and 512 samples/tree e.g.,
the performances are as follows:

* Haskell: 30min32secs
* C++:      5min00secs

We therefore see a speed-up with a factor of about 6. :)
However, there probably still is a bug in the C++ implementation, as the C++
implementation yields worse results than the Haskell one.
Investigation is underway. :)

haskell:	Cov: 0.529	Prec: 3.696	Rec: 635.880	Auc: 0.699	Rank: 300.110	Avg: 7.490
C++:		Cov: 0.367	Prec: 3.054	Rec: 784.28	Auc: 0.5764	Rank: 424.64	Avg: 7.49

(Both were measured with eigenweight = 5, 512 trees and 512 samples/tree.)


Eigenweight results
-------------------

With 512 trees and 512 samples/tree using our C++ implementation, we looked
at the effect of the eigenweight. Here are the results, where e.g.
"rforestlog3" means an eigenweight of 3.

rforestlog1	 Cov: 0.356	 Prec: 3.212	 Rec: 787.24	 Auc: 0.5876	 Rank: 413.60	 Avg: 7.49
rforestlog2	 Cov: 0.362	 Prec: 3.230	 Rec: 771.59	 Auc: 0.5989	 Rank: 402.29	 Avg: 7.49
rforestlog3	 Cov: 0.365	 Prec: 3.216	 Rec: 773.05	 Auc: 0.5944	 Rank: 406.78	 Avg: 7.49
rforestlog4	 Cov: 0.368	 Prec: 3.147	 Rec: 777.58	 Auc: 0.5859	 Rank: 415.14	 Avg: 7.49
rforestlog5	 Cov: 0.367	 Prec: 3.054	 Rec: 784.28	 Auc: 0.5764	 Rank: 424.64	 Avg: 7.49
rforestlog6	 Cov: 0.364	 Prec: 2.975	 Rec: 789.11	 Auc: 0.5675	 Rank: 433.46	 Avg: 7.49

In this table, we see that most of the time, an eigenweight of 2 yields the
best results, except for Cov, where an eigenweight of 4 seems to be optimal.



19.9.2014
=========


Random forest results
---------------------

At the moment, the results of the Random Forest implementation are not very good. Here they are:

n_samples128n_trees256:		Cov: 0.361	Prec: 2.726	Rec: 789.930	Auc: 0.566	Rank: 432.810	Avg: 7.490	
n_samples512n_trees128:		Cov: 0.482	Prec: 3.358	Rec: 480.290	Auc: 0.614	Rank: 260.240	Avg: 7.490	
n_samples512n_trees512:		Cov: 0.529	Prec: 3.696	Rec: 635.880	Auc: 0.699	Rank: 300.110	Avg: 7.490	
n_samples512n_trees1024:	Cov: 0.545	Prec: 3.779	Rec: 624.860	Auc: 0.708	Rank: 293.080	Avg: 7.490	
n_samples128n_trees128:		Cov: 0.315	Prec: 2.375	Rec: 637.200	Auc: 0.486	Rank: 391.070	Avg: 7.490	
n_samples256n_trees128:		Cov: 0.390	Prec: 2.816	Rec: 549.500	Auc: 0.548	Rank: 315.600	Avg: 7.490	
n_samples256n_trees1024:	Cov: 0.517	Prec: 3.661	Rec: 647.560	Auc: 0.691	Rank: 310.240	Avg: 7.490	
n_samples512n_trees256:		Cov: 0.511	Prec: 3.550	Rec: 606.800	Auc: 0.676	Rank: 295.800	Avg: 7.490	
n_samples256n_trees512:		Cov: 0.482	Prec: 3.426	Rec: 679.670	Auc: 0.667	Rank: 334.700	Avg: 7.490	
n_samples128n_trees1024:	Cov: 0.459	Prec: 3.398	Rec: 705.940	Auc: 0.647	Rank: 353.870	Avg: 7.490	
n_samples256n_trees256:		Cov: 0.433	Prec: 3.102	Rec: 689.100	Auc: 0.626	Rank: 360.070	Avg: 7.490	
n_samples128n_trees512:		Cov: 0.403	Prec: 3.052	Rec: 744.680	Auc: 0.610	Rank: 391.420	Avg: 7.490	


Furthermore, even if we combine the best results (512 samples, 1024 trees, which took about one hour to calculate)
with the results of k-NN, we get an AUC of only very little over 0.92. This is easily beaten by a combination of
k-NN with Naive Bayes, which gives as an AUC of a bit more over 0.92. Considering that Naive Bayes takes much less
runtime than our Random Forests, that is a clearly negative result for RF.

I estimated the impact of considering TF-IDF data in RF would have, namely by not considering it in k-NN.
If you replace the TF-IDF weights by a constant factor 1 in k-NN, its results become only marginally worse,
such that I find it probable that adding TF-IDF to Random Forests would not greatly improve its results,
coming close to those of k-NN or Naive Bayes.


What direction to take?
-----------------------

I read some chapters of "Artificial Intelligence: A Modern Approach" (Russell, Norvig), and think that
Deep Learning and Support Vector Machines could be interesting approaches to try on our data.

Therefore, I did some research as to which degree ready-made solutions implementing the above approaches
do exist, in particular for Haskell. Seeing that the package HLearn combines several popular machine learning
techniques, I corresponded with its author, Mike Izbicki, who pointed out that his HLearn package still had
quite inferior support of machine learning methods. This had to be partially attributed to the relatively
bad performance of hmatrix, which yields performance about 100% inferior even when compared to Python bindings.

Then, I checked out Haskell's bindings-svm, which is a binding for Support Vector Machines. That package
has the disadvantage of having a gruesome interface, only barely hiding its roots in C.
An alternative Haskell package svm is implemented in pure Haskell, but seems quite limited due to relatively
rigid assumption on the structure of the dataset (which is somehow limited to Doubles, and I doubt that
we can use it on our theorem proving dataset.



12.9.2014
=========


Meeting with Cezary
-------------------

Having discussed the previous statistics I created, we agreed on a new kind of statistics:
This should show on the x-axis the number of occurences n of a feature, and on the y-axis the number of features which occur n times.
The new plot has been created with the command:

    gnuplot -e "set terminal png; set logscale xy; plot 'histogram.txt'" > histogram.png

This plot should help us in deciding at which point we cut off the long tail. (If I understood correctly.)

Furthermore, given the new results of the predictor, we should check their complementarity with the results of other provers.
This should be done with the `combf` program.

When playing around with the parameters shows sufficiently good results, I should send Cezary (who's going back to Praha) a draft of a paper structure.


Further unification
-------------------

Mr. Schaper contributed to fixing the predictor interface, which is now nearing completion.


Current results
---------------

~~~
$ ./evaluate_results.hs | sort
n_samples100:	Cov: 0.240	Prec: 1.949	Rec: 179.883	Auc: 0.195	Rank: 148.010	Avg: 7.490	
n_samples25:	Cov: 0.194	Prec: 1.723	Rec: 159.200	Auc: 0.150	Rank: 135.460	Avg: 7.490	
n_trees100:	Cov: 0.207	Prec: 1.679	Rec: 567.890	Auc: 0.346	Rank: 396.373	Avg: 7.490	
n_trees25:	Cov: 0.205	Prec: 1.783	Rec: 184.270	Auc: 0.168	Rank: 155.327	Avg: 7.490	
n_trees50:	Cov: 0.177	Prec: 1.526	Rec: 326.820	Auc: 0.243	Rank: 254.960	Avg: 7.490
~~~

As we can see, increasing the tree size seems to have a much greater impact than increasing the sample size.



11.9.2014
=========


Parallelisation
---------------

I researched whether our program as it stands could be easily parallelised, but it seems to take more effort.
This is mostly due to the fact that we calculate our trees in a monad, which is not easily parallelisable.
Furthermore, we would probably have to derive from NFData to be able to use "parMap rdeepseq", and I don't think it's worth the effort for now.


Generic predictor interface in Haskell
--------------------------------------

What has been done in the C++ version of the predictor should also be done in the Haskell version.
Consulting Mr. Schaper has brought a breakthrough in the design of the predictor interface, which now allows for arbitrary configuration types.
This means that each predictor can parse its settings itself and store it in an arbitrary structure!



10.9.2014
=========


Statistics
----------

I created a statistics function which roughly outputs the average overall appearance of the most popular features of all samples.
First, for each sample, we sort its features by how often they appear in total in all samples.
Then, ... bleh, I'm too lazy to explain that, look at the code. :)
We visualised the data with: gnuplot -persist -e "plot 'frequency_stats.txt' with linespoints"
And saved the data with: gnuplot -e "set terminal png; plot 'frequency_stats.txt' with linespoints" > frequency_stats.png
Strangely, the resulting graph showed several peaks, but in general behaved as expected.


Naive splitting criterion
-------------------------

In addition to the Gini index, I implemented the splitting based on how well any feature splits the samples into two as most as possible equally sized sets.
The results are quite encouraging, with the following parameters:

* Samples:  1000
* Trees:    200
* Seed:     1

~~~
$ eval/mlstats WTF "test/deps" "test/seq"
WTF	 Cov: 0.552	 Prec: 3.859	 Rec: 462.06	 Auc: 0.6653	 Rank: 231.44	 Avg: 7.49
~~~

That's better than anything we ever produced with Gini index. :)
Furthermore, runtime is much reduced, which is why can now even oversample the dataset. ;)
(200 * 1000 = 200 000, which means that if we have 20 000 samples to learn, then we learn each sample by average 10 times!)


RNG
---

Furthermore, I tested Haskell's RNG:


~~~ haskell
import Data.Random.Distribution.Uniform
import Control.Monad.Trans.State.Lazy (runState)
import Data.RVar (sampleRVar)
import System.Random (mkStdGen)
import qualified Data.IntMultiSet as IMS
import Data.Random (RVar)

let l = repeat $ uniform (0::Int) 10000
let result = (runState . sampleRVar) (sequence $ take 1000000 l) (mkStdGen 100)
let ms = IMS.fromList $ fst result
let res = zip [1..] $ map snd $ IMS.toOccurList ms
writeFile "rng_stats" $ unlines $ map (\ (i, v) -> show i ++ " " ++ show v) res
~~~


(Copy-paste into GHCi to write a file "rng_stats".)
Visualisation via:

    gnuplot -e "plot 'rng_stats'" -persist
