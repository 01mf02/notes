10.04.2017
==========


Total number of inferences for monteCoP
---------------------------------------

I calculated the total number of inferences for problems for monteCoP.
As the log files of monteCoP contain the required information on several lines,
it is not trivial to obtain the required fields with `sed`.
However, with the help of Mr. Prokoš, I was able to come up with
a Perl solution:

    for i in `cat problems`; do
      perl -0777 -ne 'm/UCTInf: ([0-9]+).*Inf: ([0-9]+)/s; print (($1+$2) . "\n")' $i;
    done | awk '{s += $1} END {print s/NR}'

An explanation of the Perl parameters:

* The parameter `-0777` slurps in whole files at once,
  which makes matching across multiple lines possible.
* The switch `-n" does not print matches.
* The starting character `m` of the regular expression stands for "matching".
* The ending character `s` of the regular expression enables
  matching of newlines like any other character via the dot.

Reference: <http://perldoc.perl.org/perlrun.html>

To obtain the intersection of problems, I created an improved version
of the script from 23.02.2017 that does not display the total amount
of files on every line:

~~~ bash
intersect() {
    sort "$@" | uniq -cd | grep "^[^0-9]*$# " | sed 's/\s*[0-9]* //'
}
~~~

This script was used to obtain the file `problems`.
The results on the intersection of solved problems are:

Prover   |  Avg. total inferences / problem
-------- |  ------------------------------:
monteCoP |                          20243.9
lazyCoP  |                          21698.4

This is promising ...



Checkpointing OCaml with Docker
-------------------------------

To have more robust checkpointing with HOL Light, I played with Docker.
To install it, add the repository as shown by
<https://store.docker.com/editions/community/docker-ce-server-ubuntu>.
Then install Docker and the checkpointing framework CRIU with:

    sudo apt install docker-ce criu

To enable CRIU support for Docker, add to `/etc/docker/daemon.json`:

    {
        "experimental": true
    }

Then, create a `Dockerfile` with the following contents:

    FROM ocaml/opam:alpine_ocaml-4.02.3
    RUN opam depext -i camlp5
    CMD ocaml

In the directory of the `Dockerfile`, run:

    docker build -t docker-whale .
    docker run -i --name ocaml docker-whale

This should build an image with an open OCaml toplevel.
You could now do something like `#use "hol.ml";;` and then run

    docker checkpoint create ocaml test

to checkpoint the current state.
To resume the checkpoint later, run:

    docker start -i --checkpoint test ocaml

More information: <https://docs.docker.com/engine/getstarted/step_four/>.

Running OCaml with CRIU alone worked for me only when giving CRIU root access,
unfortunately. Work seems to be underway: <https://criu.org/User-mode>.
More easy to install, LXC might be an alternative to Docker.



23.03.2017
==========


nanoCoP optimisations
---------------------

I worked on following optimisations and measured their impact on
the number of inferences for the problem yellow_6__t13_yellow_6.p
with a timeout of 30s:

Optimisation               |     Inf | Depth |    DInf
-------------------------- | ------: | ----: | ------:
Start                      |  211574 |     4 |  176006
Array substitution         | 1414505 |     5 |  671766
Optimised groundness check | 1638797 |     5 |  896058
Unit list                  | 1790859 |     5 | 1048120

The last two optimisation took about 1.5h today and they
alone improved the number of inferences by 26.6%!

I detected bottlenecks using `gprof`:

    timeout 30 ./nanocop.p.native eval/bushy/yellow_6__t13_yellow_6.p
    gprof ./nanocop.p.native | gvim -



16.03.2017
==========


À la recherche du `camlp5` perdu
--------------------------------

Proust would be happy: You can find `camlp5` via:

    ocaml -I `camlp5 -where` camlp5o.cma

Courtesy of: https://github.com/jrh13/hol-light/issues/29



15.03.2017
==========


Problems with `camlp5`
----------------------

On the server, the versions of OCaml and `camlp5` were conflicting,
because I did install my own version of OCaml, but not of `camlp5`.
Consequently, I needed to install the right version of `camlp5`.

    opam install camlp5

gave me `camlp5` 6.17, which however turned out to produce build errors
for HOL Light.
After some research, I found the solution:

    opam pin add camlp5 6.14

After that, HOL Light happily built. On the other hand, loading it
with `#use "hol.ml";;` gives the error about not finding `camlp5o.cma`.

I decided to reinstall everything with OPAM.

    opam init --comp 4.02.3
    opam pin add camlp5 6.14
    opam pin add batteries 2.4.0

But even that did not fix the issue yet ...



ATP dependencies for HOL Light
------------------------------

I discussed ATP dependencies with Cezary.
According to him, for his JAR paper, he created ATP dependencies as follows:

1. Extract human dependencies for HOL Light theorems,
   and pass them to some ATPs (E, Vampire, Z3),
   yielding a set ATP1 of ATP dependencies. (About 15% solved problems.)
2. On the human dependencies, train classifiers (k-NN, Naive Bayes)
   and run the ATPs on the first n (64, 256, 1024) dependencies
   reported by the classifiers, yielding ATP2.
   This again solves about 15% of the problems, and ATP1 u ATP2
   are at 20%.
3. On the dependencies of ATP1 u ATP2, train classifiers
   and use output for ATPs, yielding ATP3 that solves about 30%.
   Step 3 can also be iterated several times with different kinds of features,
   which brings the number of solved problems up to 40%.

This process is time-intensive.

If you already have the ATP dependencies for some dataset, then
it is possible to reuse large parts of it for newer versions of the dataset.
One loses a few percent of the previous problems and
does not take into account newly added problems that way,
but it is significantly easier than creating ATP dependencies from scratch.
To counter losing the few percent of the previous problems,
Cezary used boosting, where he trained classifiers only on those problems
that were not solvable anymore.

Another option is to evaluate only on the human dependencies.
To compare the performances of different reconstruction methods,
this might be a suitable and easy way to go.
Later, we might still evaluate performance on ATP dependencies.

Yet another evaluation involves replacing calls to MESON
by calls to a tactic that first tries nanoCoP,
and only in case of failure resorts to MESON.
In the end, one counts the number of problems that nanoCoP can solve.



14.03.2017
==========


HOL Light evaluation
--------------------


Change to the HOL Light directory.
With original `fusion.ml`:

~~~ ocaml
#use "hol.ml";;
#load "unix.cma";;
#load "str.cma";;
let (hreal_tybij_fst, hreal_tybij_snd) = hreal_tybij;;
let (real_tybij_fst, real_tybij_snd) = real_tybij;;
let (num_tydef_fst, num_tydef_snd) = num_tydef;;
let (recspace_tydef_fst, recspace_tydef_snd) = recspace_tydef;;
needs "update_database.ml";;
update_database ();;
loads "../HH/hh_symbols.ml";;
write_hashes ();;
~~~

Then, with `fusion.ml` pointed to `fusion.ml.deps`:

    ln ../HH/fusion.ml.deps fusion.ml

~~~ ocaml
#use "hol.ml";;
#load "unix.cma";;
#load "str.cma";;
let (hreal_tybij_fst, hreal_tybij_snd) = hreal_tybij;;
let (real_tybij_fst, real_tybij_snd) = real_tybij;;
let (num_tydef_fst, num_tydef_snd) = num_tydef;;
let (recspace_tydef_fst, recspace_tydef_snd) = recspace_tydef;;
needs "update_database.ml";;
update_database ();;
loads "../HH/hh_symbols.ml";;
~~~

To write ATP files, first create a file containing
all conjectures with at least one premise:

    grep -v "[-\!]$" HH/deps.human > HH/towrite

Then run with original `fusion.ml`:

~~~ ocaml
loads "../../HH/hh_tac.ml";;
loads "../../HH/hh_write.ml";;
loads "../../HH/hh_svr.ml";;
loads "../../HH/hh_batch.ml";;
write_to_prove_all "HH/towrite" 1000;;
~~~

This writes lots of files of the shape `f*.p` to the current directory.
To move them:

    mv f*[.]p HH/atpd/i/f

TODO: How to obtain minimised ATP dependencies?



08.03.2017
==========


nanoCoP extension clause reconstruction
---------------------------------------

I have now worked for a bit more than one week non-stop on
nanoCoP proof reconstruction in HOL Light.

The biggest remaining problem is the reconstruction of extension steps:
An extension step can be characterised by a path in the matrix to a matrix
from which there is a path to a literal.
Only for the second path, new subgoals are considered for the extension step.
By default, this information is not explicitly collected, and reconstructing
it from the existing proof structure may be quite intricate.
I considered just creating the whole instantiated matrix
at the beginning of the proof reconstruction,
but I quickly discarded that plan, because the whole matrix is not explicitly
given at the end of the proof search -- it also would need to be reconstructed
after proof search from the proof steps.
It might be important to note at this point that matrix copies are always
*local* to the proof search -- that is, two matrix copies with the
same copy index may appear during proof search,
but denote different instantiations!

It has helped tremendously to reconstruct two nanoCoP proofs by hand
in HOL Light. This has given me confidence that it is actually not necessary
to store gamma clauses during extension clause generation, because the
gamma clauses are only used for more extension steps, and as extension clauses
can be produced multiple times without littering the proof with additional
dependencies, it does not compromise our ability to reconstruct the proof.



23.02.2017
==========


Statistics for problems that all monteCoPs solved
-------------------------------------------------

Similarly to the entry from 16.2., this generates statistics,
but not for the average for all problems solved by a particular configuration,
but for the average for all problems solved by *all* configurations,
hereby represented by the file `intersection`.
Without further ado:

    for config in -randrew -sizerew -mlrew -bayesprob -constprob -cutcla -nocut ""; do
      echo -en "${config}\t";
      grep UCT `sed "s|^|out/bushy/10s/defcnf/montecop-170209${config}/|" intersection` |
        grep -v nan | sed 's|/| |g' |
        sed 's/.*UCTIters: \(\S*\) .* UCTSimSteps: \(\S*\) UCTDiscr: /\1 \2 /' |
        awk '{for (i=1;i<=NF;i++){a[i]+=$i;}} END {for (i=1;i<=NF;i++){printf "%.2f", a[i]/NR; printf "\t"}; print NR}';
    done



Intersection of file lines
--------------------------

To get the lines of a set of files that are contained in all files,
<http://stackoverflow.com/a/40223101/4112904> has a good answer:

~~~ bash
intersect() {
    sort "$@" | uniq -cd | grep "^[^0-9]*$# "
}

# usage example
intersect file1 file2 file3
~~~

Here, `$#` denotes the number of arguments, i.e. the number of files.



21.02.2017
==========


HOL Light with checkpointing
----------------------------

I installed HOL Light:

    sudo apt install ocaml camlp5 dmtcp rlwrap
    git clone https://github.com/jrh13/hol-light/
    cd hol-light
    make
    dmtcp_checkpoint rlwrap ocaml

Then, in the OCaml session, I run:

    #use "hol.ml";;

As soon as HOL is built, I run

    dmtcp_command --checkpoint

from a different console.
After that, I can quit OCaml and resume its operation by running

    ./dmtcp_restart_script.sh

For now, OCaml always crashes when one quits it from checkpointed mode.


Making new repository from subdirectory
---------------------------------------

Here is an answer if you want to create a new repository
from parts of an existing one, preserving history:

<http://stackoverflow.com/questions/359424/detach-move-subdirectory-into-separate-git-repository/17864475#17864475>



20.02.2017
==========


Local optimisation
------------------

I have created a Haskell program to do a very simple optimisation of parameters.
All values for a certain parameter are tried, then the best one is taken
and used in the rest of the procedure for the remaining parameters.
An example how this looks like:

~~~ haskell
import Control.Monad
import Data.List
import Data.Ord
import System.Process
import Text.Printf

optimise evaluate = foldM go where
  go prev vs = do
    results <- mapM (\ v -> evaluate (prev ++ [v]) >>= return . (,) v) vs
    return (prev ++ [fst (maximumBy (comparing snd) results)])

eval :: [String] -> IO Int
eval args = readProcess "./run.sh" args [] >>= return . read

combine p vs = [p ++ " " ++ v | v <- vs]

show1f :: Double -> String
show1f = printf "%.1f"

flags =
  [ combine "-mlmean" ["min", "prod", "harm", "geom", "arit"]
  , combine "-cert" (map show1f [0.7, 0.8 .. 1.0])
  , combine "-certdamp" (map show [1 .. 4])
  , combine "-defscore" (map show1f [0.6, 0.7 .. 0.9])
  , combine "-naivereward" (map show1f [0.0, 0.1 .. 0.5])
  , combine "-exploration" (map show1f [0.7, 0.8 .. 1.0])
  , combine "-maxiters" (map show [20 .. 40])
  , combine "-simdepth" (map show [20 .. 30])
  ]

initial = []

main = optimise eval initial flags >>= putStrLn . unwords
~~~

The file `run.sh` generates for a given parameter combination
an adequate Makefile and runs it.
This allows us to resume the parameter evaluation at a later point.

~~~ bash
#!/bin/bash
set -e

ARGS="$*"
CARGS=`echo $ARGS | sed 's/ /,/g'`
FILE=montecop-170209$CARGS
TARGET=out/bushy/10s/defcnf/optimise/$FILE

(>&2 echo Current configuration: $ARGS)

echo -e "
include cop.mk datasets.mk
$TARGET: \$(BUSHY:bushy/%=\
$TARGET/%)
$TARGET/%: cop-170209/montecop.native bushy/%
	@mkdir -p \`dirname \$@\`
	-/usr/bin/time -o \$@.time timeout 10 $^ \\
          -ldata out/bushy/60s/defcnf/lazycop-170209-md10.ldata \\
	  -exppol cutcla \\
          $ARGS > \$@
" > $FILE.mk

make -j40 -f $FILE.mk $TARGET > $FILE.log 2>&1

SOLVED=`(cd $TARGET && grep -l Theorem *) | wc -l`
(>&2 echo Solved: $SOLVED)
echo $SOLVED
~~~


17.02.2017
==========


Simulation steps per simulation depth
-------------------------------------

    for i in *simdepth*; do
      echo -n "$i ";
      grep UCT $i/*.p | sed 's/.*UCTSimSteps: \(\S*\).*/\1/g' |
        awk '{sum+=$1} END {print sum/NR}';
    done | sed 's/.*simdepth//' | sort -n


How does a conclusion look like?
--------------------------------

Searching for a kind of "model conclusion" for CADE, I found
[Playing with AVATAR](http://www.cs.man.ac.uk/~regerg/papers/playing.pdf).
The experiment section also looks quite nice.


Towards the Integration of an Intuitionistic First-Order Prover into Coq
------------------------------------------------------------------------

The headline is the title of a [paper](https://arxiv.org/abs/1606.05948)
by Fabian Kunze presented at the HATT workshop at IJCAR'16.
My attention was brought again to this article as Cezary mentioned
that retrieving a sequent-style proof from a matrix proof
(such as given by inanoCoP) has been mentioned in this paper to be solved in
[Converting non-classical matrix proofs into sequent-style systems](http://dx.doi.org/10.1007/3-540-61511-3_104).
This might be a doable project after all ...



16.02.2017
==========


From raw data to a nice table
-----------------------------

To create detailed statistics from a Monte Carlo prover configuration, run

    for config in -randrew -sizerew -mlrew -bayesprob -constprob -cutcla -nocut ""; do
      echo -en "${config}\t";
      grep UCT out/bushy/10s/defcnf/montecop-170209${config}/*.p |
        grep -v nan | sed 's|/| |g' |
        sed 's/.*UCTIters: \(\S*\) .* UCTSimSteps: \(\S*\) UCTDiscr: /\1 \2 /' |
        awk '{for (i=1;i<=NF;i++){a[i]+=$i;}} END {for (i=1;i<=NF;i++){printf "%.2f", a[i]/NR; printf "\t"}; print NR}';
    done

An especially nice part is the `awk` command to create averages of all columns:

    awk '{for (i=1;i<=NF;i++){a[i]+=$i;}} END {for (i=1;i<=NF;i++){printf "%.2f", a[i]/NR; printf "\t"}; print NR}'

I really start loving `awk`.

Then, let us assume we have a `header` file:

    Configuration | UCT iterations | UCT simulation steps | Blub | Discrimination | Solved
    --------------|---------------:|---------------------:|------|---------------:|------:

To make nicely formatted Markdown from the raw input data `in` and the header:

    (cat header; sed 's/\t/ | /g' in) | pandoc -t markdown



13.02.2017
==========


Solved files per configuration
------------------------------

For every prover configuration, build the number of solved files:

    make `ls -d out/bushy/10s/* | sed 's|out|solved|'`

`ls -d dir/*` prints all files in `dir` in the format `dir/a dir/b`
instead of just `a b`.



10.02.2017
==========


Push Git tags automatically
---------------------------

Again, I forgot to push my new Git tags.
To push tags automatically, I read at
<http://stackoverflow.com/a/3745250/7379374> that
you can add the following to your repository's config:

    [remote "origin"]
        url = ...
        fetch = ...
        push = +refs/tags/*

However, I still have to do that change on every clone
of the repository.



09.02.2017
==========


Literal Refutability Woes
-------------------------

I tried the literal refutability reward on PUZ035-1.p as follows:

    ./montecop.native -nodefcnf -ldata PUZ035-1.trace eval/tptp/PUZ035-1.p

With `mlmean` = harm, this takes 47.283s;
with `mlmean` = min, this takes 6.107s.

This is quite good when one considers that with e.g.

    ./montecop.native eval/tptp/PUZ035-1.p -mlreward 0 -naivereward 1

monteCoP runs for more than one minute without finding the proof.

However, this is quite bad when one knows that
leanCoP proves the same problem after 919 inferences at depth 6
(without cut).

    ./lazycop.native eval/tptp/PUZ035-1.p -nodefcnf -nocut3 -nocut2 -nocut1

Perhaps there is another bug lurking?
The Monte Carlo tree looked quite decent, meaning that it looked balanced
and there were hardly any remaining unvisited states at the top.
Furthermore, in the monteCoP proofs, lemma steps appeared,
so that is also working.

Interestingly enough, with the cut expansion policy,
the proof is found in under one second:

    ./montecop.native eval/tptp/PUZ035-1.p -mlreward 0 -exppol cutlit

Still, the number of UCT iterations (9557) is much too high for my taste.
Furthermore, when using the `maxiters` switch to enable the hybrid prover,
the proof is also found blazingly fast (0.034s for `maxiters` = 100).

**UPDATE**: I think I figured out why monteCoP performs so badly here:
The proof of PUZ035-1.p is relatively long, involving more than 40 steps,
but the required depth (6) is relatively small.
My hypothesis is that when running monteCoP without the cut expansion policy,
the proof search will be exponential in the number of *proof steps* required,
whereas in leanCoP, it will be exponential in the required *depth*.
This is a crucial difference.



08.02.2017
==========


Smooth Bézier plots
-------------------

Cezary was interested to know how I made a Bézier interpolation
for my plots. Here is the code:

~~~ tex
\begin{tikzpicture}
\begin{axis}
\addplot +[no markers, raw gnuplot] gnuplot {
        plot 'data.txt' smooth sbezier;
};
\addplot +[no markers, raw gnuplot] gnuplot {
        plot 'data.txt';
};
\end{axis}
\end{tikzpicture}
~~~

Unfortunately, this requires `pdflatex` to be run with `-shell-escape`.
But the plots do look quite pretty ... :)


Bob Veroff's hints technique
----------------------------

Taken from my old notes:

Record intermediate lemmata that were generated during Prover9 proof search,
then give high weight to them when encountered in new problem.
Does not consider prover state!

Works well in Prover9, but for Mizar, it is less successful
(possibly because of smaller domain).


Constant weighting
------------------

Also taken from my old notes:

In E, constant weighting can be learnt from previous proofs.
This is probably less powerful than ML internal guidance, but faster.



07.02.2017
==========


Normed Naive Bayesian Transition Probability
--------------------------------------------

To determine which transition to take in a random simulation,
I have experimented with Naive Bayes similarly to FEMaLeCoP,
but a recurring problem was that the probabilities were
much too small.

Furthermore, I noticed that it is not sufficient to just
consider the intersection of current features and training features,
because that actually punishes the intersection of features
instead of encouraging it.
Only when the "punishment" is compensated by an even more severe
punishment of disjoint features, it is fair.

To norm the label probability, we subtract from the
(logarithmic) label probability the
*largest* label probability for all currently compared labels.
That means that the normed label probability of the label with
the highest label probability will be 1.

To norm the feature probabilities, we calculate the
intersecting feature probabilities and the number of disjoint features.
The disjoint features are weighted with the
*smallest* intersecting feature probability among all comparing
label probabilities.
That means that we attribute to features present in the current situation,
but not having been present in the training example,
the worst probability of any present feature.
Then, the mean of the (logarithmic) feature probabilities is taken.

To the sum of normed label and feature probabilities,
we apply a normalisation function, to exclude cases
where the probability is still too small.

These measures have so far not brought a substantial improvement over
the inverse size heuristic.
However, I tested them only on `wellord1__t52_wellord1.p` so far,
so a larger test on the whole MPTP2078 might bring different results.


Discrimination Power of State Reward Heuristics
-----------------------------------------------

We want to measure how well our state reward heuristics are able to
discriminate states that led to a found proof from states
that do not lead to a found proof.
For this, we calculate the ratio of:

* the average rewards of direct ancestors of a proof state, and
* the average rewards of all siblings of direct ancestors of a proof state.


One-liner to get average value of parameter
-------------------------------------------

Given files with lots of lines like:

    % Inf: 5 UCTIters: 34 UCTInf: 270 UCTSimSteps: 366 UCTDiscr: 1.057516 Depth: 0 DInf: 5 Str: 1

To get the average of the UCTDiscr values, use `sed` + `awk`:

    sed -n 's/.*UCTDiscr: \(\S*\).*/\1/p' *.p | awk '{s+=$1; n++} END {print s/n}'

Here, the `\S` matches non-whitespace.
In case one needs to explicitly match floating-point numbers, use:

    sed -n 's/.*UCTDiscr: \([0-9]*\.[0-9]*\).*/\1/p' *.p


Queuing commands
----------------

When I have a command `a` that is taking some time and I want to
schedule command `b` to be executed when `a` has terminated,
I can press CTRL-z while `a` is running, then run

    fg; b

Found at: <http://stackoverflow.com/a/35587710/4112904>


Renaming Git tags
-----------------

    git tag new old
    git tag -d old
    git push origin :refs/tags/old
    git push --tags

Found at: <http://stackoverflow.com/a/5719854/4112904>



26.01.2017
==========


Hybrid system performance
-------------------------

I tested monteCoP in hybrid mode, where the outcome of Monte Carlo Tree Search
is used to guide a modified lazyCoP.
Similarly to the experiments with varying simulation depth,
I varied the maximal number of MCTS iterations per lazyCoP inference
from 8, 16, 32, 64.

Iterations  |  Solved
----------: | ------:
         8  |     338
        16  |     342
        32  |     327
        64  |     314

I expected that hybrid monteCoP would prove more problems than stand-alone
monteCoP, but that it would prove less unique problems.
That does not seem to be the case, as with 16 iterations,
the number of unique problems is 59, compared to 60 for stand-alone monteCoP.


Better graph creation
---------------------

For a larger set of parameters, the method I showed yesterday is cumbersome.
An improved method with smoothed Bézier plot follows:

    make `ls out/bushy/1s | grep srsd | awk '{print "solved/bushy/1s/" $1}'`
    wc -l solved/bushy/1s/*srsd* | sed \$d | awk '{print $2 " " $1}' | sed 's/.*srsd//' | sort -n > graphdata
    gnuplot -e "set term png; set output 'gnuplot.png'; plot 'graphdata' notitle with linespoints, 'graphdata' notitle smooth bezier with lines"

`sed \$d` is a nifty command to display every but the last input line.


Skolemisation without content names
-----------------------------------

I looked at the problem `wellord1__t52_wellord1.p` and found monteCoP
to solve it regularly with different seeds, whereas lazyCoP was not
able to solve it at all.
However, when I enabled "-nocontent" for smaller names for graph generation,
monteCoP could not solve the problem anymore.

I found out that with content-name based Skolemisation,
several existentials were mapped to the same Skolem function,
which apparently facilitated proof search.
Therefore, I adapted the non-content-name based Skolemisation
to reuse Skolem indices when encountering an existential
that is equivalent to another existential previously Skolemised.

I want to investigate whether the current Skolemisation is really sound.



25.01.2017
==========


Greedy set cover
----------------

I wrote a little Haskell script that takes a list of files as arguments.
Each file should contain a list of problems solved, one per line.
The script then iteratively gets the next file the solves a maximum number
of problems that all the previous ones were not able to solve.
Code:

~~~ haskell
import Data.List (maximumBy, delete)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

greedy :: (Ord a, Eq t) => Set a -> [(t, Set a)] -> [(t, Int)]
greedy elems [] = []
greedy elems sets =
  let sets' = map (\ (name, set) -> (name, Set.difference set elems)) sets
      (name, set) = maximumBy (comparing (Set.size . snd)) sets'
  in  (name, Set.size set) : greedy (Set.union elems set) (delete (name, set) sets')

main =
  getArgs >>=
  mapM (\ fp -> readFile fp >>= \ f -> return (fp, Set.fromList (lines f))) >>=
  return . greedy Set.empty >>=
  mapM print
~~~

The first four lines of output on my recently generated configurations are:

~~~
("lazycop-170118-single",450)
("montecop-170123-srsd24",59)
("montecop-170123-srcc",11)
("lazycop-170118-nocut",7)
~~~

The numbers behind the configuration show how many problems the configuration
adds to all previously found problems.



Varying simulation depth influence
----------------------------------

I made a Makefile meta-rule (using `define` macros) that generates
Makefile recipes in dependency of some parameter, in this case
maximal simulation depth.
That allows me to run an arbitrary number of different configuration values
for all problem files without blowing up the Makefile.
Unfortunately, I have to pay this price with an increase of the Makefile
processing time from around 3s to 12s (by auto-generating 128 recipes).
For that reason, I commented out the rule generation, and it takes only
to remove a `#` to enable it back.
To visualise simulation depth influence, I use:

~~~
make out/bushy/1s/montecop-170123-srsd
for i in 0 1 2 4 8 16 24 32 64; do make solved/bushy/1s/montecop-170123-srsd$i; done
cd solved/bushy/1s
for i in 0 1 2 4 8 16 24 32 64; do echo $i `wc -l montecop-170123-srsd$i | awk '{print $1}'`; done > graphdata
gnuplot -e "set term png; set output 'gnuplot.png'; plot 'graphdata' with linespoints"
~~~

For the moment, the value 24 has brought the best results, yielding
296 solved problems. I will nevertheless run another evaluation on the server,
to have more data and to see how much the results vary due to randomness.


Problems only monteCoP can solve
--------------------------------

I analysed which problems neither lazyCoP with nor without cut
can solve, but some monteCoP configuration can.
So far, I found 26 such problems.

Of these, the shortest are `wellord1__t52_wellord1.p` and
`xboole_1__t116_xboole_1.p` -- both have 6 lines.
However, `wellord1__t52_wellord1.p` gives a stack overflow
when run with lazyCoP. monteCoP solves the problem.

    ./montecop.native -mlreward 0 -sizereward 1 -exppol cutlit eval/bushy/xboole_1__t116_xboole_1.p

However, I was not able to quickly find another seed with which
monteCoP succeeded, so it was a bit of a luck find.
Anyway, the proof is 23 steps long, and goes to depth 11,
which is why lazyCoP is not able to find it.



24.01.2017
==========


monteCoP cut revisited
----------------------

I tried to run monteCoP with the cut expansion policy,
both on literals and clauses.
For literals, it solved 283 problems and for clauses even 293,
compared to only 266 with the default expansion policy.
My fear that with cut, complementarity toward lazyCoP could be lost,
was not fulfilled:
Even when comparing monteCoP + cut (clauses) with lazyCoP + cut,
monteCoP proves **60** unique problems -- a plus of about **13%**!


monteCoP simulation depth
-------------------------

Just for fun, I tried setting the simulation depth to zero.
The result was quite bad; it solved only 154 problems.
Still, it would be nice to include a graph that shows the
number of solved problems as a function of simulation depth.


Dampening of previously encountered contrapositives
---------------------------------------------------

When a contrapositive has already been used on the current branch,
then the antecedent dampening constant will reduce the probability
of choosing the contrapositive again, based on the number of times
it has been used.
This increases the number of solved problems (without cut) from
266 to 276 when using a dampening of 0.3.



23.01.2017
==========


monteCoP heureka
----------------

I found a bug that was caused by new nodes in the Monte Carlo tree
not being given reward and visit count. This caused the Monte Carlo tree
to be expanded almost only in a single direction, not considering
many options that were just below the root.
This change caused the number of bushy problems solved in 1s to increase
from 96 to 266! lazyCoP solves 457 problems, and monteCoP adds
**51** new problems!
And in addition to that, I now have the option to create
graphs of the Monte Carlo trees.
(This was crucial in finding the bug.)

Lesson learnt: Visualise your information!

To create an output graph with monteCoP, use the following example
that prints the Monte Carlo tree after 50 iterations and then quits:

    ./montecop.native problems/rotate.p -mlreward 0 -sizereward 1 -simdepth 5 -seed 3 -maxiters 50 -dotout test.dot && dot -Tps test.dot -o test.ps



19.01.2017
==========


Bash-fu
-------

To obtain a list of uniquely solved problems sorted by processing time:

~~~ bash
for i in `comm -23 solved/bushy/1s/lazycop-170118-single solved/bushy/1s/montecop-170118-sr`
do
  echo `head -1 out/bushy/1s/lazycop-170118-single/$i.time` $i
done | sort -n | awk '{print $7}'
~~~

To then link a list of problems to the size of the problem:

    for i in `cat sorted`; do wc -l bushy/$i; done


Strange monteCoP divergence
---------------------------

I analysed the monteCoP performance on the problem `enumset1__t93_enumset1.p`
and created a shorter version of it:

~~~
fof(rotate2, conjecture, (! [A] :  (! [B] :  (! [C] : p(A,B,C)=p(C,A,B)) ) ) ).
fof(rotate1, axiom,      (! [A] :  (! [B] :  (! [C] : p(A,B,C)=p(B,C,A)) ) ) ).
~~~

When running monteCoP with this problem, such as

    ./montecop.native rotate.p -mlreward 0 -simdepth 5 -seed 3

then it succeeds immediately for many seeds (8 out of the first 10).
However, in the case of seeds 3 and 6, for example,
it gets stuck and does not even finish after 30 seconds.
Interestingly, this seems to be independent of many settings,
such as size reward, clause weight, or dampening of previous contrapositives.
The proof length increases steadily, though.

In comparison, lazyCoP (without cut or with cut does not matter)
needs only 9 inferences for the same problem (depth 3).

My hypothesis is that if even the resolution of such a small problem
depends so greatly on chance, then this approach will not work
for larger problems.
The conclusion is that I need to figure out why exactly monteCoP gets stuck.
Debugging ahead! :)



18.01.2017
==========


Tests with size heuristic
-------------------------

After discussion with Cezary, I created a non-machine-learning heuristic
for monteCoP that rates prover states by the sizes of the remaining goals.
As first test, I ran the new heuristic on all MPTP2078 problems that
a single lazyCoP strategy could solve in 1s:

~~~ bash
for i in `cd out/bushy/1s/lazycop-161209-single/ && grep -l Theorem *`
do
  echo $i
  /usr/bin/time -o out/bushy/5s/montecop-lessnaive/$i.time timeout 5 \
    ../montecop.native -mlreward 0 -sizereward 1 -clascore const -maxiters 100 -simdepth 10 \
    bushy/$i > out/bushy/5s/montecop-lessnaive/$i
done
~~~

This solved 303 problems, compared to 457 problems for lazyCoP.
To find out how to improve that result, I checked the times lazyCoP required
for the problems that monteCoP was not able to solve:

    for i in `cd 5s/montecop-lessnaive/ && grep -l Unknown *.p`; do echo `head -1 1s/lazycop-161209-single/$i.time` $i; done | sort -n

This indicated for example the problem `orders_2__t82_orders_2.p`, which
lazyCoP could solve in around 0.00s, but monteCoP frequently required around
5s to 20s.

Tomorrow, I want to look into, say, the five problems with the greatest
time difference, and see how to improve the monteCoP performance on them.
Furthermore, I want to also try the dampening of probabilities of
previously used contrapositives on paths.



16.01.2017
==========


ParamILS results
----------------

I looked at the results of the evaluation from 12.01.2017
Some observations:

* The most successful extension probability heuristic was
  the naive inverse one in 31 cases and FEMaLe in a single (!) case.
* The maximum number of MC iterations was always the
  smallest available value, namely 25 (others were 50 and 100).
* The simulation depth also all converged to values between 10 and 20
  (others were 40 and 80).
* The machine learning evaluation heuristic seems to be only partially
  successful: 14 times its influence was 0, 8 times it was 0.5, and
  7 times it was 1. Even the second-best overall solution has an
  ML influence of 0 -- interestingly coupled with a naive reward
  that is also 0. That means that the second-best solution has
  an evaluation heuristic that gives a uniform score of 0!
* The best expansion policy is 20 times cutlit and 12 times fst.
  However, the two best solutions both use fst.


FEMaLeCoP unique MPTP2078 problems
----------------------------------

I compared

    leancop -nodef -feanosubst -dtree -learn

with

    leancop -nodef

The first configuration solves 670, the second solves 662 problems.
The number of unique problems for the first configuration is 33.
That means that the union of first and second configuration proves 695 problems.
This is a plus of 4.98% with respect to the second configuration.
Still, in the FEMaLeCoP paper, it is claimed that machine learning adds
90 problems (15.7%) wrt original solutions.

Cezary told me that Josef could look at his experiment data
after the LPAR deadline.



13.01.2017
==========


Claws Mail reply recipients
---------------------------

When I received messages from the CL mailing list and replied to them,
Claws Mail would always insert the mailing list as recipient.
That led to me sending mails to all CL members by accident.

That behaviour can be deactivated by disabling
Configuration -> Preferences ->
Compose -> Writing -> Reply button invokes mailing list reply.
I did not find that option when browsing the preferences,
but I found it after two minutes of grepping the source code. :)



12.01.2017
==========


ParamILS reboot
---------------

I revisited my previous attempts to optimise monteCoP parameter with ParamILS.

First, I thought about how to measure the `runlength` of a single
monteCoP run. (As opposed to the `runtime`.)
As the newer monteCoP combines a lazyCoP master prover with a Monte Carlo slave,
one has to consider the number of inferences calculated for both of them.
So I think that the sum of master + base unification steps is a good estimate.

Second, I noted that the `maxiters` option does not accept the same unit
as the `runlength` discussed above, because `maxiters` considers
UCT *iterations*, not *inferences* performed within.
For that reason, I thought about making a new switch similar to `maxiters`,
but one that stops monteCoP as soon as sum of master + slave inferences
exceeds a certain value.
Luckily, that was not necessary, because after looking at the ParamILS output,
I noted that the `cutoff_length` always took the value $2^32 - 1$,
which is the maximal 32-bit integer. This is used by ParamILS because I set

    cutoff_length = max

in `scenario.txt`. Even if this would be set to a different value,
it would remain constant, so I do not need to worry about the unit
I use to measure the `runlength` and the `cutoff_length`.
For now, I am going to ignore the `cutoff_length` completely.


CL meeting
----------

Excerpt from the log:

### Travel reimbursement

- deadline: Freistellung in VIS at least a week before travelling!
- entry "allgemeine Anmerkung zur Freistellung" should include useful information,
  like link to conference, link to website of project partner, reason to go there, etc.
- collect receipts, bank-account transfer

### Publications

- send information to SJ with complete information (see guidelines in Wiki)
- do it immediately
- also AFP entries and workshop papers should be entered, though they may
  partially only shown as project related publications
- online first papers should only be displayed on project website,
  move to CL publication website after regular appearance


Moving from grid02 to colo12
----------------------------

Due to Jan running lots of experiments on grid02, I moved my experiments
to colo12.
An obstacle was outdated or not installed software on the server.
I needed to manually install:

* GNU make
* timeout (GNU coreutils)
* OPAM (for OCaml Batteries)

The GNU tools were easily installed with `./configure --prefix=...`.
OPAM was a bit harder, but also quite doable:
I had to adapt a path "/usr/local" in `setup.ml` with my local prefix.

Installing OCaml Batteries was then only a matter of running:

    opam install batteries.2.4.0

(I also tried installing Batteries without OPAM, but that quickly
resulted in errors.)



10.01.2017
==========


Cezary's Writing Advice
-----------------------

* Put conclusion of a paragraph already at its beginning.
  For example: "We therefore focus on ..."
* Give the intuition in two sentences, then say it formally.



07.01.2017
==========


Technology Exploration: Racket
------------------------------

I spent some time getting to know the programming language Racket.
I documented my learning under: http://gedenkt.at/blog/falling-in-love-with-racket/



05.01.2017
==========


Verifying FEMaLeCoP results
---------------------------

To find out about the source of the bad FEMaLeCoP results from 30.12.2016,
I asked Cezary, who gave me an even older FEMaLeCoP version than the one
I previously used, namely from December 2015,
which is reasonable given that his FEMaLeCoP preprint on his website

http://cl-informatik.uibk.ac.at/users/cek/docs/15/ckju-lpar15.pdf

dates from 11 November 2015.
His explanation for the bad performance was that he might have run
FEMaLeCoP with different parameters, for example:

* `-nodef`: Disables definitional CNF.
* `-feanosubst`: Disables the calculation of features
  from terms that are present in the substitution.
* `-dtree`: Uses dtree for unification and will also
  prefilter unification options before evaluating them with
  the Naive Bayes classifier.

I evaluated these options. The results were generated with:

    for i in out/bushy/60s/leancop-151223*; do if [ -d "$i" ]; then echo $i `(cd $i && grep -l Theorem *) | wc -l`; fi; done


| Options       |  Solved problems |
| :------------ | ---------------: |
| (none)        |              626 |
| nd            |              662 |
| nd+lrn        |              628 |
| nd,fns+lrn    |              634 |
| nd,dt         |              644 |
| nd,fns,dt+lrn |              670 |

Table: Results for FEMaLeCoP from December 2015.
  nd stands for nodef, fns for feanosubst, and dt for dtree.
  lrn is the version with learning data fed from the non-learn version.

Interesting points:

* Switching on dtree worsens the results for the non-learning version,
  but improves it for the learning version.
* feanosubst has a smaller impact than I would have expected,
  given that it should reduce the number of features quite a bit.
* The number of nd,fns+lrn (634) is the one that is closest to the
  FEMaLeCoP results given in the paper (635).
* The best machine-learning results (670) add only **1.2%** to the
  best non-machine-learning results.

As of now, it is totally unclear to me where the relatively bad result
for the unguided OCaml-leanCoP from the FEMaLeCoP paper (574)
comes from.


Bogus learning
--------------

I had an intuition, namely to enable learning without providing a data file.
This will still compute features and calculate Bayesian relevance,
but this will not contribute in a positive way.

The result (without any other options than `-learn`) was: 574 solved problems.
This is exactly the number that was given in the FEMaLeCoP paper.
So my hypothesis is that this was the used configuration
for what is called in the paper the "unaided OCaml-leanCoP".
If this hypothesis holds, then I find the text in the paper misleading:

> Unaided OCaml-leanCoP is first run on all the 2078 bushy problems
> with a time limit of 60s. This solves 574 problems.

This section does not mention that the unaided OCaml-leanCoP performs
any calculations related to machine-learning.



02.01.2017
==========


Plotting integrated data with `awk`
-----------------------------------

I wrote a blog post at http://gedenkt.at/blog/plotting-integrated-data/
about plotting how many experiments succeed until a certain time.
I now created a nice `awk` one-liner that does the job of the
by multiple orders larger original Haskell program:

    sort -n | awk '{p++; print p " " $1}'

It can be used as follows, where the `.time` files are outputs of
`/usr/bin/time`:

    grep --no-filename user out/bushy/60s/leancop-160211/*.time | sort -n | awk -F'u' '{p++; print p " " $1}'

(The `-F` switch sets the delimiter that `awk` uses to split input lines.)

The `gnuplot` command on the blog to plot the data stays the same:

    gnuplot -e "set term png; set output 'gnuplot.png'; plot 'integrated'"



30.12.2016
==========


`yellow_1__t23_yellow_1.p`
--------------------------

I looked at the problems that were solved by lazyCoP,
but not by monteCoP. Among those, I particularly noted
the problem `yellow_1__t23_yellow_1.p`, which is solved by
lazyCoP in roughly 0.01s (168 inferences),
but monteCoP takes between 7 and 19 seconds!

First, I thought that switching back on prefiltering of
extension options before calculating their Bayesian relevance
could improve performance, but this turned out to have a
non-noticeable influence.
(As a side note, prefiltering is now configurable via the switch
`-prefilter`.)
However, decreasing the simulation depth has a major influence:
Before, I used a simulation depth of 80.
With simdepth = 20, monteCoP needs "only" around 2s.
For one exemplary run, monteCoP uses 2564 UCT iterations
with a total of 43777 simulation steps. That amounts to
an average 17,07 simulation depth, which is close to the maximum simdepth.
In other problems that I have debugged before, I remember
that the average simulation depth seemed to be much lower than
the maximum simdepth (typically only around 2 or 3),
which indicates that for problems that are made such that
there are hardly any "dead ends", i.e. you are easily stuck in an
infinite reasoning loop, a low simdepth is crucial.


Old-fashioned inferences
------------------------

I noticed that monteCoP missed many problems that were solved by
lazyCoP in a very small amount of time, say 0.01s.
For that reason, I have introduced a parameter that allows to
interweave lazyCoP and monteCoP inferences such that at first,
lazyCoP inferences are performed until a certain number of
inferences (per depth), and only then Monte Carlo simulations are run.
They idea is that the more efficient brute-force search of lazyCoP
is exploited until the search space gets too large,
and then monteCoP should (hopefully) play its strengths.

In the first evaluation, with a number of traditional inferences of $10^5$
and inverse clause weight, monteCoP proves 91 problems.
This is more than FEMaLeCoP, which proves only 89 problems,
but still much less than lazyCoP, which proves 97 problems!
Moreover, even FEMaLeCoP only proves one problem more than lazyCoP.
This is very strange ...



29.12.2016
==========


Cut expansion policy
--------------------

I revisited the "cut-like" expansion policy where I will not take the
first simulation step as new tree state, but the simulation step that
has the fewest subgoals available. I found out that previously,
my calculation of the number of subgoals was wrong, so I have now
introduced two metrics:

1. Number of subgoal clauses.
2. Sum of literals over all subgoal clauses.

In my experiments, I found that metric 1 proves 66 problems,
whereas metric 2 proves 72 problems. Not using a cut-like
expansion policy solves 73 problems.
Metric 2 solves 5 new problems, but loses 6, so it is not very complementary.



26.12.2016
==========


Normalised Bayesian relevance
-----------------------------

The FEMaLeCoP Bayesian relevance has some tricks up its sleeves
to improve predictions. Unfortunately, at the same time,
it is not as probability measure, because its results are not normed
between 0 and 1, and norming them is not straightforward.
For that reason, I created a new, "strict" Bayesian relevance method
that is actually a probability measure.
It currently only considers features shared between learnt data
and the current context; however, this restriction might fall in the future.
I have also not integrated IDF yet, as the terms that IDF is calculated from
are already used in the basic Bayesian relevance calculation,
so perhaps we are already doing something very close to IDF without knowing it?

In my first experiments, the obtained probabilities frequently were
much too small to be usable. I evaluated this on the TPTP file PUZ035-1.p,
where I first ran lazyCoP on it to generate training data (only clauses),
then ran monteCoP with a maximum of 100 UCT iterations per inference.
My new Bayesian relevance used a mean of **7858** UCT iterations
(mean over 9 different runs) in 5.83s before solving the problem.
For comparison, a constant relevance of 1 requires on average only **2749** iterations
in 0.79s and a simple linear normalisation between 0 and 1
of the original FEMaLeCoP score requires **1304** iterations in 1.75s.
(Note that the lower amount of iterations for FEMaLeCoP is neutralised
by its lower prediction speed.)

The results for my new Bayesian relevance were clearly unacceptable.
I found that dividing the logarithmic relevance by
the number of shared features before exponentiation massively improves
performance, namely to an average of only **418** UCT iterations in 0.45s.

The evaluation code is:

~~~ bash
make montecop.native
time for i in {1,2,3,4,5,6,7,8,9}
do
  ./montecop.native -maxiters 100 eval/tptp/PUZ035-1.p \
    -learn -datai datab -clascore 3 -seed $i | grep Iters
done | awk '{total += $5} END {print total/NR}'
~~~



23.12.2016
==========


Exponential Skolemisation
-------------------------

When running lazyCoP on problem tmap_1__t144_tmap_1.p,
it crashed my computer, taking up all memory.
After setting `ulimit -v 1000000`, I could diagnose that
the problem was due to the Skolemisation being exponential.
I observed that this was due to me changing the Skolemisation,
but also the previous Skolemisation method had problems,
namely that it introduced unnecessary Skolem function arguments;
for example for $\exists a b. p(a, b)$, it would create
$p(a, b(a))$ instead of $p(a, b)$.
I fixed this with a new Skolemisation method that is still
suitable for machine learning, but tries to give Skolem functions
the smallest name possible.
With this new Skolemisation, the number of solved Mizar training problems
goes up from 308 to 314, which is the same as before I started
messing with nanoCoP. :)

Furthermore, I tried to define a class of problems parametrised
by a natural number that previously exposed exponential behaviour.
I wrote a problem generator in Haskell (`skolemex.hs`)
and found out that the generated problems are relatively hard
for lazyCoP (the depth required to solve the problem is proportional to
the size of the problem), whereas nanoCoP requires only depth 1.



16.12.2016
==========


Saving substitution at every proof step
---------------------------------------

In the original FEMaLeCoP, the substitution was saved for every proof step,
which I have later replaced by saving only a single substitution
for the whole proof.
I have noted just now that this makes a difference when showing the proof,
namely that saving a single substitution will show a variable to be
bound already before it has actually been bound later,
whereas the previous approach would show a variable to be bound
only from the point where it was actually bound.
In the machine learning data, this results in more features
for the new approach. It is yet unclear whether this helps or hurts.



13.12.2016
==========

Incomplete clause statistics?
-----------------------------

monteCoP uses statistics about literals saying how often the literal was
successfully or unsuccessfully tried for refutation.
I saw that for some clauses, statistics about some literals did exist,
but not about the other literals. That, however, is natural,
because if even refuting the first literal fails,
the other literals are not even tried.
So that is not a sign of incomplete statistics.



12.12.2016
==========


nanoCoP results
---------------

I translated nanoCoP to OCaml and evaluated it on the MPTP2078 dataset
with a timeout of 1s, using just a single mode with cut.
The results are surprisingly good: The original Prolog version proves
270 problems, while my OCaml port proves 373!

Still, my clausal OCaml version (lazyCoP) proves 446 problems,
so that indicates that the non-clausal overhead for these problems
does not pay off, probably because the problems do not have a
very deep structure (many nested and/or binders).


Contextual Skolemisation
------------------------

For machine learning, it is important to have a consistent naming scheme
for Skolem variables, such that the "same" Skolem variables that
appear in multiple problems obtain the same name whenever possible.
The previous approach by Cezary would just identify a Skolem variable
induced by $\exists x. p(x)$ by `?[X]: p(X)`, and annotate all
Skolem variables of a premise with a running counter.
The counter is necessary because sometimes syntactically equal terms
can give rise to different Skolem variables.
This is e.g. exposed in the following TPTP problem:

~~~
fof(p1, axiom, (![X]: (~p(X) | (?[Y]: q(X,Y))))).
fof(p2, axiom, (![X]: ( p(X) | (?[Y]: q(X,Y))))).
~~~

Here, Cezary's Skolemisation maps both occurrences of `?[Y]: q(X,Y)`
to the same Skolem variable. While I haven't yet been able to show
that this makes the proving unsound, I'm also not convinced that soundness
is maintained.
I have defined a new Skolemisation naming method, which one could call
"Contextual Skolemisation". There, all the "parallel" disjuncts (the context)
are saved in the name of the Skolem variable.
This also makes the trick with the counter unnecessary.
I would be interested to prove soundness of that method.


Theorem ordering
----------------

I reproved all the MPTP2078 problems with lazyCoP.
Compared to a previous run on Oct 3 with 314 solved problems,
the new version from today proves only 307 problems.
I looked at the problems and saw that e.g. `funct_2__t65_funct_2.p`
was previously proven in about 0.12s, but could not be proven by the
new version even after 300s!
Using `git bisect` (an awesome tool, by the way!), I was able to show
that reordering the equality axioms had a large impact on the time
needed to prove the problem.
Furthermore, because cut was active during the first strategy,
the proof was not found at all during the first strategy,
but because the maximum depth was set to 10,
it actually never got beyond the first strategy.
A remedy would be to limit the time spent per depth for all strategies
but the last, but I think that for now, I'm happy to know the cause.
The other problems, such as `setfam_1__t62_setfam_1.p`,
are similar in the way that they contain equality and that
they starve at the level where the old lazyCoP found the proof,
but because the new version searches the space in a different order,
it times out.



11.11.2016
==========


Git permissions
---------------

When creating a Git repository on a server, we had the problem
that permissions were not set correctly by Git.
This problem is solved by setting up a new repository as follows:

    mkdir new-repo
    chown user:group new-repo
    cd new-repo
    git init --bare --shared

To migrate an already existing repository with the wrong permission settings,
make a new repository (with right permission settings),
replace the old repository by the new repository and
`git push` from a local copy of the repository.

Another really nice command is

    git gc

which compresses the repository and deletes files in the objects directory.



27.10.2016
==========


cut4
----

I was already suspecting since some time that cut4
-- taking only the first proof for a set of literals --
is equivalent to cut1 + cut2 + cut3.
However, when comparing running with cut4 and without cut4,
the version with cut4 performed a bit better.
To solve this mystery, I compared the proof search traces
between single-strategy runs with cut1 + cut2 + cut3
with and without cut4.
The result was that the proof search traces were exactly the same.
This strongly indicates that cut4 is equivalent to cut1 + cut2 + cut3.
The reason why it performed better was probably because of strategy scheduling.


Finding problems not solved by any prover
-----------------------------------------

    (cd oraclecop/ && grep -l -L Theorem * | grep -v ".time") | sort > ocunsolved
    (cd lazycop/   && grep -l -L Theorem * | grep -v ".time") | sort > lcunsolved

We can use this to calculate the ratio of inferences for unsolved problems:

~~~ bash
for i in `comm -12 lcunsolved ocunsolved`
do
  echo $i `grep Inf oraclecop/$i` `grep Inf lazycop/$i`
done > merged
cat merged | awk '{sum+=$15/($4+$6); count++} END {print sum/count}'
~~~

The result is 1062.16.



26.10.2016
==========


oracleCoP
---------

I have implemented a version of lazyCoP that performs Monte Carlo simulations
before choosing between potential extension clauses.
When the MCTS finds a proof for the current literal, it returns it.
However, it is possible to still resume MCTS after that,
because the MCTS returns its evolving tree as a lazy list of trees,
together with potential proofs.
This makes it possible to emulate monteCoP, by just setting the
number of MCTS iterations to infinity.
That way, at the first point where an extension clause has to be chosen,
MCTS takes over until it finds a proof.

The first results look quite promising:
On the 2078 bushy MPTP problems, lazyCoP proves 624 and oracleCoP proves 475
in 100s. Of the 475, 26 problems are unique.

I measured the ratio of inferences between lazyCoP and oracleCoP.
For this, I defined the number of inferences for oracleCoP to be
the number of extension steps à la lazyCoP plus the number of UCT iterations.
(Furthermore, in case that UCT finds a proof, we add the number of
extension steps in the proof to the number of inferences.
This is to prevent that the number of extension inferences can become 0.)
If we do not add the number of UCT iterations,
then only 9 lazyCoP proof searches yield smaller number of inferences
(vs. 441 bigger), whereas adding UCT iterations gives a more realistic
ratio of 188 smaller (vs. 250 bigger and 12 equal).
The average ratio of inferences is 13.1609, meaning that on average
lazyCoP took 13 times as many inferences as oracleCoP.
This average was obtained by:

~~~ bash
for i in `comm -12 solved/bushy/100s/lazycop solved/bushy/100s/oraclecop`
do
  echo $i `grep Inf out/bushy/100s/oraclecop/$i` `grep Inf out/bushy/100s/lazycop/$i`
done > merged
cat merged | awk '{sum+=$15/($4+$6); count++} END {print sum/count}'
~~~



17.10.2016
==========


More lazy CoP
-------------

I now only calculate database entries (potentially fitting matrix entries)
in case all lemma and reduction steps were tried.
This improves performance marginally.
I gained a more influential improvement by only calculating the
literal hash in case of tracing.
This makes lazyCoP prove 438 bushy problems, compared with 441 by contiCoP.
To compare the times:

    (cd lazycop && grep -l Theorem *) | sort > lc
    (cd conticop && grep -l Theorem *) | sort > cc
    (for i in `comm -12 cc lc`; do echo $i `grep user conticop/$i.time` `grep user lazycop/$i.time` | awk '{print $2 " " $8}'; done) | sort -n


Cutting FEMaLeCoP
-----------------

Disclaimer: When I write "FEMaLeCoP", I mean FEMaLeCoP without any learnt data.

Enabling cut4 in the newly optimised lazyCoP yields
523 solved bushy MPTP problems, whereas FEMaLeCoP yields only
521 and contiCoP yields only 441.
The difference between FEMaLeCoP and contiCoP is mostly their
strategy schedule.
lazyCoP+cut4 uses the same strategy schedule as contiCoP,
only that every strategy is run with cut4.
Thus I have finally beaten FEMaLeCoP! :)
Another interesting fact is that lazyCoP+cut4 proves 47 unique problems,
whereas FEMaLeCoP proves 45 unique problems,
so they are quite complementary.



14.10.2016
==========


Lemma optimisation
------------------

Researching the cause for the divergence of lazyCoP and contiCoP on PUZ030-2.p,
I found out that contiCoP has a rather good optimisation wrt leanCoP
that it will only add a lemma if the proof was done by an extension step.
Otherwise, adding a lemma is superfluous, because the new lemma literal
would already be present among the existing lemmas or the path.
Adding lemmas only for extension steps makes the verbose prover output
the same for lazyCoP and contiCoP. :)
This optimisation should also be implemented for monteCoP.


Regularity
----------

PUZ055-1.p has exposed yet another difference between lazyCoP and contiCoP.
In lazyCoP, I checked for regularity only a single time for each entire clause.
However, I did not consider that after proving a literal of the clause,
the regularity check applied to the rest of the clause can fail,
even if it did not fail before. That is because the substitution can change,
and thus the regularity check as well.
Therefore, I now perform the regularity check again on
the whole remaining clause when a literal was proved.

This last change makes all prover traces between lazyCoP and contiCoP 100% equal
on the TPTP puzzle problems. :)


Number of proven problems for multiple provers
----------------------------------------------

    for i in *; do echo $i `grep Theorem -l $i/* | wc -l`; done


Compare number of inferences for two provers
--------------------------------------------

    (for i in `cd conticop && ls *.p`; do echo $i `grep Inf conticop/$i` `grep Inf lazycop/$i`; done) | awk '{print $1 " " $4 " " $13}'



13.10.2016
==========


Average ratio of inferences between provers
-------------------------------------------

    (for i in `comm -12 mc4 lc`; do echo $i `grep Inf montecop4/$i` `grep Inf lazycop-singlestrat/$i` | awk '{print $4 " " $11}'; done) > ratio
    awk '{ total += $1/$2; count++ } END { print total/count }' ratio


Finding the verbose prover files that diverge the earliest
----------------------------------------------------------

`cmp` is a nice little command that
prints the first difference between two files.
I use it to find out where the proof search diverges
between contiCoP and lazyCoP for many problems,
because I want to find a small example that I can analyse.

    (for i in `cd conticop && grep -l Theorem *.p`; do cmp conticop/$i lazycop/$i; done) | awk '{print $7 " " $1}' | sort -n

On the TPTP puzzle problems, PUZ030-2.p is the problem with the
earliest divergence (on line 83).

I start to like `awk`.
Now it is probably only a matter of days until I start
fantasising about Perl. :)



11.10.2016
==========


lazyCoP
-------

### On MZR@Turing testing set (400 problems)

* lazyCoP with cut4: 123 problems
* lazyCoP without cut4: 91 problems
* FEMaLeCoP: 117 problems


monteCoP
--------

### On MZR@Turing testing set (400 problems)

* monteCoP with cut: 74 problems
* monteCoP without cut: 79 problems

Furthermore, the version with cut solves only one problem
that the version without cut does not.


10s
---

Evaluations with 300s took too long to test,
so I made some evaluations with 10s.

* femalecop: 101
* lazycop: 84
* lazycop-nocut4: 83
* montecop3: 61
* montecop4: 68
* montecop4 with exploration oscillation amplitude 0.2 and period 50 iterations: 63


lazyCoP pt. 2
-------------

I found out why lazyCoP performed so badly in the previous evaluation:
It is due to two reasons:
First, FEMaLeCoP uses a strategy schedule that is superior in this test set.
Second, there are still some differences in the proof search.
They can be seen by running contiCoP with the verbose flag,
for example on PUZ004-1.p.



10.10.2016
==========


ParamILS test evaluation parameters
-----------------------------------

When ParamILS finishes the tuning, it runs the found parameters
once on all test problems and returns the final score.
To get the best configurations on the test problems,
I used:

    (for i in *algoAlgo-test*; do echo `cat $i` $i; done) | sort -n


lazyCoP cut4
------------

I think that I found out the reason why lazyCoP performed different
inferences than FEMaLeCoP: It is due to the fact that it performed
a cut on the different proofs for subclauses.
I made this cut configurable, and now at least in my initial evaluation
it proves exactly the same number of PUZ problems as FEMaLeCoP in 2s
(51), whereas with the cut, it proves only 49.



6.10.2016
=========


ParamILS round two
------------------

I took the last line of each trajectory and sorted them all.
Then, I took two solutions, namely the second-best
(because it was tried on more than double as many problems than the first-best)
and the eighth-best (because it was the first solution after the
second-best to have been tested on more problems).



5.10.2016
=========


CoP fight
---------

I noticed that on the MZR@T-100 testing dataset, lazyCoP performed quite well
compared to FEMaLeCoP.
lazyCoP gives 123 solved problems, while FEMaLeCoP (without learning data)
gives only 117.
I then looked at the times that lazyCoP needed to prove its unique problems:

    (for i in `comm -23 solved/lazycop-testing solved/femalecop-testing`; do head -1 out/lazycop-testing/$i.time | cut -f 1 -d "u"; done) | sort -n

Four unique problems are proven between 0s and 1s,
five problems between 1s and 100s, and three problems above 100s.
So perhaps I did by chance cut at a good position? :)



4.10.2016
=========


Converting ParamILS output
--------------------------

The output of the ParamILS trajectory files can be awkward to interpret.
For example, one output line is:

    80000.09000001299, 23781.86200767439, 2000, 202, 2.0, cert='1.5', certdamp='10.0', clascore='0'

I am interested in the key-value pairs.
To get them out:

    cut -d " " -f 6- | ./splitparams.sh

The `splitparams.sh`:

~~~ bash
#!/bin/bash
IFS=',' read -ra ADDR
for i in "${ADDR[@]}"; do
  echo -n $i | sed "s/\(.*\)='\(.*\)'/-\1 \2 /"
done
echo
~~~

This gives for the above example line the following output:

    -cert 1.5 -certdamp 10.0 -clascore 0


Results
-------

I evaluated the MZR@Turing-100 testing data with the best found parameters
for monteCoP and lazyCoP, both running 300s per problem.
The results are quite disappointing: monteCoP yields only 29 solved problems,
while lazyCoP gives 123. Furthermore, all 29 problems are contained in the 123.

It is interesting to note that lazyCoP is able to solve 80 problems in under 1s
and still 43 more in over 1s.
In the case of monteCoP, 28 of 29 problems are solved in under 1s, with the
29th problem being solved in 1.53s.
(The same problem "mcart_1__t31_mcart_1.p.time", btw, is solved by lazyCoP
in 0.01s.)

What is the outcome of this result?
I started to read "Single-Player Monte-Carlo Tree Search" by Schadd et al
<https://dke.maastrichtuniversity.nl/m.winands/documents/CGSameGame.pdf>
and might draw conclusions from it for improving monteCoP.
Furthermore, I might bias the selection of clauses similarly to FEMaLeCoP.
Furthermore, I should try to look whether I put some bug into monteCoP.
I should also collect some statistics as to at which depths I run out of
proof options and how often that happens.
It might also be helpful to actually have pictures of the search tree.



3.10.2016
=========


Second ParamILS setup
---------------------

I discussed my ParamILS setup with Honza and Josef.
They suggested me the following setup:

* Run lazyCoP with a high timeout (e.g. 300s) on all 1000 Mizar training problems.
* Divide the *solved* problems into sets S1 and S2, where |S1| is about 5 times |S2|.
* Run ParamILS with training set S1 and test set S2.
* Evaluate with the best ParamILS parameters the 400 Mizar testing problems.

For ParamILS, I should choose a shorter timeout per problem, e.g. 1s or 2s.
Furthermore, I should optimise for *runlength* and not for *runtime*.

Honza also told me how to read the ParamILS trajectory files:
The first column is the time at which a problem was evaluated,
and the second column is the quality of the current solution,
where a low value signifies good quality (as it corresponds to
small average number of inferences required).
The third column is the number of problems evaluated.
The average quality normally increases at first, but then decreases
as more and more problems are tried.
Honza usually gets the *last* line of the trajectory to obtain the
best solution, but the best solutions are also in the result files.

Running lazyCoP for 300s instead of 5s now solves 314 instead of 236 problems.
The training data almost doubles from 288K to 560K.

To move the problems solved from lazyCoP to a new directory:

    for i in `ls out/lazycop-300s/*.trace`; do cp "training/`basename $i .trace`" newtrain/; done

I then chose 50 random problems from the 314 solved to serve as
testing problems for ParamILS.
I then regenerated the training and testing files for ParamILS via:

    find montecop/training/ -type f > montecop/training.txt
    find montecop/testing/ -type f > montecop/testing.txt


Discussion with Cezary
----------------------

I discussed with Cezary via Tox <https://tox.chat/>, and even though he
previously reported problems with his USB microphone, it worked perfectly!
I see no reason to use Skype anymore.

We discussed the following points:

* I should consider registering for one more course at UIBK.
* I should check application deadline for extending my scholarship.
* I should not publish work-related code on GitHub because of
  university guidelines, instead use our group Git.
* For CPP, I should consider that the CL group already sends about five papers.
* It could be that Fabian Kunze is already working on a nanoCoP port.
  In that case, it might pay off to visit Jasmin Blanchette's group in Nancy
  for a week.



30.9.2016
=========


ParamILS
--------

ParamILS first finds parameters for a certain training set,
then evaluates them on a testing set.
I created a testing set with 100 problems by randomly picking them
from the 400 Mizar@Turing100 testing problems:

    ls | sort -R | tail -300 | while read file; do mv $file ../testing; done

I created ParamILS wrappers for monteCoP, which turned out to be
much less effort than expected with a single Bash script.
I am just not sure about the role of the `tunerTimeout` variable:
I suspect that this limits the time that ParamILS spends on treating
the training data before passing on to the testing data.
I set this value to 80000s (that is roughly 24h).
To run ParamILS in parallel, I use Honza's trick that I run it
with different values of `numRun`, namely from 1 to number of cores.
To use monteCoP with ParamILS, I also added an option to monteCoP
that sets the random seed.
As "best solution" value for a monteCoP run, I return the size of the proof,
measured by the number of lines of the monteCoP output.



29.9.2016
=========


Mizar@Turing100
---------------

The Mizar@Turing100 challenge is a set of problems taken from MPTP2078
that was used in CASC-J6 to test machine learning capabilities.
Provers would need to learn from training data and would then be
run on (previously unknown) testing data.
The training data contains often quite large problems where
some given premises might not be necessary for the proof.
For that reason, I extracted the original MPTP2078 names from the CASC files.
Training data is at: <http://www.cs.miami.edu/~tptp/CASC/J6/MZR4Training.tgz>.
Testing data is at: <http://www.cs.miami.edu/~tptp/CASC/J6/Problems.tgz>,
in the MZR and MZS directories.
There are 1000 training and 400 testing problems.
To get the MPTP2078 filenames from the CASC files, run:

    grep conjecture * | sed 's/.*fof(\([^_]*\)_\([^,]*\).*/\2__\1_\2/'



28.9.2016
=========


nanoCoP
-------


On Monday, Jens Otten came and held a talk at PiWo about nanoCoP.
He stayed until Wednesday and explained a lot about it to me.
I made a preliminary version of nanoCoP in Haskell that
documents some non-trivial points in the implementation.


21.9.2016
=========


Skolemisation
-------------

Again, Skolemisation is rearing its ugly head.
When leanCoP (or any of its OCaml versions) treats existential quantifiers,
it skolemises them.
However, if this Skolemisation is not done uniformly among different problems,
the "same" formulae are not syntactically equivalent any more.
For this reason, the current Skolemisation in leanCoP introduces
Skolem constants that carry in their name the *definiens*.
As this definiens tends to become very large and blows up the formula size,
it is one of the main obstacles for the comparison of terms.
Therefore, I learnt today from Josef that FEMaLeCoP was evaluated on
a version of the MML that has been uniformly skolemised with short Skolem names.

However, rereading the FEMaLeCoP paper at
<http://cl-informatik.uibk.ac.at/users/cek/docs/15/ckju-lpar15.pdf>,
there it is written that the consistent clausification is done inside FEMaLeCoP
and not as a preprocessing step outside of it.
FEMaLeCoP then deals with the resulting long names by hashing them.
The same could be done for monteCoP, just with the extension that also
literals of the clause would need to be hashed.


MD5 in OCaml
------------

I tested Cezary's MD5 function:

~~~ ocaml
let md5s s = Int64.to_int ((Obj.magic (Digest.to_hex (Digest.string s))) : int64)
~~~

It seems to be able to produce values in a wider range than, say, Hashtbl.hash.
For example, `md5s "asdf" = 3775486759822111330`, whereas
`Hashtbl.hash "asdf" = 455139366`.



20.9.2016
=========


Compiling literal statistics
----------------------------

I ran lazyCoP with statistics creation on all MPTP problems for 1s,
to prevent statistics to grow too quickly.
The resulting gzipped (!) statistics take 13G. Even grepping through
all these statistics to see how many problems were solved takes
very long. (I killed the counting after about 30mins.)
For that reason, I wrote a program that transforms the gzipped statistics
to a more condensed format, that counts literal/clause usages
and eliminates duplicates.
It seems that the condensed statistics will take around 10G.
I stopped the accumulation of the data, because it took just too long.
To decrease the data size, it will probably be better to learn data
only from the successful proofs.
For this, I should log the literal/clause usage into separate files
from the other prover output. Then, I can filter out the data
from the solved problems faster.


Accessing an SSH server without password
----------------------------------------

Thibault showed me how to access an SSH server without always typing in
your password. On the machine that you want to access from, run:

    ssh-copy-id username@server

This copies the current machine's SSH keys to the server and lets you
access the server in the future without typing your key. Magic!



19.9.2016
=========


cut
---

I found out why taking only the first proof option (cut) in lazyCoP
kept good performance, but why it heavily deteriorates results in stateCoP.
The problem is that in lazyCoP, a proof option is only available if
the proof via that option has succeeded, whereas in stateCoP
a proof option is available if just the first step of its proof
is successful.
So enabling cut in stateCoP seems like a non-trivial task.
On the other hand, it seems that lazyCoP implicitly does a cut,
because it only looks at the first proof alternative via `LazyList.peek`.
However, it is not clear at the moment whether implementing a proper cut
in stateCoP would be useful, because monteCoP probably would not use cut
at all because it does not backtrack.


RAVE
----

AMAF (All Moves As First) updates statistics not only for the
nodes on the current path, but also for all nodes below the root
that are identical to nodes on the current path.
One AMAF implementation is RAVE (Rapid Action Value Estimation):
The more often a node has been visited, the less the AMAF statistics
are weighted.
It is possible to integrate RAVE into monteCoP, by keeping
statistics for each contrapositive, and changing the statistics
when a contrapositive was used.
The statistics could be used to bias probabilities or to
modify rewards of visited nodes.


Monday meeting
--------------

The fact that I have explored no machine-learning-based reward heuristics
in UCT has irated Josef.
For that reason, I thought more about reward heuristics.
In each prover state, we have the following information:

1. Path (literals)
2. Used contrapositives
3. Open subgoals (clauses)

In FEMaLeCoP, the used contrapositives are recorded in conjunction with the path.
However, FEMaLeCoP estimates the *usefulness* of a particular contrapositive
in comparison to other contrapositives, whereas we have to estimate the
*provability* of the current state.
The provability crucially depends on the open subgoals, so it makes sense
to estimate the refutability of the subgoals.
If we consider the subgoals to be independent and refutability to be a
probability, then the total refutability is the product of the
literal refutabilites.
We can record data about the refutability of a literal by recording
proof attempts. A literal was refuted iff the cut clause was reached.
This information could be printed in clear text during the proof search,
e.g. the contrapositive hash together with the literal number.
(I should be careful to make sure that the CNFication produces equivalent
clauses.)
It rests a problem how to achieve refutabilities that are roughly between
0 and 1. Assuming refutability of a literal is the ratio of
successful refutations and refutation attempts (successful or not),
the values of this are probably quite small, thus
the product of several refutabilities are tending towards 0,
which is not good as this is added to the exploitation term, which will
dominate the result.

At a later step, I can try to consider also the used contrapositives
on the branch to determine the reward, as well as use the Naive Bayes scores
to bias the probabilities when selecting extension clauses.
As Josef said, there is much experimentation to be done.

Furthermore, Josef pointed out that it might be beneficial to compare
the number of inferences needed to solve a problem instead of using a timeout,
because the machine learning might slow things down initially.


Mizar proof recording
---------------------

I made an option for lazyCoP to print which literals it was able to refute.
However, the problem is that the file size gets out of hand pretty quickly --
for example, I calculated that the ~43000 Mizar problems take about 1TB.
For that reason, I use `gzip` to reduce the size of the proof data.
This is very effective; for example, a proof that has 24M before takes only
about 500K afterwards. `bzip2` is even more effective, but unfortunately
also much slower.



15.9.2016
=========


Experiments
-----------

Setting weight of extension clauses to 1 gives 29 solved problems.
Using cut3 (via `List.take 1 e`) gives only 3 solved problems!
This looks very strange ...
Also stateCoP exhibits the same behaviour -- not even PUZ001-1
can be solved when one takes only the first extension possibility.
lazyCoP solves the problem, however.
I adapted the database code to match lazyCoP and stateCoP,
but still stateCoP does not even prove PUZ001-1 with cut3.



14.9.2016
=========


Experiments
-----------

Setting weight of path steps to 5 while keeping
weight of extension steps at 1 / size of clause gives only
26 solved puzzle problems,
while giving weight 1 to path steps solved 29 puzzle problems.
Both UCT v1 and v2 solve 29 puzzle problems.


11.9.2016
=========


Random selection of actions
---------------------------

I changed the UCT implementation such that unexplored actions
are selected according to a certain probability, and not greedily.
This improves the TSP score to 35990 with score function 1/r^6.
Furthermore, I adapted the Haskell implementation to match the OCaml one.



9.9.2016
========


UCT reimplementation
--------------------

After having read the UCT survey, I understood that their (fairly detailed)
UCT algorithm differs from my implementation in some important points.
So I implemented their version. The biggest difference is:
Back-propagated rewards are only calculated at the *end* of a tree branch.
The so-called rewards I calculated so far for every action
are now only used to bias the selection of actions.
Furthermore, the tree is only expanded a single time per UCT iteration,
the rest of the tree that is built will be forgotten.
That should help save memory.

The new implementation gave me a new lowest distance for the TSP.
However, for this, it was necessary to massively bias
the action probability towards the shortest local distance.
All cities that were not yet visited are ranked by distance to
the last visited village. The nearest city gets rank 1.
Below are the best results by score function:

* 1/r^2: 42286
* 1/r^3: 38612
* 1/r^4: 37618
* 1/r^5: **36920**
* 1/r^6: 37319
* 1/r^7: 38696



7.9.2016
========


ParamILS
--------

I talked with Honza about parameter optimisation, to exclude me
throwing away a heuristic before having tried different parameters.
He uses ParamILS <http://www.cs.ubc.ca/labs/beta/Projects/ParamILS/>
to find `eprover` configurations.

It works as follows: He specifies in a file a list of parameters
and their possible values, together with a default value.
He also has a file where he puts the input problems.
Furthermore, he has a file where he defines ParamILS parameters,
such as the timeout per problem (`cutoff_time`) and the
total runtime of the parameter search (`tunerTimeout`).
His reward is usually the time used per problem or the number of inferences.
ParamILS then tries the same problem multiple times,
modifying the parameters until it goes to the next problem,
thus finally covering all problems.
He starts several instances of ParamILS in parallel with different
starting parameters and then takes the best (last) parameters of
all parallel runs.

Josef warned me however to get too much into parameter optimisation,
because it is apparently a potent drug.


Bug
---

I fixed the bug in the unification code that caused the stack overflow
and used the opportunity to clean up the unification code.


Two-player-based approach
-------------------------

I discussed with Chad as well as with Josef whether to implement
an approach that is based on two players: a good and a bad player.
The good player selects an extension clause, and the bad player
selects an ordering of literals, where optimally the first literal
should already turn out to not be refutable in short time.
Chad suggested to use for the bad player choices the minimum reward
and for the good player choices the maximum reward.
This would however require a deeper adaption of the UCT algorithm.
For this, I will read the MCTS survey to see how this is usually done,
for example in games such as chess.

A more lightweight alternative would be to fix the order of literals,
based on the current prover state.
For example, one could also run a one-lookahead search on all literals
to see whether there exists one that is not refutable in a single step,
then one could immediately conclude that the extension clause will
not succeed. This might save a lot of time in the end.
In case there is at least one potential proof step for all literals,
one could sort the literals by the number of potential proof steps.

Another thing to try is to learn from previous proofs which literals
of a contrapositive were the hardest to refute, i.e.
how many steps their refutation took or
how often they were not refutable at all, and then start with these.


6.9.2016
========


monteCoP is born!
-----------------

I put together stateCoP and UCT and obtained monteCoP.
As heuristic, I give lemma steps reward 1, path steps a reward of
1 divided by (1 + the number of new variables in the substitution),
and extension steps get reward 1 divided by
(1 + the number of variables in the extension clause + the number of
new literals).
I cut the proof search after 100 proofs steps on each branch.
This approach proved three new problems compared to
contiCoP and lazyCoP. However, quite some problems fail with

    Fatal error: exception Stack overflow

Recompiling with `-tag debug` and running it with OCAMLRUNPARAM=b
(after removing the try/catch in the main function) shows that
this is probably due to a bug where a circular substitution is built.
E.g. PUZ015-1.p exhibits this problem.
lazyCoP unfortunately has the same problem.

I tried pruning the tree by throwing away a child if it has
neither children nor actions left.
This solves two problems more, e.g. PUZ001+1.p, where 3662 children
are pruned away.


Normed rewards
--------------

I experimented with rewards in the interval [0, 1].
At first, I wrote a function that takes a list of state-reward pairs
and returns them with the rewards normed.
Norming the TSP rewards this way gave me worse results.
The reason is that the norm function works only locally on the
currently available options -- what should it do when there is
only a single state-reward pair available?
What is the norm of the reward then?

For this reason, I now norm the distances beforehand, which
gives me good results. Still, using a $c_p = \sqrt 2$ gives me
worse results than $c_p = 0$. $c_p = 0.1$ produces no better
solution, and $c_p = 0.2$ already produces worse solutions.
So it seems that the TSP is not really exploration-friendly.

The OCaml version seemed to consistently produce worse results
than the Haskell version. After lots of debugging, I found out
that the OCaml version sorted the best states by total reward
and not average reward. The key to finding the bug was to print
the choices of the algorithm, which were completely identical,
thus excluding a bug in the actual algorithm.


5.9.2016
========


UCT bug
-------

In our weekly Monday meeting, I presented my view of how UCT works.
Among others, I said that the reward of a tree state is the
sum of the rewards of its children. At this point Josef and Jiří
were expressing their concern that the reward should be more likely
the average or the maximum child reward.
It turns out that in the UCT paper, average rewards are used,
so I changed my implementation.
The TSP problem benchmark now gives a lowest path length of 39,062,
whereas it was previously 39,087.
However, to obtain this path length, it was necessary to get the
best state by average reward and not by number of visits.
For example, when printing on every iteration the best state by reward,
we obtain 452 solutions with a path length below 40000, however
only a single solution when we get the best state by number of visits,
and that solution is worse than the best other solution.

*Update*: Brought it down to 38,721 by setting $c_p$ to 0.
That goes a bit against the "Monte Carlo spirit", but I'll test
whether I can get better results with a somewhat changed parameter set.
Josef pointed out to me that in the UCT paper, a $c_p$ value of
1/sqrt 2 is said to satisfy the tail conditions; however, this assumes
that the rewards lie in the interval [0, 1], which is currently
not the case for our formulation of the TSP problem.
So I will test a new characterisation of the TSP with normed reward.



3.9.2016
========


List-based vs. array-based substitution
---------------------------------------

I tested stateCoP with a timeout of five seconds on the TPTP puzzle problems,
where it solved only 42 problems solved.
To find out whether this is due to the new list-based substitution,
I also ran lazyCoP with list- and array-based substitutions on
the same problems, with the result that the array-based one solved
54 and the list-based one solved 56 problems! This was rather unexpected.
Furthermore, the 56 problems are not a superset of the 54 problems.

I concluded that the worse performance of stateCoP is due to the
strict evaluation of all extension step options.
It will be interesting to see how much UCT is able to improve upon this.


Simplifying UCT
---------------

I noticed that a UCT problem can be simply characterised by yielding the
potential successor states plus their rewards, given the current state.
This eliminates the need to talk about actions that can be used to
transform the current state into a successor state.
Furthermore, I noted that my usage of lens functions in the Haskell version
could be easily omitted without blowing up the program size.
The previous Haskell version took 2394 bytes, whereas the new one only takes
2068 bytes.



2.9.2016
========


stateCoP
--------

I started and finished a version of leanCoP that is a mixture of
contiCoP and lazyCoP, which I called stateCoP.
From contiCoP comes the capability to carry around the proof obligations,
but stateCoP does it by saving a list of open goals. This way,
the proof obligations can be easily analysed.
From lazyCoP comes the idea that the subsequent possible proof steps
are returned as a list of successor states. Currently, this is returned
as a strict list, because in UCT, it is in general necessary to
evaluate all possible options before deciding for one.
This might hurt performance, but could be optimised later.

The unique feature of stateCoP is that it only always executes a single
inference step, and then returns the successor states.
This makes it possible to easily integrate it into UCT.
A full, iterative proof-search can be expressed as a one-liner.

However, it is not possible to use the previously used array-based substitution
mechanism, because stateCoP will always first try to find all possible
extension steps (thus unifying the current literal with all corresponding
literals in the matrix), then work off all the options one after the other.
However, in doing so, it can happen that the unification information about
later extension steps is overwritten after the first extension step has been
evaluated.

Adapting the array-based substitution to remedy this problem seemed hard,
so I migrated to a list-based substitution, which is completely functional
and does not carry around a global substitution.
After implementing this, stateCoP worked.



31.8.2016
=========


Emacs adventure
---------------

I was curious and wanted to try Emacs.
Unfortunately, after following some sections of the tutorial,
I felt strong pain in my left hand, probably stemming from the
repeated usage of the CTRL key. Looking for "emacs rsi" on the internet,
one can obtain lots of people complaining about it.
I even considered for some minutes getting a foot pedal to simulate keypresses,
but if you need a foot pedal to use an editor,
you should probably not use the editor in the first place.


OCaml UCT
---------

I implemented UCT in OCaml.
While testing it, I noted that `LazyList.take` from OCaml Batteries is not lazy.
I reported this at <https://github.com/ocaml-batteries-team/batteries-included/issues/688>
and got the first answer already six minutes later.
In the meanwhile, I implemented my own lazy `take` function.

Furthermore, to test UCT I implemented the travelling salesman problem
in Haskell and OCaml, where I could reuse my code from the
"Biologisch inspirierte Optimierungstechniken" course at
<https://github.com/01mf02/bio>.
The results are quite encouraging: With a very simple reward function that
always rewards the nearest city, I get down to a path of length 39,087,
whereas my best solution from the course with an Ant Colony Optimisation (ACO)
algorithm got only a path of length close to 46000.

The runtime of the Haskell and OCaml versions are quite similar, namely
15 seconds for the Haskell and 13 seconds for the OCaml version.
I compiled the Haskell version with

    ghc -O2 -Wall Salesman

and the OCaml version with

    ocamlbuild -ocamlopt "ocamlopt -unsafe -inline 100" -package batteries uct.native



30.8.2016
=========


UCT
---

I spent all day trying to understand UCT. The article by Kocsis and Szepesvári
was very useful, even if it is somewhat unclear at some places.
Some links that helped me to understand UCT:

* <https://github.com/icasperzen/hs-carbon-examples>
* <https://github.com/patperry/hs-monte-carlo>
* <http://www.princeton.edu/~rvdb/JAVA/sail/sail.html>
* <https://github.com/PetterS/monte-carlo-tree-search/tree/master/games>
* <https://jeffbradberry.com/posts/2015/09/intro-to-monte-carlo-tree-search/>
* <https://spin.atomicobject.com/2015/12/12/monte-carlo-tree-search-algorithm-game-ai/>
* <http://senseis.xmp.net/?UCT>


29.8.2016
=========


Reading
-------

I've read <http://neuralnetworksanddeeplearning.com/chap4.html>.
This article gives the intuition for a proof of universality of
neural networks, i.e. that neural networks can with arbitrarily high precision
compute any (continuous) function.
The main idea is that neural networks can be thought of as
additions of step functions at arbitrary positions and with arbitrary height,
and thus one could trivially approximate a function by sampling it and
determining the step function parameters appropriately.
This argument does not explain to me yet how this is going to work
for a function with an infinite domain and a finite neural network,
but for finite domains, it gives a good intuition.

Looking for articles about Monte Carlo tree search, I found
"Bandit based Monte-Carlo Planning" by Kocsis and Szepesvári (2006)
<https://web.engr.oregonstate.edu/~afern/classes/cs533/notes/uct.pdf>.
In this article, I learned about Markov decision processes, but some
terms, such as discounting, were not explained. I turned e.g. to
<https://www.cs.rice.edu/~vardi/dag01/givan1.pdf> for explanation.
The multi-armed bandit problem was introduced:
<https://en.wikipedia.org/wiki/Multi-armed_bandit>.

An interesting article comparing different Monte-Carlo Tree Search
algorithms is "A Survey of Monte Carlo Tree Search Methods" at
<http://repository.essex.ac.uk/4117/1/MCTS-Survey.pdf>.


Limiting RAM usage to 1GB
-------------------------

To avoid killing Bob's processes, limit all processes on server to 1GB RAM:

    ulimit -v 1000000


Constructing problems from statements and dependencies
------------------------------------------------------

Given a file 'deps.a' which has on each line the name of a conjecture, a ':'
and possibly a space-separated number of dependencies;
furthermore a file 'statements' which has on each line a TPTP axiom with
a conjecture name, construct a set of files which corresponds to the
problems consisting of the conjecture and the dependencies used to prove it:

    perl -e 'open(F,"statements"); while(<F>) {m/^fof.([^,]*),/ or die; \
      $h{$1}=$_} while(<>) {m/(.*):(.+)/; ($c,$r)=($1,$2); @r=split(" ",$r); \
      $d=$h{$c}; $d=~ s/, axiom,/, conjecture,/; open(G,">problems/$c"); \
      foreach $k (@r) {print G $h{$k}}; print G $d; close G}' deps.a

Courtesy of Josef Urban, master of Perl one-liner technology.
Takes about three seconds to construct 32000 problems.


25.8.2016
=========


Resuming proof attempts
-----------------------

If different (random) proof search trees are to be tried, evaluated and then
potentially resumed, it is probably best to resume a proof search by
characterising each Monte Carlo tree with the associated seed and then run
that again. No need for continuations or something fancy. :)

I read "Learning Heuristics for a Theorem Prover using Back Propagation"
(Ertel et al., 1989). The authors seem to use simple neural networks to
learn the optimal maximal proof search depth.
I also read parts of "Integrating Rule Based Reasoning and Neural Networks"
(Ertel et al., 1995), which proposes hybrid models combining inductive and
deductive reasoning. They give medical systems as an example, where data
from patients (inductive) and rules from doctors (deductive) are available.
The authors argue that probabilistic models are inherently hybrid.
In that article, the authors also use a neural network (LRAAM)
to classify terms, to assign meaningful and discriminating features to terms.

However, unlike <https://en.wikipedia.org/wiki/Monte_Carlo_tree_search>
is claiming, it seems that Ertel et al. did not use Monte Carlo tree search,
because they just evaluate possible options in a proof search using
previously learnt knowledge, but they are not actually attempting
different proofs inside the same proof search, then evaluating those.



24.8.2016
=========


Proof attempts
--------------

I have implemented a version of lazyCoP that is able to save also
parts of the proof that did *not* contribute to the final proof.
This works much better than calculating the statistics during
the proof search and then combining them at every step.
The final number of inferences is equivalent to the number of inferences
according to the imperatively calculated number.
Unfortunately, the new version takes longer to prove -- for one problem,
the old version took about 0.8s, whereas the new version took 2.4s.
However, it is probably too early to care much about this,
especially as on the PUZ category of TPTP, the new version still proves
52 problems, in contrast to 54 problems of the old version.
As a little bonus feature, I made coloured proof output that differentiates
between used and unused proof parts. Probably even cooler would be some
graph output, but as Josef said, it probably takes too much time to make,
and the current representation is nice already.

Josef suggested to learn from previous proofs a matrix where for every clause
the number of its occurrences in the proof-relevant and proof-irrelevant part
would be stored.
This information could be used in an evaluation heuristic for Monte-Carlo
tree search. The disadvantage is that is would be dependent on previously
analysed proofs. For that reason, I am tempted to try easier heuristics first.

Josef suggested to try to find out which literals are the hardest to refute
in a clause, and evaluate the Monte-Carlo tree search on these literals.
How to find out the refutability hardness of literals?
One way could be to learn this from previous proofs.
On the other hand, it might be that refuting an "easy" literal restricts
the search space for the hard literal (via the substitution) such that
the hard literal actually becomes easy. For that reason, one could try
to estimate the influence of literals on each other, eventually during
Monte-Carlo tree search as well. But this is probably quite complicated.

One of my ideas was to try to refute random literals from a clause,
and resume the search that produced the most unlikely proof, i.e.
a proof of a literal that is unique (no other searches refuted it)
and had a low probability of being found, because it consisted of
many choices. The probability is the inverse of the product of
the cardinalities of the unifying clauses along the path.

Another interesting way to evaluate partial proof trees is to
measure the number of different possibilities to refute all subgoals,
possibly combining this also with learnt information which potentially
unifying clauses have contributed in the past to most refutations.

What role should features of a partial proof tree play?

Neural networks could be another way to find the most promising
proof attempt. However, I think their evaluation should be postponed
until I have somebody at my disposal who is experienced with neural networks.
According to Josef, when Cezary will be back from the US,
he might have that experience.

Another problem consists in how to resume a partial proof search.
Chad suggested the use of zippers to represent "proofs with holes" as in
<https://en.wikipedia.org/wiki/Zipper_(data_structure)>.



22.8.2016
=========


Frustration
-----------

I tried to return statistics for every proof and subproof.
This proved to be amazingly difficult and complicated the whole code a lot,
to an extent that it feels like twice as big and actually contains an error
that I was not able to find for hours, namely that the inferences are not
counted correctly.
I am thinking of calculating the statistics on demand only when I actually
need them, saving the proof attempts along with the actual proof so I can
analyse them.
Josef also suggested implementing the Monte-Carlo algorithm in Prolog or
in case of extreme desperation just printing the inference information
to stdout and parsing it from there. I am not *that* desperate, however ... :)



19.8.2016
=========


Debugging leanCoP
-----------------

Running some TPTP problems in leanCoP would result in error messages like

~~~
calling an undefined procedure f(_3116) in module eclipse
abort
~~~

I debugged this together with Jiří. First, we found out that putting
simple `print` commands did not print anything. Printing to stderr
was the solution:

~~~ prolog
write(user_error, 'Hello World'), nl.
~~~

We finally traced the problem to leanCoP not being able to parse CNF files.
I'm sorry, but: Aargh!
To solve this (and the fact that the OCaml leanCoPs cannot parse `include`
statements), I built into the Makefile a rule that converts all input problems
to FOF with includes expanded:

~~~ makefile
fof/%: tptp/%
	@mkdir -p "`dirname $@`"
	./tptp4X -x -t fofify "$<" > "$@"
~~~

The next strange error were Prolog messages of the form

    ERROR: Out of global stack

on the server. A call to

    ulimit -a

showed that the stack size was set to a rather low value of 8MB.
For future runs of Prolog, I should set it to:

    ulimit -s unlimited

Next, I compared the performance of leanCoP 2.1 with a version where
I modified the lemma rule from

    member(LitL,Lem), Lit==LitL, Cla1=[], Proof1=[]

to

    member(LitL,Lem), Lit==LitL, Cla1=[], Proof1=[], !

On the first evaluation on the server grid02 (with 32 instances running
in parallel) on all FOF TPTP problems, both versions proved roughly
the same number of problems, but different ones.
Therefore, I reran both provers only on the problems that they could not solve,
but the other one could. For this, I just deleted all the logs of
these problem via

    for i in `comm -23 solved/leancop-2.1 solved/leancop-2.1-cut1`; do find out/leancop-2.1-cut1/ -name $i -exec rm \{\} \;; done

and reran `make`. The result was quite surprising:

    mf@grid02:~/cop/eval$ wc -l solved/leancop-2.1*
     1985 solved/leancop-2.1
     1985 solved/leancop-2.1-cut1

So actually the cut1 does not seem to have a great influence on the
performance of leanCoP on the TPTP problems. This seems counter-intuitive
to me, and I cannot exclude the possibility of me having made a mistake
somewhere during the evaluation, although I rechecked everything.
By the way, also the problems solved by both provers were nearly exactly
the same, with 1983 problems being solved by both provers.

Next, I did a similar evaluation on the PUZ problems for OCaml leanCoP and
monteCoP. Here, at first leanCoP proved 55 problems and monteCoP 53,
but once I reran both provers on the problems that only the other prover solved,
it was 55 against 55.



18.8.2016
=========


Large problems
--------------

Some problems, such as PUZ015-2.006.p, produce quite large proofs (about 500M),
which causes the proof printing to be killed when a timeout is set.


`cut1`
------

I realised that the contination-style leanCoP can safely cut the
literal proving if the negated literal is among the lemmas.
This is activated by the `cut1` flag.
I implemented the same functionality in monteCoP, which gives a
slight increase of inferences, but unfortunately not more solved problems.

I also measured the total number of inferences per prover on the PUZ problems
via the amazing command

    paste -s -d+ | bc

and the results are that monteCoP without cut after successful lemma step
gives 61,130,086, monteCoP with this cut gives 69,721,307, and
leanCoP (OCaml version) gives 180,159,238.

Furthermore, I implemented the `cut1` in the original Prolog leanCoP 2.1,
where it increases the number of solved problems in the PUZ category
without axioms from 11 to 13. I notified Jens about this.


Lazy list speed
---------------

Because the number of inferences of the lazy list version of leanCoP
is so much lower than the continuation passing style version, I thought
the culprit could be that fact that the potential contrapositives were
always retrieved, even if the lemma or the path rule would find a proof.
To test this hypothesis, I made a function that can be used to construct
a lazy list from a lazily passed strict list.

~~~ ocaml
let list_fun (f : 'a list Lazy.t) : unit -> 'a option =
  let l = ref f in
  fun () -> match Lazy.force !l with x::xs -> l := lazy xs; Some x | [] -> None
~~~

You can use it for example via

    let x = list_fun (lazy (print_endline "Hi"; [1; 2])) |> LazyList.from_while;;

which returns an `int LazyList.t`, but does not compute the actual list yet,
unlike `LazyList.of_list`.
I also contributed my function to the OCaml Batteries team at
<https://github.com/ocaml-batteries-team/batteries-included/issues/687>.

Unfortunately, the version of monteCoP using this technique to delay
the contrapositive calculation until needed seems to be slower than
just finding the contrapositives strictly.
In particular, the number of inferences performed on PUZ054-1.p
drops from 1,249,780 to 1,199,180, whereas the continuation passing leanCoP
achieves 2,304,523.

A further speed increase is obtained by saving the database entries already
as lazy lists, which prevents us from having to convert to lazy lists
every time we obtain a database entry.
Furthermore, it seems that there really is a difference between
`LazyList.of_list` and `LazyList.eager_of_list`, where using the latter
can make the whole program about 5%-10% faster.



17.8.2016
=========


Testing monteCoP
----------------

To test ATPs on the CNF/FOF problems (containing '+' or '-'
in the filename) in TPTP, I created a Makefile:

~~~ makefile
# input files (FOF/CNF TPTP files contain either + or - in filename)
INFILES=$(shell find in/ -type f -regex ".*\(\+\|-\).*\.p" | sort -R)

# timeout (in seconds)
TIME=5

all: solved/leancop solved/montecop

out/leancop: $(patsubst in/%,out/leancop/%,$(INFILES))
out/leancop/%: in/%
	@mkdir -p "`dirname $@`"
	-timeout $(TIME) ../leancop.native "$<" > "$@"

out/montecop: $(patsubst in/%,out/montecop/%,$(INFILES))
out/montecop/%: in/%
	@mkdir -p "`dirname $@`"
	-timeout $(TIME) ../montecop.native "$<" > "$@"

# collect solved files for a given prover
solved/%: out/%
	@mkdir -p "`dirname $@`"
	grep -rl $< -e "Theorem" | xargs -L1 basename | sort > "$@"
~~~

We can compare the results via:

    comm solved/*

I see that 43 problems are solved by both monteCoP and leanCoP,
but 6 problems were solved only by leanCoP and 3 only by monteCoP.
Furthermore, for example the file "PUZ047-1.p" shows that monteCoP needed
a lower number of inferences than leanCoP.

* monteCoP: Inf: 4325 Depth: 7 DInf: 705 Str: 2
* leanCoP: Inf: 5432 Depth: 7 DInf: 1507 Str: 2

Looking at some of the files, my hypothesis was that monteCoP still performs
fewer inferences in the same time than leanCoP.
We can output the number of inferences for all problems via:

    grep -r out/montecop/ -e "Inf" | cut -d ' ' -f 3 | sort -n

Saving the results into files `lc` and `mc`, we can visualise them via:

    gnuplot -persist -e "plot 'mc', 'lc'"

The plot clearly shows that leanCoP is able to perform more inferences
than monteCoP. So what is holding back monteCoP?

Note: monteCoP is currently not doing anything like Monte-Carlo tree search,
however it is the implementation based on lazy lists that should be
enhanced by Monte-Carlo elements.


Common Lisp
-----------

Out of curiosity I wanted to learn some Lisp, probably because
Chad impressed me with his amazingly fast in-console text manipulation.
I chose to follow <http://lisp.plasticki.com/>, where I got until
the section about strings. Here are my solutions.

~~~ commonlisp
(defun square (x) (* x x))
(defun triangular (n) (/ (* n (+ n 1)) 2))
(defun dice () (+ 1 (random 6)))
(defun two-dice () (list (dice) (dice)))

(defparameter *km-miles* 0.621371192)
(defun convert-km (km) (* km *km-miles*))
(defun convert-miles (miles) (/ miles *km-miles*))

(defun average3 (a b c) (/ (+ a b c) 3))
(defun cubesum (a b) (let* ((sum (+ a b))) (* sum sum sum)))
(defun pseudo-primes (x) (+ (- (* x x) x) 41))

(defun swap (l) (cons (second l) (cons (first l) (rest (rest l)))))
(defun dup (l) (let* ((fst (first l))) (cons fst (cons fst (rest l)))))
(defun random-elt (l) (nth (random (length l)) l))
(defun last-elt (l) (nth (- (length l) 1) l)

(defun midverse (s)
  (let* ((l (length s)))
    (concatenate 'string
      (subseq s 0 1)
      (reverse (subseq s 1 (- l 1)))
      (subseq s (- l 1) l))))
(defun rotate (s n) (concatenate 'string (subseq s n) (subseq s 0 n)))
~~~

Fun fact: After doing these exercises, my shift key would work less reliably
due to typing so many parentheses. :)



16.8.2016
=========


Discussion with Josef
---------------------

* Monte Carlo: Josef pointed out that in Monte Carlo based tree search,
  one should try different random paths with limited depth,
  learn from these explorations, then continue the search deeper,
  integrating the best elements from the sample explorations.
  The problem lies in the evaluation function: What is a promising path?
  In the beginning, one could just check how many subgoals there are
  on the path.
* Josef pointed out that it might be beneficial to first treat subgoals
  that look hard to refute. If the refutation for this fails,
  one does not need to consider the parallel subgoals any more.
* Model checking: Using a model checker such as in MaLARea could help
  in two ways: First, if the model checker finds that some subgoal a
  is actually satisfiable, then the whole clause cannot be refuted
  and all alternative subgoals fail.
  Second, if the model checker finds a subgoal a unsatisfiable,
  the theorem prover can go work on a different subgoal first,
  because it estimates that a can be refuted later.
* DeepMath / HoloPhrasm: We discussed neural networks,
  and Josef expressed the hope that they could replace the human intuition
  to check whether a proof is currently going in the right direction.
  I think he mentioned recurrent neural networks which could do
  classification of textual examples.
  Browsing on the internet, I stumbled over this quite nice presentation:
  <http://www.coling-2014.org/COLING%202014%20Tutorial-fix%20-%20Tomas%20Mikolov.pdf>


Lazy lists
----------

I analysed the leanCoP source code and found that enumerating
different possibilities should probably best be done with lazy lists.
However, the lazy lists in the OCaml standard library,
<http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html>,
seem to be quite awkward to use; on the other hand,
OCaml Batteries has a quite nice lazy list module,
seems reasonably documented and can be installed in Ubuntu via

    apt-get install ocaml-batteries-included

One should also place
<https://github.com/ocaml-batteries-team/batteries-included/blob/master/ocamlinit>
in the source directory. While already at it, one can also write

    #directory "_build";;

into that file so that when running a toplevel version of the code,
one does not always change into the build directory before opening modules.

Anyway, I created a first version of leanCoP with lazy lists that already claims
to prove PUZ001-1.p, but the number of inferences looks suspiciously small.
To be investigated.



15.8.2016
=========


Monte Carlo leanCoP preparations
--------------------------------

I decided to integrate some simple probabilistic heuristics in leanCoP
that go beyond the ideas that were tried in randoCoP.
For example, the selection of a contrapositive should assign a weight
to each contrapositive (e.g. using k-NN with the active path symbols)
and then, a contrapositive is drawn with a certain probability
based on the weight.
To copy some ideas from the whiteboard:

* The more other choices exist, the lower the chance for a deep path.
* The more effort parallel subgoals took (e.g. number of different
  contrapositives tried, or number or inferences per subgoal),
  the higher the chance for doing more effort in current subgoal
  (e.g. higher path limit, more different options tried).
* We can learn how much effort it takes to discharge
  subgoals of a certain clause. This could influence the depth probability.
* Is completeness maintained?

To start this off, I want to begin with a clean leanCoP implementation
that I can easily understand and modify. For this, I took the leanCoP
implementation in the repository and compared it with FeMaLeCoP,
trying to synchronise them such that `diff` reports few differences.
The OCaml leanCoP has the nice feature that it reports which strategy
succeeded, but it did not output the proof.
I adapted FeMaLeCoP that it now also outputs the used strategy and
leanCoP that it outputs proofs.
Furthermore, to be able to access the option in leanCoP about the
proofs of "parallel" subgoals, I believe it is necessary to let go
of the continuation passing. However, I need to make sure that this
still works with the cut.


`ocamlopt` flags
----------------

I found the solution to my `ocamlopt` problem from 10.8.2016.
One can pass options to `ocamlbuild` such as:

    ocamlbuild -ocamlopt "ocamlopt -unsafe -inline 100" $(FILE)



12.8.2016
=========


`SEU/SEU652^1.p`
----------------

I ran the problem `SEU/SEU652^1.p` with the premise selection code and
noted that it terminates after about 15 seconds with error code 141.
Strangely, when we increase verbosity to above 4, the problem
gets solved by mode 484, which calls `eprover`.
Also just calling mode 484 on the problem works.
It might be a problem of interplay between different modes being run.


First premise selection results
-------------------------------

I integrated k-NN into Satallax, as described yesterday.
As dependencies, I just used the empty list for every axiom.

I ran Satallax on a file `thf-manyaxioms` that contains
one TPTP problem name per line, consisting of 31 problems:

~~~ bash
#!/bin/bash
for i in `cat thf-manyaxioms`
do
  FILE=`find ~/Downloads/TPTP/ -name $i.p`
  if [ -n "$FILE" ]; then
    echo $FILE
    ./satallax.native -t 30 $FILE
  fi
done
~~~

The result is a bit awkward to interpret, because it has the
file name and the result on different lines. Excerpt:

~~~
/home/mfaerber/Downloads/TPTP/SEU/SEU652^1.p
% SZS status Theorem
% Mode: mode518
% Inferences: 236
/home/mfaerber/Downloads/TPTP/SEU/SEU653^1.p
% SZS status Timeout
% Inferences: 19547
~~~

To get them into shape, a little bit of Haskell:

~~~ haskell
import Data.Maybe
import Data.List

split [] = []
split (fst : lines) =
  let (hd, tl) = break (isJust . stripPrefix "/home") lines
  in (fst : hd) : split tl

main = interact (unlines . map (intercalate " - ") . split . lines)
~~~

Example output:

~~~
/home/mfaerber/Downloads/TPTP/SEU/SEU652^1.p - % SZS status Theorem - % Mode: mode518 - % Inferences: 236
/home/mfaerber/Downloads/TPTP/SEU/SEU653^1.p - % SZS status Timeout - % Inferences: 19547
~~~

To get all the problems from `pslog` that Satallax was able to prove:

    runhaskell splitres.hs < pslog | grep Theorem | cut -d " " -f 1 > psth

I did this for versions with and without k-NN premise selection.
To compare them, I ran:

    comm <(sort nopsth) <(sort psth)

It turns out that without premise selection, 13 problems were solved,
versus 17 with premise selection. Of these, 2 were uniquely solved
without PS and 6 uniquely with PS. The union therefore is 19.
Optimistically calculating, that is a plus of 46% on these problems! :)
Perhaps even better results could be achieved by integrating more
PS methods (such as SInE resp. MePo) and combining their results,
or by tweaking the ranking, e.g. giving the same rank to axioms
that got the same weight from k-NN.


k-NN age
--------

I experimented with the `age` in k-NN.
At first I thought that removing it improves results, but then I found out
that this was due to a bug, namely that I did rank all theorems,
not only the first n ones, where n specified by the user.
The bug being fixed, it looked like the current age calculation
really works best.

* age: age(k) = 500000000 - 100 * k
* agelin: age(k) = -100 * k
* noage: age(k) = 0

Results:

                    Samples      Cov     Prec      Rec      AUC     Rank
------------------ -------- -------- -------- -------- -------- --------
knnisabelle-age        1650    0.833    5.983   412.39   0.9491   104.23
knnisabelle-agelin     1650    0.832    5.977   418.12   0.9483   105.98
knnisabelle-noage      1650    0.831    5.954   622.09   0.9144   175.37



OCaml is dangerous
------------------

Consider the following code:

~~~ ocaml
if Hashtbl.mem to_eval i then
  print_eval no_th stdout i (knn_eval (th_syms i) (sym_ths, sym_wght) deps i 2048);
add_syms (th_syms i)
~~~

Imagine you want to refactor this such that the call to `knn_eval`
is on its own line:

~~~ ocaml
if Hashtbl.mem to_eval i then
  let ans = (knn_eval (th_syms i) (sym_ths, sym_wght) deps i 2048) in
  print_eval no_th stdout i ans;
add_syms (th_syms i)
~~~

However, this will get parsed to:

~~~ ocaml
if Hashtbl.mem to_eval i then
  (let ans = (knn_eval (th_syms i) (sym_ths, sym_wght) deps i 2048) in
  print_eval no_th stdout i ans;
  add_syms (th_syms i))
~~~

To get the desired behaviour, you need to write:

~~~ ocaml
if Hashtbl.mem to_eval i then
  (let ans = (knn_eval (th_syms i) (sym_ths, sym_wght) deps i 2048) in
  print_eval no_th stdout i ans);
add_syms (th_syms i)
~~~

So by adding the `let`, the `if` captures multiple commands after it.
Extremely unintuitive, if you ask me. Solution? Use Haskell! :)



11.8.2016
=========


Sledgehammer proof terms
------------------------

Chad gave me a set of problems where he suspects that the large number of
axioms prevents Satallax from finding a proof, however,
Isabelle found proofs for these problems.
I would like to find out which axioms Isabelle used to prove the problems.
I tried things like

    ./isabelle tptp_sledgehammer 10 test.p

but the output is quite cryptic:

~~~
0:00:05 elapsed time, 0:00:14 cpu time, factor 2.80
running spass for 1 s
SUCCESS: spass
% SZS status GaveUp
poly: : warning: The type of (it) contains a free type variable. Setting it to a unique
~~~

Does this now mean that SPASS was successful or that it did not find a proof
and gave up?

Chad suggested me to generate the output files with

    ./isabelle tptp_translate FOF test.p

and then run some first-order prover on them, then get out the axioms it used.


OCaml k-NN
----------

I tested the OCaml version of k-NN in the repository, and its performance
seems to strongly depend on the value of k chosen.
For k = 100, it approaches to the performance of the C++ version,
but it is still not completely there.
For this reasons, I created a new OCaml version based on the C++ version,
which now exceeds the C++ version's AUC a bit for the Mizar dataset,
probably due to the `fast_sort` algorithm used.
It seems really that the sorting makes a difference both in
runtime and in statistical performance.
`knnold100` represents the previous OCaml implementation of k-NN with
k = 100.

* `heap_sort`: 0:05.42elapsed
* `fast_sort`: 0:05.09elapsed
* `knnold100`: 0:01.99elapsed

Statistics:

           Samples      Cov     Prec      Rec      AUC     Rank
--------- -------- -------- -------- -------- -------- --------
heapsort      1689    0.637   10.728   413.80   0.8947   181.00
fastsort      1689    0.877   13.616   304.58   0.9376    60.41
knnold100     1689    0.751   10.809   420.16   0.9173    86.71


Integrating premise selection into Satallax
-------------------------------------------

I discussed with Chad how to integrate premise selection into Satallax.
He told me that the `RELEVANCE_DELAY` is the place to look at,
because this influences the order of theorems used based on a simple heuristic
that checks which axioms have symbols in common with the conjectures.
This also considers the polarity of the subformula where the symbol appears.
The easiest thing to do would be to just add the rank (0, 1, ...) of the axiom
according to the premise selection to this relevance delay.
However, Chad pointed out that this might lead to already the second axiom
never being used, so he suggested me to group the axioms.
For example, instead of adding (0, 1, 2, ...), I might add
(0, 0, 0, ..., 1, 1, 1, ...).

I did some work on integrating the premise selection, but I got distracted
by the convoluted search.ml file which I wanted to refactor by splitting
it into preprocessing and search parts, but I stopped this in the end
because I noted that the preprocessing is too closely intertwined with the
actual search.


Neural ATP
----------

Josef sent me an article about a neural automated theorem prover for
higher-order logic. When Chad saw it proved problems from MetaMath,
he first commented that MetaMath was not really higher-order logic,
so I take the results with a grain of salt.



10.8.2016
=========


OCaml flags
-----------

Having worked on the C++ version of MePo, I naturally want to
port it to OCaml so we can use it in Satallax.
As there already exist some classifiers in OCaml, I looked at
their Makefile, which contained entries like:

~~~
knn: utils.ml format.ml tfidf.ml knn.ml
       ocamlopt -inline 100 -unsafe unix.cmxa str.cmxa $^ -o $@
       rm *[.]cm* *[.]o
~~~

The new version is:

    %.native: *.ml
    	ocamlbuild -libs str,unix $@

I searched for a solution to include the `-inline 100 -unsafe` flags
from ocamlopt in the `ocamlbuild` version, but failed.
Strangely, according to
<https://groups.google.com/forum/#!topic/fa.caml/z-vQtT_rNgk>,
it should work via

    ocamlbuild -cflags -inline,2 target.native

but doing that gives me the error

    /usr/bin/ocamlc: unknown option '-inline'.

The problem is that `ocamlopt` is not used, and `ocamlc`
does not seem to support the `-inline` flag. How to solve this?


MePo pt. 2
----------

I worked some more on the C++ version of MePo, reducing runtime from
41sec to about 35sec by not entering a while loop if it is clear
from the start that it will return some value.
Furthermore, for the Euclidean distance, I improved runtime from
5min2sec to 46sec. The reason for the slow runtime was the copying
of the current symbol map (C++ map from long to double), which was
copied in the first place to modify it by and by, to achieve the
set-theoretic difference.
While this is good idea once the number of features becomes
very large, the runtime penalty induced by copying the map outweighs
the benefits.

Runtime results:

* Cosine: 35sec
* Jaccard: 37sec
* Euclid: 46sec

Statistical results:

              Samples      Cov     Prec      Rec      AUC     Rank
------------ -------- -------- -------- -------- -------- --------
mepocosmizar     1689    0.673    9.387   476.16   0.8123   149.82
mepojacmizar     1689    0.657    9.111   530.70   0.7757   184.99
mepoeucmizar     1689    0.567    7.989   566.35   0.7468   198.00



9.8.2016
========


MePo
----

To integrate SInE in Satallax as a baseline comparison for ML methods,
I asked Cezary some time ago whether he had an implementation of SInE.
His response:

> We already quite extensively use Sine in E and Vampire.
> In fact the Epar strategies suggest more use of Sine in Mizar problems
> / Flyspeck problems than with usual TPTP.
>
> Also sine and mepo are quite similar in the overall way they work -
> recursively adding more to a set of similar things with some trigger -
> and the 'mepoX.cpp' do implement versions of this algorithm adapted to
> higher-order logic and without the special Isabelle tweaks.
>
> And we also did E.T. which combined Sine with kNN and NB in a FOF prover.

So I looked at "mepo3.cpp" in the repository and first transformed it
to the style of the other "modernised" predictors, to actually understand
what it does.
This new version of MePo runs much faster than the old one;
on the Mizar-MPTP2k dataset, the old takes 18min56sec vs 0min41sec for the new
with identical statistics.
However, they are both a lot slower than k-NN, which takes only
1sec43msec and has better statistical values:

           Samples      Cov     Prec      Rec      AUC     Rank
--------- -------- -------- -------- -------- -------- --------
mepomizar     1689    0.673    9.387   476.16   0.8123   149.82
knnmizar      1689    0.874   13.619   249.75   0.9202    54.41


SInE
----

I read the article "Sine Qua Non for Large Theory Reasoning" (2011) by
Kryštof Hoder and Andrei Voronkov at
<http://link.springer.com/chapter/10.1007%2F978-3-642-22438-6_23>.
A nice overview of SInE is
<http://resources.mpi-inf.mpg.de/departments/rg1/conferences/deduction10/slides/krystof-hoder.pdf>.

However, for me a major weakness of SInE seems to be that the trigger relation
is a black-or-white thing. This is also indicated by the
generality threshold and the tolerance, which make the algorithm work better
under some circumstances, but smell like workarounds.
I think it would be better to have a notion of "trigger probability",
where a symbol s triggers an axiom A with a certainty p.
The certainty of an axiom A could then be the sum of certainties from
any symbol s to A.
The certainty of a symbol could be its inverse number of occurrences.
Or something like that. One should probably also make an inductive definition,
where axioms and symbols are k-certain,
otherwise our calculation might not terminate.

However, I'm asking myself whether the idea I roughly sketched out there
is not already implemented somewhere. I would be surprised if that was not case.
Furthermore, has SInE in Vampire received any substantial updates since
the writing of the article (2011)?


Knights and Knaves
------------------

Chad and I further discussed the TPTP problem PUZ/PUZ081^1.p,
and noted that one axiom was actually not used in the Satallax proof.
That is because the conjecture says something like
"there exists a knight and a knave", and the proof actually does not say
*who* is the knight and who is the knave. It rather makes a case distinction
and says that in both cases (Mel is a knight or Mel is not a knight) there
exists a knight and a knave.
The original problem description is thus actually hard (or impossible?)
to model in classical logic. However, by embedding
intuitionistic first-order logic in classical higher-order logic,
it might be possible to state the problem in such a way that it actually
corresponds to the original "knights and knaves" problem.


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
to show only $\vdash p \to \bot$ nor is it possible to show only $\vdash p$.

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



21.6.2016
=========

Cut
---

Using cut on Chad's problem seems to prevent finding a solution,
in leanCoP as well as in mlCoP.



17.6.2016
=========

Prenex normal form
------------------

Unfortunately, we cannot abolish the usage of prenex normal form,
because the clausification depends on it to function properly.
For example, clausifying $a \lor (\forall x. (P(x) \land b))$
will not rewrite via $x \lor (b \land c) \implies (x \land b) \lor (x \land c)$,
because the forall quantifier is in the way.
Adding new rewrite rules will also not work, because one can have arbitrarily
complex nestings of quantifiers.

`simp_tac`
----------

To use `simp_tac` during the preparation tactics, `simp_tac` alone is
not sufficient, because $\neg (a \land hashek)$ does not get rewritten.
On the other hand, when using `asm_full_simp_tac`, the different clauses
seem to interact with each other, which can lead to the actual conclusion
being lost. So `full_simp_tac` so far seems to be a good compromise.



13.6.2016
=========

A counterexample for Skolem functions with congruence axioms
------------------------------------------------------------

$\forall x. \exists y. P x y \land \lnot  Px y \land x = c$

Skolemised: $\forall x. P x (f x) \land \lnot P x (f x) \land x = c$

* $P a (f a)$
* $\lnot P a (f a)$
* $a = c$

* $P b (f b)$
* $\lnot P b (f b)$
* $b = c$

$P a (f a)$ is resolved with $\lnot P b (f b)$ by rewriting $a$ with $b$.
That only works, however, if congruence axioms for Skolem functions
are produced.

A counterexample for inner Skolemisation
----------------------------------------

1. $\lnot P(a, c)$
2. $\forall w. P(w, c)$
3. $\forall x y. P(x, y) \land \exists z. \lnot P(z, y)$

Inner-Skolemize $F_3$, yielding: $P(x, y) \land \lnot P(f(y), y)$

Instantiate $F_3$ two times:

1. $P(a, c) \land \lnot P(f(c), c)$
2. $P(f(c), c) \land \lnot P(f(c), c)$

We can resolve these two and get: $P(a, c) \land \lnot P(f(c), c)$.
Resolving this with original $F_1$ and $F_2$ gives $\bot$.

However, the first resolve step did only work because the $f(c)$ witnesses
were the same in the inner Skolemisation; in the natural deduction scenario,
these would actually be different witnesses!
So one has to do something about them if one uses inner Skolemisation.



8.6.2016
========

$\sigma$ is a confluent TRS, so equality modulo $\sigma$ is decidable

Construct *one* deep formula for all copies of a clause, then
extract relevant parts for each clause copy



30.5.2016
=========

* Every clause has disjoint variables (via offset).
* Every Skolem function will contain only variables that have occurred
  in "higher" $\forall$s.
* We want to create a single ground instance for all copies of a clause.
* To avoid duplicate existential retrieval, we need to find paths that are
  equal modulo $\sigma$.
* Idea: Construct expansion tree for each clause copy separately,
  then merge them.
* For this, store the offset of the clause copy at every $\forall$ of its
  expansion tree, then when merging two expansion trees, store list of
  offsets among every $\forall$ quantifier.
* Question: How to detect case where e.g. $P(x) \land \lnot P(x)$,
  where $x$ does not have to be instantiated, but still contributes
  to the result?
  Read again Pfenning? TODO: Answer Miller mail!
* Inner Skolemisation: Example where it makes a difference:
      - $\forall x y. P(x, y) \land (\exists z. P(y, z))$
      - Skolemised: $P(x, y) \land P(y, f(y))$
  In proofs, this would create different witnesses for $f(y)$ even if
  the values of $y$ would be the same, because the Isabelle witnesses
  for $f(y)$ depend also on $x$.
  However, it might be possible nonetheless to reconstruct such proofs,
  because the refutation of a formula containing such an existential witness
  can only work if the refuting formula contains an all-quantified variable
  at the place of the witness. (Does this claim really hold?)
* Store witness to be the (Skolem function ID, variables),
  where a variable is an offset and a variable ID



26.4.2016
=========

`HEADGOAL` is called *twice* when there exists at least one goal:
Once on a state with no subgoals, and once on a state with the actual
first subgoal. I suspect this is such that there always exists a head.

For example:

~~~ sml
fun dummy_method opt ctxt facts = HEADGOAL (K (print_tac ctxt "Hoy") THEN' K all_tac)

val _ =
  Theory.setup
    (Method.setup @{binding dummy}
      (Scan.lift (Scan.option Parse.nat) >> (METHOD oo dummy_method))
      "Dummy")
~~~

~~~ isabelle
lemma "True" apply (dummy)
~~~

Output:

~~~
proof (prove)
goal (1 subgoal):
 1. True 
Hoy
No subgoals! 
Hoy
 1. True
~~~



21.1.2016
=========


Getting the total number of assumptions in Flyspeck problems
------------------------------------------------------------

    (for i in *.p; do echo $i `grep Assm $i | wc -l`; done) > assmn
    cat assmn | cut -d ' ' -f 2 | paste -sd+ - | bc

Inspiration from: <http://stackoverflow.com/questions/926069/add-up-a-column-of-numbers-at-the-unix-shell>



18.1.2016
=========


Experiments with 2s automatic mode
----------------------------------

I ran Satallax in 2s automatic mode on all Flyspeck problems.
The result: 3139 proven problems
This against: 3303 proven problems in 1s w/o and 1s with guidance

The difference is only about 5%, but might be put into perspective if
we look at the modes used in the 2s automatic mode: There, about 404
new solved problems come from a mode that was not used in the 1s run.
In total, 11 different modes found proofs during the 2s run, while
during the 1s run, it was only 7.

Running the evaluation again for 2s, but limited to the modes that
were used in proofs found during the 1s run, we get now 2751 solved problems.
That is a plus of only 5.1%, compared to the plus of 26% with internal guidance.



15.1.2016
=========


TPTP problem categories with most THF files
-------------------------------------------

The top three:

* SEU: 847
* SYO: 658
* SEV: 409

Proudly presented by:

    (for i in TPTP-v6.2.0/Problems/*; do echo `ls $i/*^?.p | wc -l` $i; done) | sort -n

We get the following results:

* SEU: 501 solved, 0 with ascending normalisation
* SYO: 433 solved, 3 with ascending normalisation, 1 without
* SEV: 152 solved, 5 with ascending normalisation, 2 without

Interesting about the SEU results is that there were very few negative examples.

Just for reference, the 902 Mizar problems: 572 solved, 15 with ascending normalisation, 2 without

All of these experiments were run in automatic mode. This might be responsible
for the bad results for Mizar, or it could be the filtering of terms with
fresh variables.



14.1.2016
=========


Experiments with whole Flyspeck (14185 problems)
------------------------------------------------

Automatic mode, simple classifier:

* Training proven: 2616
* ML additional proven: 687 (+26%)

Training:

    1672 mode175
     476 mode188
     319 mode238
      74 mode288
      41 mode252
      29 mode19c
       4 mode289

ML:

    423 mode288
    156 mode175
     79 mode188
     11 mode289
     11 mode238
      3 mode252
      2 mode19c


Mode 483, simple classifier:

* Training proven: 2054
* ML additional proven: 270 (+13%)


Get number of problems proven by mode
-------------------------------------

    grep mode training/*.log | cut -d ' ' -f 2 | sort | uniq -c | sort -rn



13.1.2016
=========


Normalisation
-------------

Without normalisation: 43
With complete normalisation: 37


Dissertationsvereinbarung
-------------------------

I talked with Aart and René, and we came to the following agreement:

* SS2016: trial of MIP seminar, two courses in "generic competences"
* SS2017: MIP seminar, defensio



8.1.2016
========

Experiments to run
------------------

* Run Satallax with 5s and see how many theorems solved then with ML
* Run Satallax on whole Flyspeck
* Adjust weights for simple classifier


On-the-fly evaluation
---------------------

I tried to build the initial training files by updating the simple classifier
on the fly, that is, after every solved training file. The results were quite
discouraging, however: We increase from 276 only to 278 solved problems.
The number is the same whether we learn the problems in order or reversed.

To be more precise: 269 problems solved by both, 7 problems only solved by
training, and 9 problems only solved by on-the-fly.


7.1.2016
========


Term classification
-------------------

I've experimented with the term classifier and positive/negative examples.
So far, the experiments show that adding even very small values for every
feature that appears both in a training example and among the current
features (axioms) gives worse experimental outcome. So far, I did not consider
IDF data, but it seems unlikely to me that this will change things.
I'll experiment with the `fi` and `fr` functions of the Bayesian classifier.
Furthermore, it seems that the current Bayesian classifier is too slow,
in particular the `relevance` function: With it (not modifying the priority),
maximally 45 problems (sometimes less) could be solved, while without it,
minimally 49 problems were solved (sometimes 51).



4.1.2016
========


Different formulae for simple classifier
----------------------------------------

I tried different formulae for the simple classifier.
The constants are $w_p = -1$ and $w_n = 1$.

* 55: $|p-n| / (p+n) * (w_p * p + w_n * n)$ (confidence)
* 51: $(w_p * p + w_n * n)$
* 27: $5 * (w_p * p + w_n * n)/(p+n)$
* 22: $w_p * \log(p+1) + w_n * \log(n+1)$
* 17: $2 * (w_p * p + w_n * n)/(p+n)$



Discriminating conjectures appearing in axioms
----------------------------------------------

Happy new year! Whee!

I looked at how many conjectures (goals) among the first 1000 Flyspeck problems
appeared among axioms of solved problems. Only 15 such conjectures exist, of a
total of 268 conjectures. (Because we have 268 solved problems.)
As this is less than 10%, it is improbable that a special treatment of such
training examples would have a large impact. I thought for example about
discriminating terms that appeared in the proof of a conjecture that is now
present among the set of axioms. This would delay the treatment of
intermediate lemmata that were however only useful to prove the conjecture.
Yet, for different datasets, this approach might be a viable route.



22.11.2015
==========


New Flyspeck results
--------------------

* Filtering out all terms containing fresh names solves 59 problems (+21.8%)!
* Only learning axiom terms solves 48 problems.

So we are doing a bit more than premise selection, even if learning the
premises seems to play the most important role in the learning process.

Running Satallax in parallel gives worse results, namely it solves only
52 instead of 59 problems, for example.


Automatic mode
--------------

I tested Satallax also in automatic mode on Flyspeck, both 1s and 2s.

* 1s: 344 theorems
* 2s: 414 theorems

Generating training data from the 1s data was not straightforward, because
Satallax could frequently not find the proof again, which I do not understand
currently yet. Even setting the timeout to the double of what it was before
does not help. I should try to merge the refutation term and training phases
to see whether this works, possibly not saving the data about the currently
processed terms anymore (I suspect this to be a real performance killer in
larger proofs!).

I trained Satallax on a subset of 267 theorems for which training data could
be reconstructed in 1s. This was able to prove 71 theorems which could not
previously be solved in 1s (also not in refutation term generation)!
Compared to running Satallax in automatic mode for 2s, there remain
35 unique theorems. This number might increase if we were able to generate
complete training data in automatic mode.



21.11.2015
==========


Flyspeck
--------

Using the simple classifier on the first 1000 problems of the Flyspeck dataset
brings about 52 new theorems compared to the 271 theorems
Satallax proves by itself. That is an increase of about 19%!


Normalising
-----------

I noted that many learnt terms only differ in their fresh variables, e.g.:

    imp (imp (v1_zfmisc_1 __13) False) (imp (v1_xboole_0 __13) False)
    imp (imp (v1_zfmisc_1 __14) False) (imp (v1_xboole_0 __14) False)

To counter this, I made two new schemes, uniform and ascending:
Uniform replaces all fresh variables by the same fresh variable, while
ascending labels fresh variables per term in a uniform (ascending) way.

Results (new theorems proven):

* Uniform: ~5
* Ascending: 30
* No treatment: 52

The performance difference should be negligible, as reproving the training
data takes 18s both with and without calculation of normalised terms.



16.11.2015
==========


Positive vs. negative examples
------------------------------

I plotted the results from the simple classifier:

    sed 's/\(.*\) lresults-pw\(.*\)-nw\(.*\)/\1 \2 \3/' results > gpresults
    splot "gpresults" using 2:3:1

The negative examples impact the learning much more than the positive ones;
when the negative examples are rewarded, not a single problem can be
additionally solved, whereas punishing positive examples still solves quite
some problems until a certain point, where a few less problems can be solved.

The message is clear: We have to consider negative examples in all classifiers!



15.11.2015
==========


Future work
-----------

I would like to try:

- term -> term Bayes classification
- obtaining features only from initial branch vs. from processed terms

And always remember the motto:

GET STUFF DONE! :)


Experimental results
--------------------

I tried the Bayesian classifier (symbols -> symbols) as well as term weights.
The results were quite bad; not a single problem could be additionally solved
by these learning methods.
I tried different parameters using PSO, namely 490 different configurations
for Bayes and 138 configurations for term weights.
It might be a good idea to try these methods first on the solved problems to see
whether proving performance can be initially improved for these. Furthermore,
I should do more debugging to see whether the priorities assigned actually
make sense.


Getting number of proven theorems
---------------------------------

    (for i in lresults-*; do echo `grep Theorem $i/* | wc -l` $i; done) | sort -n



7.11.2015
=========


Simple classifier
-----------------

I implemented a simple classifier that just checks whether a proof term was
previously present and adjusts the weight accordingly. First experiments
were promising, with 18 of 372 previously unsolved problems solved (4.8%).
Next work is to adapt the weights, as well as to integrate IDF and to try
feature-dependent classification. Furthermore, I'll evaluate how many of
the problems can be solved by the automatic strategy and how the learning
impacts the number of solved problems there.


Best Satallax mode
------------------

Contrary to the note from 30.11.2015, the Mizar mode that solves by far the
most Mizar problems seems to be mode483, not mode438. It yields 529 solved
problems, with the training data being about 31.1MB. By contrast, mode438
produces training data of several hundreds of megabytes.


4.11.2015
=========


Command to display hasher statistics
------------------------------------

    ~/satallax/hasher.native classifier training/* | sort -n | egrep -v 'Read|updated|calcul|Wrote' | gvim -


Order of branch axioms
----------------------

When constructing the initial branch, the axioms parsed from the THF file
are added in reverse order of occurrence to the initial branch reference.
Before the start of the search, the initial branch is reversed, such that
earlier theorems in the THF file come earlier in the list.
If this step is *not* taken, then speed severely degrades, namely on 529
Mizar problems it goes from 8s to about 16s.
Can we somehow use this effect, e.g. by shuffling the theorem order?



3.11.2015
=========

Using femalecop & hasher
------------------------

The training process works as follows:

    ./leancop tptp/PUZ001-1.tptp > data
    ./hasher
    ./leancop -learn tptp/PUZ001-1.tptp

The first command creates training data, which the hasher than uses to write a
condensed datai file. When leancop is called with the learn parameter, it tries
to read datai and uses it for internal guidance.



30.11.2015
==========


Mizar problems solved by mode
-----------------------------

Solved     Mode
------- -------
    236 mode438
    235 mode439
     67 mode495
     16 mode445
     10 mode371
      1 mode405

Running mode438 with timeout 10s on all problems yielded 464 solved problems.
However, the training data gets quite large, namely 3.6GB.
Running it with timeout of 0.1s solved 148 problems, producing only 15MB.
Interestingly, even for problems which were solved with both timeouts,
the training data with t=10s is frequently larger than with t=0.1s:
For example, arytm_3__329_37 produces training data of 105KB for t=0.1s,
whereas it produces 1.1MB for t=10s. Probably Satallax behaves differently
internally depending on how much time it knows it still has available?
Oops, it might be I actually ran it in automatic mode ...



18.11.2015
==========


Inefficient processed feature calculation (always recalc)
---------------------------------------------------------

* Without any ML: 0m0.808s
* With proc feature calc: 0m17.373s
* With term feats: 0m17.400s
* The whole calculation, but without influence on final result: 0m17.312s

Efficient processed feature update
----------------------------------

Total time: 0m1.142s
YESS!! It seems really that the calculation of the features takes most
of the time! Just caching this gives us an enormous boost.


15.11.2015
==========


Term weights
------------

With term weights:
Solved: 526 (66.9%)  Failed: 260  Total: 786  Avg time: 0.68s
Unique: 2

Without term weights (two runs):
Solved: 552 (69.3%)  Failed: 245  Total: 797  Avg time: 0.49s
Solved: 551 (70.2%)  Failed: 234  Total: 785  Avg time: 0.42s
Unique: 27

So far, using term weights has made the proving much slower.
However, it is not yet clear whether this comes from slow term weight
calculation of from a possibly bad influence on the proving process.



11.11.2015
==========


Learning instantiations
-----------------------

I discussed with Chad which things might be nicely predicted. We ranked:

1. ProcessProp1
2. NewInst
3. Mating

How about the new instantiation? When a NewInst command is taken from the
priority queue, its term is instantiated for every universal quantifier
than matches its type.
We could save which formulas that were instantiated with which terms
contributed to a proof, to find out in future situations how useful a certain
instantiation might be.

Furthermore, we could generate types as features.


10.11.2015
==========


Discussion after my talk
------------------------

- Symbol features might quickly converge to some constant state
- Preselection of training data based on features
- Features that better reflect prover state, e.g. "how many existentials
  have we eliminated currently", or other things à la TPTP as in E
- Does it make sense to even have a notion of "recent actions" that
  characterise prover state? If so, what should go there?
- Skolemisation -> is it a problem (generated constants)?
- Bob Veroff -> hints technique
  record intermediate lemmata that were generated during Prover9 proof
  search, then give high weight to them when encountered in new problem --
  does not consider prover state at all!
  good for Prover9 problems, but for Mizar less successful (possibly because
  smaller domain)
- E -> constant weighting can be learned from previous proofs,
  probably less powerful than ML internal guidance, but faster and easier to
  change in E



4.11.2015
=========


Statistics
----------

About 95%-99% of the commands in the priority queue are ProcessProp1 commands.
Of these, many are often the same. When we do machine learning, we should
probably filter these out to avoid recalculation of priority queue scores,
for performance reasons.


What are we gonna do?
---------------------

We first record the proof states S together with the used propositions P.
Then, we use the hasher to create features F from proof states S.
In the prover, we first filter out only the training data related to the
propositions of the current problem (axioms).
Then, at every point we call insertWithPriority with a ProcessProp1 command,
we look up the proposition in the training set and calculate its usefulness
with NB or k-NN.

Extension ideas:

- Store with propositions P in proof database also the uniqueness of P, i.e.
  how many failed proof attempts have used P.


Review of "Learning Search Control Knowledge for Equational Theorem Proving"
----------------------------------------------------------------------------

This article by Stephan Schulz describes internal guidance obtained from
previous proofs.
As training data, he selects both clauses that directly contributed to the
proof, as well as clauses that did not directly contribute to it, but which
could be relatively easily derived from the contributing clauses.
The clauses that directly contributed are deemed successful, while those near
but not contributing are considered harmful. The others are not considered.
The clauses are normalised wrt a certain lexicographic ordering, which ensures
that results can be generalised to similar situations, with different symbols.

The features are not generated per clause, but per *problem*.
They include TPTP features (e.g. number of Horn clauses), as well as
number of symbols of a certain arity.

In a new proof, first a subset of previous proof experience is selected that
corresponds to the given (new) problem. Then, the information from this
proof experience is merged, giving for every clause the information in how
many proofs it was used and how far away it was on average from the
contributing clauses.

When a new clause is to be evaluated for usefulness for the problem, it is
looked up in the existing database, and if the same clause (modulo
normalisation) was used before, the information is used.
The information is combined with a traditional heuristic, which just counts
symbols.


Satallax compilation issues
---------------------------

Bug? Compiling Satallax 2.8 only works if I copy dllminisatinterface.so from Satallax 2.7 to bin. Else I get:

~~~
ocamlc  -I bin -o bin/satallax unix.cma /home/mfaerber/satallax-2.8/bin/dllminisatinterface.so \
  bin/minisatinterface.cmo bin/syntax.cmo bin/config.cmo bin/flags.cmo bin/match.cmo \
  bin/priorityqueue.cmo bin/state.cmo bin/search.cmo bin/refutation.cmo bin/flag.cmo \
  bin/litcount.cmo bin/branch.cmo bin/step.cmo bin/suche.cmo bin/norm.cmo bin/translation.cmo \
  bin/coq.cmo bin/latex.cmo bin/proofterm.cmo bin/coqlexer.cmo bin/coqparser.cmo \
  bin/tptp_lexer.cmo bin/formula.cmo bin/tptp_config.cmo bin/tptp_parser.cmo bin/version.cmo \
  bin/satallaxmain.cmo bin/satallax.cmo
File "_none_", line 1:
Error: Error on dynamically loaded library: /home/mfaerber/satallax-2.8/bin/dllminisatinterface.so: \
  /home/mfaerber/satallax-2.8/bin/dllminisatinterface.so: undefined symbol: _ZTVN10__cxxabiv117__class_type_infoE
~~~



3.11.2015
=========


Random ideas
------------

Idea: Record all proof attempts by different strategies, and give higher weight to those proof steps according to the number of strategies that solved a problem
For example, if only a single strategy was able to solve a problem, and only in this strategy, a certain axiom was used, give high importance to that axiom in conjunction with the problem parameters

Probably, negative influences are a bad idea, because not having used an axiom probably does not indicate it has a negative influence on the proof.
However, if for example many failed proof attempts did all use a certain axiom while all successful proofs did not use it, we might think about negatively discriminating this axiom.
Perhaps introduce "bonus points" if the successful proofs are quite different, i.e. use different axioms.

Use internal guidance only in conjunction with axioms or also for generated clauses?
With generated clauses, we probably get quite a blowup in data, unless we do some clever filtering.
For example, if we have a clause that is produced during the successful creation of proofs of many problems which share some characteristics (for example, some algebraic property), then upon encountering this clause among the active formulae, we could immediately give it a higher relevance on the priority queue. This means that the relevance might be influenced by how often a clause was encountered in successful proofs.

Chad is uncomfortable with using fixed symbol names; use some kind of "holes", e.g. ?(x, y) /\ ?(y, z) ==> ?(x, z) for transitivity? How (or whether) to do this efficiently can probably be answered by Cezary / Thibault.
OR: Look at Stephan's paper: "Learning Search Control Knowledge for Equational Theorem Proving" (2003).

Combining clauses yielded by different strategies? (Better Together)
This would require the estimation of "clause quality", for example: size of clause, number of variables, shared subterms with conjecture, ...
Can we use machine learning for this?
For example, we could do something like Stephan Schulz, where we allow some kind of language to specify in which situations we might use clauses from other strategies, depending on the state of both strategies and the clause itself. However, this is probably quite complicated, and I did not find information about this clause selection language. Ask Stephan?

MaLeS uses some features such as "number of clauses", and then normalizes them such that some features are not incredibly overweighted.

The ProcessProp1 command might be the most influential command, according to Chad.

A nice interjection point to influence the selection of a processed proposition might be either:
a) the insertWithPriority function in state.ml -- there, we could catch all insertWithPriority calls and filter for ProcessProp1 instances, which we could then influence accordingly. However, a potential problem is that in state.ml, we might not have access to all available data, such as the prover state. Verify this!
b) the processSearchOption function in search.ml -- however, here we have the disadvantage that we have to wait for the command to be chosen by the priority queue, which kind of defeats the purpose of running some commands earlier than others
So let's better go for option a. :)

Verbosity > 8 -> gives me number of formulas!!

Perhaps use priority of parent formula for child active formulae? Would require modification of getNext() to also return priority ...

To discriminate active from passive: Hashtbl.mem processed
Or go over atom_hash and filter out processed

How about the other options, such as mating / confrontation? Do we have much of a choice there? Why is the choice of the right ProcessProp1 so influential and mating/conf. not? (Or perhaps it is?)

Relevancy pruning is used in E, but we assume that this can be done as a preprocessing step before calling Satallax.

Most imminent engineering tasks:

- Extract state - clause pairs from proofs (or attempts thereof).
- Define state of a prover (wrt machine learning).
- Compress data?
- Influence proof search at same points where we extracted state/clause pairs.

So we probably need some flag for ML data recording and for ML data loading/using.
Cezary's machine learners are usually made to load data in premise selection format, how is it done in FEMaLeCoP?

TODO: Inspect size of priority queue! Also percentage of ProcessProp1 commands on it!

We might also like to learn decision trees based on previous observations which clauses were selected based on which features. Random forest OMG.

Chad suggested the following schema to record state-clause pairs:

1. Run Satallax, recording hashes of every formula that is used.
2. If a proof is found, keep those hashes of formulas that were actually used.
3. Rerun Satallax, and write out information for all the proof states which match with a hash.

This allows us to record the required information without modifying the existing data structures of Satallax.



30.10.2015
==========


Satallax + LaTeX
----------------

Satallax can output LaTeX when

    result_latex = true

is set in src/pfterm/flag.ml. The LaTeX template is then:

    \documentclass[12pt,a4paper,oneside,ngerman]{scrbook}
    \usepackage{color}
    \usepackage{stmaryrd}

    \begin{document}
    % insert Satallax proof here
    \end{document}



31.8.2015
=========


Discussion with Cezary
----------------------

The formalisation of ML could consist of several parts:

- General framework of ML (what are features, etc.)
- Algebraic properties of learners
- Nearest neighbours
- Naive Bayes
- Sparse matrices

Interesting conferences are e.g. ITP, CADE.

For the integration of machine learning into Metis, read PhD thesis of Schulz:
<http://wwwlehre.dhbw-stuttgart.de/~sschulz/PAPERS/Schulz2000-diss.ps.gz>.
Furthermore, FEMaLeCoP.


Conferences
-----------

- ITP (Nancy):
  <http://www.loria.fr/~jablanch/itp2016/bid.html>
  22 - 26 August, 2016
- IJCAR/CADE (Coimbra):
  <http://www.ijcar-2016.info/>
  27 June - 2 July, 2016
- AITP (Obergurgl):
  <http://cl-informatik.uibk.ac.at/users/cek/aitp16/>
  3 - 6 April, 2016
- FLAIRS (Key Largo, Florida, USA)
  <http://www.flairs-29.info/>
  16 - 18 May, 2016



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



30.7.2015
=========


Building Isabelle theories
--------------------------

    isabelle build -d . -l theory_name


To install AFP:

    hg clone https://bitbucket.org/isa-afp/afp-devel
    mv afp-devel $ISABELLE_DIR

To build a specific AFP theory, e.g. "Containers":

    isabelle build -d $ISABELLE_DIR/afp-devel/thys/ -l Containers

To use:

    AFP="/home/mfaerber/Downloads/Isabelle2015/afp-devel/thys/" isabelle jedit -d . -d $AFP -l Mat-Container



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



17.7.2015
=========


Some Flyspeck theorems only provable by Metis
---------------------------------------------

    (|- ~(a = &0) ==> ~(&1 / a = &0),
    [|- !x y. ~(x * y = &0) <=> ~(x = &0) /\ ~(y = &0);
     |- ~(a = &0) <=> &1 / a * a = &1]);

    (|- !m n p. ~(n = 0) ==> (m MOD n * p MOD n) MOD n = (m * p) MOD n,
    [|- m * n = n * m;
     |- !m n p. ~(n = 0) ==> (m * p MOD n) MOD n = (m * p) MOD n]);

    (|- !s1 s2. LENGTH s1 = LENGTH s2 ==> LENGTH (TL s1) = LENGTH (TL s2),
    [|- !l. LENGTH l = 0 <=> l = [];  LENGTH_EQ_NIL
     |- !l. ~(l = []) ==> LENGTH (TL l) = LENGTH l - 1]);  LENGTH_TL

    (|- !s. IMAGE (\x. x) s = s,   IMAGE_ID
    [|- !s t. ~(s = t) <=> (?x. x IN t <=> ~(x IN s));    NOT_EQUAL_SETS
     |- !y s f. y IN IMAGE f s <=> (?x. y = f x /\ x IN s)]);  IN_IMAGE


Output from extreme debugging session
-------------------------------------

~~~ ocaml
(* rinse & repeat *)
let res = match (Resolution.iterate res) with Undecided r -> r;;
let act = Resolution.active res;;
let wt = Resolution.waiting res;;
let Some ((d,cl),_) = Waiting.remove wt;;
let () = print_endline ("Resolution.iterate: cl " ^ (Clause.toString cl));;
let active = act;;
let (Some cl') = Active.simplifyActive Active.maxSimplify active cl;;
let active = Active.addClause active cl;;
let cl = Clause.freshVars cl;;

(* deduce *)
let Active.Active {parameters=parameters;literals=literals;equations=equations;subterms=subterms} = active;;
(* just one element in set, p2 p3 c7 *)
let lits = Clause.largestLiterals cl;;

(* deduceResolution *)
let acc = [];;
let [lit] = Literal.Set.toList lits;;
let (_,atm) = lit;;

(* Literal_net.unify *)
let {Literal_net.positive=positive;negative=negative} = literals;;
let negated = (Literal.negate lit);;
let (false, atm) = negated;;

(* Atom_net.unify *)
let tm = Atom_net.atomToTerm atm;;
(*Term_net.unify negative (Atom_net.atomToTerm atm);;*)

(* Term_net.unify *)
let (Term_net.Net (parm, _, Some (_,net))) = negative;;
(* Term_net.finally parm (Term_net.mat [] [(Name.Map.newMap (), net, [tm])]);; *)
(* actually: Term_net.mat [] [(Name.Map.newMap (), net, [tm])];; *)

(* Term_net.mat (last) *)
let in = [(Name.Map.newMap (), net, [tm])];;
let ((qsub, Term_net.Multiple (v,fns), Term.Fn (f,a) :: tms) :: rest) = in;;
let rest = match v with None -> rest;;
let peeky = Name_arity.Map.peek fns (f, List.length a);;
let rest = match peeky with Some net -> (qsub, net, a @ tms) :: rest;;

(* Term_net.mat (second to last) *)
~~~


Old commands
------------

~~~
sed \
 -e 's/datatype /type /g' \
 -e 's/fun /let /g' \
 -e 's/val /let /g' \
 -e 's/ =>/ ->/g' \
 -e 's/SOME/Some/g' \
 -e 's/NONE/None/g' \
 -e 's/List.foldl/MList.foldl/g' \
 -e 's/List.null/MList.null/g' \
 -e 's/fn /fun /g' \
 -e 's/raise \(.*\);/raise (\1);/g' \
 -e 's/;/;;/g' \
 -e 's/case \(.*\) of/match \1 with/g' \
 -e 's/NameAritySet/NameArity.Set/g' \
 -e 's/NameArityMap/NameArity.Map/g' \
 -e 's/NameSet/Name.Set/g' \
 -e 's/NameMap/Name.Map/g' \
 -e 's/andalso/\&\&/g' \
 -e 's/orelse/||/g'
~~~

What the F ... was that one good for (rename.sh)?

    sed -i "s/\(^\|[^[:alnum:]]\)$1\(\$\|[^[:alnum:]]\)/\1$2\2/g" $3


OCaml woes
----------

To use the Metis toplevel, you have to do:

    make metis.top
    rlwrap ./metis.top

Then you can open modules, such as:

    open Resolution;;

This works only because of the file .ocamlinit, which changes the directory
to the build directory (where the .cmo files are).
Otherwise, one cannot open any modules!



10.7.2015
=========


Extracting ATP dependencies
---------------------------

    grep "[a-z0-9]$" deps.atp


Getting names of a list of proven theorems
------------------------------------------

~~~ ocaml
let uniquesh = Utils.foldl_file (fun acc x -> x :: acc) [] "uniques_metis";;
let uniquesh' = map hd (map (Utils.string_split_on '.') uniquesh);;
let uniquest = map find_hash uniquesh';;
let uniqdepsh = map (fun h -> (h, assoc h probs)) uniquesh';;
let uniqdepst = map (fun (p, ds) -> (find_thm (find_hash p), map (find_thm o find_hash) ds)) uniqdepsh;;
~~~



12.6.2015
=========


HOL Light cheat sheet
---------------------

* DISCH_TAC: move implications into assumptions
* ACCEPT_TAC tm: solve the current goal with tm



29.5.2015
=========


Meeting with Cezary
-------------------

I finished the initial Metis translation to OCaml. Next will be the linkup
to existings TPTP parsers. For this, Cezary suggested to look at
hh2/translate/fol/fof.ml, in particular at the `file_mat` function, where
he suggested to remove the line with `eqsym` because it is specific to Leancop?
In the same directory, I can try leancop with

    make leancop
    ./leancop PUZ001-1.tptp

Strangely, Leancop finds a proof for PUZ001-1 (SZS status Theorem), but Metis
gives SZS status Unsatisfiable for the same file.



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



26.3.2015
=========


cron
----

To automatically start the server revival script, I made a cron entry using
`crontab -e` containing the following (viewed via `crontab -l`):

~~~
# Check at the beginning of every hour whether server is still running.
0 * * * * ~/check_server.sh
~~~



25.3.2015
=========


Server restart script
---------------------

Because colo12 seems to have restarted some times recently, I had to write a
script which checks if HH is running and starts the server if not.

~~~ bash
#!/bin/bash
# Restart the HOL(y)Hammer server if it is not currently running.

SRV_BIN=dist/build/server/server
SRV_DIR=~/hh2/online

if ! pgrep -f $SRV_BIN -u $USER > /dev/null
then
  echo "Starting HOL(y)Hammer server."
  cd $SRV_DIR
  nohup $SRV_BIN &> /dev/null &
fi
~~~

For historical reasons, compare with the script for the original HH1 server:

~~~ bash
#!/bin/bash

#########################################################################
# Script to check if process 'hh_redirector' is running from user 'cek' #
# Written by: bwinder #
# Written on: 2014-02-14 ( Valentine's Day Edition ) #
#########################################################################

#Check if process 'hh_redirector' is running from user 'cek'
process=`ps aux | grep '[.]/hh_redirector' | grep 'cek' | grep -v grep`

if [ -z "$process" ]
then
#Run the process
nohup /home/ami/cek/hh/hh_redirector &> /dev/null &
fi
~~~



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



16.3.2015
=========


Data formats
------------

The TPTP data formats include:

* CNF: conjunctive normal form.
* FOF: first-order CNF formulas.
* TFF0: FOF plus types.
* TFF1: TFF0 plus shallow polymorphism
* THF0: TFF0 plus higher-order functions
* THF1: currently proposal, combines TFF1 and THF0, roughly as expressive as HOL

Based on this, we have our own data format, HH, which is more powerful than
THF1, for one can also export data from Coq or Matita to it. However, it does
not have a semantics, but it is still useful for tasks such as feature
extraction.


Restructuring
-------------

According to Cezary, Thibault did work on thf1hh1, which he claims to do mostly
things that thf0hh1 already did. If this is true, this might be up for a merge.
Furthermore, the relationship between predict and predict_knn should be cleared.

To make things clearer, it would also be nice to rename all files main$x.ml to
just $x.ml, while moving the "library" files (like read.ml) to suitable
subdirectories. What should these subdirectories be?


Lambda lifting
--------------

Currently, lambda lifting only works for HOL Light. A generalisation of this to
different ITPs would be desirable. The functionality is in hh_tac.ml, I want to
look at this.



12.3.2015
=========


Bugs, bugs, bugs
----------------

In today's meeting with Cezary, he was able to expose several bugs:

* HOL4:
    - &1 = 1+1
    - 1+1
* HOL Light:
    - [] = []
    - 1+1
    - EVEN (length [])
* Interface: When navigating to "files/bla.html" (and bla.html does not exist),
  the user is redirected to the main page, but with a missing image. We should
  display a "page not found" error instead.

Furthermore, we discussed integrating Cezary's new web service in the new HH2.
We agreed that Cezary could put HTML/JS files into the "files" directory,
and the server would spawn processes that write to files, which the user
can then request.
We will also take the same approach for the existing HH2 infrastructure. That
means that I will create a stand-alone program for HOL(y)Hammer that will
implement the backend and write intermediate results to stdout. The starting
server will then redirect stdout to some file (cache/hl/HL-Core/...), where the
file name needs to be properly escaped.

Finally, Cezary wants the different strategies to be activated on the server.
He also told me that 'epclextract' was no longer needed with E 1.8. I want to
investigate how to yield the right output from it then.



11.3.2015
=========


Discussion with Cezary
----------------------

We discussed the HOL(y)Hammer online service. Cezary expressed concerns that:

* The online service needs to be restarted for changes to be applied, thus
  causing users to be deconnected.
* He is currently not able to integrate new services into hh2.cl-informatik,
  such as a LaTeX parser, because he cannot use PHP.
* Type-checking messages could be more detailed.
* The backend reimplements many things that have already been available, such
  as calling provers, parsing etc.
* The backend does not yet implement things such as reconstruction (via meson),
  minimisation, and decision procedures.
* The backend has an additional dependency on Haskell, which the original HH
  did not have.
* The server does currently not generate any log files about visitors and
  entered conjectures.
* HTML rendering of developments is not available. That was in the old server
  for HOL Light, and Josef's tool also supports HOL4 (according to Cezary).

He pointed out two main uses of HOL(y)Hammer versions: One that can be run on the
server and which may be hard to set up, one that users can run locally and which
should have a minimal number of dependencies for easy installation.

As requirements, he wants the following features:

* Fast conjecture parser outside the ITPs (using the .sx files)
* Decision procedures
* Strategies (different ATPs, premise selectors etc.)
* Minimisation (optionally)
* Reconstruction

(Apparently, these features already exist for HOL Light in the "translate"
subdirectory, and may even be used for HOL4 in the future.)

Furthermore, the service should be extensible, allowing it to interfere with
different "academic-wares" such as Josef's Perl scripts. ;)



5.3.2015
========


Better PATH on colo12
---------------------

To not always have to write PATH=... at every command, I wrote the following
into my ~/.bash_profile:

~~~ bash
PATH=/home/ami/cek/flyspeck-workbench2/ocaml/bin:$PATH
PATH=/home/ami/cek/bin:$PATH
PATH=/home/software/tct3/.cabal/bin/:$PATH
PATH=$HOME/bin:$HOME/usr/bin:$PATH

LD_LIBRARY_PATH=$HOME/usr/lib

export PATH
export LD_LIBRARY_PATH
~~~


Server binaries
---------------

Unfortunately, colo12 has only very old versions of software, especially of
g++ and ocaml.
To work around that, we can use the following workaround for OCaml:

    PATH=/home/ami/cek/flyspeck-workbench2/ocaml/bin:$PATH

And for g++ (watch out, the version here is also from 2013 and experimental!):

    PATH=/home/ami/cek/bin:$PATH


TODO list
---------

- [x] Install Thibault's new exports
- [x] Normalize theorem names
- [ ] Make password/user request on upload
- [ ] If no proof found, return reliably
- [x] If parse failed (e.g. HOL4 "!x:real. x = x"), return
- [x] If no axioms used, say "No axioms required."
- [x] Show that parsing worked
- [ ] Write "about" page.
- [ ] Add logos: UIBK, Nijmegen?
- [x] Implement new strategies
- [x] Fix parse for HL: "ODD x \/ ODD (x+1)"
- [ ] Install Flyspeck
- [x] Allquantify variables for HOL Light
- [ ] Resolve HOL Light crash on exit


HOL cheatsheet
--------------

* ~p = "not p"
* x EXP 2 if x is natural
* x pow 2 if x is rational or integer
* abs(x) is absolute value



27.2.2015
=========


Exporting conjectures from HOL Light
------------------------------------

Given a conjecture and a HOL Light corpus, we want to generate files that allow
us to run a predictor on them, and generate a THF file from the predictions.

The masterplan for this is:

* Load a HOL Light checkpoint with the corpus pre-loaded.
* Execute the following code (or something similar) in HOL Light:

      let myfile = open_out "bla.thf";;
      oc := my_file;;
      let tm = ... (* see hh_svr.ml how to do that, or parse with Hl_parser? *)
      term thf_syntax tm;; (* for this, look at export/hl/writer.ml *)
      close_out oc;;

  This should write the conjecture from 'tm' to "bla.thf".
* Run hh_h4 on bla.thf with the corpus files from $CORPUSDIR/data.
* The rest goes like with HOL4.


Running E
---------

I looked at Thibault's "hh.sh" script in HOL(y)Hammer for HOL4. It runs a very
ugly-looking Perl script "runepar2.pl", that is a wrapper for 'eprover' and
'epclextract': 'eprover' basically generates a proof, and 'epclextract'
displays it in a way that we can interpret it.
Then, Thibault extracts from the resulting E proof the axioms which E has used
from the given problem file. He also extracts whether the proof was successful.

These tasks can be simplified:

    ./eprover --auto --tstp-in -l4 --pcl-terms-compressed --pcl-compact $FILE

will read $FILE in our format (which is TPTP 3) and write results to stdout.
You can also use the flag

    --cpu-limit <secs>

to limit E's execution time to the specified amount in seconds.
If we use

    --memory-limit=Auto

E will claim most of the physical memory of the machine. That is currently
used in "runepar2.pl", but I am not sure whether this should done on a web
server ...

Then,

    ./epclextract --tstp-out -f -C --competition-framing $FILE

reads from $FILE an E proof and prints the necessary steps on stdout to prove
the original conjecture.

If the proof was successful, the output from 'epclextract' contains the line

    # SZS status Theorem

The axioms used are printed in lines such as:

    fof(3, axiom,![X1]:(s(t_bool,X1)=s(t_bool,t)|s(t_bool,X1)=s(t_bool,f)),file('../tmpThreadId3562573/atp_in', aHLu_BOOLu_CASES)).

The resulting axiom is HL_BOOL_CASES. The transformation seems to be given in
the function 'unescape_hh' from Thibault's "thfWriter.sml".
Question to Thibault: Will this format stay the same with your new version of
HOL4 export?



26.2.2015
=========


Stackage
--------

To avoid Cabal hell in the future, I now use Stackage as default package
repository for Haskell.

In my ~/.cabal/config file, I have now lines like the following:

    -- remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive
    remote-repo: stackage-lts-1.9:http://www.stackage.org/snapshot/lts-1.9

This restricts us to installing *only* Stackage-approved packages in standard
Haskell environments. However, we require some non-Stackage packages for the
HOL(y)Hammer server, so we create a sandbox in the "online" directory via

    cabal sandbox init

and put the following into a new "cabal.config" in the "online" directory:

    remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive

Then follows the contents of cabal.config from:

    http://www.stackage.org/snapshot/lts-1.9

That way, we enjoy the package constraints from Stackage, but we can also
install additional packages when we see fit.

Note that it makes sense to install src-haskell-exts system-wide (via apt-get
or similar), because otherwise that package takes ages to install ...



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



11.2.2015
=========


Checkpointing HOL4
------------------

I tried to checkpoint HOL4 with dmtcp to have a uniform way of creating
checkpoints between HOL4 and HOL Light, but this turned out to be a real bitch.
The problem was the HOL4 creates files of the format `~/.polyml/poly-stats-$pid`,
and deletes them again as soon as it quits. However, when rerunning a
checkpointed HOL4 instance, it would not find its corresponding `poly-stats-$pid`,
because its original instance would have quit and deleted it. After fiddling
with this for four hours, I discovered the really nice interface of Poly/ML to
checkpoint it natively, and now I'm happily using this. It seems to work well
with HOL4 so far.



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


Nice interface
--------------

Discovered an interesting online service, whose interface has some great ideas:

http://lindat.mff.cuni.cz/services/morphodita/

En plus, c'est Tchèque. :)



9.2.2015
========


Predictor pt. 2
---------------

I found out the format which Thibault's predictor expects. You need a directory
called "theories", in which the predictor needs some files:

* conjecture: Contains a conjecture, in tt(..) format. How to generate this?
* thygraph: has to be given as second argument to write_thf_thyl() in HOL4

An example conjecture:
  tt(conjecture, conjecture, (prim__rec/_3C num/0 (arithmetic/NUMERAL (arithmetic/BIT1 arithmetic/ZERO)))).

The predictor hh_h4 needs to be called from the directory "translate".
In this directory, there has to be a directory "predictions", where predictions
and intermediate files are written to. The final result is available in
"predictions/predict_hh_out".

I'm quite pissed that I have to find out all of this by trial-and-error.


Predictor
---------

"Le petit outil", as I like to call it, has trouble avancing.
This is the utility that should read .p / .hd files and a conjecture, then
output usable premises for the conjecture. Thibault originally agreed to write
this for me, but after discussing it today, he said he did not have time. :~|

It turns out that the codebase is really badly documented, and in general a
pain to work with. The file in question is translate/main_h4.ml, which Thibault
originally advertised as usable also for predictions with HOL Light, but I'm not
so sure about this anymore, because in this file several files related to HOL4
export are mentioned.
Even only looking at this gives me the shivers. I have literally no clue which
file in translate/ is responsible for what. That's really demotivating.


Exporting from HOL 4 reloaded
-----------------------------

So I did now actually try the HOL 4 export. This worked relatively painlessly.

~~~
load "thfWriter";

(* copied from holyHammer.sml *)
fun current_thyl () = mk_set (map (fst o fst) (DB.listDB()));

(* first parameter is directory where features/dependencies are written,
   second parameter is file where theory order is written *)
thfWriter.write_thf_thyl "theoryname" "theoryname.deps" (current_thyl());
~~~

This creates in the current working directory a new directory "theoryname",
into which the file "theoryname.deps" and the .p / .hd files are written.

According to Thibault, one should be able to load a custom theory before
"thfWriter", then the theory writing should consider the custom theory.

I integrated this procedure into a Haskell script, which can now automatically
load theories and export data from them. \o/



6.2.2015
========


Existing corpora
----------------

I tried to build several proof corpora for HOL Light.

* Gödel's incompleteness theorem: Fails in phase 2 with exception "pD".
* Model of HOL: Same as above.

The reason for this was that the "data" directory was not exported. This was due
to the fact that I ran in OCaml the command:

    #use "make.ml";; hist_save();;

However, I learned the hard way that `hist_save();;` was not executed, probably
because it was on the same line as the preprocessor directive. OCaml really
makes you shoot yourself in the foot here by not warning at all.

With this fixed, I was able to successfully build the following corpora:

* Model of HOL
* Gödel's incompleteness theorem
* Boyer-Moore

For all of these, also data was exported correctly! One thing for which to
watch out for is that when one creates a checkpoint for a corpus, one has
to watch out that no checkpoint previously exists there, because otherwise
the new OCaml instance is getting added to the previous one, and you cannot
write input to any of the OCaml instances anymore. For this reason, I now
simply delete all previous checkpoints and also data directories from all
corpora before exporting data.

An annoyance is that the exported data gets very long paths; for example,
I have the very nice path

    ~/Dokumente/Uni/2014WS/hh2/online/corpora/hl/Model\ of\ HOL/data/home/michi/Dokumente/Uni/2014WS/hh2/online/corpora/hl/Model\ of\ HOL/Model/

due to the function `noholname` in historian?.ml stripping away only the HOL
directory prefix, but not the prefix of the corpus. It remains to clarify
whether we should change this behaviour or if that could create name clashes.


5.2.2015
========


DMTCP
-----

I found out how to properly utilise DMTCP to checkpoint OCaml code.
Basically, one calls ocaml with `dmtcp_checkpoint ocaml`, and from the OCaml
session you call `dmtcp_command --checkpoint`, which creates the checkpoint.
Then, to take up again an existing checkpoint, you call `dmtcp_restart $CKPT`,
and when you call the checkpointing command from OCaml again, the existing
checkpoint gets updated.
Most conveniently, when you run the checkpoint from a different directory, it
also changes its current working directory (cwd) to the new directory.


Recording proof dependencies in HOL Light
-----------------------------------------

I found out how to record proof data from new external libraries.

The first step writes .statements files to "data":

~~~
rm fusion.ml
ln -s write/fusion1.ml fusion.ml
echo -e '#use "write/hol1.ml";; \n#use "$THEORY/make.ml";; \nhist_save();;\n' | ocaml
~~~

The second step writes the desired .p (theorems) and .hd (dependencies) files:

~~~
rm fusion.ml
ln -s write/fusion2.ml fusion.ml
echo -e '#use "write/hol2.ml";; \n#use "$THEORY/make.ml";;' | ocaml
~~~

It is important to note that in the second step, for every theory for which
.statements files exist, the desired .p and .hd files are created.

That means that the "delta step" for the both phases are:

    #use "$THEORY/make.ml;; hist_save();;

respectively

    #use "$THEORY/make.ml;;



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



Exporting from HOL Light
------------------------

First, how to install HOL Light?

~~~
sudo apt-get install ocaml camlp5
svn co http://hol-light.googlecode.com/svn/trunk hollight
cd hollight
make
~~~

Then, it is time to bring in Cezary's dependency tracking:

~~~
ln -s ../../export/hollight write

rm fusion.ml
ln -s write/fusion1.ml fusion.ml
echo -e '#use "write/hol1.ml";; \nhist_save();;\n' | ocaml

rm fusion.ml
ln -s write/fusion2.ml fusion.ml
echo -e '#use "write/hol2.ml";; \n' | ocaml
~~~

This creates a directory "data", where the information about theorems is saved.
There, also some OMDoc documentation is stored, but it does not seem to be
directly usable as HTML documentation. How to do the latter?



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


CICM
----

http://cicm-conference.org/2015/cicm.php

Abstract submission deadline: 16 February 2015
Submission deadline: 23 February 2015


Export from HOL 4
-----------------

I talked with Thibault, who gave me advice on how to export from HOL 4:
He has forked HOL 4, which one obtains by:

    git clone https://github.com/barakeel/HOL

Building is the same as I wrote yesterday.
To export data (here for an example theory "euclid.sml", one runs `bin/hol` with
the following input:

~~~
load "euclid";
load "thfWriter";

(* copied from holyHammer.sml *)
fun current_thyl () = mk_set (map (fst o fst) (DB.listDB()));

(* first parameter is directory where features/dependencies are written,
   second parameter is file where theory order is written *)
thfWriter.write_thf_thyl "euclid" "euclid.deps";
~~~

Note that absolute paths should be used, because Thibault's code can change the
current working directory.

Thibault told me he would update the current HOL fork to be much better, which
should among others eliminate the need to redeclare `current_thyl`. If it is not
online until 9.2., I have official permission from him to slap him. :)



1.2.2015
========


HOL Light documentation
-----------------------

I found out that a hand-rolled Perl script is used right now to generate
documentation for proof corpora. Is it desirable to replace this with something
more "official"? :)

Actually, how many people really use the current documentation pages on colo12?


HOL 4
-----

When installing HOL 4, I first wanted to use Ubuntu's Poly/ML packages (5.2.1),
but it soon turned out that HOL 4 failed to compile with this.
(Build segfaults.)

I then used Poly/ML 5.5.2 from the Poly/ML website and compiled with:

./configure --prefix=/usr --enable-shared
make
sudo make install

Then, I compiled HOL 4 with:

poly < tools/smart-configure.sml
bin/build



30.1.2015
=========


Happstack
---------

I consider using Happstack instead of the previous PHP version, because I don't
know PHP and don't want to learn something new that I consider to be crappy. :)

However, I'm having trouble installing Happstack on colo12, as there seem to be
a Cabal version installed which has trouble installing Happstack, and even
updating itself does not seem to work.
See: https://github.com/haskell/cabal/issues/1137

However, Benni told me that he considers installing a new server soon, on which
a different Cabal version would be available. Let's see.



29.1.2015
=========

HOL(y)Hammer: Online ATP Service for HOL Light
http://arxiv.org/pdf/1309.4962v1.pdf

Requirements:
- OCaml
- dmtcp (used to checkpoint OCaml instances)

The following command does not seem to work --- why?

~~~ bash
echo '&1 + &1 = &2' | nc colo12-c703.uibk.ac.at 8080
~~~

Where does the `sorter` program in bin/ come from?



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
