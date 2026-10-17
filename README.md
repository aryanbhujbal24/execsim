# execsim

A price–time priority limit order book with an integrated matching engine,
written in OCaml, together with optimal-execution schedules that can be run
against it and costed by implementation shortfall.

It exists because an execution schedule computed from a model is a prediction.
Running it through an actual matching engine — walking real price levels,
paying the real spread, exhausting real depth — is how you find out what the
model left out.

```
$ make test
success (ran 5 tests)          # 7,000 generated operation sequences
Test Successful in 0.003s. 22 tests run.

$ make bench
  submit, rests on the book             5880000 ops/sec      170 ns/op     10.00 words/op
  cancel, O(1) via intrusive unlink    12137385 ops/sec       82 ns/op      3.00 words/op
  submit_iter, callback per fill        9714029 ops/sec      103 ns/op     11.00 words/op
  market order sweeping ~20 levels      1835541 ops/sec      545 ns/op     11.00 words/op
```

---

## Why OCaml

Not as a flourish. Three properties of the language do real work here.

**Illegal states are unrepresentable rather than merely unwelcome.** A resting
order carries a `Qty.Pos.t`, a quantity that cannot be zero because no
constructor produces one. An order with nothing left to fill is not an order
resting at zero size — it has left the book — and the type says so. Taking a
fill out of a resting quantity returns

```ocaml
val sub : t -> qty -> [ `Remaining of t | `Exhausted ]
```

so the caller cannot forget to unlink an exhausted order: there is no path
through the code that ignores the `` `Exhausted`` case.

**Prices and quantities cannot be confused.** `Px.t` and `Qty.t` are abstract.
You can offset a price by ticks and subtract two prices to get a distance, but
you cannot add two prices, and you cannot add a price to a quantity. The
classic unit-confusion bug does not compile.

**Every status is a closed sum.** There is no integer status code and no
`Other` constructor anywhere. Adding a time-in-force or a rejection reason
turns every site that fails to handle it into a compile error rather than a
surprise in production.

---

## The book

A dense array of price levels indexed by tick, one array per side, over a
preallocated pool of orders held in parallel `int array`s. Each level owns an
intrusive doubly-linked FIFO queue through that pool: `next` and `prev` are
integer slot indices and `-1` is the null link, so the queue needs no cons
cells.

```
          level array (by tick)          order pool (structure of arrays)
          ┌──────┬──────┬──────┐         id   │ 7  │ 12 │ 3  │ …
  asks →  │ head │ head │ head │────┐    qty  │ 50 │ 40 │ 10 │ …
          └──────┴──────┴──────┘    └───▶ next │ 1  │ 2  │ -1 │ …
            ▲ best_ask                   prev │ -1 │ 0  │ 1  │ …
                                              └─ O(1) unlink on cancel
```

| operation | cost |
|---|---|
| submit, resting | O(1) |
| cancel | O(1) — index lookup, then unlink |
| match | O(levels crossed) |
| best bid/ask | O(1) cached, repaired on depletion |

Cancels dominate message traffic on real venues, so a design that walks a price
level to cancel is a design that falls over under load. The cost of this layout
is a bounded price range fixed at construction — but venues apply price bands
anyway, and a band is exactly what makes the dense array sound.

---

## Allocation, measured rather than asserted

The first version of this README claimed the matching path allocated nothing.
The benchmark disagreed, so the claim was wrong. Attributing it by call site
found the cause: every lookup in the id index allocated a closure, because the
probe loop was written as a `let rec` nested inside the function and without
flambda the compiler cannot avoid capturing its environment.

Hoisting the probe loops to top level and passing state explicitly
(`lib/idmap.ml`) removed it. The effect was not marginal:

| | before | after |
|---|---|---|
| cancel | 250 ns, 17 words | **82 ns, 3 words** |
| submit, resting | 470 ns, 24 words | **170 ns, 10 words** |
| match, one fill | 257 ns, 15 words | **103 ns, 11 words** |

What remains is now exactly accounted for, and none of it is the book:

```
Order.limit record           8.00 w/op   the request value itself
submit_iter (rest)          10.00 w/op   = request + 2 for the status variant
Book.cancel                  3.00 w/op   the `Cancelled variant
submit_iter (one fill)      11.00 w/op   = request + 3
submit (builds fill list)   24.00 w/op   = request + 16 to box the fills
```

The levels, the queues and the index allocate nothing. Everything left is the
cost of returning a value, which is why `submit_iter` exists: it reports fills
through a callback with flat arguments, so the fill list is never built. Run
`make alloc` to reproduce the table.

---

## Execution schedules

Almgren–Chriss minimises

```
  Σ η̃ nₖ² / τ  +  λ Σ σₖ² τ xₖ²
```

over the holdings path, trading impact against timing risk. Two solvers:

**`Schedule.sinh_path`** — the closed form under constant volatility,
`xₖ/X = sinh(κ̃(T − tₖ)) / sinh(κ̃T)`.

Note the tilde. The decay rate in the discrete solution is *not* the continuous
`κ = √(λσ²/η̃)` that most presentations quote; it is the `κ̃` satisfying
`2(cosh(κ̃τ) − 1)/τ² = κ²`. They agree only as `τ → 0`. `Schedule.kappa_tilde`
computes the one the discrete problem actually has, and a test pins the
defining relation.

**`Schedule.variable_vol_path`** — the general case. Intraday volatility is
forecastable, so assuming it constant throws information away. With a
deterministic volatility path the objective stays quadratic with a tridiagonal
Hessian, so the optimum still solves a linear system, in O(N) by the Thomas
algorithm. The sinh solution is the special case with a flat path.

That last sentence is a test, not a remark:

```
closed form vs tridiagonal solver: max gap 1.96e-11 shares   (on a total of 6,000)
```

Two independent derivations agreeing to eleven decimal places is the cheapest
correctness evidence available, and it needs no reference implementation.

---

## Impact

The textbook model assumes impact is linear in trading rate, because linearity
is what makes the schedule solvable. Fitting

```
  Δp = λ · sign(q) · |q/V| ^ δ
```

to trades matched against the prevailing quote gives δ well below one.
`Impact.estimated` carries **δ = 0.687** and a **93% permanent** share,
estimated from 2.02M CME E-mini Nasdaq-100 trades; the exponent's 95% interval
was [0.605, 0.769], excluding both linearity (1.0) and the square-root law
(0.5).

The permanent share matters more than it looks. Impact that permanently
repositions the mid is paid whatever schedule you choose, so it drops out of
the optimisation entirely. At 93% permanent there is far less to optimise than
the linear model implies — a result about the problem, not about the solver.

Replace the constants in `lib/impact.ml` when you re-estimate.

---

## Testing

```
make test
```

**Five property suites, 7,000 generated operation sequences.** QCheck
interleaves submits, cancels, market orders and every time-in-force, then
asserts after each one that:

- the book never crosses, cached level quantities match their queues, and
  arrival order is preserved within every level;
- quantity is conserved — every share submitted either traded, rests, was
  killed as an IOC/FOK remainder, or was cancelled, and shares are never
  created or destroyed;
- a taker never trades through its own limit price;
- `fillable` predicts the realised fill exactly, since execution strategies
  size child orders off it;
- fill-or-kill is atomic, and cancel removes exactly the resting quantity.

On failure QCheck shrinks to a minimal counterexample, which is usually short
enough to read directly.

**Twenty-two unit tests** pin the behaviours worth confirming by eye: price
improvement (the maker sets the price, so an aggressive buyer at 10010 against
an offer at 10005 trades at 10005), time priority, multi-level sweeps, pool
growth, and the schedule cross-validation above.

---

## Layout

```
lib/
  px.ml      qty.ml      prices and quantities; the type-level invariants
  order.ml               orders, fills, statuses — all closed sums
  idmap.ml               allocation-free int→int index
  book.ml                the order book and matching engine
  impact.ml              concave impact, δ = 0.687
  schedule.ml            Almgren–Chriss: closed form and tridiagonal solver
  sim.ml                 run a schedule through the engine, cost it
bin/main.ml              demo driver
test/                    property suite, unit suites
bench/                   throughput and allocation
```

```bash
make build        # dune build
make test         # property + unit suites
make bench        # throughput, release profile
make alloc        # minor-heap words per op, by call site
make demo         # build a book, run four strategies, compare shortfall
```

Requires OCaml ≥ 4.14, dune ≥ 3.0, and `base`. Tests additionally need
`alcotest` and `qcheck-core`. CI builds and tests on 4.14 and 5.1.

---

## What this does not do

Worth stating, since the gaps are deliberate rather than overlooked.

- **No wire protocol.** Orders arrive as values, not as decoded ITCH or FIX
  messages. A feed handler is a separate concern and a separate project.
- **No self-trade prevention, no iceberg or stop orders, no auctions.** Each is
  a well-defined addition; none changes the data structure.
- **Simulated liquidity is a model, not a replay.** `Sim.Resilience` refills a
  fixed fraction of consumed depth between slices. Real resilience is
  stochastic and state-dependent, so the shortfall numbers compare strategies
  against each other, not against what a venue would actually have cost.
- **Single-threaded.** A real engine shards by instrument; nothing here would
  resist that, but nothing here does it.
- **`Array.unsafe_get` in the index probe loops only.** The book itself uses
  checked access, which costs a few percent and is worth it.

---

## Licence

MIT.
