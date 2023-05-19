---
title: A Combinatorical Identity
date: "2023-05-17"
---

# Story

Once upon a time,
during a final exam review session,
one of my friends was trying to prove this complex analysis homework problem:

$$
\int_{-\infty}^\infty \prod_{m=1}^M \frac1{t^2 + m^2} dt = \frac{\pi}{(2M - 1) (M - 1)! M!}
$$

To finish the proof, there was a combinatorical identity that needed to be shown.
Look, I have no idea and I haven't taken complex analysis (at least not yet)
so I can't even explain.

Anyways, it was equivalent to this:
$$
\sum_{k=0}^n (-1)^{k+1} k \binom{2n}{n+k} 
= \binom{2n-2}{n-1}
$$

I'll present two proofs,
one using generating functions
and one using the
[DIE method](https://doi.org/10.1080/07468342.2008.11922293).

## Proof 1: Generating Functions

First, a couple of facts that I stole from the internet:
$$
(1+x)^p = \sum_n \binom{p}n x^n
$$
$$
\frac{x^n}{(1-x)^{n+1}} = \sum_t \binom{t}{n} x^t
$$
$$
\frac{x}{(1-x)^2} = \sum_{k=0}^\infty k x^k
$$

We're going to let $n$ be a free variable and
introduce $s$ to replace the $2n$ in the binomial coefficient:
$$
\sum_{k=0}^n (-1)^{k+1} k \binom{s}{n+k}.
$$

Now, we sum over $s$.
Let $u = \frac{x}{1-x}$.
$$
\begin{align*}
    \sum_s \sum_{k=0}^n (-1)^{k+1} k \binom{s}{n+k} x^s
    &= \sum_{k=0}^n (-1)^{k+1} k \sum_s \binom{s}{n+k} x^s \\
    &= \sum_{k=0}^n (-1)^{k+1} k \frac{x^{n+k}}{(1-x)^{n+k+1}} \\
    &= \frac{x^n}{(1-x)^{n+1}} \sum_{k=0}^n (-1)^{k+1} k \frac{x^{k}}{(1-x)^{k}} \\
    &= \frac{x^n}{(1-x)^{n+1}} \sum_{k=0}^n (-1)^{k+1} k u^k \\
    &= - \frac{x^n}{(1-x)^{n+1}} \sum_{k=0}^n k (-u)^k \\
    &= - \frac{x^n}{(1-x)^{n+1}} \frac{-u}{(1+u)^2} \\
    &= \frac{x^n}{(1-x)^{n+1}} x (1-x) \\
    &= x^2 \frac{x^{n-1}}{(1-x)^{n-1+1}} \\
    &= x^2 \sum_t \binom{t}{n-1} x^t
\end{align*}
$$

Looking at the $x^{2n}$ coefficient gives the identity.
($s = 2n$ and $t = 2n - 2$)

## Proof 2: DIE

Consider a row of $2n$ squares
where each square is either coloured "black"
or one of two light colours: "white" and "super-white".
Then, $k \binom{2n}{n+k}$
is counting the number of ways to paint $n + k$ squares with a light colour
with exactly one of the first $k$ from the left being super-white.

We assume $1 \leq k \geq n$.

Suppose the super-white square on the right of some square,
which must be coloured either black or white.
We can toggle the colour to get an involution
and still get a valid colouring
by adding or subtracting one to $k$.

$$
\square\boxtimes \iff \blacksquare\boxtimes
$$

The $k$ in the two colourings differ by one
so these configurations will cancel in the summation.

This involution is not possible if
the super-white square is the left-most square.

In this case, we may toggle the color of the square
to the right of the super-white one.

$$
\boxtimes \square \cdots \iff \boxtimes \blacksquare \cdots
$$

This is always possible if the colour the square on the right is black
($k \mapsto k+1$)
or if the colour is white and $k > 1$ ($k \mapsto k - 1$).
This mapping is an involution.

The exception is when $k = 1$ and the
first two square are super-white and white.
Out of the remaining $2n - 2$ squares, there must be $n - 1$ white ones
for a total of $n + k = n + 1$ light squares.
Thus, there are $\binom{2n - 2}{n - 1}$
ways for this to happen.
These are counted positively so we get the desired identity.

# Generalization

Why exactly $2n$ squares and not some arbitrary amount $s$?

Interesting enough,
both methods give rise to the following generalization:
$$
\sum_{k=0}^n (-1)^{k+1} k \binom{s}{n+k}
= \binom{s - 2}{n - 1}
$$

As a final remark, we see that
$$
\binom{2n}{n+k} = \binom{2n}{n-k}
$$
but from the generating function solution,
we find that
$$
\sum_{k=0}^n (-1)^{k+1} k \binom{s}{n+k}
= \sum_{k=0}^n (-1)^{k+1} k \binom{s}{n-k}.
$$

I hope everything above is correct.
Maybe I'll check over it later.
