---
title: Haskell is just like C
date: "2023-06-24"
---

> *Originally published in [mathNEWS 152.2](https://mathnews.uwaterloo.ca/wp-content/uploads/2023/06/mathNEWS-152-2.pdf)*

It’s common to think that programming in Haskell is very different from programming in C because Haskell is pure and functional while C is imperative and full of side effects.
However, with enough effort, they are more similar than you might think. 
Consider this simple C code:
```c
#include <stdio.h>
#include <stdlib.h>

struct Node {
    int val;
    truct Node* next;
};

struct Node* cons(int val, struct Node* next) {
    struct Node* node =
    malloc(sizeof(struct Node));
    node->val = val;
    node->next = next;
    return node;
}

int car(struct Node* node) {
    return node->val;
}

struct Node* cdr(struct Node* node) {
    return node->next;
}

void freeNode(struct Node* node) {
    if (node != NULL) {
        struct Node* next = node->next;
        free(node);
        freeNode(next);
    }
}

int main() {
    struct Node* l = cons(6, cons(9, NULL));
    printf("%d%d", car(l), car(cdr(l)));
    freeNode(l);
}
```

Suppose we want to translate this into Haskell. Well, Haskell does have `malloc` and `free` functions as well as `printf`. We need some sort of `null :: Ptr a` so we implement it as follows.
```hs
import Foreign
import System.IO.Unsafe

null :: Ptr a
null = unsafePerformIO $
    castPtr <$> (malloc :: IO (Ptr ()))
{-# NOINLINE null #-}
```

Not sure what the `NOINLINE` does but the language server suggested it.
Dereferencing this is, of course, consistent with what we expect from C because dereferencing null in C is undefined behaviour.

Now we make our `Node` data type and give it a `Storable`
instance. (I wish it was derivable but whatever.)
```hs
data Node = Node Int (Ptr Node)

instance Storable Node where
    sizeOf :: Node -> Int
    sizeOf _ = sizeOf (0 :: Int) + sizeOf null

    alignment :: Node -> Int
    alignment (Node n _) = alignment n
    
    peek :: Ptr Node -> IO Node
    peek p = do
        n <- peek $ castPtr p
        next <- peekByteOff p (sizeOf n)
        return $ Node n next
    
    poke :: Ptr Node -> Node -> IO ()
    poke p (Node n next) = do
        poke (castPtr p) n
        pokeByteOff p (sizeOf n) next
```

Next, it’s time to implement `cons`, `car` and `cdr`.
We introduce our own infix operators to prove a point on how similar the code is to the C code.
```hs
val (Node val _) = val
next (Node _ nxt) = nxt

(-->) :: Ptr Node -> (Node -> a) -> IO a
p --> f = f <$> peek p
(<--) :: Ptr Node -> Node -> IO ()
p <-- n = poke p n

cons :: Int -> Ptr Node -> IO (Ptr Node)
cons val next = do
    p <- malloc
    p <-- Node val next
    return p

car :: Ptr Node -> IO Int
car p = p --> val

cdr :: Ptr Node -> IO (Ptr Node)
cdr p = p --> next
```

Look at how similar the code is to the C code.
Let’s also implement `freeNode`:
```hs
import Control.Monad

freeNode :: Ptr Node -> IO ()
freeNode p =
    unless (p == null) $ do
        nxt <- p --> next
        free p
        freeNode nxt
```

Finally, we can translate `main`:
```hs
import Text.Printf

main :: IO ()
main = do
    l <- cons 6 =<< cons 9 null
    a <- car l
    b <- car =<< cdr l
    printf "%d%d" a b
    freeNode l
```

See, that was pretty similar to the original C code... right?
As a final remark, if you dislike the need to do everything in the `IO` monad, you can wrap everything in `unsafePerformIO`.
I’m sure it’ll be fine...
