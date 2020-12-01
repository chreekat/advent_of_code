The problem can be solved naively with an unfold.

Let's write a simplified version of it.

There are two moons, A and B. They start with a particular position on the X
axis.

If they start at the same position, nothing ever happens.

Va Vb  Xa Xb  Ga Gb
 0  0   0  0   0  0

If they start with Xa = 0 and Xb = 1, Va becomes 1 and Vb, -1.

After a step, Xa becomes 1 and Xb, -1.

Their Vs drop to zero. After a step, they don't move. Now we've reached a
mirrored position, so things carry on this way forever.

Va Vb  Xa Xb  Ga Gb
 0  0   0  1   1 -1
 1 -1   1  0  -1  1
 0  0   1  0  -1  1
-1  1   0  1   1 -1
 0  0   0  1   1 -1
5

Now let's start at Xa=0 and Xb=4.

Va Vb  Xa Xb  Ga Gb
 0  0   0  4   1 -1
 1 -1   1  3   1 -1
 2 -2   3  1  -1  1
 1 -1   4  0  -1  1
 0  0   4  0  -1  1
-1  1   3  1  -1  1
-2  2   1  3   1 -1
-1  1   0  4   1 -1
 0  0   0  4   1 -1
9

Mirrored position, done.

Ok. Now let's do three moons, A B C.

Zeros, no change.

Two sharing a position: gives a higher G to the third.

Va Vb Vc  Xa Xb Xc  Ga Gb Gc
 0  0  0   0  0  1   1  1 -2
 1  1 -2   1  1 -1  -1 -1  2
 0  0  0   1  1 -1  -1 -1  2
-1 -1  2   0  0  1   1  1 -2
 0  0  0   0  0  1   1  1 -2
5

Va Vb Vc  Xa Xb Xc  Ga Gb Gc
 0  0  0   0  0  4   1  1 -2
 1  1 -2   1  1  2   1  1 -2
 2  2 -4   3  3 -2  -1 -1  2
 1  1 -2   4  4 -4  -1 -1  2
 0  0  0   4  4 -4  -1 -1  2
-1 -1  2   3  3 -2  -1 -1  2
-2 -2  4   1  1  2   1  1 -2
-1 -1  2   0  0  4   1  1 -2
 0  0  0   0  0  4   1  1 -2
9

Except this time we come back to the starting position with different 

Va Vb Vc  Xa Xb Xc  Ga Gb Gc
 0  0  0   0  1  4   2  0 -2
 2  0 -2   2  1  2   2  0 -2
 1  2 -3   3  3 -1  -1  2 -1
 0  1 -1   3  4 -2  -1 -1  2
 0 -1  1   3  3 -1   0 -2  2
-1 -2  3   2  1  2  -1 -1  2
-2  0  2   0  1  4  -1  2 -1
 0  0  0   0  1  4   2  0 -2
8

Va Vb Vc  Xa Xb Xc  Ga Gb Gc
 0  0  0   0  2  4   2  0 -2
 2  0 -2   2  2  2   0  0  0
 2  0 -2   4  2  0  -2  0  2
 0  0  0   4  2  0  -2  0  2
-2  0  2   2  2  2   0  0  0
-2  0  2   0  2  4   2  0 -2
 0  0  0   0  2  4   2  0 -2
7

Va Vb Vc  Xa Xb Xc  Ga Gb Gc
 0  0  0   0  2  8   2  0 -2
 2  0 -2   2  2  6   1  1 -2
 3  1 -4   5  3  2  -2  0  2
 1  1 -2   6  4  0  -2  0  2
-1  1  0   5  5  0  -1 -1  2
-2  0  2   3  5  2   0 -2  2
-2 -2  4   1  3  6   2  0 -2
 0 -2  2   1  1  8   1  1 -2
 1 -1  0   2  0  8   0  2 -2
 1  1 -2   3  1  6   0  2 -2
 1  3 -4   4  4  2  -1 -1  2
 0  2 -2   4  6  0   0 -2  2
 0  0  0   4  6  0   0 -2  2
 0 -2  2   4  4  2  -1 -1  2
-1 -3  4   3  1  6   0  2 -2
-1 -1  2   2  0  8   0  2 -2
-1  1  0   1  1  8   1  1 -2
 0  2 -2   1  3  6   2  0 -2
 2  2 -4   3  5  2   0 -2  2
 2  0 -2   5  5  0  -1 -1  2
 1 -1  0   6  4  0  -2  0  2
-1 -1  2   5  3  2  -2  0  2
-3 -1  4   2  2  6   1  1 -2
-2  0  2   0  2  8   2  0 -2
 0  0  0   0  2  8   2  0 -2
25
