Description:
Credit / Follow Up:

This spec acts as an easier / simpler van Laarhoven prelude to the more general problem of implementing various Profunctor Optics (such as lenses, traversals, prisms etc.). For that problem, I recommend you check out user tel's kata Lensmaker (1 kyu)! If you solve this kata, give that one a try.
The Basics: Lens' s a

A Lens of type Lens' s a is a very general way to deal with reading and updating some (single, always-present) "focus" value a within a "source" value s. For example, the lenses _1 and _2 make it easy to access the first and second elements of a 2-tuple:

_1 :: Lens' (a, x) a -- source `s` is `(a, x)`, focus `a` is first el
_2 :: Lens' (x, a) a -- source `s` is `(x, a)`, focus `a` is second el
-- implementations elided

specs = do
    view _1         ("hi", 5)  `shouldBe`  "hi"
    over _1 (++"!") ("hi", 5)  `shouldBe`  ("hi!", 5)
    set  _1 "yo"    ("hi", 5)  `shouldBe`  ("yo", 5)

    view _2      ("hi", 5)  `shouldBe`  5
    over _2 (+1) ("hi", 5)  `shouldBe`  ("hi", 6)
    set  _2 1500 ("hi", 5)  `shouldBe`  ("hi", 1500)

Using Lenses: view, over, and set

Three of the most common helper functions used with lenses include:

    view :: Lens' s a -> s -> a: extract a focus a from a source s
    over :: Lens' s a -> (a -> a) -> s -> s: using a mapper a -> a, immutably update a focus a within a source s
    set :: Lens' s a -> a -> s -> s: immutably overwrite a focus a within a source s

Enabling Polymorphism: Lens s t a b

Unfortunately, a Lens' s a is overly restrictive in that we are not allowed to change the type of our focus. If we could change the focus from a -> b, then the source data s itself would have to change from s -> t. For example, changing the focused second element of a tuple from Int -> Bool changes the source tuple type from (x, Int) -> (x, Bool).

Accordingly, the more general type for lenses is Lens s t a b:

    given some source of type s,
    containing a focus value of type a,
    a lens can allow the user to change the focus to type b,
    generating a new source value of type t.

Said slightly differenty, a Lens s t a b:

    can transform a source s to a source t,
    by transforming a focus a to a focus b

_1 :: Lens (a, x) (b, x) a b -- source `s` is `(a, x)`, new src `t` is `(b, x)`
_2 :: Lens (x, a) (x, b) a b -- source `s` is `(x, a)`, new src `t` is `(x, b)`
-- implementations elided

specs = do
    view _1      ("hi", 5)  `shouldBe`  "hi"
    over _1 head ("hi", 5)  `shouldBe`  ('h', 5)  -- focus `String -> Char`
    set  _1 True ("hi", 5)  `shouldBe`  (True, 5) -- focus `String -> Bool`

    view _2      ("hi", 5)  `shouldBe`  5
    over _2 even ("hi", 5)  `shouldBe`  ("hi", False) -- focus `Int -> Bool`
    set  _2 0.5  ("hi", 5)  `shouldBe`  ("hi", 0.5)  -- focus `Int -> Float`

The Full Lens Type

The full type definition of Lens we will use is the version devised by Twan van Laarhoven, and is provided in the kata.

type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)

It might help to read this type as follows:

    "If you give me a way to put the (modified) focus into a particular functor,"
    "I will give you a way to put the (modified) source into that functor."

At first glance, this type might seem surprisingly unrelated to viewing, updating, and setting a focus within a source. And yet, by supplying the right functor instance and doing a little wiring, we can define the view, update, and set functions merely by following the types.

Figuring out how to do so is the subject of this challenge.
The Payoff: Lenses Compose!

A great thing about lenses is that they compose directly, using ordinary function composition (.). In this way, you can "drill down" through multiple nested types and create an easy API to read and update deeply buried values.

Note: perhaps counter-intuitively, the "deeper" lens goes on the right!

secondThenFirst :: Lens (x, (a, y)) (x, (b, y)) a b
secondThenFirst = _2 . _1

spec = do
    view secondThenFirst    (1, (True, "hi"))  `shouldBe`  True
    set  secondThenFirst [] (1, (True, "hi"))  `shouldBe`  (1, ([], "hi"))

Longer chains allow simulating OOP-style "dot access" to nested records:

newPerson = set (friend.pet.name) "Rex" person

-- or using operators from the Lens library:
newPerson2 = person & friend.pet.name .~ "Rex"

The Kata

    Finish implementing the two functor instances provided, which you will need
    Implement view, over, and set, for which you will want those functors
    Implement some example lenses like _1, _2, _name, and _celsius
    Demonstrate your knowledge of lens composition with the _1_1_1_name lens
    Implement the lens function, which makes lenses from getter-setter pairs

Tips:

    Follow the types. Carefully following the types involved will almost solve the specs for you!
    Read the specs. All of the specs that you will be tested against are visible in the example spec section; they may help contextualize how your functions are being used. Functions prefaced with L. are from the real lens library.
    We turn on TupleSections to make some of the tuple functions easier; the function (,x) is equivalent to \a -> (a, x). This is purely a convenience and not mandatory to solve the specs.

