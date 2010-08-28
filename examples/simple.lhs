> import Data.HList.Field hiding (tuple)
> import Data.HList.Tuple

First, lets create a label:

> data L1 = L1 
> instance Show L1 where
>   show _ = "L1"
> l1 = Label L1

Note the '_' in the definition of show for L1.  This definition doesn't 
evalaute the run-time value of the argument, which is necessary since 
field labels are always undefined.

Quick function to make us a new one-element record:

> r1 = emptyRecord .# l1 =: 1

Here we show two methods of updating fields.

> r2 = (l1 =: 2) r1
> r3 = r2 .# l1 =: 3

We can still lookup using (#) from HList.

> v3 = r2 # l1

Here are some nested fields.

> r4 = emptyRecord .# l1 =: (r3 .# l1 =: 4)
> r5 = r4 .# l1 .# l1 =: 5
> v5 = r5 # l1 # l1
> r6 = emptyRecord .# l1 =: (r5 .# l1 .# l1 =: 6)
> r7 = r6 .# l1 .# l1 .# l1 =: 7
> v7 = r7 # l1 # l1 # l1

(=~) updates a field from its current value.

> r8 = r7 .# l1 .# l1 .# l1 =~ (+1)
> v8 = r8 # l1 # l1 # l1

That's pretty much it!  

