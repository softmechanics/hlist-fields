> import Data.HList.Field hiding (tuple)
> import Data.HList.Tuple

First, lets create a label:

> data L1 = L1 
> instance Show L1 where
>   show _ = "L1"
> l1 = Label L1

Note the '_' in the definition of show for L1.  This doesn't evalaute 
the run-time value of the argument, which is necessary since field 
labels are always undefined.

Create a new record:

> r1 = emptyRecord .# l1 =: 1

Lookup:

> v1 = r1 # l1

Update:

> r2 = (l1 =: 2) r1
> r3 = r2 .# l1 =: 3

Nested fields:

> r4 = emptyRecord .# l1 =: (r3 .# l1 =: 4)
> r5 = r4 .# l1 .# l1 =: 5
> v5 = r5 # l1 # l1
> r6 = emptyRecord .# l1 =: (r5 .# l1 .# l1 =: 6)
> r7 = r6 .# l1 .# l1 .# l1 =: 7
> v7 = r7 # l1 # l1 # l1

(=~) updates a field from its current value.

> r8 = r7 .# l1 .# l1 .# l1 =~ (+1)
> v8 = r8 # l1 # l1 # l1

Set/update multiple fields:

> data L2 = L2
> instance Show L2 where
>   show _ = "L2"
> l2 = Label L2

> r9 = emptyRecord .# (l1 =: 1, l2 =: 2)
> r10 = r9 .# (l1 =: "hello world", l2 =~ (+1))
> r11 = emptyRecord .# l1 =: r10
> r12 = r11 .# l1 .# (l1 =~ words, l2 =~ (+1))

