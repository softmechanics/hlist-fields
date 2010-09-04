> {-# LANGUAGE TemplateHaskell #-}
> import Data.HList.Field
> import Data.HList.Field.Label (Label(..))
> import Data.HList.Tuple
> import Data.HList.Record (emptyRecord)

First, lets create some labels:

> $(mkLabel "l1")
> $(mkLabel "l2")

Here are some alternate was to create labels:

> $(mkLabels ["l3", "l4"])
> l5 = $(label "l5")

Note that while label and labels can only be used at the top level to create module-scope labels, 
labelE can be used in let bindings, etc.

Create a new record:

> r1 = emptyRecord .# l1 =: 1

Lookup:

> v1 = r1 # l1
> v1' = r1 # $(label "l1")

Update:

> r2 = r1 .# l1 =: 2

Nested fields:

> r3 = emptyRecord .# l1 =: (r2 .# l1 =: 3)
> r4 = r3 .# l1 .# l1 =: 4
> v4 = r4 # l1 # l1
> r5 = emptyRecord .# l1 =: (r5 .# l1 .# l1 =: 5)
> r6 = r5 .# l1 .# l1 .# l1 =: 6
> v6 = r6 # l1 # l1 # l1

(=~) updates a field from its current value.

> r7 = r6 .# l1 .# l1 .# l1 =~ (+1)
> v7 = r7 # l1 # l1 # l1

Set/update multiple fields:

> r8 = emptyRecord .# (l1 =: 1, l2 =: 2)
> r9 = r8 .# (l1 =: "hello world", l2 =~ (+1))
> r10 = emptyRecord .# l1 =: r9
> r11 = r10 .# l1 .# (l1 =~ words, l2 =~ (+1))
> v12 = tuple $ r11 # l1 # (l1, l2)

