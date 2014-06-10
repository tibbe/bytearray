Purpose
=======

Portable low level interface, on par with the FFI's Foreign libraries, but
instead of access to foreign memory with Ptrs, it's low level access to
memory within the GC'd heap.

It is not intended to be an application level type, e.g. array of Word16. It
will allow heterogeneous primitive types to be stored.


Details
=======

ST and RealWorld
------------------

We rely on ST for handling mutable byte arrays. All Haskell implementations
that we know of except nhc98 support ST.

One concern that we had is that while in GHC there is a very close connection
between ST and IO, this may not be so in other implementations. In particular
GHC supports the RealWorld type and stToIO :: ST RealWorld a -> IO a.
However we surveyed hugs, jhc and uhc and all of them support both RealWorld
and stToIO. In addition both of these are described in the original 1994 paper
on ST. So we feel we can rely on this as standard practice.

This means that we can support mixing ST and IO, e.g. with copying between
mutable byte arrays and pointers.


Array copies
--------------

We have several types of array, plus FFI `Ptr`s. We want to allow copying
memory between all combinations. There's `ByteArray`, `MutableByteArray` and
`Ptr`. This gives 9 combinations, except that we don't need to provide `Ptr`
to `Ptr` copies here as that is provided by the FFI libraries.


Freeze and unfreeze
---------------------

Some Haskell implementations, such as GHC, support O(1) freeze and unfreeze
that reuse the same memory area. This may not be possible on all compilers
or targets. So some implementations may need to implement O(n) copying freeze
and unfreeze.

Consider an API user that wants to do a safe copying freeze. If the API provides
only one freeze then on an implementation with O(n) freeze, you would end up
copying twice. This would tend to suggest that we should provide an API that
includes safe and unsafe versions of freeze and unfreeze to allow both to be
optimal. However we don't actually need to provide copying and potentially
non-copying forms of freeze because we provide a ByteArray copy function that
constructs a new immutable ByteArray from (a slice of) a MutableByteArray. So
safe copying freeze can be implemented with a single copy that way, and we only
need a single freeze primitive that may be O(1) or O(n).


Bikeshedding
============

Names and location

Data.ByteArray           -- ByteArray and MutableByteArray
Data.ByteArray.Unsafe?   -- the unsafe ops, maybe all of them?
Data.ByteArray.IO?       -- IO wrapper of ST

Data.BoxedArray
Data.BoxedArray.Unsafe?

Data.UnboxedArray        -- wrap ByteArray with phantom type param, 
Data.UnboxedArray.Unsafe?

The unboxed array is a layer on top of Data.ByteArray that can be used
for applications e.g. Data.Text might use UnboxedArray Word16. It uses
a phantom type to ensure it's always indexed at the right type (where as
ByteArray allows heterogeneous access).
