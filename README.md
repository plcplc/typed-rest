typed-rest
==========

A suite of Haskell packages for HTTP servers and clients with typesafe REST
interfaces.

The central contribution of these packages are the typed representation of REST
resources, and machinery that uses these resource types to guide the generation
of clients that can request them and servers that can serve them.

This means translating HTTP URIs to haskell types and generating corresponding
client functions for requesting resources and server middleware for serving
resources as modeled by the types.

The state of these packages is highly experimental, and the documentation
assumes that the reader is comfortable with the `DataKinds` extension and type
families.

Client Example
--------------

Given a REST interface to a web store with `/store/inventory` representing the
current storage inventory of the shop, for our purposes encoded as a JSON list
of item objects. And assume that `/store/inventory/<item>` refers to the
inventory status of a particular item identified by an id number in place of
`<item>`.

In our setting, this would be represented by the types (with imports and
instance declarations censored for ease of display):

    import Network.HTTP.Rest.Signature

    data StoreItem = StoreItem {...}

    inventoryRes = RestResource
      :: RestSig
        (S "store" :/: S "inventory" :/: Nil)
        ('HttpGet [StoreItem])

    inventoryItemRes = RestResource
      :: RestSig
        (S "store" :/: S "inventory" :/: A "itemNumber" Int :/: Nil)
        ('HttpGet [StoreItem])

Now `inventoryRes` represents the API of the inventory resource. As such, the
name `inventoryRes` is interesting only inasmuch as it is a witness of its
type, which is used to drive instance selection when synthesizing client
functions or server middleware.

To actually use the `inventoryRes` (for example as a client), we rely on the
`RequestResource` typeclass (here as a ghci session) :

    > :set -XOverloadedStrings
    > :set -XTypeOperators
    > :set -XDataKinds
    > import Network.HTTP.Rest.Client

    > :t requestResourceDef "example.com" inventoryRes
    requestResource "example.com" inventoryRes
      :: RequestPathSig (S "store" :/: S "inventory") (RequestMethodSig ('HttpGet [StoreItem])

    > :kind! RequestPathSig (S "store" :/: S "inventory") (RequestMethodSig ('HttpGet [StoreItem])
    RequestPathSig (S "store" :/: S "inventory") (RequestMethodSig ('HttpGet [StoreItem]) :: *
    = IO [StoreItem]

    > :t requestResourceDef "example.com" inventoryItemRes
    > :kind! RequestPathSig (S "store" :/: S "inventory" :/: A "itemNumber" Int :/: Nil) (RequestMethodSig ('HttpGet [StoreItem])
    RequestPathSig (S "store" :/: S "inventory" :/: A "itemNumber" Int :/: Nil) (RequestMethodSig ('HttpGet [StoreItem]) :: *
    = Int -> IO [StoreItem]

The `requestResourceDef` function construct requestor actions given a hostname
and a `RestSig`. The type of a requestor function is computed by the associated
type families `RequestPathSig` and `RequestMethodSig`. The resulting types are
accessible to the programmer by means of the `:kind!` ghci command.

Informally, the resulting type of `RequestPathsig` can be described as taking
as arguments each of the `'A "variable" a` parts of a http path (i.e. ignoring
`'S "..."` components), ending with the type of the request payload and
resulting in `IO responesType` (All determined by `RequestMethodSig`. Example:

    > :kind! RequestPathSig
        (A "foo" Int :/: S "bar" :/: A "baz" (Maybe Bool) :/: Nil)
        (RequestMethodSig (HttpPost String Int))
    RequestPathSig
      (A "foo" Int :/: S "bar" :/: A "baz" (Maybe Bool) :/: Nil)
      (RequestMethodSig (HttpPost String Int)) :: *
    = Int -> Maybe Bool -> String -> IO Int

Server Example
--------------

(TODO!)

Building
--------

NixOS nix-expressions are provided for each package, along with a
project-global `default.nix` that just wires up each package independently. No
attempt has yet been made for integration with the existing haskell packaging
infrastructure in NixOS.

Regular linux distributions (or maybe even entirely different operating
systems) should just be able to install via `cabal`, with the caveat that this
is untested by the package author, and I cannot guarantee any support. However,
I would be very surprised if this was out of reach for a moderately experienced
cabal user.

Notifications on observed upper and lower bounds different from those recorded
in the cabal files though (as well as other bugs in general) is greatly
appreaciated.
