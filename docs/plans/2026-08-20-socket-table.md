# A socket is a kernel object, not a file description

Preparatory to `SystemNative_Bind`. No guest-visible behaviour changes: this
moves where a socket lives, and adds the invariant that keeps the two homes in
step.

## Why now

`OpenFileTarget.Socket` carried the whole `SocketDescription`, and that type's
own docstring recorded the limit:

> A socket that must outlive or precede every descriptor — a completed connection
> waiting in a listening socket's backlog — would break that, and wants the table.

`SystemNative_Accept` produces exactly such a socket. `Bind` and `Listen` do not,
so this could in principle have waited — but bind adds the first *mutable* socket
state, and adding it to the wrong home first means moving more later.

It is deliberately its own change, with no behaviour attached. Landing it with
`Bind` would have made one PR out of a representation migration, three entry
points, two `KernelConfig` knobs, a typed-address codec and a per-flavour
conflict model.

## What moved

| before | after |
| --- | --- |
| `OpenFileTarget.Socket of SocketDescription` | `OpenFileTarget.Socket of SocketId` |
| `SocketDescription.Id` | the key of `EmulatedKernel.Sockets` |
| `FileDescriptorRegistry.NextSocketId` | `EmulatedKernel.NextSocketId` |
| `FileDescriptorRegistry.createSocket domain kind protocol` | `EmulatedKernel.createSocket domain kind protocol`, which mints the identity and allocates the descriptor together |
| `FileDescriptorRegistry.close` | returns the description it destroyed, if any; `EmulatedKernel.closeFd` wraps it and frees the socket |

`SocketDescription` loses its `Id` rather than keeping it beside the key: a field
that duplicates the key is free to disagree with it.

`FileDescriptorRegistry.close` reporting what it destroyed is the part with teeth.
The registry cannot free a socket itself — it compiles before the kernel that
holds them — and the caller cannot infer it from the descriptor, because closing
one of two descriptors onto a socket must free nothing. So the fact that has to
cross the boundary is "a description died", which is what the new return value
says.

## The invariant, and that it is temporary

`EmulatedKernel.checkInvariants` rejects three states:

- `DanglingSocket` — a description names a socket the table does not hold. This
  is what makes `EmulatedKernel.socket` total: it is a lookup that fails loudly
  rather than returning an option, and this clause is why no caller has to handle
  the `None`.
- `UnreferencedSocket` — the table holds a socket no description names. **This
  clause is the one `SystemNative_Accept` will relax**, to "named by a
  description or held in some backlog": a completed connection is precisely a
  socket with no descriptor yet. Until then it catches a close that forgot to
  clean up.
- `NextSocketIdNotFresh` — relocated from `FileDescriptorRegistryDefect`, and now
  stated against the table rather than against the descriptions, which is where a
  socket lives.

`FileDescriptorRegistryDefect.DuplicateSocketId` stays in the registry: two
descriptions naming one socket is a claim about descriptions, and stays checkable
without the kernel.

## Tests

`TestEmulatedKernelSockets.fs` is new and holds every claim that needs both
tables. Three of its tests are relocations, with their claims intact: the socket
triple, the `NextSocketId` freshness pair. Two are new behaviour that only exists
because the tables are now separate — closing a `dup` leaves the socket alive,
and closing a non-socket descriptor leaves the socket table alone — and each of
those is a mutant that every other test in the file survives.

`TestFileDescriptorRegistry`'s random-mix property keeps its registry claims and
mints identities itself; the kernel file gains its own random-mix property over
`EmulatedKernel.createSocket`/`closeFd`, which is what connects the hand-forged
defects to the code paths that maintain them.
