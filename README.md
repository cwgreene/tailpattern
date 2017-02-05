# Tailpattern

Tails files of a given pattern in a directory, including files that
don't exist yet.  This was created because YouCompleteMe generates log
files with unique, randomly generated names in /tmp/ for server logging,
making testing changes annoying since I can't just keep a terminal window
open which is always emitting debug output.

Example (proposed API):

```
  tailpattern '/tmp/tsserver_*log'
  tailpattern -e '/tmp/tsserver_(.*)log\1'
```

The first is glob based, the second uses regular expressions and capture
groups.  I don't yet know if capture groups will be able to be used across
multiple patterns or not.  This could be useful for situations where you
have a key used in multiple files (like stderr.1.log and stdout.1.log)
and you want to group them.

## Why Haskell?

Done in haskell, mostly because doing asynchronous callbacks is a pain in
python; and the alternative was basically this, C++, or node. I suppose
Java. Maybe that one next time.

Wanted to get a better idea of how to do this type of program in haskell,
so this is why it's in haskell.
