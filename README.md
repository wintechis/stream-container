# Stream Container

## Running
```
$ ./stream-container --help
Usage: index.js [-H|--hostname HOSTNAME] [-P|--port PORT] [-p|--predicate URI]
                [-g|--grace SECONDS]
                [-d|--data-provider SUBJECT_URI OBJECT_LIST]
                [-m|--member-relation URI] [-t|--content-timestamp-relation URI]
                [-r|--poison-relation URI] [-c|--content-poison-relation URI]
                [-w|--window MEMBERSHIP_RESOURCE START END]
  Start a Stream Container web server

Available options:
  -H,--hostname HOSTNAME   The hostname under which the stream container shall
                           run. (default: localhost)
  -P,--port PORT           The port for the stream container to listen
                           on. (default: :8080)
  -p,--predicate URI       Predicate that is assigned to the stream container
                           (purely informative).
  -g,--grace SECONDS       How many seconds should data points be retained after
                           they are out of the last window. (default: 10)
  -d,--data-provider SUBJECT_URI OBJECT_LIST
                           Subject and list of objects that the stream container
                           should simulate.
  -m,--member-relation URI The
                           ldp:hasMemberRelation. (default: <http://vocab.ex.org/inWindow>)
  -t,--content-timestamp-relation URI
                           The
                           ldpsc:contentTimestampRelation. (default: <http://vocab.ex.org/hasTimestamp>)
  -r,--poison-relation URI The
                           ldpsc:hasPoisonRelation. (default: <http://vocab.ex.org/isPoisoned>)
  -c,--content-poison-relation URI
                           The
                           ldpsc:contentPoisonRelation. (default: <http://vocab.ex.org/hasPoison>)
  -w,--window MEMBERSHIP_RESOURCE START END
                           A window for the Stream Container to start with.
  -h,--help                Show this help text
```

## Building
You need a [PureScript](https://www.purescript.org/) compiler and [Spago](https://github.com/purescript/spago) installed.

```
spago install
spago build
```
