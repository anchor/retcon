# datasource

## create

### notification received with document but no internal key
  * success: document created in datasource, internal key recorded
  * failure:
    - server: log error, doesn't do anything else.
    - datasource: left in whichever state the command left it in.

## read

### process a work item
  * success: dispatch create/update
  * failure: dispatch delete if there is an internal key, log error if there isn't.

### part of update
  * failure: does absolutely nothing, not even log.

### part of applying patch:
  * failure: log, then use the initial document instead.

## update

### notification received with internal key & document
  * success: merged document set for that datasource.
  * failure: log, that datasource remains in whichever state the update left it in.

### resolve patch received
  * success: merged patch applied to datasource.
  * failure: log, that datasource remains in whichever state the update left it in.

## delete
  * success: deleted from datasource.
  * failure: remove foreign key and pretend delete succeeded!

# async failures

## retcond

user spawns retcon server (1), which spawns an API server (2) and some peasant workers (3).

### worker die (exception):
  * API server: should keep going?
  * retcon server: also keep going

### last worker die:
  * API server: should die with the same exception?
  * retcon server: ditto

### API server die:
  * all workers: should ded 2?
  * retcon server: ditto

### retcon server die:
children all ded

# API errors

failure to complete a request cannot be communicated back to the client, but they must always be logged.