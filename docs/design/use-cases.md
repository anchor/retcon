# Use cases

This document is to be used before any production release to verify that all of
the promised use cases are provided. The testing of these cases is automated
and may be run via:

	cabal test acceptance-test

The following use cases reference two data sources, *upstream* and *local*.

These are arbitrary and are named like so to provide a tangible example.

## Initial state

Before each test, we can assume an *upstream* and *local* data source
containing one common entity, *user*. These data sources have one entry in them
respectively, that correlate with each other. Retcon is in "sync" with these
entities and holds an initial document from both.

## Upstream change is propogated locally

The fields called *first-name* and *last-name* are modified in the *upstream*
data source.

A user now uses the enqueueChangeNotification library call to synchronise the
"local" data source. The upstream change is merged locally and both records
become congruent.

The user now issues a subsequent duplicate enqueueChangeNotification and
nothing further is changed.

## Upstream delete is handled

A record is deleted in the *upstream* data source, that deletion is propogated
*downstream* after a call to enqueueChangeNotification referencing the upstream
change.

## Comparing and resolving conflicting diffs from different sources

A record receives updates to the same fields from *local* and *upstream* at
the same time, and the changes conflict.

The user tries to synchronise the changes via enqueueChangeNotification and
then lists conflicts via getConflicted and the conflicting diff is shown. The
user now decides to keep the local change and pushes this choice via
enqueueResolveDiff.

A subsequent call to getConflicted returns an empty set and the change is
applied to both *upstream* and *downstream*.

### Send out notifications of conflicts

Current conflicts should be assembled in a listable form and then distributed
to system maintainers via a notification interface defined by Retcon's
configuration.

### Display original document and available pending diffs

The administrative interface will need to display the original document, and
offer a selection of diff operations that can be applied to update the
document. To do this, we'll need to be able to display the original document
as JSON, and we'll also a list of diff operations that apply to the document.
The document and list of diffs can then be displayed in a web (or other)
interface for staff to use and update.

### Submit selection of diffs to apply to the document

The administrative interface will submit a series of diff portion IDs to
apply to the document. Retcon will then retrieve the diff portions from the
database, perform the operations that each portion defines against the
original document, and then propagate the updated document to *local* and
*upstream*.
