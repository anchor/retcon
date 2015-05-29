Retcon Data Sources
===================

This document describes the interface to be implemented by a Retcon data source
program.

Overview
--------

A *data source* is a program which Retcon can invoke when it needs to interact
with an external system. Each invocation of a data source processes a single
operation and they can be implemented in any language.

Data sources should implement the four CRUD operations:

- Create a new document;
- Read an existing document;
- Update an existing document; and
- Delete an existing document.

While you can leave out some of the CRUD operations for a data source, this is
not safe in general and you should implement all four unless you know (and can
show to your colleagues) that the omitted operation/s will never be needed.

Formats
-------

The following data will be communicated between Retcon and a data source program:

- JSON documents describing data items.

- Text strings representing foreign keys.

JSON documents must follow the JSON specification and not include third-party
extensions (like `Date` values).

Foreign keys should be non-empty sequences of the following ASCII-safe
characters only:

- uppercase alphabetic;
- lowercase alphabetic ;
- numeric;
- `.` `-` `,` `_`;
- space.

Interface
---------

Data source programs should implement the following interface.

### General

In short:

- Did it work? You must exit with zero, and may log a message to stderr if you
like.

- Did it fail? You must exit with non-zero and must log a message to stderr.

All operations must exit with status 0 when they complete successfully. They
should produce any output required to the stdout stream and may produce
debugging messages to stderr. These messages will be logged by Retcon at the
DEBUG log level.

All operations must exit with status 1 when they failed because they could not
communicate with the external system. They must print a useful error message to
their stderr stream. These messages will be logged by Retcon at the ALERT log
level.

All operations which operate on an existing document must exit with status
2 when they failed because the document does not exist. They must print
a useful error message to their stderr stream. These messages will be logged by
Retcon at the ERROR log level.

Operations which otherwise fail must exit with either status 3 or a status
above 10, and must print a useful error message to their stderr stream. These
message will be logged by Retcon at the ERROR level.

### Create

The create operation inserts a new data item into a data source.

The JSON of the new data item is provided on stdin.

The data source must output the foreign key of the new data item on its stdout.

The data source must exit with status 1 if it could not communicate with the
external system.

The data source must exit with status 3 if the external system failed to create
the new data item.

### Read

The read operation retrieves a data item from the data source.

The foreign key of the data item to retrieve is passed as a command-line argument.

The data source must output JSON of the data item on its stdout.

The data source must exit with status 1 if it could not communicate with the
external system.

The data source must exit with status 2 if the requested data item does not
exist in the external system.

The data source must exit with status 3 if the requested data item does exist
but could not be read.

### Update

The update operation replaces a data item with a modified copy of the data item.

The foreign key of the data item to updated is passed as a command-line argument.

The new JSON of the data item is provided on stdin.

Some external systems will change the foreign key every time an update occurs.
If the foreign key is changed by the update operation, the data source must
output the new foreign key on its stdout.

The data source must exit with status 1 if it could not communicate with the
external system.

The data source must exit with status 2 if the requested data item does not
exist in the external system.

The data source must exit with status 3 if the external system failed to update
the data item.

### Delete

The delete operation deletes a data item from a data source.

The foreign key of the data item to updated is passed as a command-line argument.

The data source must exit with status 1 if it could not communicate with the
external system.

The data source must exit with status 2 if the requested data item does not
exist in the external system. This requirement exists so that Retcon can notice
that the *pre*-condition to this operation was violated, even though the
*post*-condition is met.

The data source must exit with status 3 if the external system failed to delete
the data item.
