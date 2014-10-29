--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | The `retcon` table is the primary store if a particular entity document in
-- the database.
CREATE TABLE retcon (
    entity VARCHAR(64) NOT NULL,
    id SERIAL NOT NULL,

    PRIMARY KEY (entity, id)
);

-- | The `retcon_fk` table records information about a particular entity
-- document as stored in a foreign system.
CREATE TABLE retcon_fk (
    entity VARCHAR(64) NOT NULL,
    id     INTEGER NOT NULL,
    source VARCHAR(64) NOT NULL,
    fk     TEXT NOT NULL,

    PRIMARY KEY (entity, source, fk),
    UNIQUE (entity, id, source),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id)
);

-- | The `retcon_initial` table records the "initial" document used in the last
-- run for a particular entity document.
CREATE TABLE retcon_initial (
    entity VARCHAR(64) NOT NULL,
    id     INTEGER NOT NULL,
    document JSON NOT NULL,

    PRIMARY KEY (entity, id),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id)
);

-- | The `retcon_diff` table records each diff merged into an initial document.
CREATE TABLE retcon_diff (
    entity    VARCHAR(64) NOT NULL,
    id        INTEGER NOT NULL,
    diff_id   SERIAL NOT NULL,
    submitted TIMESTAMP NOT NULL DEFAULT NOW(),
    is_conflict BOOLEAN NOT NULL DEFAULT FALSE,
    content   JSON NOT NULL,

    PRIMARY KEY (diff_id),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id)
);

-- | The `retcon_diff_conflicts` table records diff operations which could not
-- be merged into a diff due to conflicts.
CREATE TABLE retcon_diff_conflicts (
    operation_id SERIAL NOT NULL,
    diff_id   INTEGER NOT NULL,
    content   JSON NOT NULL,

    PRIMARY KEY (operation_id),
    FOREIGN KEY (diff_id) REFERENCES retcon_diff (diff_id)
);

-- | The `retcon_notifications` table records pending messages to notify humans of
-- conflicts.
CREATE TABLE retcon_notifications (
    entity  VARCHAR(64) NOT NULL,
    id      INTEGER NOT NULL,
    diff_id INTEGER NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (entity, id, diff_id),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id),
    FOREIGN KEY (diff_id) REFERENCES retcon_diff (diff_id)
);

-- | Enumeration of states for records in the `retcon_workitems` table.
CREATE TYPE queue_state AS ENUM (
    -- Items begin as NEW
    'new',
    -- Items are claimed by a worked and begin PROCESSING
    'processing',
    -- Items are COMPLETED
    'completed'
);

-- | the `retcon_workitems` table acts as a persistent queue for `WorkItem`s
CREATE TABLE retcon_workitems (
    id       SERIAL NOT NULL,
    content  JSON NOT NULL,

    -- Each item is a state machine.
    state queue_state DEFAULT 'new',

    PRIMARY KEY (id)
);
