-- | SQL definition for the MMR type
-- Assuming a relational database schema, the MMR can be represented as follows:
--
-- SQL instructions to create the tables:


-- Table: mmr_rights
CREATE TABLE mmr_rights (
    hash BYTEA NOT NULL,
    right_hash BYTEA NOT NULL,
    PRIMARY KEY (hash)
);

-- Table: mmr_lefts
CREATE TABLE mmr_lefts (
    hash BYTEA NOT NULL,
    left_hash BYTEA NOT NULL,
    PRIMARY KEY (hash)
);

-- Table: mmr_orphans
CREATE TABLE mmr_orphans (
    level INT NOT NULL,
    hash BYTEA NOT NULL,
    PRIMARY KEY (level)
);

