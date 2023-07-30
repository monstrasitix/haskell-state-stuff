CREATE TABLE IF NOT EXISTS user (
    id INTEGER PRIMARY KEY
    , firstName VARCHAR(255) NOT NULL
    , lastName VARCHAR(255) NOT NULL
);

INSERT INTO user (id, firstName, lastName) VALUES
    (1, "John", "Doe"),
    (2, "Sally", "Murphy"),
    (3, "Bob", "Dylan"),
    (4, "Sue", "Conor"),
    (5, "Mark", "Jones"),
    (6, "Katie", "Doe"),
    (7, "Michael", "Jordan"),
    (8, "Gwyn", "Murk");