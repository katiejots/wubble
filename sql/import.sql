DROP TABLE IF EXISTS definition;
DROP TABLE IF EXISTS wordset;
DROP TABLE IF EXISTS wordset_definition;

CREATE TABLE definition (
                         id INTEGER PRIMARY KEY
                       , word VARCHAR NOT NULL UNIQUE
                       , meaning VARCHAR NOT NULL
                       );

CREATE TABLE wordset (
                      id INTEGER PRIMARY KEY
                    , name VARCHAR NOT NULL UNIQUE
                    , topic VARCHAR NOT NULL
                    );

CREATE TABLE wordset_definition (
                                 wordset_id INTEGER
                               , definition_id INTEGER 
                               , PRIMARY KEY (wordset_id, definition_id)
                               );

INSERT INTO definition (id, word, meaning) VALUES (1, 'higher-order function', 'takes another function as an argument or returns one as a result');
INSERT INTO definition (id, word, meaning) VALUES (2, 'immutable data', 'values that cannot be changed');
INSERT INTO definition (id, word, meaning) VALUES (3, 'referential transparency', 'a property of expressions that can be replaced with their value without changing the behaviour of a program');
INSERT INTO definition (id, word, meaning) VALUES (4, 'lazy evaluation', 'deferring the computation of values until they are needed');
INSERT INTO definition (id, word, meaning) VALUES (5, 'monad', 'a structure that represents computations defined as sequences of steps');
INSERT INTO definition (id, word, meaning) VALUES (6, 'aye-aye', 'a singular nocturnal quadruped, the lemur found in Madagascar and remarkable for its long fingers, sharp nails, and rodent-like incisor teeth');
INSERT INTO definition (id, word, meaning) VALUES (7, 'patagonian mara', 'a herbivorous, somewhat rabbit-like animal found in open and semi-open habitats in Argentina');
INSERT INTO definition (id, word, meaning) VALUES (8, 'gerenuk', 'a type of long-necked gazelle native to central and eastern Africa');
INSERT INTO definition (id, word, meaning) VALUES (9, 'babirusa', 'any of several mammals in the genus Babyrousa in the pig family Suidae, in which the upper tusk grows upward');
INSERT INTO definition (id, word, meaning) VALUES (10, 'lamprey', 'a long slender primitive eel-like freshwater and saltwater fish with a sucking mouth with rasping teeth but no jaw');
INSERT INTO definition (id, word, meaning) VALUES (11, 'fossa', 'a carnivorous mammal endemic to Madagascar');
INSERT INTO definition (id, word, meaning) VALUES (12, 'sunda colugo', 'a flying lemur found throughout Southeast Asia');
INSERT INTO definition (id, word, meaning) VALUES (13, 'lugubrious', 'mournful, gloomy');
INSERT INTO definition (id, word, meaning) VALUES (14, 'viscous', 'having a thick, sticky consistency between solid and liquid');
INSERT INTO definition (id, word, meaning) VALUES (15, 'clogged', 'having an obstructed flow');
INSERT INTO definition (id, word, meaning) VALUES (16, 'moist', 'slightly wet');
INSERT INTO definition (id, word, meaning) VALUES (17, 'bulbous', 'having the shape of or resembling a bulb');

INSERT INTO wordset (id, name, topic) VALUES (1, 'Functional Programming 101', 'programming');
INSERT INTO wordset (id, name, topic) VALUES (2, 'Strange Animals', 'wildlife');
INSERT INTO wordset (id, name, topic) VALUES (3, 'Unpleasant Adjectives', 'literature');

INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (1, 1);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (1, 2);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (1, 3);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (1, 4);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (1, 5);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 6);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 7);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 8);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 9);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 10);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 11);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (2, 12);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (3, 13);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (3, 14);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (3, 15);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (3, 16);
INSERT INTO wordset_definition (wordset_id, definition_id) VALUES (3, 17);
