DROP TABLE IF EXISTS words;
DROP TABLE IF EXISTS translations;

CREATE TABLE words (
    id INTEGER PRIMARY KEY,
    word TEXT NOT NULL UNIQUE,
    annotation TEXT,
);

CREATE TABLE translations (
    id INTEGER PRIMARY KEY,
    word_id INTEGER,
    translation TEXT NOT NULL,
    FOREIGN KEY(word_id) REFERENCES words(id)
);
