CREATE TABLE IF NOT EXISTS articles (
  id VARCHAR PRIMARY KEY,
  slug VARCHAR NOT NULL UNIQUE,
  title VARCHAR NOT NULL,
  description VARCHAR NOT NULL,
  body TEXT NOT NULL,
  tags VARCHAR[] NOT NULL,
  author_id VARCHAR NOT NULL,
  favoritesCount INT DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ
);