-- D1 schema for indexed GQL queries and metadata

CREATE TABLE blocks (
  id        TEXT PRIMARY KEY,
  height    INTEGER NOT NULL UNIQUE,
  timestamp INTEGER NOT NULL,
  previous  TEXT NOT NULL DEFAULT ''
);
CREATE INDEX idx_blocks_height ON blocks(height);

CREATE TABLE txs (
  id           TEXT PRIMARY KEY,
  block_id     TEXT NOT NULL,
  block_height INTEGER NOT NULL,
  owner        TEXT NOT NULL DEFAULT '',
  recipient    TEXT NOT NULL DEFAULT '',
  anchor       TEXT NOT NULL DEFAULT '',
  signature    TEXT,
  data_size    TEXT NOT NULL DEFAULT '0',
  data_type    TEXT NOT NULL DEFAULT '',
  bundle_id    TEXT,
  parent_id    TEXT
);
CREATE INDEX idx_txs_owner ON txs(owner);
CREATE INDEX idx_txs_recipient ON txs(recipient);
CREATE INDEX idx_txs_block_height ON txs(block_height);

CREATE TABLE tx_tags (
  tx_id TEXT NOT NULL,
  name  TEXT NOT NULL,
  value TEXT NOT NULL
);
CREATE INDEX idx_tx_tags_name_value ON tx_tags(name, value);
CREATE INDEX idx_tx_tags_tx_id ON tx_tags(tx_id);

CREATE TABLE addrmap (
  address TEXT PRIMARY KEY,
  key     TEXT NOT NULL DEFAULT ''
);

CREATE TABLE modules (
  name    TEXT PRIMARY KEY,
  wasm_id TEXT NOT NULL
);

CREATE TABLE wasms (
  id      TEXT PRIMARY KEY,
  format  TEXT NOT NULL DEFAULT 'wasm64-unknown-emscripten-draft_2024_02_15',
  file    TEXT,
  variant TEXT
);
