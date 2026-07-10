# Market Data — SQLite Database Guide

## File naming

Each database file covers one trading day and is named by bar size and date:

```
bars_15min_20260612.db
```

---

## Table naming

Each contract is stored in its own table named `{PRODUCT}_{TENOR}`:

| Product | Code | Example tables |
|---|---|---|
| Brent (ICE) | `CO` | `CO_N26`, `CO_Q26`, `CO_U26` ... |
| WTI (CME) | `CL` | `CL_N26`, `CL_Q26`, `CL_U26` ... |

---

## Schema

Every table has the same structure:

```sql
CREATE TABLE "CO_N26" (
    timestamp TEXT PRIMARY KEY,  -- bar open time, UTC: "YYYY-MM-DD HH:MM:SS"
    open      REAL NOT NULL,
    high      REAL NOT NULL,
    low       REAL NOT NULL,
    close     REAL NOT NULL,
    volume    REAL NOT NULL
);
```

- `timestamp` is the **start** of the bar in UTC
- Rows only exist where trades occurred — no empty rows for quiet periods
- All prices are in the contract's native units

---

## Session hours

Bars only cover exchange trading hours. Ticks outside these windows are excluded.

| Product | Exchange | Session |
|---|---|---|
| CO (Brent) | ICE | 01:00 – 23:00 London time |
| CL (WTI) | CME | 17:00 – 16:00 Chicago time (spans midnight) |

---

## Querying the database

### Python (sqlite3)

```python
import sqlite3

conn = sqlite3.connect(r"I:\Public\Siddharth Raj\lightstreamer_data\bars_15min_20260612.db")

# List all available contracts
tables = conn.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name").fetchall()
print([t[0] for t in tables])

# Query a contract
rows = conn.execute('SELECT * FROM "CO_N26" ORDER BY timestamp').fetchall()
for row in rows:
    print(row)

conn.close()
```

### Python (pandas)

```python
import sqlite3
import pandas as pd

conn = sqlite3.connect(r"I:\Public\Siddharth Raj\lightstreamer_data\bars_15min_20260612.db")

df = pd.read_sql('SELECT * FROM "CO_N26" ORDER BY timestamp', conn, parse_dates=["timestamp"])
df = df.set_index("timestamp")
print(df.head())
```

### GUI (no code)

Download **DB Browser for SQLite** from https://sqlitebrowser.org — open the `.db` file directly to browse tables and run queries.

---

## Example SQL queries

```sql
-- All bars for a contract
SELECT * FROM "CO_N26" ORDER BY timestamp;

-- Today's bars only
SELECT * FROM "CO_N26"
WHERE timestamp >= date('now')
ORDER BY timestamp;

-- Daily OHLCV summary
SELECT
    date(timestamp)  AS date,
    MIN(open)        AS session_open,
    MAX(high)        AS session_high,
    MIN(low)         AS session_low,
    MAX(close)       AS session_close,
    SUM(volume)      AS total_volume
FROM "CO_N26"
GROUP BY date(timestamp)
ORDER BY date;

-- Brent vs WTI spread
SELECT
    b.timestamp,
    b.close          AS brent_close,
    w.close          AS wti_close,
    b.close - w.close AS spread
FROM "CO_N26" b
JOIN "CL_N26" w ON b.timestamp = w.timestamp
ORDER BY b.timestamp;
```
