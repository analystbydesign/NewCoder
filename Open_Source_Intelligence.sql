


CREATE TABLE youtube_osint (
    id SERIAL PRIMARY KEY,
    video_id TEXT,
    title TEXT,
    description TEXT,
    channel_title TEXT,
    published_at TIMESTAMP,
    collected_at TIMESTAMP
);

-- ----------------------------------------
-- TABLE: osint_results
-- Stores cleaned OSINT results per video/comment

CREATE TABLE osint_results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    video_id TEXT,
    source_query TEXT,
    keyword TEXT,
    cleaned_text TEXT,
    sentiment_neg REAL,
    sentiment_neu REAL,
    sentiment_pos REAL,
    sentiment_compound REAL,
    risk_score REAL,
    collected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- ----------------------------------------
-- Seed keywords (canonical → weight)
-- ----------------------------------------

CREATE TABLE defence_keywords (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    canonical_keyword TEXT UNIQUE,
    weight INTEGER
);

INSERT INTO defence_keywords (canonical_keyword, weight) VALUES
('counterterrorism', 3),
('cyber defence', 2),
('artificial intelligence', 1),
('electronic warfare', 3),
('information operations', 3),
('cyber attacks', 4);



--------next after python
def insert_result(conn, video_id, source_query, analysis):
    cursor = conn.cursor()
    for kw in (analysis["keywords"] or ["none"]):
        cursor.execute("""
            INSERT INTO osint_results (
                video_id,
                source_query,
                keyword,
                cleaned_text,
                sentiment_neg,
                sentiment_neu,
                sentiment_pos,
                sentiment_compound,
                risk_score
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            video_id,
            source_query,
            kw,
            analysis["cleaned_text"],
            analysis["sentiment"]["neg"],
            analysis["sentiment"]["neu"],
            analysis["sentiment"]["pos"],
            analysis["sentiment"]["compound"],
            analysis["risk_score"],
        ))
    conn.commit()







