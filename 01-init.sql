CREATE USER tc_admin WITH PASSWORD 'tc_root_password';
CREATE USER tc_shiny_user WITH PASSWORD 'tc_shiny_pass';
CREATE USER tc_eng_user WITH PASSWORD 'tc_eng_pass';

CREATE DATABASE tc;
\c tc

CREATE TABLE IF NOT EXISTS public.panther_info (
    panther_id INT GENERATED ALWAYS AS IDENTITY,
    panther_sn VARCHAR(10),
    PRIMARY KEY(panther_id),
    UNIQUE(panther_sn)
);

CREATE TABLE IF NOT EXISTS public.tc_info(
    tc_id INT GENERATED ALWAYS AS IDENTITY,
    tc_sn VARCHAR(10),
    tc_pn VARCHAR(20),
    PRIMARY KEY(tc_id),
    UNIQUE(tc_sn, tc_pn)
);

CREATE TYPE scan_type AS ENUM ('peek', 'background', 'm_background');

CREATE TABLE IF NOT EXISTS public.scans(
    scan_id INT GENERATED ALWAYS AS IDENTITY,
    panther_id INT,
    tc_id INT,
    md5 VARCHAR(32),
    scan scan_type,
    start_timestamp TIMESTAMP,
    end_timestamp TIMESTAMP,
    file BYTEA,
    PRIMARY KEY(scan_id),
    CONSTRAINT fk_panther
      FOREIGN KEY(panther_id) 
	  REFERENCES panther_info(panther_id),
    CONSTRAINT fk_tc
      FOREIGN KEY(tc_id) 
	  REFERENCES tc_info(tc_id)
);

GRANT ALL PRIVILEGES ON DATABASE tc TO tc_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public to tc_admin;

GRANT CONNECT ON DATABASE tc to tc_shiny_user;
GRANT INSERT, SELECT ON ALL TABLES IN SCHEMA public TO tc_shiny_user;
GRANT CONNECT ON DATABASE tc to tc_eng_user;
GRANT INSERT, SELECT, UPDATE ON ALL TABLES IN SCHEMA public TO tc_eng_user;
-- tests
-- INSERT INTO scan_files VALUES ('test@test.com', 'Test*123');
-- INSERT INTO products (title, price, category) VALUES ('Truco', 9.90, 13);